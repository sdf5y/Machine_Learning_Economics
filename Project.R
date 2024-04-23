setwd("C:/Users/18045/Documents/R/ML/ML Project") #Sean's WD
#setwd("") #Nirav's WD
#setwd("") #Elise's WD
#setwd("") #Isaac's WD
#setwd("") #Truman's WD
#install.packages("clustMixType", "wesanderson")
library(readr)
library(data.table)
library(readxl)
library(tidyverse)
library(formattable)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(scales)
library(corrplot)
#install.packages('clustMixType')
library(clustMixType)
#install.packages('wesanderson')
library(wesanderson)
#install.packages("zipcodeR")
library(zipcodeR)
#install.packages('stringr')
library(stringr)
library(glmnet)
library(caret)
library(caTools)
library(xgboost)
library(Matrix)

#Loading The Data----

group3 <- read_csv("group3.csv", col_types = cols(...1 = col_skip()))

fips_data <- read_csv("zip_fips.csv", col_types = cols(...1 = col_skip(), 
                                                       STCOUNTYFP = col_number()))

countydebt <- read_excel("dia_lbls_all_overall_county_2022_02_14Sep2023.xlsx")

census <- read_csv("cc-est2022-all.csv")

#Isolating the Dependent Variable-----

group3$relief<- ifelse(group3$Company.response.to.consumer %in% c("Closed with monetary relief", 
                                                                  "Closed with relief", 
                                                                  "Closed with non-monetary relief"),
                       1,0)

#Cleaning-----

#drop non states 
group3$drop <- as.numeric(group3$State %in% c("NONE", "None", "DC", "AA", "AS", "AP", "AE", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"))
group3 <- subset(group3, !(State %in% c("AE", 'AP'))) #dropping military bases

fips_data <- fips_data[!fips_data$STATE %in% c("NONE", "None", "DC", "AA", "AS", 'AP', "AE", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"),]

ctfips <-  (ifelse((fips_data$STATE == 'CT' & str_detect(fips_data$STCOUNTYFP, "9")), 
       as.numeric(paste0("01", fips_data$STCOUNTYFP)), fips_data$STCOUNTYFP))

fips_data$STCOUNTYFP <- ctfips

countydebt$`County FIPS` <- as.numeric(countydebt$`County FIPS`)

#Named Factors ----
group3 <- group3 %>%
  mutate(Company.response.to.consumer = case_when(Company.response.to.consumer == "In progress" ~ 0,
                                                  Company.response.to.consumer == "Closed with explanation" ~ 1,
                                                  Company.response.to.consumer == "Untimely response" ~ 2,
                                                  Company.response.to.consumer == "Closed with non-monetary relief" ~ 3,
                                                  Company.response.to.consumer == "Closed with monetary relief" ~ 4,
                                                  Company.response.to.consumer == "Closed" ~ 5),
         Sub.product = case_when(Sub.product == "Other debt" ~ "Misc. debt",
                                 Sub.product == 'Other (i.e. phone, health club, etc.)' ~ "Misc. debt",
                                 Sub.product == "Telecommunications debt" ~ "Misc. debt",
                                 Sub.product == 'I do not know' ~ "Misc. debt",
                                 Sub.product == "Federal student loan debt" ~ "Federal Student Loans",
                                 Sub.product == "Federal student loan"   ~ "Federal Student Loans",
                                 Sub.product == "Private student loan debt" ~ "Non-Federal Student Loans",
                                 Sub.product == "Non-federal student loan"  ~ "Non-Federal Student Loans",
                                 Sub.product == "Auto" ~ "Auto debt",
                                 Sub.product == "Auto debt" ~ "Auto debt",
                                 Sub.product == "Credit card debt" ~ "Credit card debt",
                                 Sub.product == "Credit card" ~ "Credit card debt", 
                                 Sub.product == 'Mortgage debt'~ "Home debt", #fixed a typo in mortgage (by Nirav). 
                                 Sub.product == "Mortgage" ~ "Home debt",
                                 Sub.product == 'Medical' ~ "Medical debt",
                                 Sub.product == "Medical debt" ~ "Medical debt",
                                 Sub.product == "Rental debt" ~ "Rental debt",
                                 Sub.product == "Payday loan debt" ~ "Payday debt",
                                 Sub.product == "Payday loan" ~ "Payday debt"),
         Issue = case_when(Issue == "Attempts to collect debt not owed" ~ "Collect Debt Not Owed",
                           Issue == 'Cont\'d attempts collect debt not owed' ~ "Collect Debt Not Owed",
                           Issue == "Took or threatened to take negative or legal action" ~ "Aggressive Response",
                           Issue == 'Taking/threatening an illegal action' ~ "Aggressive Response",
                           Issue == "Threatened to contact someone or share information improperly" ~ "Improper Communication Tactic",
                           Issue == "Communication tactics" ~ "Improper Communication Tactic",
                           Issue == "False statements or representation" ~ "Improper Communication Tactic",
                           Issue == "Disclosure verification of debt" ~ "Improper Communication Tactic",
                           Issue == "Improper contact or sharing of info" ~ "Improper Communication Tactic",
                           Issue == "Written notification about debt" ~ "Debt Notifications",
                           Issue == "Electronic communications"   ~ "Debt Notifications"),
         Company.public.response = case_when(Company.public.response == "Company has responded to the consumer and the CFPB and chooses not to provide a public response" ~ "No Comment",
                                           Company.public.response == "Company chooses not to provide a public response" ~ "No Comment",
                                           Company.public.response == "Company believes complaint represents an opportunity for improvement to better serve consumers" ~ "Improve Service",
                                           Company.public.response == "Company believes the complaint provided an opportunity to answer consumer's questions" ~ "Improve Service",
                                           Company.public.response == "Company believes the complaint is the result of a misunderstanding" ~ "Misunderstanding",
                                           Company.public.response == "Company believes complaint is the result of an isolated error" ~ "Misunderstanding"),
         Consumer.consent.provided. = case_when(Consumer.consent.provided. == "Consent provided" ~ "Consent provided",
                                                Consumer.consent.provided. == "None"  ~ "No consent provided",
                                                Consumer.consent.provided. == "Consent not provided"  ~ "No consent provided",
                                                Consumer.consent.provided. == "Other"  ~ "No consent provided",
                                                Consumer.consent.provided. == "Consent withdrawn"   ~ "No consent provided",
                                                Consumer.consent.provided. == "N/A" ~ "No consent provided"))

#Dummy for after date (form changes NA imputation)
group3$Date.received <- as.Date(group3$Date.received, format = "%m/%d/%y")

group3$Dispute_prior <- ifelse(group3$Date.received  > as.Date('04/24/17', format= "%m/%d/%y"), 1,0)

#####Zip Code Cleaning#### -----
USA_zippop <- zip_code_db

#Clean Zip Reference data first ----

fips_data$ZIP <- as.character(fips_data$ZIP)
group3$ZIP.code <- as.character(group3$ZIP.code)

#check zips including numbers
table(str_detect( as.character(fips_data$ZIP), "[0-9]+$")) #looks like they're all numbers

#check zips less than 5 characters
table(nchar(fips_data$ZIP) < 5) #2900 entries less than 5 digits - leading zeros?
invalidzip <- fips_data$ZIP[nchar(fips_data$ZIP) < 5]

table(nchar(fips_data$ZIP) > 5) #No entries greater than 5 digits

#Check leading zeros
table(grepl("^0", group3$ZIP.code)) #looks like they already have leading zeros

#LEADING Zero Code replacement 
#Place a leading zero for the problem zips
zip <- as.numeric(fips_data$ZIP)
for(i in 1:length(zip)){
  if(nchar(as.numeric(zip[i])) < 5){
    zip[i] <- paste0("0", zip[i])
  }
}

fips_data$ZIP <- (zip)
table(nchar(fips_data$ZIP) < 5 ) #incorrect zip identification

error_zip <- fips_data[nchar(fips_data$ZIP) < 5, ]

#manual fix 
fips_data$ZIP[fips_data$ZIP == error_zip$ZIP] <- '00501'

#Group3 Clean Zips -----

#check zips including numbers
table(str_detect( as.character(group3$ZIP.code), "[0-9]+$")) #looks like they're all numbers

#check zips less than 5 characters
table(nchar(group3$ZIP.code) < 5) #No entries less than 5 digits
table(nchar(group3$ZIP.code) > 5) #No entries greater than 5 digits

#Check leading zeros
table(grepl("^0", group3$ZIP.code)) #looks like they already have leading zeros

unique_zips <- unique(group3$ZIP.code)

zip_binary_map <- unique_zips %in% fips_data$ZIP

table(zip_binary_map)

error_zips <- unique_zips[!zip_binary_map] #erroneous zips

#T.test of error zips -----
t.test(table(zip_binary_map), alternative = 'two.sided') #fail to reject the null - the error zips are not significant at 5% level

for (error_state in unique(group3$State[group3$ZIP.code %in% error_zips])) {
  mode_zip <- names(sort(table(group3$ZIP.code[group3$State == error_state]), decreasing = TRUE)[1])
  group3$ZIP.code[group3$ZIP.code %in% error_zips & group3$State == error_state] <- mode_zip
}

#LEADING Zero Code replacement for the problem zips
#zip <- as.character(group3$ZIP.code[zip_binary_map == FALSE])
#for(i in 1:length(zip)){
#  if(nchar(as.numeric(zip[i])) < 5){
#    zip[i] <- paste0("0", zip[i])
#  }
#}

#insert zips in data

#Percusive Maintainence ---
# run the loop until there are no issues

error_zips <- unique_zips[!zip_binary_map] #82 erroneous zips
table(zip_binary_map)

for (error_state in unique(group3$State[group3$ZIP.code %in% error_zips])) {
  mode_zip <- names(sort(table(group3$ZIP.code[group3$State == error_state]), decreasing = TRUE)[1])
  group3$ZIP.code[group3$ZIP.code %in% error_zips & group3$State == error_state] <- mode_zip
}

#Check if this worked
unique_zips <- unique(group3$ZIP.code)
zip_binary_map <- unique_zips %in% fips_data$ZIP
table(zip_binary_map) #looks like there are no issues anymore 
 
#rm(i, zip, zip_binary_map, unique_zips, unclean_zips) #remove these variables when done.

#FIPS Fix ------

state_data <- data.frame(
  Postal_Abbr = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", 
    "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", 
    "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
  ),
  FIPS_Code = c(
    "0", "0", "0", "0", "0", "0", "0", "10", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
    "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",  
    "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"
  )
)

countydebt$`County FIPS` <- as.character(paste0("0", countydebt$`County FIPS`))

fips_data$STCOUNTYFP <- as.character(paste0("0", fips_data$STCOUNTYFP))

#Q3 ----

#merge 
merg_zips <- merge(group3, fips_data, by.x =  'ZIP.code', by.y = 'ZIP')

#Q4 -----

merg_fips <- merge(merg_zips, countydebt,  by.x = 'STCOUNTYFP', by.y = 'County FIPS')  

#Q5----

census_fips <- (paste(census$STATE, census$COUNTY, sep = ""))

census_fips <- ifelse(!(census_fips %in% fips_data$STCOUNTYFP), paste0('0', census_fips), census_fips)
table(census_fips %in% fips_data$STCOUNTYFP)

census <- cbind(census, census_fips)

county_totals <- subset(census, (AGEGRP == 0 & YEAR == 1))

#Age dummies

unique_fips <- unique(census$census_fips)
results_list <- list()
results_list2 <- list()
fip <- 0

for (fip in unique_fips) {
  result <- census %>%
    filter(census_fips == fip, YEAR == 1, AGEGRP >= 1 & AGEGRP <= 5) %>%
    summarise(TOTAL_POP = sum(TOT_POP))
  
  results_list[[fip]] <- result
  
  result2 <- census %>%
    filter(census_fips == fip, YEAR == 1, AGEGRP >= 13 & AGEGRP <= 18) %>%
    summarise(TOTAL_POP = sum(TOT_POP))
  
  results_list2[[fip]] <- result2
}

combined_results <- bind_rows(results_list, .id = "Pop_less25")
combined_results2 <- bind_rows(results_list2, .id = "Pop_over64")

#Ethnicity 

colnames(county_totals)

county_demos <- cbind(bind_rows(results_list, .id = "Pop_less25"), bind_rows(results_list2, .id = "Pop_over64"))

county_demos$Pop_Hispanic <- rowSums(county_totals[, c('H_FEMALE', 'H_MALE')])

colnames(county_demos) <- c("Fips", "Pop_less25", "Fips", "Pop_over64", "Pop_Hispanic")
county_demos <- county_demos[,-3]

#older Americans and servicefolk
group3$servicemenber <- ifelse(str_detect(group3$Tags, "Servicemember"), 1, 0)
group3$olderAm <- ifelse(str_detect(group3$Tags, "Older American"), 1, 0)

#Race counts per county

#whites
indx <- grepl('WA', colnames(county_totals))
whites_df <- county_totals[indx]
w_sum <- rowSums(whites_df)

#Blacks
indx <- grepl('BA', colnames(county_totals))
black_df <- county_totals[indx]
b_sum <- rowSums(black_df)

#Asian
indx <- grepl('AA', colnames(county_totals))
asian_df <- county_totals[indx]
a_sum <- rowSums(asian_df)

#Native Americans
indx <- grepl('IA', colnames(county_totals))
indx2 <- grepl('NA', colnames(county_totals))
native_df <- county_totals[indx]
native2_df <- county_totals[indx2]
native2_df <- native2_df[,-c(1,2)]
n_sum <- rowSums(native_df)
nsum2 <- rowSums(native2_df)

combo_native <- n_sum + nsum2

Fips <- county_totals$census_fips

#including them into demographic df
races_all <- cbind(Fips, w_sum, b_sum, a_sum, combo_native)
races_all <- as.data.frame(races_all) 
races_all[, 2:5] <- apply(races_all[, 2:5], 2, as.numeric)

county_demos <- left_join(county_demos, races_all, by = "Fips")

#include male and female county totals
county_demos$TotalMale <- county_totals$TOT_MALE
county_demos$TotalFemale <- county_totals$TOT_FEMALE

#Q6-----

#matching fips to zips

merg_county_demos <- merge(county_demos, fips_data, by.x = 'Fips', by.y = "STCOUNTYFP")

clean_group3 <- left_join(group3, merg_county_demos, by = c('ZIP.code' = 'ZIP'))

hopefully_all <- left_join(clean_group3, countydebt, by = c('Fips' = 'County FIPS'))

#making csvs to save time in cleanup 
write.csv(county_demos, "county_demos.csv")
write.csv(merg_fips, 'merg_fips.csv')
write.csv(clean_group3, 'clean_group3.csv')
write.csv(hopefully_all, 'hopefully.csv')

#Q7------

colnames(hopefully_all)
temp <-  hopefully_all[, c(38:58)]
temp <- sapply(temp, as.numeric)

temp_imputed <- apply(temp, 2, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
})

#### PCA
library(ggcorrplot)
library("FactoMineR")
library(factoextra)
# PCA is based on correlations, not distance.
## So we need to store the correlation matrix.

corr_matrix <- cor(temp_imputed)
corr_matrix <- scale(corr_matrix)
ggcorrplot(corr_matrix)

## do PCA
debt.pca <- princomp(corr_matrix)
summary(debt.pca)
fviz_eig(debt.pca, addlabels = TRUE)

comps <- debt.pca$scores[,1:4]

# loadings for first 5 components
debt.pca$loadings[, 1:4]                       

#scree plot
variance_explained <- debt.pca$sd^2 / sum(debt.pca$sd^2)*100 

variance_explained[1:7] 

qplot(c(1:21), variance_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 100)    


#get 4 components 
feature_vector <- (debt.pca$loadings)
feature_vector_transposed <- t(feature_vector)

original_data_transposed <- t(temp_imputed)

dim(original_data_transposed)
dim(feature_vector)

data_reoriented <- feature_vector_transposed %*% original_data_transposed

data_reoriented_df <- as.data.frame(data_reoriented)
data_reoriented_df <- t(data_reoriented_df)

comps <- data_reoriented_df[,c(1:4)]

comps <- as.data.frame(comps)

#now merge to the dataset
temp2 <- cbind(hopefully_all[,-c(35:58)], comps)

colnames(temp2)

write.csv(temp2, 'straightouttacompton.csv')

######## PRIOR CODE GENERATES CSVs. LOAD CSV FROM HERE ONWARD ########--------

NWA <- read.csv('straightouttacompton.csv')
NWA <- NWA[,-1]

colnames(NWA)

#Q8 ----

### Clustering code
# make a separate dataset, and then make sure each variable is the right class. 
set.seed(23748234)

ca <- NWA[, c("Sub.product",
                 "Issue",
                 "Pop_over64",
                 "TotalFemale",
                 "Average household income, Comm of color",
                 "Share of people of color")    ]

ca[,1] <- as.factor(ca[,1] )
ca[,2] <- as.factor(ca[,2] )
ca[,3] <- scale(as.numeric(ca[,3] ))
ca[,4] <- scale(as.numeric(ca[,4] ))
ca[,5] <- scale(as.numeric(ca[,5] ))
ca[,6] <- scale(as.numeric(ca[,6] ))

colnames(ca) <- c("Sub.product",
                  "Issue",
                  "Pop_over64",
                  "TotalFemale",
                  "Average.household.income..Comm.of.color",
                  "Share.of.people.of.color") 

ca$medicaldebt <- as.factor(ifelse(ca$Sub.product == "Medical debt", 1,0))

count(is.na(ca))

# 2 cluster
kpres2 <- kproto(x=ca[,c(2:7)], k=2, na.rm = 'imp.internal')

# 3 cluster
kpres3 <- kproto(x=ca[,c(2:7)], k=3,  na.rm = 'imp.internal')

# Plots
clprofiles(kpres3, ca[,c(2:7)], col = wes_palette("Royal1", 4, type = "continuous")) #c('blue', 'red', 'green')) 

# scree plot

n.scree<-ncol(ca)-2
Es<- numeric(n.scree)
for(i in 1:n.scree){ 
  kpres <- kproto(ca,k=i,nstart=5, verbose = FALSE) 
  Es[i] <- kpres$tot.withinss 
} 

plot(1:n.scree, Es[1:5], type="b", ylab="ObjectiveFunction",
     xlab="#Clusters", main="ScreePlot") #figure2

NWA$MedicalDebtClusters <- kpres3$cluster

colnames(NWA) 

write.csv(NWA, 'q9.csv')

#Q9 -----

q9 <- read.csv('q9.csv')
colnames(q9)

library(lubridate)

q9$Year <- year(q9$Date.received)

q9 <- q9 %>%
  select(Sub.product, Issue, Sub.issue, Consumer.consent.provided., Submitted.via, Timely.response. , relief, drop, 
         Dispute_prior, servicemenber, olderAm, 
  Fips, Pop_less25, Pop_over64, Pop_Hispanic, w_sum, b_sum, a_sum, combo_native, TotalMale, TotalFemale,
  Share.of.people.of.color, Average.household.income..All, Average.household.income..Comm.of.color, Average.household.income..White.comm,
 Comp.1, Comp.2,Comp.3, Comp.4, MedicalDebtClusters,  Year)

table(q9$Year) 

#Transformations -------

#make factors
q9_s <- data.frame(
  lapply(q9, function(x) {
    if(is.character(x)) factor(x) else x
  })
)

logged_vs <- log(q9_s[,c(13:25)])
lognames <- colnames(logged_vs)
lognames <- paste("log", lognames)
colnames(logged_vs) <- lognames

squared_vs <- q9_s[,c(13:25)]^2
sqnames <- colnames(squared_vs)
sqnames <- paste("sq", sqnames)
colnames(squared_vs) <- sqnames

standardized_vs <- scale(q9_s[,c(13:25)])
stan_names <- colnames(standardized_vs)
stan_names <- paste("stan", stan_names)
colnames(standardized_vs) <- stan_names

q9_s <- cbind(q9_s, logged_vs, squared_vs, standardized_vs)

q9_s <- q9_s %>%
  filter(complete.cases(.))

write.csv(q9_s, "q9_s.csv")

##### train and test ------
library(caret)
train_control <- trainControl(method = "cv", 
                              number = 10) 

library(caTools)
set.seed(275142345)
split <- sample.split(q9_s, SplitRatio = 0.7)
train <- subset(q9_s, split == "TRUE")
test <- subset(q9_s, split == "FALSE")

#Q10-----

#logit----
logit_m <- glm(relief ~ (`log Share.of.people.of.color`), data = train)

plot(logit_m)

summary(logit_m)

model <- train(relief ~ Share.of.people.of.color, data = q9_s,  
               method = "glm", 
               trControl = train_control)

forward_glm <- step(, direction = "forward", scope = formula(~ .))
backward_glm <- step(, direction = "backward", scope = formula(~ .))  
step_glm <- step(, direction = "both", scope = formula(~ .)) 

#Lasso ----
library(glmnet)

X <- model.matrix(q9_2$relief ~., data = q9_2)[, -1]  # Predictors
Y <- q9_2$relief

#Lasso logistic regression model with 10-fold cross-validation
logit_model <- cv.glmnet(X, Y, alpha = 1, family = "binomial", link = "logit", nfolds = 10, newx = X)


coef(logit_model, s = "lambda.min")

#Regression tree ------
intrain <- createDataPartition(y = q9_s$relief, p= 0.8)[[1]]
train_q9_s <- q9_s[intrain,]
test_q9_s <- q9_s[-intrain,]

library(rpart)
library(rpart.plot)

minsplit <- 20

rpart.control_params <- rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.00001,
              maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
              surrogatestyle = 0, maxdepth = 30)

relief.tree <- rpart(relief~., data= train_q9_s, control = rpart.control_params, method = "class")

summary(relief.tree)

rpart.plot(relief.tree)

relief.tree$variable.importance

#Predictions
predicted_labels <- predict(relief.tree, newdata = test_q9_s, type = "class")

#misclassification rate
misclassification_rate <- mean(predicted_labels != test_q9_s$relief)

print(misclassification_rate
      
######################################### Random forest ###############################################
set.seed(275142)
RF<- randomForest(relief~., data= q9_s,
                             importance=TRUE, ntree=500)

RF

#Checking at importance: 
RF$importance

#Importance frame: 
importance_frame <- measure_importance(RF)
importance_frame

### plot variable depth
min_depth_frame<- min_depth_distribution(RF)
head(min_depth_frame, n = 10)
plot_min_depth_distribution(min_depth_frame)

### Plot multiway importance
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

### conditional min depth
## pick 5 most important variables
(vars <- important_variables(importance_frame, k = 5, measures =
                               c("mean_min_depth", "no_of_trees")))
interactions_frame <- min_depth_interactions(RF, vars)
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])
plot_min_depth_interactions(interactions_frame)

################################################ XG Boost #######################################################
set.seed(27514)
split.imp <- sample.split(q9_s, SplitRatio = 0.7)
train1 <-  subset(q9_s, split.imp == "TRUE")
test1 <- subset(q9_s, split.imp == "FALSE")

# make them numeric matrix
xtrain<-sparse.model.matrix(relief ~. -1, data = train1)
ytrain <- as.array(train1$relief)
xtest <- sparse.model.matrix(relief ~ .-1, data = test1)
ytest <- as.array(test1$relief)

### run train model
xgb1 <- xgboost(data = xtrain, label = ytrain,
                nrounds = 500)

# run test model
y_pred <- predict(xgb1, xtest)
y_pred
# get MSE
test.MSE<-mean((ytest - y_pred)^2)
test.MSE
# get residual (if continuous outcome)
r<-ytest - y_pred
qqnorm(r)

### plot
xgb.plot.multi.trees(xgb1)

#Various different hyperparameters:
#eta:
eta=c(0.05, 0.1, 0.2,0.5,1)

# = colsample_bylevel candidates = #
cs=c(1/3,2/3,1)

# = max_depth candidates = #
md=c(2,4,6,10)

# = sub_sample candidates = #
ss=c(0.25,0.5,0.75,1)

# = standard model is the second value of each vector above = #
standard=c(2,2,3,2)

# = min_child_weights candidates = #
mcw=c(1,10,100,400)

# = gamma candidates = #
gamma=c(0.1,1,10,100)

#a) ETA search:
set.seed(13856)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,nrow(test1), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(xtrain, label = ytrain, nrounds = 500, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, xtest)
}


conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))
(RMSE_eta = sqrt(colMeans((as.numeric(ytest)-pred_eta)^2)))

#b) Colsample_bylevel:
set.seed(1284654)
conv_cs = matrix(NA,500,length(cs))
pred_cs = matrix(NA,nrow(test1), length(cs))
colnames(conv_cs) = colnames(pred_cs) = cs

for(i in 1:length(cs)){
  params = list(eta = eta[standard[1]], colsample_bylevel = cs[i],
                subsample = ss[standard[4]], max_depth = md[standard[3]],
                min_child_weigth = 1)
  xgb=xgboost(xtrain, label = ytrain,nrounds = 500, params = params)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, xtest)
}


conv_cs = data.frame(iter=1:500, conv_cs)
conv_cs = melt(conv_cs, id.vars = "iter")
ggplot(data = conv_cs) + geom_line(aes(x = iter, y = value, color = variable))
(RMSE_cs = sqrt(colMeans((as.numeric(ytest)-pred_cs)^2)))

#c) Max Depth: 
set.seed(1284654)
conv_md=matrix(NA,500,length(md))
pred_md=matrix(NA,nrow(test1),length(md))
colnames(conv_md)=colnames(pred_md)=md

for(i in 1:length(md)){
  params=list(eta=eta[standard[1]],colsample_bylevel=cs[standard[2]],
              subsample=ss[standard[4]],max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(xtrain, label = ytrain,nrounds = 500,params=params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, xtest)
}

conv_md=data.frame(iter=1:500,conv_md)
conv_md=melt(conv_md,id.vars = "iter")
ggplot(data=conv_md)+geom_line(aes(x=iter,y=value,color=variable))
(RMSE_md=sqrt(colMeans((as.numeric(ytest)-pred_md)^2)))

#d) Sub Sample: 
set.seed(1)
conv_ss=matrix(NA,500,length(ss))
pred_ss=matrix(NA,nrow(test1),length(ss))
colnames(conv_ss)=colnames(pred_ss)=ss

for(i in 1:length(ss)){
  params=list(eta=eta[standard[1]],colsample_bylevel=cs[standard[2]],
              subsample=ss[i],max_depth=md[standard[3]],
              min_child_weigth=1)
  xgb=xgboost(xtrain, label = ytrain,nrounds = 500,params=params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, xtest)
}

conv_ss=data.frame(iter=1:500,conv_ss)
conv_ss=melt(conv_ss,id.vars = "iter")
ggplot(data=conv_ss)+geom_line(aes(x=iter,y=value,color=variable))
(RMSE_ss=sqrt(colMeans((as.numeric(ytest)-pred_ss)^2)))
#We plan on proceeding with 0.75 because there isn't significant difference between the RMSE value of 
#0.75 and 1. Secondly, we use 0.75 to account for any possible outliers. 

#e) min_child weight:
set.seed(12754)
conv_mcw = matrix(NA,500,length(mcw))
pred_mcw = matrix(NA,nrow(test1), length(mcw))
colnames(conv_mcw) = colnames(pred_mcw) = mcw

for(i in 1:length(mcw)){
  params = list(eta = 0.1, colsample_bylevel=2/3,
                subsample = 1, max_depth = 6,
                min_child_weight = mcw[i], gamma = 0)
  xgb = xgboost(xtrain, label = ytrain, nrounds = 500, params = params)
  conv_mcw[,i] = xgb$evaluation_log$train_rmse
  pred_mcw[,i] = predict(xgb, xtest)
}

conv_mcw = data.frame(iter=1:500, conv_mcw)
conv_mcw = melt(conv_mcw, id.vars = "iter")
ggplot(data = conv_mcw) + geom_line(aes(x = iter, y = value, color = variable))
(RMSE_mcw = sqrt(colMeans((as.numeric(ytest)-pred_mcw)^2)))
#we select the hyperparameter = 100 because of the same reason as above. 


#f) Gamma: 
set.seed(12897564)
conv_gamma = matrix(NA,500,length(gamma))
pred_gamma = matrix(NA,nrow(test1), length(gamma))
colnames(conv_gamma) = colnames(pred_gamma) = gamma

for(i in 1:length(gamma)){
  params = list(eta = 0.1, colsample_bylevel=2/3,
                subsample = 1, max_depth = 6, min_child_weight = 1,
                gamma = gamma[i])
  xgb = xgboost(xtrain, label = ytrain, nrounds = 500, params = params)
  conv_gamma[,i] = xgb$evaluation_log$train_rmse
  pred_gamma[,i] = predict(xgb, xtest)
}

conv_gamma = data.frame(iter=1:500, conv_gamma)
conv_gamma = melt(conv_gamma, id.vars = "iter")
ggplot(data = conv_gamma) + geom_line(aes(x = iter, y = value, color = variable))
(RMSE_gamma = sqrt(colMeans((as.numeric(ytest)-pred_gamma)^2)))



#Setting the values: 
xgb1 <- xgboost(data = xtrain, label = ytrain,
                nrounds = 500)

#set these values manually by looking at RMSE values:
params = list(eta = .05, colsample_bylevel = 1/3,
              subsample = 0.75 , max_depth = 2,
              min_child_weigth = 100)
params


xgb.train <- xgboost(data = xtrain, label = ytrain,
                     params = params,
                     nrounds = 500, objective = "reg:squarederror")


xgb.test <- xgboost(data = xtest, label = ytest,
                    params = params,
                    nrounds = 500, objective = "reg:squarederror")


# run test model
y_pred <- predict(xgb.train, xtest)
# get MSE
test.MSE2<-mean((ytest - y_pred)^2)
test.MSE2
# get residual (if continuous outcome)
r2<-ytest - y_pred
plot(r2, ylab = "residuals", main = "XGB residuals")
qqnorm(r2)


#Plot: 
#get the first three trees
xgb.plot.tree(model = xgb.train, trees = 0:4)
xgb.plot.multi.trees(xgb.train)
importance_matrix <- xgb.importance(model = xgb.train)
importance_matrix
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")


#################################### XG Boost without olderAM variable ##############################################

#Dropping the 'older american variable, i.e., olderAM' and re-running the XG BOOST: 
attach(q9_s)

#Dropping the olderAM variable: 
q9_s1<-q9_s%>%
  select(-olderAm)
view(q9_s1)

#Following is the XG boost code:      

################################### XG Boost 1 #####################################################
#convert all character columns to factor
q9_s1<- data.frame(
  lapply(q9_s1, function(x) {
    if(is.character(x)) factor(x) else x
  })
)
set.seed(27514)
split.imp1 <- sample.split(q9_s1, SplitRatio = 0.7)
train2 <-  subset(q9_s1, split.imp == "TRUE")
test2 <- subset(q9_s1, split.imp == "FALSE")

# make them numeric matrix
xtrain1<-sparse.model.matrix(relief ~. -1, data = train2)
ytrain1<- as.array(train2$relief)
xtest1<- sparse.model.matrix(relief ~ .-1, data = test2)
ytest1<- as.array(test2$relief)

### run train model
xgb2<- xgboost(data = xtrain1, label = ytrain1,
                nrounds = 500)

# run test model
y_pred1<- predict(xgb2, xtest1)
y_pred1
# get MSE
test.MSE1<-mean((ytest1 - y_pred1)^2)
test.MSE1
# get residual (if continuous outcome)
r1<-ytest1 - y_pred1
qqnorm(r1)

### plot
xgb.plot.multi.trees(xgb2)

#Various different hyperparameters:
#eta:
eta=c(0.05, 0.1, 0.2,0.5,1)

# = colsample_bylevel candidates = #
cs=c(1/3,2/3,1)

# = max_depth candidates = #
md=c(2,4,6,10)

# = sub_sample candidates = #
ss=c(0.25,0.5,0.75,1)

# = standard model is the second value of each vector above = #
standard=c(2,2,3,2)

# = min_child_weights candidates = #
mcw=c(1,10,100,400)

# = gamma candidates = #
gamma=c(0.1,1,10,100)

#a) ETA search:
set.seed(13856)
conv_eta1 = matrix(NA,500,length(eta))
pred_eta1 = matrix(NA,nrow(test2), length(eta))
colnames(conv_eta1) = colnames(pred_eta1) = eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(xtrain1, label = ytrain1, nrounds = 500, params = params)
  conv_eta1[,i] = xgb$evaluation_log$train_rmse
  pred_eta1[,i] = predict(xgb, xtest1)
}


conv_eta1 = data.frame(iter=1:500, conv_eta1)
conv_eta1 = melt(conv_eta1, id.vars = "iter")
ggplot(data = conv_eta1) + geom_line(aes(x = iter, y = value, color = variable))
(RMSE_eta1 = sqrt(colMeans((as.numeric(ytest1)-pred_eta1)^2)))

#b) Colsample_bylevel:
set.seed(1284654)
conv_cs1 = matrix(NA,500,length(cs))
pred_cs1 = matrix(NA,nrow(test2), length(cs))
colnames(conv_cs1) = colnames(pred_cs1) = cs

for(i in 1:length(cs)){
  params = list(eta = eta[standard[1]], colsample_bylevel = cs[i],
                subsample = ss[standard[4]], max_depth = md[standard[3]],
                min_child_weigth = 1)
  xgb=xgboost(xtrain1, label = ytrain1,nrounds = 500, params = params)
  conv_cs1[,i] = xgb$evaluation_log$train_rmse
  pred_cs1[,i] = predict(xgb, xtest1)
}


conv_cs1 = data.frame(iter=1:500, conv_cs1)
conv_cs1 = melt(conv_cs1, id.vars = "iter")
ggplot(data = conv_cs1) + geom_line(aes(x = iter, y = value, color = variable))
(RMSE_cs1 = sqrt(colMeans((as.numeric(ytest1)-pred_cs1)^2)))

#c) Max Depth: 
set.seed(1284654)
conv_md1=matrix(NA,500,length(md))
pred_md1=matrix(NA,nrow(test2),length(md))
colnames(conv_md1)=colnames(pred_md1)=md

for(i in 1:length(md)){
  params=list(eta=eta[standard[1]],colsample_bylevel=cs[standard[2]],
              subsample=ss[standard[4]],max_depth=md[i],
              min_child_weigth=1)
  xgb=xgboost(xtrain1, label = ytrain1,nrounds = 500,params=params)
  conv_md1[,i] = xgb$evaluation_log$train_rmse
  pred_md1[,i] = predict(xgb, xtest1)
}

conv_md1=data.frame(iter=1:500,conv_md1)
conv_md1=melt(conv_md1,id.vars = "iter")
ggplot(data=conv_md1)+geom_line(aes(x=iter,y=value,color=variable))
(RMSE_md1=sqrt(colMeans((as.numeric(ytest1)-pred_md1)^2)))

#d) Sub Sample: 
set.seed(1)
conv_ss1=matrix(NA,500,length(ss))
pred_ss1=matrix(NA,nrow(test2),length(ss))
colnames(conv_ss1)=colnames(pred_ss1)=ss

for(i in 1:length(ss)){
  params=list(eta=eta[standard[1]],colsample_bylevel=cs[standard[2]],
              subsample=ss[i],max_depth=md[standard[3]],
              min_child_weigth=1)
  xgb=xgboost(xtrain1, label = ytrain1,nrounds = 500,params=params)
  conv_ss1[,i] = xgb$evaluation_log$train_rmse
  pred_ss1[,i] = predict(xgb, xtest1)
}

conv_ss1=data.frame(iter=1:500,conv_ss1)
conv_ss1=melt(conv_ss1,id.vars = "iter")
ggplot(data=conv_ss1)+geom_line(aes(x=iter,y=value,color=variable))
(RMSE_ss1=sqrt(colMeans((as.numeric(ytest1)-pred_ss1)^2)))


#e) min_child weight:
set.seed(12754)
conv_mcw1= matrix(NA,500,length(mcw))
pred_mcw1= matrix(NA,nrow(test2), length(mcw))
colnames(conv_mcw1) = colnames(pred_mcw1) = mcw

for(i in 1:length(mcw)){
  params = list(eta = 0.1, colsample_bylevel=2/3,
                subsample = 1, max_depth = 6,
                min_child_weight = mcw[i], gamma = 0)
  xgb = xgboost(xtrain1, label = ytrain1, nrounds = 500, params = params)
  conv_mcw1[,i] = xgb$evaluation_log$train_rmse
  pred_mcw1[,i] = predict(xgb, xtest1)
}

conv_mcw1= data.frame(iter=1:500, conv_mcw1)
conv_mcw1= melt(conv_mcw1, id.vars = "iter")
ggplot(data = conv_mcw1) + geom_line(aes(x = iter, y = value, color = variable))
(RMSE_mcw1 = sqrt(colMeans((as.numeric(ytest1)-pred_mcw1)^2)))
#we select the hyperparameter = 100 because of the same reason as above. 


#f) Gamma: 
set.seed(12897564)
conv_gamma1= matrix(NA,500,length(gamma))
pred_gamma1= matrix(NA,nrow(test2), length(gamma))
colnames(conv_gamma1) = colnames(pred_gamma1) = gamma

for(i in 1:length(gamma)){
  params = list(eta = 0.1, colsample_bylevel=2/3,
                subsample = 1, max_depth = 6, min_child_weight = 1,
                gamma = gamma[i])
  xgb = xgboost(xtrain1, label = ytrain1, nrounds = 500, params = params)
  conv_gamma1[,i] = xgb$evaluation_log$train_rmse
  pred_gamma1[,i] = predict(xgb, xtest1)
}

conv_gamma1 = data.frame(iter=1:500, conv_gamma1)
conv_gamma1 = melt(conv_gamma1, id.vars = "iter")
ggplot(data = conv_gamma1) + geom_line(aes(x = iter, y = value, color = variable))
(RMSE_gamma1 = sqrt(colMeans((as.numeric(ytest1)-pred_gamma1)^2)))



#Setting the values: 
xgb2<- xgboost(data = xtrain1, label = ytrain1,
                nrounds = 500)

#set these values manually by looking at RMSE values:
params1 = list(eta = .05, colsample_bylevel = 1/3,
              subsample = 1 , max_depth = 2,
              min_child_weigth = 400)
params1


xgb.train1 <- xgboost(data = xtrain1, label = ytrain1,
                     params = params1,
                     nrounds = 500, objective = "reg:squarederror")


xgb.test1 <- xgboost(data = xtest1, label = ytest1,
                    params = params1,
                    nrounds = 500, objective = "reg:squarederror")


# run test model
y_pred1.1 <- predict(xgb.train1, xtest1)
y_pred1.1
# get MSE
test.MSE3<-mean((ytest1 - y_pred1.1)^2)
test.MSE3
# get residual (if continuous outcome)
r3<-ytest1 - y_pred1.1
plot(r3, ylab = "residuals", main = "XGB residuals")
qqnorm(r3)


#Plot: 
#get the first three trees
xgb.plot.tree(model = xgb.train1, trees = 0:4)
xgb.plot.multi.trees(xgb.train1)
importance_matrix1 <- xgb.importance(model = xgb.train1)
importance_matrix1
xgb.plot.importance(importance_matrix1, xlab = "Feature Importance")
