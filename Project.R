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

#Loading The Data----

group3 <- read_csv("group3.csv")
group3 <- group3[-1] #inefficient, but works

fips_data <- read_csv("zip_fips.csv")
fips_data <- fips_data[-1]#inefficient, but works

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

#countydebt <- countydebt[,-c(4, 7, 10, 13, 16, 19, 22, 26)]

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

unique(group3$Company.response.to.consumer)

#Zip Code Cleaning -----
USA_zippop <- zip_code_db

#clean fips data first ----

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

#Group3 fix zips -----

#check zips including numbers
table(str_detect( as.character(group3$ZIP.code), "[0-9]+$")) #looks like they're all numbers

#check zips less than 5 characters
table(nchar(group3$ZIP.code) < 5) #No entries less than 5 digits
table(nchar(group3$ZIP.code) > 5) #No entries greater than 5 digits

#Check leading zeros
table(grepl("^0", group3$ZIP.code)) #looks like they already have leading zeros

unique_zips <- unique(group3$ZIP.code)

zip_binary_map <- unique_zips %in% fips_data$ZIP

table(zip_binary_map) #45 incorrect zips

error_zips <- unique_zips[!zip_binary_map] #45 erroneous zips

#replace military states with nearest, largest port state
#group3$State <- replace(group3$State, group3$State %in% c('AE', 'AP'), c('NY', 'CA'))

for (error_state in unique(group3$State[group3$ZIP.code %in% error_zips])) {
  mode_zip <- names(sort(table(group3$ZIP.code[group3$State == error_state]), decreasing = TRUE)[1])
  group3$ZIP.code[group3$ZIP.code %in% error_zips & group3$State == error_state] <- mode_zip
}

#LEADING Zero Code replacement 
#Place a leading zero for the problem zips
zip <- as.character(group3$ZIP.code[zip_binary_map == FALSE])
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

#replace military states with nearest, largest port state
#group3$State <- replace(group3$State, group3$State %in% c('AE', 'AP'), c('VA', 'CA'))

for (error_state in unique(group3$State[group3$ZIP.code %in% error_zips])) {
  mode_zip <- names(sort(table(group3$ZIP.code[group3$State == error_state]), decreasing = TRUE)[1])
  group3$ZIP.code[group3$ZIP.code %in% error_zips & group3$State == error_state] <- mode_zip
}

#Check if this worked
unique_zips <- unique(group3$ZIP.code)
zip_binary_map <- unique_zips %in% fips_data$ZIP
table(zip_binary_map) #looks like there are no issues anymore 

#rm(i, zip, zip_binary_map, zip_binary_map_1, unique_zips, unclean_zips) #remove these variables when done.

#Q3 ----

#merge 
merg_zips <- merge(group3, fips_data, by.x =  'ZIP.code', by.y = 'ZIP')

#Q4 -----

merg_fips <- merge(merg_zips, countydebt,  by.x = 'STCOUNTYFP', by.y = 'County FIPS') #by = c('STCOUNTYFP' =  'County FIPS')) 

#Q5----

census_fips <- (paste(census$STATE, census$COUNTY, sep = ""))

census <- cbind(census, census_fips)

#Prep Split Dataset by Male/Female
indx <- grepl('_FEMALE', colnames(census))
female_df <- census[indx]
rm(indx)

indx <- grepl('_MALE', colnames(census))
male_df <- census[indx]
colnames(male_df)

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

county_totals <- subset(census, (AGEGRP == 0 & YEAR == 1))
colnames(county_totals)

results_list3 <- list()
i <- 1
for (i in 1:nrow(county_totals)){
  result <- sum(county_totals$H_FEMALE[i], county_totals$H_MALE[i])
  results_list3[[i]] <- result
}

combined_results3 <- data.frame('Pop_Hispanic' = unlist(results_list3))

county_demos <- cbind(bind_rows(results_list, .id = "Pop_less25"), bind_rows(results_list2, .id = "Pop_over64"), combined_results3)

colnames(county_demos) <- c("Fips", "Pop_less25", "Fips", "Pop_over64", "Pop_Hispanic")
county_demos <- county_demos[,-3]

#combo_demographies <- merge(county_totals, temp, by.x = 'census_fips', by.y = 'Fips')

#older Americans and servicefolk
group3$servicemenber <- ifelse(str_detect(group3$Tags, "Servicemember"), 1, 0)
group3$olderAm <- ifelse(str_detect(group3$Tags, "Older American"), 1, 0)

#Race counts per county

#whites
indx <- grepl('WA', colnames(census))
whites_df <- census[indx]
w_sum <- rowSums(whites_df)

#Blacks
indx <- grepl('BA', colnames(census))
black_df <- census[indx]
b_sum <- rowSums(black_df)

#Asian
indx <- grepl('AA', colnames(census))
asian_df <- census[indx]
a_sum <- rowSums(asian_df)

#Native Americans
indx <- grepl('IA', colnames(census))
indx2 <- grepl('NA', colnames(census))
native_df <- census[indx]
native2_df <- census[indx2]
native2_df <- native2_df[,-c(1,2)]
n_sum <- rowSums(native_df)
nsum2 <- rowSums(native2_df)

combo_native <- n_sum + nsum2

#including them into demographic df
races_all <- cbind(census_fips, w_sum, b_sum, a_sum, combo_native)
races_all <- as.data.frame(races_all) 

results_list4 <- list()

for (fip in unique_fips) {
  result <- races_all %>%
    mutate_at(vars(w_sum, b_sum, a_sum, combo_native), as.numeric) %>%
    filter(census_fips == fip) %>%
    summarise(w_total = sum(w_sum),  
              b_total = sum(b_sum),
              a_total = sum(a_sum),
              combo_native_total = sum(combo_native))
  
  results_list4[[fip]] <- result
}

combined_results4 <- do.call(rbind, results_list4)

county_demos <- cbind(county_demos, combined_results4)

#include male and female county totals
county_demos$TotalMale <- county_totals$TOM_MALE
county_demos$TotalFemale <- county_totals$TOT_FEMALE

#Q6-----

#matching fips to zips

merg_county_demos <- merge(county_demos, fips_data, by.x = 'Fips', by.y = "STCOUNTYFP")
clean_group3 <- left_join(group3, merg_county_demos, by = c('ZIP.code' = 'ZIP'))

clean_group3$Fips <- as.character(clean_group3$Fips)
countydebt$`County FIPS` <- as.character(countydebt$`County FIPS`)
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
temp_nn <- na.omit(temp)
#temp <- replace(temp, c('NA', "na"), NA)

#### PCA
library(ggcorrplot)
library("FactoMineR")
library(factoextra)
# PCA is based on correlations, not distance.
## So we need to store the correlation matrix.

corr_matrix <- cor(temp_nn)
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

temp2 <- cbind(hopefully_all[,-c(35:55)], comps)

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
                 "Average.household.income..Comm.of.color",
                 "Share.of.people.of.color")    ]

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

ca <- ca %>%
  filter(complete.cases(.))

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

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

q9$year_end <- substrRight(q9$Date.received, 2)
for(i in 1:length(q9)){
    q9$Year[i] <- paste0("20", q9$year_end[i])
}

q9 <- q9 %>%
  select(Sub.product, Issue, Sub.issue, State, ZIP.code, Tags, Consumer.consent.provided., Submitted.via, Timely.response. , relief, drop, 
  Dispute_prior, servicemenber, olderAm, 
  Fips, Pop_less25, Pop_over64, Pop_Hispanic, w_total, b_total, a_total, combo_native_total, TotalMale, TotalFemale,
  Share.of.people.of.color, Average.household.income..All, Average.household.income..Comm.of.color, Average.household.income..White.comm,
 Comp.1, Comp.2,Comp.3, Comp.4, MedicalDebtClusters,  Year)

table(q9$Year) #we are dropping the variables below the year 2024

q9_2 <- subset(q9, Year == 2024)
write.csv(q9_2, "q9_2.csv")

q9_2 <- q9_2[,-37] #dropping year variable

# Transformations -------

#make factors
q9_s <- data.frame(
  lapply(q9_2, function(x) {
    if(is.character(x)) factor(x) else x
  })
)

logged_vs <- log(q9_s[,c(16:28)])
lognames <- colnames(logged_vs)
lognames <- paste("log", lognames)
colnames(logged_vs) <- lognames

squared_vs <- q9_s[,c(16:28)]^2
sqnames <- colnames(squared_vs)
sqnames <- paste("sq", sqnames)
colnames(squared_vs) <- sqnames

standardized_vs <- scale(q9_s[,c(16:28)])
stan_names <- colnames(standardized_vs)
stan_names <- paste("stan", stan_names)
colnames(standardized_vs) <- stan_names

q9_s <- cbind(q9_s, logged_vs, squared_vs, standardized_vs)

##### train and test ------
library(caret)
train_control <- trainControl(method = "cv", 
                              number = 10) 

library(caTools)
set.seed(27514234556432)
split <- sample.split(q9_s, SplitRatio = 0.7)
train <- subset(q9_s, split == "TRUE")
test <- subset(q9_s, split == "FALSE")

#Q10-----

#logit----
logit_m <- glm(relief ~ ., data = train)

plot(logit_m)

summary(logit_m)

model <- train(relief ~ Share.of.people.of.color, data = q9_s,  
               method = "glm", 
               trControl = train_control)

forward_glm <- step(glm1.5, direction = "forward", scope = formula(~ .))
backward_glm <- step(glm1.5, direction = "backward", scope = formula(~ .))  
step_glm <- step(glm1.5, direction = "both", scope = formula(~ .)) 


#lasso ----

#regression tree----

#Random Forest-----

#Gradient Boosting/ XGBoost------
