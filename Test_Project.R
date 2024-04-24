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

testgroup3 <- read_csv("test_all.csv", col_types = cols(...1 = col_skip(), 
                                                                    X = col_skip()))

county_demos <- read_csv("county_demos.csv", col_types = cols(...1 = col_skip()))

countydebt <- read_excel("dia_lbls_all_overall_county_2022_02_14Sep2023.xlsx")

fips_data <- read_csv("zip_fips.csv", col_types = cols(...1 = col_skip(), 
                                                       STCOUNTYFP = col_number()))

#Isolating the Dependent Variable-----

testgroup3$relief<- ifelse(testgroup3$Company.response.to.consumer %in% c("Closed with monetary relief", 
                                                                  "Closed with relief", 
                                                                  "Closed with non-monetary relief"),
                       1,0)

#Cleaning-----

#drop non states 
testgroup3$drop <- as.numeric(testgroup3$State %in% c("NONE", "None", "DC", "AA", "AS", "AP", "AE", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"))
testgroup3 <- subset(testgroup3, !(State %in% c("AE", 'AP'))) #dropping military bases

#older Americans and servicefolk
testgroup3$servicemember <- ifelse(str_detect(testgroup3$Tags, "Servicemember"), 1, 0)
testgroup3$olderAm <- ifelse(str_detect(testgroup3$Tags, "Older American"), 1, 0)

fips_data <- fips_data[!fips_data$STATE %in% c("NONE", "None", "DC", "AA", "AS", 'AP', "AE", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"),]

ctfips <-  (ifelse((fips_data$STATE == 'CT' & str_detect(fips_data$STCOUNTYFP, "9")), 
                   as.numeric(paste0("01", fips_data$STCOUNTYFP)), fips_data$STCOUNTYFP))

fips_data$STCOUNTYFP <- ctfips

countydebt$`County FIPS` <- as.numeric(countydebt$`County FIPS`)

#Named Factors ----
testgroup3 <- testgroup3 %>%
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
testgroup3$Date.received <- as.Date(testgroup3$Date.received, format = "%m/%d/%y")

testgroup3$Dispute_prior <- ifelse(testgroup3$Date.received  > as.Date('04/24/17', format= "%m/%d/%y"), 1,0)

#####Zip Code Cleaning#### -----
USA_zippop <- zip_code_db

#Clean Zip Reference data first ----

fips_data$ZIP <- as.character(fips_data$ZIP)
testgroup3$ZIP.code <- as.character(testgroup3$ZIP.code)

#check zips including numbers
table(str_detect( as.character(fips_data$ZIP), "[0-9]+$")) #looks like they're all numbers

#check zips less than 5 characters
table(nchar(fips_data$ZIP) < 5) #2900 entries less than 5 digits - leading zeros?
invalidzip <- fips_data$ZIP[nchar(fips_data$ZIP) < 5]

table(nchar(fips_data$ZIP) > 5) #No entries greater than 5 digits

#Check leading zeros
table(grepl("^0", testgroup3$ZIP.code)) #looks like they already have leading zeros

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
table(str_detect( as.character(testgroup3$ZIP.code), "[0-9]+$")) #looks like they're all numbers

#check zips less than 5 characters
table(nchar(testgroup3$ZIP.code) < 5) #No entries less than 5 digits
table(nchar(testgroup3$ZIP.code) > 5) #No entries greater than 5 digits

#Check leading zeros
table(grepl("^0", testgroup3$ZIP.code)) #looks like they already have leading zeros

unique_zips <- unique(testgroup3$ZIP.code)

zip_binary_map <- unique_zips %in% fips_data$ZIP

table(zip_binary_map)

error_zips <- unique_zips[!zip_binary_map] #erroneous zips

#T.test of error zips -----
t.test(table(zip_binary_map), alternative = 'two.sided') #fail to reject the null - the error zips are not significant at 5% level

for (error_state in unique(testgroup3$State[testgroup3$ZIP.code %in% error_zips])) {
  mode_zip <- names(sort(table(testgroup3$ZIP.code[testgroup3$State == error_state]), decreasing = TRUE)[1])
  testgroup3$ZIP.code[testgroup3$ZIP.code %in% error_zips & testgroup3$State == error_state] <- mode_zip
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

for (error_state in unique(testgroup3$State[testgroup3$ZIP.code %in% error_zips])) {
  mode_zip <- names(sort(table(testgroup3$ZIP.code[testgroup3$State == error_state]), decreasing = TRUE)[1])
  group3$ZIP.code[testgroup3$ZIP.code %in% error_zips & testgroup3$State == error_state] <- mode_zip
}

#Check if this worked
unique_zips <- unique(testgroup3$ZIP.code)
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

#Merges ---- 

merg_zips <- merge(testgroup3, fips_data, by.x =  'ZIP.code', by.y = 'ZIP')

merg_fips <- merge(merg_zips, countydebt,  by.x = 'STCOUNTYFP', by.y = 'County FIPS')

cleantest <- merge(merg_fips, county_demos,  by.x = 'STCOUNTYFP', by.y = 'Fips')

#PCA ----

colnames(cleantest)

temp <- cleantest[, c(32:52)]
temp <- sapply(temp, as.numeric)

imputation_map_pca_val <- is.na(temp)

temp_imputed <- apply(temp, 2, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
})

#### PCA
library(ggcorrplot)
library("FactoMineR")
library(factoextra)

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
temp2 <- cbind(cleantest[, -c(29:52)], comps)

colnames(temp2)

write.csv(temp2, "NWA_test.csv")

#### Clustering code ------
# make a separate dataset, and then make sure each variable is the right class. 
set.seed(23748234)

NWA_test <- read_csv("NWA_test.csv", col_types = cols(...1 = col_skip()))

ca <- NWA_test[, c("Sub.product",  
                   "Issue",
                   "Pop_over64",
                   "TotalFemale",
                   "Average household income, Comm of color",
                   "Share of people of color")]

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

impute_map_medical_val <- is.na(ca)

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

NWA_test$MedicalDebtClusters <- kpres3$cluster

colnames(NWA_test) 

write.csv(NWA_test, 'q9_test.csv')

#Q9 -----

q9_test <- read_csv("q9_test.csv", col_types = cols(...1 = col_skip()))
colnames(q9_test)
#rename to match
q9_test$Fips <- q9_test$STCOUNTYFP

q9_test$`Share.of.people.of.color` <- q9_test$`Share of people of color`
q9_test$`Average.household.income..All` <- q9_test$`Average household income, All`
q9_test$`Average.household.income..Comm.of.color` <- q9_test$`Average household income, Comm of color`
q9_test$`Average.household.income..White.comm` <- q9_test$`Average household income, White comm`

library(lubridate)

q9_test$Year <- as.factor(year(q9_test$Date.received))

q9_test <- q9_test %>%
  select(Sub.product, Issue, Sub.issue, Consumer.consent.provided., Submitted.via, Timely.response. , relief, drop, 
         Dispute_prior, servicemember, olderAm, 
         Fips, Pop_less25, Pop_over64, Pop_Hispanic, White, Black, Asian, Indigenous, Native, TotalMale, TotalFemale,
         Share.of.people.of.color, Average.household.income..All, Average.household.income..Comm.of.color, Average.household.income..White.comm,
         Comp.1, Comp.2,Comp.3, Comp.4, MedicalDebtClusters,  Year)

table(q9_test$Year) 

#Transformations -------

#make factors
q9_s_test <- data.frame(
  lapply(q9_test, function(x) {
    if(is.character(x)) factor(x) else x
  })
)

q9_s_test <- q9_s_test %>%
  filter(complete.cases(.))

write.csv(q9_s_test, "q9_s_test.csv")
