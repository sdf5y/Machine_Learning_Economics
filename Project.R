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
library(clustMixType)
library(wesanderson)
#install.packages("zipcodeR")
library(zipcodeR)

#Loading The Data----

group3 <- read.csv("group3.csv")
group3 <- group3[-1] #inefficient, but works

fips_data <- read.csv("zip_fips.csv")
fips_data <- fips_data[-1]#inefficient, but works

#Isolating the Dependent Variable-----

group3$relief<- ifelse(group3$Company.response.to.consumer %in% c("Closed with monetary relief", 
                                                                  "Closed with relief", 
                                                                  "Closed with non-monetary relief"),
                       1,0)

### drop non states- already run. 
group3$drop<- as.numeric(group3$State %in% c("NONE", "None", "DC", "AA", "AS", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"))

#Cleaning-----

group3 <- group3 %>%
  mutate(Company.response.to.consumer = case_when(Company.response.to.consumer == "In progress" ~ 0,
                                                  Company.response.to.consumer == "Closed with explanation" ~ 1,
                                                  Company.response.to.consumer == "Untimely response" ~ 2,
                                                  Company.response.to.consumer == "Closed with non-monetary relief" ~ 3,
                                                  Company.response.to.consumer == "Closed with monetary relief" ~ 4,
                                                  Company.response.to.consumer == "Closed" ~ 5))
   
group3 <- group3 %>%
  mutate(Sub.product = case_when(Sub.product == "Other debt" ~ "Misc. debt",
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
                                 Sub.product == 'Morgage debt'~ "Home debt", 
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
                           Issue == "Electronic communications"   ~ "Debt Notifications") )
                                 


unique(group3$Company.public.response)

group3 <- group3 %>%
  mutate(Company.public.response = case_when(Company.public.response == "Company has responded to the consumer and the CFPB and chooses not to provide a public response" ~ "No Comment",
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
                                                Consumer.consent.provided. == "N/A" ~ "No consent provided",))





#Zip code cleaning 

#Get all unique zips in our dataset
unique_zips <- unique(fips_data$ZIP)

#Get all unique USA zips
USA_zippop <- zip_code_db

#Map zips in our data not in the USA zip file
zip_binary_map <- ifelse(unique_zips %in% USA_zippop$zipcode, T,F)

#Isolate zip indicies not in USA zip file
rows_false <- zip_binary_map[zip_binary_map == FALSE ]

#Subset list of unclean zips
unclean_zips <- fips_data$ZIP[zip_binary_map == FALSE]

#Place a leading '0' for the problem zips
zip <- as.character(unclean_zips)
for(i in 1:length(zip)){
  if(as.numeric(zip[i]) < 10000){
    zip[i] <- paste0("0", zip[i])
  }
}

#retest leading zips if they are correct zips
zip_binary_map_1 <- ifelse((zip) %in% USA_zippop$zipcode, T,F)

table(zip_binary_map_1) # missing 179 zip codes. To save time, we are dropping these variables

#


### Clustering code -----
# make a separate dataset, and then make sure each variable is the right class. 
ca <- group3[, c("Sub.product",
                 "Issue",
                 "Share.with.any.debt.in.collections..All" ,
                 "Share.of.people.of.color",
                 "Average.household.income..All",
                 "pct_female")    ]
ca[,1]<- as.factor(ca[,1] )
ca[,2]<- as.factor(ca[,2] )
ca[,3]<- scale(as.numeric(ca[,3] ))
ca[,4]<- scale(as.numeric(ca[,4] ))
ca[,5]<- scale(as.numeric(ca[,5] ))
ca[,6]<- scale(as.numeric(ca[,6] ))


# 2 cluster
kpres2 <-kproto(x=ca,k=2)
# 3 cluster
kpres3 <-kproto(x=ca,k=3)
# Plots
clprofiles(kpres2, ca, col=wes_palette("Royal1",4, type="continuous")) 
# scree plot

n.scree<-ncol(ca)-2
Es<- numeric(n.scree)
for(i in 1:n.scree){ 
  kpres<-kproto(ca,k=i,nstart=5, verbose = FALSE) 
  Es[i]<-kpres$tot.withinss } 

plot(1:n.scree, Es[1:5], type="b",ylab="ObjectiveFunction",
     xlab="#Clusters", main="ScreePlot") #figure2





