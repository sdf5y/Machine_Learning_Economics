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
unique_zips <- unique(fips_data$ZIP)

unique(group3$Company.response.to.consumer)

group3 <- group3 %>%
  mutate(Company.response.to.consumer = case_when(Company.response.to.consumer == "In progress" ~ 0,
                                                  Company.response.to.consumer == "Closed with explanation" ~ 1,
                                                  Company.response.to.consumer == "Untimely response" ~ 2,
                                                  Company.response.to.consumer == "Closed with non-monetary relief" ~ 3,
                                                  Company.response.to.consumer == "Closed with monetary relief" ~ 4,
                                                  Company.response.to.consumer == "Closed" ~ 5))
   
unique(group3$Product) #one category
unique(group3$Sub.product) #19 types of sub categories of debt

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
                                 Sub.product == "Payday loan" ~ "Payday debt")) #missing one variable, lets check it later
                                 
unique(group3$Sub.product)                                 


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





