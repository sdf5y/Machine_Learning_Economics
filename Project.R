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
group3 <- data_df[-1] #inefficient, but works

fips_data <- read.csv("zip_fips.csv")
fips_data <- fips_data[-1]#inefficient, but works

#Isolating the Dependent Variable-----

group3$relief<- ifelse(group3$Company.response.to.consumer %in% c("Closed with monetary relief", 
                                                                  "Closed with relief", 
                                                                  "Closed with non-monetary relief"),
                       1,0)

### drop non states- already run. 
group3$drop<- as.numeric(group3$State %in% c("NONE", "None", "DC", "AA", "AS", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"))

### Clustering code 
# make a separate dataset, and then make sure each variable is the right class. 
ca <- group3[, c( "Sub.product" ,
              "Issue" ,
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




#Cleaning-----
unique_zips <- unique(fips_data$ZIP)




