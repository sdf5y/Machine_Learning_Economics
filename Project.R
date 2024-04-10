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
group3$drop<- as.numeric(group3$State %in% c("NONE", "None", "DC", "AA", "AS", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"))
fips_data <- fips_data[!fips_data$STATE %in% c("NONE", "None", "DC", "AA", "AS", "FM","GU", "MH", "MP", "PR", "VI", "UNITED STATES MINOR OUTLYING ISLANDS"),]

countydebt <- countydebt[,-c(4, 7, 10, 13, 16, 19, 22, 26)]

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

#Numbered Factors ----
group3 <- group3 %>%
  mutate(Company.response.to.consumer = case_when(Company.response.to.consumer == "In progress" ~ 0,
                                                  Company.response.to.consumer == "Closed with explanation" ~ 1,
                                                  Company.response.to.consumer == "Untimely response" ~ 2,
                                                  Company.response.to.consumer == "Closed with non-monetary relief" ~ 3,
                                                  Company.response.to.consumer == "Closed with monetary relief" ~ 4,
                                                  Company.response.to.consumer == "Closed" ~ 5),
         Sub.product = case_when(Sub.product == "Other debt" ~ 0,
                                 Sub.product == 'Other (i.e. phone, health club, etc.)' ~ 0,
                                 Sub.product == "Telecommunications debt" ~ 0,
                                 Sub.product == 'I do not know' ~ 0,
                                 Sub.product == "Federal student loan debt" ~ 1,
                                 Sub.product == "Federal student loan"   ~ 1,
                                 Sub.product == "Private student loan debt" ~ 2,
                                 Sub.product == "Non-federal student loan"  ~ 2,
                                 Sub.product == "Auto" ~ 3,
                                 Sub.product == "Auto debt" ~ 3,
                                 Sub.product == "Credit card debt" ~ 4,
                                 Sub.product == "Credit card" ~ 4, 
                                 Sub.product == 'Morgage debt'~ 5, 
                                 Sub.product == "Mortgage" ~ 5,
                                 Sub.product == 'Medical' ~ 6,
                                 Sub.product == "Medical debt" ~ 6,
                                 Sub.product == "Rental debt" ~ 7,
                                 Sub.product == "Payday loan debt" ~ 8,
                                 Sub.product == "Payday loan" ~ 8),
         Issue = case_when(Issue == "Attempts to collect debt not owed" ~ 0,
                           Issue == 'Cont\'d attempts collect debt not owed' ~ 0,
                           Issue == "Took or threatened to take negative or legal action" ~ 1,
                           Issue == 'Taking/threatening an illegal action' ~ 1,
                           Issue == "Threatened to contact someone or share information improperly" ~ 2,
                           Issue == "Communication tactics" ~ 2,
                           Issue == "False statements or representation" ~ 2,
                           Issue == "Disclosure verification of debt" ~ 2,
                           Issue == "Improper contact or sharing of info" ~ 2,
                           Issue == "Written notification about debt" ~ 3,
                           Issue == "Electronic communications"   ~ 3),
         Company.public.response = case_when(Company.public.response == "Company has responded to the consumer and the CFPB and chooses not to provide a public response" ~ 0,
                                             Company.public.response == "Company chooses not to provide a public response" ~ 0,
                                             Company.public.response == "Company believes complaint represents an opportunity for improvement to better serve consumers" ~ 1,
                                             Company.public.response == "Company believes the complaint provided an opportunity to answer consumer's questions" ~ 1,
                                             Company.public.response == "Company believes the complaint is the result of a misunderstanding" ~ 2,
                                             Company.public.response == "Company believes complaint is the result of an isolated error" ~ 2),
         Consumer.consent.provided. = case_when(Consumer.consent.provided. == "Consent provided" ~ 1,
                                                Consumer.consent.provided. == "None"  ~ 0,
                                                Consumer.consent.provided. == "Consent not provided"  ~ 0,
                                                Consumer.consent.provided. == "Other"  ~ 0,
                                                Consumer.consent.provided. == "Consent withdrawn"   ~ 0,
                                                Consumer.consent.provided. == "N/A" ~ 0))

#Dummy for after date (form changes NA imputation)
group3$Dispute_prior <- ifelse(group3$Date.received  > '04/24/17', 1,0)

unique(group3$Company.response.to.consumer)

#Zip Code Cleaning -----

#Evaluate the number of incorrect zip codes
#Fips Zips Clean 
unique_zips <- unique(fips_data$ZIP) #unique fips zips
USA_zippop <- zip_code_db #unique usa zips

#Map zips in our data not in the USA zip file
zip_binary_map <- ifelse(unique_zips %in% USA_zippop$zipcode, T,F)

#Place a leading zero for the problem zips
zip <- as.character(fips_data$ZIP[zip_binary_map == FALSE])
for(i in 1:length(zip)){
  if(as.numeric(zip[i]) < 10000){
    zip[i] <- paste0("0", zip[i])
  }
}

#retest leading zips if they are correct zips
table(ifelse(zip %in% USA_zippop$zipcode, T,F)) # 2 zip codes are still incorrect. To save time, we are dropping these variables

#recreate zips in fips data
fips_data$ZIP[which(zip_binary_map == F)] <- zip

table(ifelse(fips_data$ZIP %in% USA_zippop$zipcode, T,F))

#rm(i, zip, zip_binary_map, zip_binary_map_1, unique_zips, unclean_zips) #remove these variables when done.

#Main dataset zip cleaning
unique_zips <- unique(group3$ZIP.code)

#Get all unique USA zips
USA_zippop <- zip_code_db

#Map zips in our data not in the USA zip file
zip_binary_map <- ifelse(group3$ZIP.code %in% USA_zippop$zipcode, T,F)

#Place a leading zero for the problem zips
zip <- as.character(group3$ZIP.code[zip_binary_map == FALSE])
for(i in 1:length(zip)){
  if(as.numeric(zip[i]) < 10000){
    zip[i] <- paste0("0", zip[i])
  }
}

#retest leading zips if they are correct zips

table(ifelse(zip %in% USA_zippop$zipcode, T,F)) # None have leading zero issues. Use another method

#insert zips in data

group3$ZIP.code[which(zip_binary_map == F)] <- zip

table(ifelse(group3$ZIP.code %in% USA_zippop$zipcode, T,F))

rm(i, zip, zip_binary_map, zip_binary_map_1, unique_zips, unclean_zips) #remove these variables when done.

#install.packages('stringr')

table(str_detect( as.character(group3$ZIP.code), "[0-9]+$")) #looks like they're all numbers

group3$zip_err_5less <- ifelse(nchar(group3$ZIP.code) >5 , 1,0) #6 entries greater than 5 digits

table(group3$zip_err_5less )

group3 <- subset(group3, nchar(group3$zip_err_5less)==T) #dropping the 6 rows that 

group3 <- subset(group3, group3$ZIP.code %in% USA_zippop$zipcode) #dropping zips not in usa zip codes

#Q3 ----

#merge 
merg_zips <- merge(group3, fips_data, by.x =  'ZIP.code', by.y = 'ZIP')

#Q4 -----

merg_fips <- merge(merg_zips, countydebt,  by.x = 'STCOUNTYFP', by.y = 'County FIPS') #by = c('STCOUNTYFP' =  'County FIPS')) 

#Q5----

census_fips <- (paste(census$STATE, census$COUNTY, sep = ""))

census <- cbind(census, census_fips)

#Prep Split Dataset by Male/Female ----
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

#Q6-----

#matching fips to zips

merg_county_demos <- merge(county_demos, fips_data, by.x = 'Fips', by.y = "STCOUNTYFP")
clean_group3 <- left_join(group3, merg_county_demos, by = c('ZIP.code' = 'ZIP'))

#Q7------

#making csvs to save time in cleanup -----
#write.csv(group3, "cleangroup3.csv")
#write.csv(county_demos, "county_demos.csv")
#write.csv(census, "cleancensus.csv")
#write.csv(merg_fips, 'merg_fips.csv')

data_census <- merge(merg_fips, census, by.x = 'STCOUNTYFP', by.y = "census_fips")

write.csv(data_census, "final_group3.csv")



### cluster analysis
# must normalize data

z <- male_df[,-c(1,25)]
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)
distance = dist(nor)

library(factoextra)
## 4 clusters for 4 race groups
male_km <- kmeans(nor, 4, nstart = 79)
print(male_km)
fviz_cluster(male_km, data = nor,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "pink"),
             geom = "point",
             ellipse.type = "convex",ggtheme = theme_bw()
)

male_clusters <- male_km$cluster
temp <- male_df[male_clusters==4,]

print(temp)

student.hclust = hclust(distance, method = 'average')
print(student.hclust)
plot(student.hclust, label=FALSE, main='Default from hclust for Student data')

## Choose the number of clusters and get the groupings
student_cut <- cutree(student.hclust, k = 5)
table(student_cut)




variance_explained <- male_re$sd^2/sum(male_re$sd^2)*100 
variance_explained[1:which.max(variance_explained >= 0.9)] 

qplot(c(1:length(variance_explained)) , variance_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot of Males Ethnicities") +
  ylim(0, 100) 

#install.packages('pls')
library(pls)

pls_model <- plsr(y_stan ~ ., data = as.data.frame(x_stan), ncomp = 12)  
summary(pls_model) #99.92 of the variance explained with 4 comps

pls_model$loadings[, 1:5]

#rm(x_stan, y_stan, variance_explained, pls_model, male_re)



library(ggcorrplot)
library("FactoMineR")
# PCA is based on correlations, not distance.
## So we need to store the correlation matrix.
corr_matrix <- cor(male_df[,-c(1, 25)])
ggcorrplot(corr_matrix)

## do PCA
male_eth_race.pca <- princomp(corr_matrix)
summary(male_eth_race.pca)
fviz_eig(male_eth_race.pca, addlabels = TRUE)

# loadings for first 5 components
loadings_m <- male_eth_race.pca$loadings[, 1:5]
loadings_m
eigenvalues <- male_eth_race.pca$sdev^2

loadings_original_scale <- loadings_m[, 1:5] * sqrt(eigenvalues)


#scree plot
variance_explained <- male_eth_race.pca $sd^2/sum(male_eth_race.pca$sd^2)*100 
variance_explained[1:8] 
qplot(c(1:8), variance_explained[1:8]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 100) 

qplot(c(1:length(variance_explained[1:8])) , variance_explained[1:8]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot of Males Ethnicities") +
  ylim(0, 100)












#female PCA

x_stan <- scale(female_df[,-c(1)])
y_stan <- scale(female_df[, 1])

female_re <- prcomp(x_stan)

variance_explained <- female_re$sd^2/sum(female_re$sd^2)*100 
variance_explained[1:which.max(variance_explained >= 0.9)] 

qplot(c(1:length(variance_explained)) , variance_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot of Females Ethnicities") +
  ylim(0, 100) 

#install.packages('pls')
library(pls)

pls_model <- plsr(y_stan ~ ., data = as.data.frame(x_stan), ncomp = 12)  
summary(pls_model) #99.93 of the variance explained with 4 comps

pls_model$loadings[, 1:5]

rm(x_stan, y_stan, variance_explained, pls_model, female_re)




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





