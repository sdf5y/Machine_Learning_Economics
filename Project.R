setwd("C:/Users/18045/Documents/R/ML") #Sean's WD
#setwd("") #Nirav's WD
#setwd("") #Elise's WD
#setwd("") #Isaac's WD
#setwd("") #Truman's WD
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

data_df <- read.csv("group3.csv")

fips_data <- read.csv("zip_fips.csv")

dia_data <- read.csv('dia_lbls_all_overall_county_2022_02_14Sep2023.csv')