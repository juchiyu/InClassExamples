
# General info: -----------------------------------------------------------
# This script is to extract the Cause of Death data from 2001-2016
# Author: Ju-Chi Yu
# Date: 9-25-2020
# Packages and functions --------------------------------------------------
library(devtools)
library(ggplot2)
library(corrplot)
library(ExPosition)
library(gutenbergr)
library(dplyr)
library(tidytext)
library(magrittr)
library(stringr)
library(stringi)
library(pbapply)

# Read table (try) --------------------------------------------------------
year <- c("2001")

filename <- sprintf("RawData/Multiple Cause of Death, %s.txt", year)

dc01 <- data.frame(read.table(filename,header = TRUE,sep = "\t"))
dc01$UCD...ICD.10.113.Cause.List

dc01.gCat <- dc01[grep("^[#]",dc01$UCD...ICD.10.113.Cause.List,value = FALSE),]
CauseList.mat <- matrix(dc01.gCat$UCD...ICD.10.113.Cause.List, nrow = 51, byrow = FALSE)
AgeList.mat <- matrix(dc01.gCat$Five.Year.Age.Groups.Code, nrow = 51, byrow = FALSE)
AgeList.vec <- apply(AgeList.mat,2,unique)
CauseList.vec <- apply(CauseList.mat,1,unique)
matrix(dc01.gCat$Deaths, nrow = 51, byrow = FALSE, dimnames = list(CauseList.vec,AgeList.vec))


# In pipeline -------------------------------------------------------------
year <- c("2001")

data <- sprintf("RawData/Multiple Cause of Death, %s.txt", year) %>% # create file name
  data.frame(read.table(.,header = TRUE,sep = "\t")) %>%             # read data file
  .[grep("^[#]",.$UCD...ICD.10.113.Cause.List,value = FALSE),]       # keep only the general causes (starts with #)

CauseList.vec <- matrix(data$UCD...ICD.10.113.Cause.List, nrow = 51, byrow = FALSE) %>% apply(1,unique)
AgeList.vec <- matrix(data$Five.Year.Age.Groups.Code, nrow = 51, byrow = FALSE) %>% apply(2,unique)

data.out <- matrix(data$Deaths, nrow = 51, byrow = FALSE, dimnames = list(CauseList.vec,AgeList.vec))


# Function ----------------------------------------------------------------

source("GetCauseByAge.R")
GetCauseByAge(year) # check

# Now for all years
year <- c(2001:2018)

# Creat an empty array

DCdata <- lapply(year, GetCauseByAge)
save(DCdata,file = "DCdata.rda")
