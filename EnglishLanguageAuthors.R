library(PTCA4CATA)
library(data4PCCAR)
library(ExPosition)
library(rio)
library(tidyverse)

rm(list = ls())
load("USAuthors/Data/AllAuthorWithYear.rda")

auth.forCA <- authors.with.birth %>% remove_rownames() %>%
   column_to_rownames(var = "Author Name") %>% 
   select(Comma, Period, Other)

resCA <- epCA(auth.forCA, DESIGN = authors.with.birth$Nationality)




