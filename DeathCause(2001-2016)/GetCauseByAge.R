GetCauseByAge <- function(year){
  data <- sprintf("RawData/Multiple Cause of Death, %s.txt", year) %>% # create file name
    data.frame(read.table(.,header = TRUE,sep = "\t")) %>%             # read data file
    .[grep("^[#]",.$UCD...ICD.10.113.Cause.List,value = FALSE),]       # keep only the general causes (starts with #)
  
  CauseList.vec <- matrix(data$UCD...ICD.10.113.Cause.List, nrow = 51, byrow = FALSE) %>% apply(1,unique)
  AgeList.vec <- matrix(data$Five.Year.Age.Groups.Code, nrow = 51, byrow = FALSE) %>% apply(2,unique)
  
  data.out <- matrix(data$Deaths, nrow = 51, byrow = FALSE, dimnames = list(CauseList.vec,AgeList.vec))
  return(data.out)
}