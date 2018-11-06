# InClassExamples
This repository stores the code (and the output) for examplar in-class data. Here are the list of the data sets:
1. Cause of death in US from 2010-2016 (extracted from https://wonder.cdc.gov/mcd.html)
    
    o rows: 51 causes of deathe
    
    o columns: age group from 1, 1-4, 5-9,...,95-99, 100+, NS (not specified)
    
    o tables: years (2011-2016)
   
   - data are counts

2. The punctuation data of authors in English literature (extracted from the Gutenberg Project package in R)
    
    o rows: authors of interest
    
    o columns: punctuations of interest
   
   - data are counts
   
   - Problem to solve: 
       
       x I am missing three books:
               
               A modest proposal by Jonathan Swift
               
               The call of the wild by Jack London
               
               Embankment at night by DH Lawrence
               
               Adventure of Huckleberry Fin Ch1-5 by Mark Twain
               
               Chapters from my autobiography by Mark Twain
               
               Eve's Diary by Mark Twain
       
       x Some books of some authors are counted twice because they include both the complete version and the version with separated       chapters. For example, Adventure of Huckleberry Fin and Eve's Diary by Mark Twain. For this problem, we need to take a closer look at every author.
       change
