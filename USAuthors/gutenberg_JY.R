# US Authors and punctuation data
#===================================
# This code was first written by Kharanshu Patel and edited by Ju-Chi Yu
graphics.off()
rm(list = ls())

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

source("GetPuncCount.Author.R")

# All authors -----------------------------------------------------------------
aut <- gutenberg_authors
# authorOI <- c("Austen, Jane", "Dickens, Charles")#,

# Authors of interest ----
authorOI <- c("Christie, Agatha",
              "Einstein, Albert",
              "Huxley, Aldous",
              "Bierce, Ambrose",
              "Potter, Beatrix",
              "Franklin, Benjamin",
              "Russell, Bertrand",
              "Darwin, Charles",
              "Dickens, Charles",
              "Herr, Charlotte B. (Charlotte Bronte)",
              "Defoe, Daniel",
              "Lawrence, D. H. (David Herbert)",
              "Poe, Edgar Allan",
              "Burroughs, Edgar Rice",
              "Brontë, Emily",
              "Shaw, Bernard",
              "Washington, George",
              "James, Henry",
              "Wells, H. G. (Herbert George)",
              "Melville, Herman",
              "Asimov, Isaac",
              "London, Jack",
              "Joyce, James",
              "Austen, Jane",
              "Mill, John Stuart",
              "Swift, Jonathan",
              "Carroll, Lewis",
              # "Byron, Lord",
              "Twain, Mark",
              "Shelley, Mary Wollstonecraft",
              "Faraday, Michael",
              "Wilde, Oscar",
              "Wodehouse, P. G. (Pelham Grenville)",
              "Frost, Robert",
              "Stevenson, Robert Louis",
              "Kipling, Rudyard",
              "Doyle, Arthur Conan",
              "Galton, Francis",
              "Saki", # Here you are! "Munro, Hector Hugh"
              "Davy, Humphry, Sir",
              "Newton, Isaac, Sir",
              "Churchill, Winston",
              "Hardy, Thomas",
              "Eliot, T. S. (Thomas Stearns)",
              "Woolf, Virginia",
              "Maugham, W. Somerset (William Somerset)"
              )

# Punctuations of interest ------------------------------------------------
matches.all <- c(",",".","?","!",":",";","-","\u2013","\'","\"") 


# Extract data ------------------------------------------------------------
AllPunctCount.Author <- pbsapply(authorOI,GetPuncCount.Author,punct = matches.all) %>% t()
