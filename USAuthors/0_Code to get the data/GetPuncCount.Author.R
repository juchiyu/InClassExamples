GetPuncCount.Author <- function(AuthorName, punct, language = 'en'){
# Extract all characters
AllText <- gutenberg_works(author == AuthorName, 
                           !str_detect(title, "Gutenberg"), # There are usually documents about how they are included in the project
                           languages = language,
                           only_text = TRUE,
                           only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE)

#-------------------------------------------------------------
# id_1 <- gutenberg_works(author == c("Austen, Jane"),
#                         !str_detect(title, "Gutenberg"), 
#                         languages = 'en',
#                         only_text = TRUE,
#                         only_languages = TRUE) %>%
#   gutenberg_download(meta_fields = "title", strip = TRUE)
#-------------------------------------------------------------

AllText.strip <- AllText %$% gutenberg_strip(text)

## Remove empty lines
empty_1 = grepl('^(\\w|\\d)*$', AllText.strip)

AllText.strip %<>% .[!empty_1] %>% paste(collapse = '\n') %>% str_replace_all("[\r\n]","")

#-------------------------------------------------------------
# empty_1 = grepl('^(\\w|\\d)*$', id_1.strip)
# id_1.strip1 = id_1.strip
# id_1.strip2 = id_1.strip
# id_1.strip1 %<>% .[!empty_1] %>% paste(collapse = '\n') %>% str_replace_all("[\r\n]","")
# id_1.strip2 = id_1.strip[! empty_1]
# id_1.strip2 = paste(id_1.strip2, collapse = '\n')
# id_1.strip2 <- str_replace_all(id_1.strip2, "[\r\n]" , "")
#-------------------------------------------------------------

AutPunc.count <- sapply(punct,  function(x) length(gregexpr(x, AllText.strip, fixed = TRUE)[[1]]))
AutPunc.count[grepl('^1$',AutPunc.count)] <- 0
return(AutPunc.count)
print(AuthorName)

}