
library(devtools)
library(ggplot2)
library(corrplot)
library(ExPosition)
library(gutenbergr)
library(dplyr)
library(tidytext)
library(stringr)
library(stringi)

aut <- gutenberg_authors

# -------------------------------------------------------------------

# Author number 1 --> Jane Austen

id_1 <- gutenberg_works(author == "Austen, Jane",
                        !str_detect(title, "Works"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE)

t.1 <- id_1 %>% count(title)

id_1.strip <- gutenberg_strip(id_1$text)

## Remove empty lines
empty_1 = grepl('^\\s*$', id_1.strip)
id_1.strip = id_1.strip[! empty_1]

id_1.strip = paste(id_1.strip, collapse = '\n')

id_1.strip <- str_replace_all(id_1.strip, "[\r\n]" , "")

matches.all <- c(",",".","?","!",":",";") 
punc.aut1 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                              id_1.strip, fixed = TRUE)[[1]]))

# Author number 2 --> Charles Dickens

id_2 <- gutenberg_works(author == "Dickens, Charles",
                        !str_detect(title, "Vol."),
                        !str_detect(title, "Works"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE)

t.2 <- id_2 %>% count(title)

id_2.strip <- gutenberg_strip(id_2$text)

## Remove empty lines
empty_2 = grepl('^\\s*$', id_2.strip)
id_2.strip = id_2.strip[! empty_2]

id_2.strip = paste(id_2.strip, collapse = '\n')

id_2.strip <- str_replace_all(id_2.strip, "[\r\n]" , "")

punc.aut2 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                              id_2.strip, fixed = TRUE)[[1]]))

# Author number 3 --> James Joyce

id_3 <- gutenberg_works(author == "Joyce, James",
                        !str_detect(title, "Works"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE)

t.3 <- id_3 %>% count(title)

id_3.strip <- gutenberg_strip(id_3$text)

## Remove empty lines
empty_3 = grepl('^\\s*$', id_3.strip)
id_3.strip = id_3.strip[! empty_3]

id_3.strip = paste(id_3.strip, collapse = '\n')

id_3.strip <- str_replace_all(id_3.strip, "[\r\n]" , "")

punc.aut3 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                              id_3.strip, fixed = TRUE)[[1]]))

# Author number 4 --> Herman Melville

id_4 <- gutenberg_works(author == "Melville, Herman",
                        !str_detect(title, "Works"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE)

t.4 <- id_4 %>% count(title)

id_4.strip <- gutenberg_strip(id_4$text)

## Remove empty lines
empty_4 = grepl('^\\s*$', id_4.strip)
id_4.strip = id_4.strip[! empty_4]

id_4.strip = paste(id_4.strip, collapse = '\n')

id_4.strip <- str_replace_all(id_4.strip, "[\r\n]" , "")

punc.aut4 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                              id_4.strip, fixed = TRUE)[[1]]))

# Author number 5 --> Mark Twain

id_5 <- gutenberg_works(author == "Twain, Mark", 
                        !str_detect(title, "Works"),
                        !str_detect(title, "Volume"),
                        !str_detect(title, "Chapter"),
                        !str_detect(title, "Part"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE)

t.5 <- id_5 %>% count(title)

id_5.strip <- gutenberg_strip(id_5$text)

## Remove empty lines
empty_5 = grepl('^\\s*$', id_5.strip)
id_5.strip = id_5.strip[! empty_5]

id_5.strip = paste(id_5.strip, collapse = '\n')

id_5.strip <- str_replace_all(id_5.strip, "[\r\n]" , "")

punc.aut5 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                              id_5.strip, fixed = TRUE)[[1]]))

# Author number 6 --> P.G. Wodehouse 

id_6 <- gutenberg_works(author == "Wodehouse, P. G. (Pelham Grenville)",
                        !str_detect(title, "Works"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title")

t.6 <- id_6 %>% count(title)

id_6.strip <- gutenberg_strip(id_6$text)

## Remove empty lines
empty_6 = grepl('^\\s*$', id_6.strip)
id_6.strip = id_6.strip[! empty_6]

id_6.strip = paste(id_6.strip, collapse = '\n')

id_6.strip <- str_replace_all(id_6.strip, "[\r\n]" , "")

punc.aut11 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                               id_6.strip, fixed = TRUE)[[1]]))

# Author number 7 --> Jonathan Swift

id_7 <- gutenberg_works(author == "Swift, Jonathan", !str_detect(title, "Works"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE) 

t.7 <- id_7 %>% count(title)

id_7.strip <- gutenberg_strip(id_7$text)

## Remove empty lines
empty_7 = grepl('^\\s*$', id_7.strip)
id_7.strip = id_7.strip[! empty_7]

id_7.strip = paste(id_7.strip, collapse = '\n')

id_7.strip <- str_replace_all(id_7.strip, "[\r\n]" , "")

punc.aut7 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                              id_7.strip, fixed = TRUE)[[1]]))

# Author number 8 --> Edgar Allen Poe

id_8 <- gutenberg_works(author == "Poe, Edgar Allan", 
                        !str_detect(title, "Works"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE)

t.8 <- id_8 %>% count(title)

id_8.strip <- gutenberg_strip(id_8$text)

## Remove empty lines
empty_8 = grepl('^\\s*$', id_8.strip)
id_8.strip = id_8.strip[! empty_8]

id_8.strip = paste(id_8.strip, collapse = '\n')

id_8.strip <- str_replace_all(id_8.strip, "[\r\n]" , "")

punc.aut8 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                              id_8.strip, fixed = TRUE)[[1]]))

# Author number 9 --> Saki

id_9 <- gutenberg_works(author == "Saki",
                        !str_detect(title, "Works"),
                        languages = 'en',
                        only_text = TRUE,
                        only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title")

t.9 <- id_9 %>% count(title)

id_9.strip <- gutenberg_strip(id_9$text)

## Remove empty lines
empty_9 = grepl('^\\s*$', id_9.strip)
id_9.strip = id_9.strip[! empty_9]

id_9.strip = paste(id_9.strip, collapse = '\n')

id_9.strip <- str_replace_all(id_9.strip, "[\r\n]" , "")

punc.aut9 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                              id_9.strip, fixed = TRUE)[[1]]))

# Author number 10 --> Mary Shelley

id_10 <- gutenberg_works(author == "Shelley, Mary Wollstonecraft",
                         !str_detect(title, "Works"),
                         languages = 'en',
                         only_text = TRUE,
                         only_languages = TRUE) %>%
  gutenberg_download(meta_fields = "title", strip = TRUE)

t.10 <- id_10 %>% count(title)

id_10.strip <- gutenberg_strip(id_10$text)

## Remove empty lines
empty_10 = grepl('^\\s*$', id_10.strip)
id_10.strip = id_10.strip[! empty_10]

id_10.strip = paste(id_10.strip, collapse = '\n')

id_10.strip <- str_replace_all(id_10.strip, "[\r\n]" , "")

punc.aut10 <- sapply(matches.all,  function(x) length(gregexpr(x, 
                                                               id_10.strip, fixed = TRUE)[[1]]))


#### -------------------  ###### ------------------------ #######------------------------------------

cont.tab <- bind_rows(punc.aut1,punc.aut2,punc.aut3,punc.aut4,punc.aut5,punc.aut6,punc.aut7,punc.aut8,punc.aut9,punc.aut10)

typeof(cont.tab)

mat.cont.tab <- as.matrix(cont.tab)

rownames(mat.cont.tab) <- c("Austen, Jane",
                            "Dickens, Charles",
                            "Joyce, James",
                            "Melville, Herman",
                            "Twain, Mark",
                            "Swift, Jonathan",
                            "Poe, Edgar Allan",
                            "Saki",
                            "Shelley, Mary Wollstonecraft",
                            "Wodehouse, P. G. (Pelham Grenville)")

################### Correspondence Analysis---------------------------------------------------

# color

col4authors = c("#00fcfe","#ff2ae3","#000969","#ab0205","#9800ff","#ff6205","#00688b","#ffe61c","gray 50","greenyellow")

col4punc = c("greenyellow","green","green4","darkolivegreen","yellow","gold","gold4")

# PCA

resPCA <- epPCA(DATA = mat.cont.tab,
                scale = TRUE,
                graphs =  FALSE # TRUE first pass only
)
eigs <- resPCA$ExPosition.Data$eigs
PCA.plot.fi <- prettyPlot(resPCA$ExPosition.Data$fi, col = col4authors, display_names = TRUE)

PCAplot.fj <- prettyPlot(resPCA$ExPosition.Data$fj, col = col4punc, display_names = TRUE)


# Inference Battery

resPCA.inf <- InPosition::epPCA.inference.battery(DATA = mat.cont.tab,
                                                  scale = 'SS1', 
)

# cA

resCA.sym  <- epCA(mat.cont.tab, symmetric = TRUE)

# asymetric
resCA.asym <- epCA(mat.cont.tab, symmetric = FALSE)

# Factor Scores
Fj.a <- resCA.asym$ExPosition.Data$fj
Fi   <- resCA.sym$ExPosition.Data$fi
Fj   <- resCA.sym$ExPosition.Data$fj

# Constraints
constraints.sym <- minmaxHelper(mat1 = Fi, mat2  = Fj)
constraints.asym <- minmaxHelper(mat1 = Fi, mat2  = Fj.a)

# Heat Map

heatMapIJ.aut <- makeggHeatMap4CT(mat.cont.tab,
                                  colorAttributes = col4punc,
                                  colorProducts = col4authors,
                                  fontSize.x = 15)
print(heatMapIJ.aut)

# Scree Plot

PlotScree(ev = eigs, 
          p.ev = resPCA.inf$Inference.Data$components$p.vals,
          title = 'Eigenvalues Inference',
          plotKaiser = TRUE
)

# Base Map

baseMap.i <- createFactorMap(Fi, constraints = constraints.sym,
                             col.points = col4authors,
                             col.labels = col4authors)
print(baseMap.i$zeMap)

baseMap.j <- createFactorMap(Fj, constraints = constraints.sym,
                             color.points = gplot::col2hex(col4punc),
                             color.labels = gplot::col2hex(col4punc),
                             pch = 11)
print(baseMap.j$zeMap)

# Factor Scores : Symmetric

symMap  <- createFactorMapIJ(Fi,Fj,
                             col.points.i = col4authors,
                             col.labels.i = col4authors,
                             col.points.j = col4punc,
                             col.labels.j = col4punc,
                             pch.i = 15, pch.j = 19,
                             alpha.labels.i = 0.8,
                             alpha.labels.j = 0.8,
                             alpha.axes = 0.2,
                             col.axes = "#333434",
                             col.background = "#fff3e1"
)
labels4CA <- PTCA4CATA::createxyLabels.gen(1,2, lambda = resCA.sym$ExPosition.Data$eigs, tau = resCA.sym$ExPosition.Data$t)

map.IJ.sym <- symMap$baseMap + symMap$I_labels + symMap$I_points +
  symMap$J_labels + symMap$J_points + labels4CA +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(map.IJ.sym)

# Factor scores : Asymmetric

asymMap  <- createFactorMapIJ(Fi,Fj.a,
                              col.points.i = col4authors,
                              col.labels.i = col4authors,
                              col.points.j = col4punc,
                              col.labels.j = col4punc,
                              pch.i = 15, pch.j = 19,
                              alpha.labels.i = 0.8,
                              alpha.labels.j = 0.8,
                              alpha.axes = 0.2,
                              col.axes = "#333434",
                              col.background = "#f6e9e9"
)

map.IJ.asym <- asymMap$baseMap + asymMap$I_labels + 
  asymMap$I_points + asymMap$J_labels + 
  asymMap$J_points + labels4CA + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(map.IJ.asym)

# Signed Contributions

signed.ctrJ <- resPCA$ExPosition.Data$cj * sign(resPCA$ExPosition.Data$fj)

### Dimension 1
ctrJ.s.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                           threshold = 1 / NROW(signed.ctrJ),
                           font.size = 5,
                           color4bar = gplots::col2hex(col4punc), 
                           main = 'Variable Contribution 1 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(ctrJ.s.1)

### Dimension 2

ctrJ.s.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                           threshold = 1 / NROW(signed.ctrJ),
                           font.size = 5,
                           color4bar = gplots::col2hex(col4punc), # we need hex code
                           main = 'Variable Contribution 2 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(ctrJ.s.2)

### Dimension 3

ctrJ.s.3 <- PrettyBarPlot2(signed.ctrJ[,3],
                           threshold = 1 / NROW(signed.ctrJ),
                           font.size = 5,
                           color4bar = gplots::col2hex(col4punc),
                           main = 'Variable Contribution 3 (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(ctrJ.s.3)

# Bootstrap Ratios

BR <- resPCA.inf$Inference.Data$fj.boots$tests$boot.ratios

### Dimension 1

laDim = 1
ba001.BR1 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            color4bar = gplots::col2hex(col4punc),
                            main = paste0(
                              'Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
                            #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(ba001.BR1)

### Dimension 2

laDim = 2
ba002.BR2 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            color4bar = gplots::col2hex(col4punc),
                            main = paste0(
                              'Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
)
print(ba002.BR2)

### Dimension 3

laDim = 3
ba002.BR3 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            color4bar = gplots::col2hex(col4punc),
                            main = paste0(
                              'Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
)
print(ba002.BR3)
