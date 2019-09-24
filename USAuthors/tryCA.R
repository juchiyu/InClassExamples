library(ExPosition)
library(PTCA4CATA)

load("AllAuthorPunct.rda")

ca.res <- epCA(AllPunctCount.Author_simple, symmetric = TRUE, graphs = FALSE)

ca.plot <- createFactorMapIJ(ca.res$ExPosition.Data$fi, ca.res$ExPosition.Data$fj,
                            text.cex.i = 2,
                            constraints = minmaxHelper(ca.res$ExPosition.Data$fi, ca.supp$fjj))

ca.plot$baseMap + ca.plot$I_labels + ca.plot$I_points + ca.plot$J_points + ca.plot$J_labels

ca.supp <- supplementaryCols(AllPunctCount.Author[,3:ncol(AllPunctCount.Author)], ca.res)

supp.plot <- createFactorMap(ca.supp$fjj,
                             col.points = wes_palette("BottleRocket2")[2],
                             col.labels = wes_palette("BottleRocket2")[2],
                             text.cex = 4,
                             pch = 17)

ca.plot$baseMap + ca.plot$I_labels + ca.plot$I_points + ca.plot$J_points + ca.plot$J_labels + supp.plot$zeMap_dots + supp.plot$zeMap_text
