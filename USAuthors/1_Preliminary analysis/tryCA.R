library(ExPosition)
library(PTCA4CATA)
library(ggplot2)
library(ggrepel)

# load("AllAuthorPunct_Fr.rda")
# load("AllAuthorPunct.rda")

load("Data/AllPunctCount.sDisCA.rda")

library(tidyverse)

alldat <- AllPunctCount.sDisCA[rowSums(AllPunctCount.sDisCA) > 0,]

ca.res <- epCA(alldat, symmetric = TRUE, graphs = FALSE)
# ca.supp <- supplementaryCols(AllPunctCount.Author[,3:ncol(AllPunctCount.Author)], ca.res)

ca.plot <- createFactorMapIJ(ca.res$ExPosition.Data$fi, ca.res$ExPosition.Data$fj,
                            text.cex.i = 3,
                            col.points.i = "maroon",
                            col.labels.i = "maroon",
                            title = "Picked Authors - Symmetric")#,
                            # constraints = minmaxHelper(ca.res$ExPosition.Data$fi, ca.supp$fjj))

ca.label <- createxyLabels.gen(1,2,
                               lambda = ca.res$ExPosition.Data$eigs,
                               tau = round(ca.res$ExPosition.Data$t),
                               axisName = "Component "
)

ca.plot$baseMap + ca.label + ca.plot$I_points + ca.plot$J_points + ca.plot$J_labels

ca.plot$baseMap + ca.plot$I_points + ca.plot$I_labels + ca.label

ca.plot <- createFactorMap(ca.res$ExPosition.Data$fi,
                           text.cex = 3,
                           col.points = "maroon",
                           col.labels = "maroon",
                           title = "Picked Authors - Symmetric")#,

ca.plot$zeMap + ca.label

ca.j.plot <- createFactorMap(ca.res$ExPosition.Data$fj,
                             text.cex = 10,
                             col.points = "maroon",
                             col.labels = "maroon",
                             title = "Picked Authors - Symmetric")#,

ca.j.plot$zeMap + ca.label

gridExtra::grid.arrange(
ca.plot$baseMap + ca.label + ca.plot$I_points + ca.plot$I_labels,
ca.plot$baseMap + ca.label + ca.plot$J_points + ca.plot$J_labels,
nrow = 1, ncol = 2
)


supp.plot <- createFactorMap(ca.supp$fjj,
                             col.points = wes_palette("BottleRocket2")[2],
                             col.labels = wes_palette("BottleRocket2")[2],
                             text.cex = 5,
                             pch = 17)

ca.plot$baseMap + ca.plot$I_labels + ca.plot$I_points + ca.plot$J_points + ca.plot$J_labels + supp.plot$zeMap_dots + supp.plot$zeMap_text
