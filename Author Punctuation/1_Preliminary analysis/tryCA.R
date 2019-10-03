library(ExPosition)
library(PTCA4CATA)
library(ggplot2)
library(ggrepel)

load("AllAuthorPunct_Fr.rda")
load("AllAuthorPunct.rda")

ca.res <- epCA(AllPunctCount.Author_simple, symmetric = FALSE, graphs = FALSE)
# ca.supp <- supplementaryCols(AllPunctCount.Author[,3:ncol(AllPunctCount.Author)], ca.res)

ca.plot <- createFactorMapIJ(ca.res$ExPosition.Data$fi, ca.res$ExPosition.Data$fj,
                            text.cex.i = 3,
                            col.points.i = "maroon",
                            col.labels.i = "maroon",
                            title = "English Authors - asymmetric")#,
                            # constraints = minmaxHelper(ca.res$ExPosition.Data$fi, ca.supp$fjj))
ca.label <- createxyLabels.gen(1,2,
                               lambda = ca.res$ExPosition.Data$eigs,
                               tau = round(ca.res$ExPosition.Data$t),
                               axisName = "Component "
)

ca.plot$baseMap + ca.label + ca.plot$I_points + ca.plot$J_points + ca.plot$J_labels

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
