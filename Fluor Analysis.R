deplete.F0 <- read.csv("/Users/kevin/Research/Experiment/Data/Fluor Data CSVs/Fluor Data/F0-1 nM F0.csv")
replete.F0 <- read.csv("/Users/kevin/Research/Experiment/Data/Fluor Data CSVs/Fluor Data/F0-100 nM F0.csv")

deplete.FM <- read.csv("/Users/kevin/Research/Experiment/Data/Fluor Data CSVs/Fluor Data/Fm-1 nM Fm.csv")
replete.FM <- read.csv("/Users/kevin/Research/Experiment/Data/Fluor Data CSVs/Fluor Data/Fm-100 nM Fm.csv")


deplete.FvFm <- (deplete.FM[,-1] - deplete.F0[,-1]) / deplete.FM[,-1]
replete.FvFm <- (replete.FM[,-1] - replete.F0[,-1]) / replete.FM[,-1]


# Fm per 1000 cells
# Deplete
# This uses cell/mL data for these cultures from "All TO Growth Curves.R"
row1.d <- (deplete.FM[1,-1] / Feb28$"1nmFe"[4]) * 1000
row2.d <- (deplete.FM[2,-1] / Feb28$"1nmFe"[5]) * 1000
row3.d <- (deplete.FM[3,-1] / Feb28$"1nmFe"[6]) * 1000
row4.d <- (deplete.FM[4,-1] / Feb28$"1nmFe"[7]) * 1000

deplete.FM.1000.cells <- as.data.frame(matrix(c(row1.d, row2.d, row3.d, row4.d), nrow=4, ncol=4, byrow=T))
colnames(deplete.FM.1000.cells) <- c("Min","Max","Mode","Average")

# Replete
row1.r <- (replete.FM[1,-1] / Feb28$"100nmFe"[4]) * 1000
row2.r <- (replete.FM[2,-1] / Feb28$"100nmFe"[5]) * 1000
row3.r <- (replete.FM[3,-1] / Feb28$"100nmFe"[6]) * 1000
row4.r <- (replete.FM[4,-1] / Feb28$"100nmFe"[7]) * 1000

replete.FM.1000.cells <- as.data.frame(matrix(c(row1.r, row2.r, row3.r, row4.r), nrow=4, ncol=4, byrow=T))
colnames(replete.FM.1000.cells) <- c("Min","Max","Mode","Average")



# Plots
plot(deplete.FM[,1], as.numeric(deplete.FM.1000.cells$Average), main-"Mean Fm / 1000 cells", ylim=c(0.1, 0.5), type="n")
points(replete.FM[,1], as.numeric(replete.FM.1000.cells$Average), pch=16, type="l")
points(deplete.FM[,1], as.numeric(deplete.FM.1000.cells$Average), ylim=c(0.1, 0.5))
# this doesn't work too well.

Dates <- deplete.FM[,1]
Values <- as.numeric(deplete.FM.1000.cells$Average)
plot(Values ~ Dates)

library(ggplot2)
qplot(deplete.FM[,1], as.numeric(deplete.FM.1000.cells$Average)) + geom_point(replete.FM[,1], replete.FM.1000.cells)
