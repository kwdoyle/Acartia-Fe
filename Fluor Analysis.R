deplete.F0 <- read.csv("/Users/kevin/Research/Experiment/Data/Fluor Data CSVs/Fluor Data/F0-1 nM F0.csv")
replete.F0 <- read.csv("/Users/kevin/Research/Experiment/Data/Fluor Data CSVs/Fluor Data/F0-100 nM F0.csv")

deplete.FM <- read.csv("/Users/kevin/Research/Experiment/Data/Fluor Data CSVs/Fluor Data/Fm-1 nM Fm.csv")
replete.FM <- read.csv("/Users/kevin/Research/Experiment/Data/Fluor Data CSVs/Fluor Data/Fm-100 nM Fm.csv")


# Fv/Fm = (Fm - F0) / Fm
deplete.FvFm <- (deplete.FM[,-1] - deplete.F0[,-1]) / deplete.FM[,-1]
replete.FvFm <- (replete.FM[,-1] - replete.F0[,-1]) / replete.FM[,-1]


# Fm per 1000 cells
# Deplete
# This uses cell/mL data from the Feb28 cultures from "All TO Growth Curves.R"
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
dates <- deplete.FM[,1]

values <- as.numeric(deplete.FM.1000.cells$Average)
values2 <- as.numeric(replete.FM.1000.cells$Average)
values3 <- deplete.FvFm$Average
values4 <- replete.FvFm$Average

# Fv/Fm
# Fm is maximal fluorescence (reaffirm this)
plot.default(values3 ~ dates, xaxt="n", ylab="Fv / Fm", xlab="", main="Mean Fv / Fm", ylim=c(0.3, 0.7),
             type="l", lwd="3", col="blue", yaxt="n")
points(values4 ~ dates, xaxt="n", type="l", lwd="3", col="red")
axis(side=2, at=seq(from=0.3, to=0.7, by=0.05), las=1)
axis(side=1, at=c(1,2,3,4), labels=dates)
legend("topright", c("replete", "deplete"), col=c("red", "blue"), lwd=2, cex=1)

# Fm per 1000 cells
plot.default(values ~ dates, xaxt="n", ylab="Fm / 1000 cells", xlab="", main="Mean Fm Per 1000 Cells",
             ylim=c(0.1, 0.5), type="l", lwd="3", col="blue", yaxt="n")
points(values2 ~ dates, xaxt="n", type="l", lwd="3", col="red")
axis(side=2, at=seq(0.1, to=0.5, by=0.05), las=1)
axis(side=1, at=c(1,2,3,4), labels=dates)
legend("topright", c("replete", "deplete"), col=c("red", "blue"), lwd=2, cex=1)

# The decrease of Fm in the replete culture from 3/5 to 3/6 is due to the culture reaching stationary phase.
# The earlier decrease of Fm for the deplete culture is presumably due to being Fe stressed.


# all the higher Fm values (before they drop) are significantly different than the the lower ones (after they drop)
t.test(c(values[1], values2[1:3]), c(values[2:4], values2[4]))

# the Fv/Fm values grouped from the same days as the Fm t-test are not significantly different from one another
t.test(c(values3[1], values4[1:3]), c(values3[2:4], values4[4]))
