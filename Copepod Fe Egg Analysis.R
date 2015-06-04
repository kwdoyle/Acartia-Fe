#==========================================
# Egg count data from 1st run of experiment
#==========================================
# 1st day
run1.day1 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 1/run 1 - day 1.csv")
colnames(run1.day1) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")

# 2nd day
run1.day2 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 1/run 1 - day 2.csv")
colnames(run1.day2) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")

# 3rd day
run1.day3 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 1/run 1 - day 3.csv")
colnames(run1.day3) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")

# 4th day
run1.day4 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 1/run 1 - day 4.csv")
colnames(run1.day4) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")

# 5th day
run1.day5 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 1/run 1 - day 5.csv")
colnames(run1.day5) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")




#==========================================
# Egg count data from 2nd run of experiment
#==========================================
# 1st day
run2.day1 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 2/run 2 - day 1.csv")
colnames(run2.day1) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")
# did day 1 have an outlier that was removed in the low treatment?

# 2nd day
run2.day2 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 2/run 2 - day 2.csv")
colnames(run2.day2) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")

# 3rd day
run2.day3 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 2/run 2 - day 3.csv")
colnames(run2.day3) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")

# 4th day
run2.day4 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 2/run 2 - day 4.csv")
colnames(run2.day4) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")

# 5th day
run2.day5 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Run 2/run 2 - day 5.csv")
colnames(run2.day5) <- c("1", "0.8", "0.7", "0.6", "0.4", "0.2", "0")


#================================
## now do individual day analyses
#================================
# function to calculate means of each treatment while removing any missing values
Avgs <- function(data) {
  newrow <- NULL
  for (i in 1:ncol(data)) {
    newrow[i] <- mean(data[,i], na.rm=TRUE)
  }
  data <- rbind(data, newrow)
}

# add means to each dataframe
run1.day1 <- Avgs(run1.day1)
run1.day2 <- Avgs(run1.day2)
run1.day3 <- Avgs(run1.day3)
run1.day4 <- Avgs(run1.day4)
run1.day5 <- Avgs(run1.day5)

run2.day1 <- Avgs(run2.day1)
run2.day2 <- Avgs(run2.day2)
run2.day3 <- Avgs(run2.day3)
run2.day4 <- Avgs(run2.day4)
run2.day5 <- Avgs(run2.day5)



##### Add confidence intervals to these? (i.e., same as error bars?)
# going to need to use 'predict' function to make the actual best fit line
#==========================================
# Plot of means of each treatment for Run 1
#==========================================
plot(colnames(run1.day1), run1.day1[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
     main="Run 1 \n not pre-screened for egg production",
     sub="each dp = mean of all wells for each treatment", type="b", pch=16, col="red")

points(colnames(run1.day2), run1.day2[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
       main="Run 1 - Day 2", type="b", pch=16, col="blue")

points(colnames(run1.day3), run1.day3[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
       main="Run 1 - Day 3", type="b", pch=16, col="green")

points(colnames(run1.day4), run1.day4[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
       main="Run 1 - Day 4", type="b", pch=16, col="brown")

points(colnames(run1.day5), run1.day5[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
       main="Run 1 - Day 5", type="b", pch=16, col="orange")

legend("topleft", c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5"),
       col=c("red", "blue", "green", "brown", "orange"), lwd=3)

# there appears to be no pattern to the frequency of egg production between days. In fact,
# they appear to produce more eggs after the 2nd day.

# however, they seem to produce less eggs overall in the 0 & 0.2 Fe treatments.
# in fact, when the means of each treatment across all wells for the 1st run are plotted,
# there is a distinct downward slope that tapers off to a straight line starting at the
# 0.6 treatment.

# the 0.7 treatment dp seems to be an outlier, but its effects are lessened when the means
# of both runs are taken

#==========================================
# Plot of means of each treatment for Run 2
#==========================================
plot(colnames(run2.day1), run2.day1[7,], ylim=c(0, 30), xlab="treatment", ylab="avg # eggs",
     main="Run 2 \n pre-screened for egg production",
     sub="each dp = mean of all wells for each treatment", type="b", pch=16, col="red")

points(colnames(run2.day2), run2.day2[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
       main="Run 2 - Day 2", type="b", pch=16, col="blue")

points(colnames(run2.day3), run2.day3[7,], ylim=c(0, 8), xlab="treatment", ylab="avg # eggs",
       main="Run 2 - Day 3", type="b", pch=16, col="green")

points(colnames(run2.day4), run2.day4[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
       main="Run 2 - Day 4", type="b", pch=16, col="brown")

points(colnames(run2.day5), run2.day5[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
       main="Run 2 - Day 5", type="b", pch=16, col="orange")

legend("topleft", c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5"),
       col=c("red", "blue", "green", "brown", "orange"), lwd=3)

# after the 1st day, they seem to produce less eggs overall for each treatment.
# this could be due to the fact that, by pre-screening, we were choosing copepods late in
# their egg-producing life stage, so they just stopped producing eggs in general

# there does seem to be an effect of Fe on egg production for the 1st day though, and this
# trend is still seen in the means of each treatment across all wells for the 2nd run


library(plyr)
# combine days from each treatment
run1 <- list(day1=run1.day1, day2=run1.day2, day3=run1.day3, day4=run1.day4, day5=run1.day5)
run2 <- list(day1=run2.day1, day2=run2.day2, day3=run2.day3, day4=run2.day4, day5=run2.day5)

# calculate means of each well across each treatment (not sure if this is even useful)
run1.means <- aaply(laply(run1, as.matrix), c(2, 3), mean, na.rm=TRUE)
run2.means <- aaply(laply(run2, as.matrix), c(2, 3), mean, na.rm=TRUE)

# remove 'mean of treatment means' row
run1.means <- run1.means[-7,]
run2.means <- run2.means[-7,]



# calculate sums across each well for each treatment
run1.sums <- aaply(laply(run1, as.matrix), c(2, 3), sum, na.rm=TRUE)
run2.sums <- aaply(laply(run2, as.matrix), c(2, 3), sum, na.rm=TRUE)

# remove 'sums of treatment means' row
run1.sums <- run1.sums[-7,]
run2.sums <- run2.sums[-7,]

# combine summation data from both runs
both.run.sums <- rbind(run1.sums, run2.sums)

# add NAs back in for the 0s
Add.na <- function(data) {
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (data[i,j]==0) {
        data[i,j] <- NA
      }
    }
  }
  return(data)
}

both.run.sums <- Add.na(both.run.sums)
run1.sums <- Add.na(run1.sums)
run2.sums <- Add.na(run2.sums)

# calculate means of each treatment across all wells and both runs
t1 <- mean(both.run.sums[,1], na.rm=TRUE)
t2 <- mean(both.run.sums[,2], na.rm=TRUE)
t3 <- mean(both.run.sums[,3], na.rm=TRUE)
t4 <- mean(both.run.sums[,4], na.rm=TRUE)
t5 <- mean(both.run.sums[,5], na.rm=TRUE)
t6 <- mean(both.run.sums[,6], na.rm=TRUE)
t7 <- mean(both.run.sums[,7], na.rm=TRUE)
treat.means <- c(t1, t2, t3, t4, t5, t6, t7)
# calculate standard errors; use these when adding CIs from the best-fit line. sqrt(n) term is subtracting out values that are NAs
std.errs <- c("1"=sd(both.run.sums[,1], na.rm=TRUE) / sqrt(length(both.run.sums[,1]) - sum(is.na(both.run.sums[,1]))),
              "0.8"=sd(both.run.sums[,2], na.rm=TRUE) / sqrt(length(both.run.sums[,2]) - sum(is.na(both.run.sums[,2]))),
              "0.7"=sd(both.run.sums[,3], na.rm=TRUE) / sqrt(length(both.run.sums[,3]) - sum(is.na(both.run.sums[,3]))),
              "0.6"=sd(both.run.sums[,4], na.rm=TRUE) / sqrt(length(both.run.sums[,4]) - sum(is.na(both.run.sums[,4]))),
              "0.4"=sd(both.run.sums[,5], na.rm=TRUE) / sqrt(length(both.run.sums[,5]) - sum(is.na(both.run.sums[,5]))),
              "0.2"=sd(both.run.sums[,6], na.rm=TRUE) / sqrt(length(both.run.sums[,6]) - sum(is.na(both.run.sums[,6]))),
              "0"=sd(both.run.sums[,7], na.rm=TRUE) / sqrt(length(both.run.sums[,7]) - sum(is.na(both.run.sums[,7]))))

plot(colnames(both.run.sums), treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs", type="b", pch=16)
# this works to plot error bars
arrows(as.numeric(colnames(both.run.sums)), treat.means-std.errs, as.numeric(colnames(both.run.sums)), treat.means+std.errs, 
       length=0.05, angle=90, code=3)
# need to fit a model (linear rise to a plateau) to this plot.
# maybe to the plot of means for run 1 and run 2 as well


# calculate mean of each treatment across all wells for Run 1
t1.1 <- mean(run1.sums[,1], na.rm=TRUE)
t2.1 <- mean(run1.sums[,2], na.rm=TRUE)
t3.1 <- mean(run1.sums[,3], na.rm=TRUE)
t4.1 <- mean(run1.sums[,4], na.rm=TRUE)
t5.1 <- mean(run1.sums[,5], na.rm=TRUE)
t6.1 <- mean(run1.sums[,6], na.rm=TRUE)
t7.1 <- mean(run1.sums[,7], na.rm=TRUE)
treat.means.1 <- c(t1.1, t2.1, t3.1, t4.1, t5.1, t6.1, t7.1)
# calculate standard errors; use these when adding CIs from the best-fit line. sqrt(n) term is subtracting out values that are NAs
std.errs.1 <- c("1"=sd(run1.sums[,1], na.rm=TRUE) / sqrt(length(run1.sums[,1]) - sum(is.na(run1.sums[,1]))),
              "0.8"=sd(run1.sums[,2], na.rm=TRUE) / sqrt(length(run1.sums[,2]) - sum(is.na(run1.sums[,2]))),
              "0.7"=sd(run1.sums[,3], na.rm=TRUE) / sqrt(length(run1.sums[,3]) - sum(is.na(run1.sums[,3]))),
              "0.6"=sd(run1.sums[,4], na.rm=TRUE) / sqrt(length(run1.sums[,4]) - sum(is.na(run1.sums[,4]))),
              "0.4"=sd(run1.sums[,5], na.rm=TRUE) / sqrt(length(run1.sums[,5]) - sum(is.na(run1.sums[,5]))),
              "0.2"=sd(run1.sums[,6], na.rm=TRUE) / sqrt(length(run1.sums[,6]) - sum(is.na(run1.sums[,6]))),
              "0"=sd(run1.sums[,7], na.rm=TRUE) / sqrt(length(run1.sums[,7]) - sum(is.na(run1.sums[,7]))))

plot(colnames(run1.sums), treat.means.1, ylim=c(0, 80), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for Run 1", type="b", pch=16)
# this works to plot error bars.
# error bars are high for some most likely due to the high variability between individuals.
# this is why taking the mean of all individuals is the preferred option? it kind of softens the high stochasticity
#==============================================================================
# do we even need to care about the std errors of each mean for each treatment? by taking the average
# across all wells, are we "accounting" for these high standard errors and thus leaving them out
# when fitting a model to this 'new data'?

# yes, because, when fitting the model, the uncertainty now is the std error in fitting the line
# to these data points, so the original std errors of each mean "go away"

# just need to find out what model to fit...
# need to make sure this model saturates to a specific number (max fecundity)


#==============================================================================
arrows(as.numeric(colnames(run1.sums)), treat.means.1-std.errs.1, as.numeric(colnames(run1.sums)), treat.means.1+std.errs.1, 
       length=0.05, angle=90, code=3)



# calculate means of each treatment across all wells for Run 2
t1.2 <- mean(run2.sums[,1], na.rm=TRUE)
t2.2 <- mean(run2.sums[,2], na.rm=TRUE)
t3.2 <- mean(run2.sums[,3], na.rm=TRUE)
t4.2 <- mean(run2.sums[,4], na.rm=TRUE)
t5.2 <- mean(run2.sums[,5], na.rm=TRUE)
t6.2 <- mean(run2.sums[,6], na.rm=TRUE)
t7.2 <- mean(run2.sums[,7], na.rm=TRUE)
treat.means.2 <- c(t1.2, t2.2, t3.2, t4.2, t5.2, t6.2, t7.2)
# calculate standard errors; use these when adding CIs from the best-fit line. sqrt(n) term is subtracting out values that are NAs
std.errs.2 <- c("1"=sd(run2.sums[,1], na.rm=TRUE) / sqrt(length(run2.sums[,1]) - sum(is.na(run2.sums[,1]))),
              "0.8"=sd(run2.sums[,2], na.rm=TRUE) / sqrt(length(run2.sums[,2]) - sum(is.na(run2.sums[,2]))),
              "0.7"=sd(run2.sums[,3], na.rm=TRUE) / sqrt(length(run2.sums[,3]) - sum(is.na(run2.sums[,3]))),
              "0.6"=sd(run2.sums[,4], na.rm=TRUE) / sqrt(length(run2.sums[,4]) - sum(is.na(run2.sums[,4]))),
              "0.4"=sd(run2.sums[,5], na.rm=TRUE) / sqrt(length(run2.sums[,5]) - sum(is.na(run2.sums[,5]))),
              "0.2"=sd(run2.sums[,6], na.rm=TRUE) / sqrt(length(run2.sums[,6]) - sum(is.na(run2.sums[,6]))),
              "0"=sd(run2.sums[,7], na.rm=TRUE) / sqrt(length(run2.sums[,7]) - sum(is.na(run2.sums[,7]))))

plot(colnames(run2.sums), treat.means.2, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for Run 2", type="b", pch=16)
# this works to plot error bars
arrows(as.numeric(colnames(run2.sums)), treat.means.2-std.errs.2, as.numeric(colnames(run2.sums)), treat.means.2+std.errs.2, 
       length=0.05, angle=90, code=3)






#========================================
# Model fitting using 'segmented' package
#========================================

Fe.ratios <- as.numeric(colnames(both.run.sums))

y <- treat.means
x <- as.numeric(colnames(both.run.sums))  # can use 'Fe.ratios' for this as well
out.lm <- lm(y ~ x)
seg.fit <- segmented(out.lm, seg.Z=~x, psi=list(x=c(0.6)),  # try other breakpoints (0.5 and 0.7)
                     control=seg.control(display=FALSE))


# add best-fit line & confidence intervals
seg.line <- broken.line(seg.fit)

plot(Fe.ratios, treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs", type="b", pch=16)
lines(Fe.ratios, seg.line$fit, col="red", lwd=2)

# correct CIs
UL <- seg.line$fit + 1.96 * seg.line$se.fit
LL <- seg.line$fit - 1.96 * seg.line$se.fit

# add to plot
lines(Fe.ratios, UL, col="red", lty=2, lwd=2)
lines(Fe.ratios, LL, col="red", lty=2, lwd=2)

# points.segmented adds the breakpoint on the plot
points.segmented(seg.fit)


#=================
# some extra stuff
#=================
# plot just the fit
plot.segmented(seg.fit, main="plot.segmented(seg.fit)")

# need to incorporate the std error in the breakpoint value seen from:
summary(seg.fit)
summary(seg.fit)$psi  # estimate for treatment breakpoint & its std error
summary(seg.fit)$psi[2]  # estimate
summary(seg.fit)$psi[3]  # s.e.

# CI on the breakpoint itself. uses the Delta method for the ratio of two random variables.
confint.segmented(seg.fit)

# check residuals
plot(Fe.ratios, residuals(seg.fit))
# analyze residuals; do a rank-order plot as well


# to calculate Fe:C ratio:
#minimum + 0.6*(difference betwn max and min) where min=deplete and max=replete
# ≈ 20



#=======================================================================================
# Plotting all the data points instead of just their means and fitting the model to that
# this is a method to weight each treatment relative to how many observations there are
#=======================================================================================
treat.1.sums <- both.run.sums[,1]
treat.2.sums <- both.run.sums[,2]
treat.3.sums <- both.run.sums[,3]
treat.4.sums <- both.run.sums[,4]
treat.5.sums <- both.run.sums[,5]
treat.6.sums <- both.run.sums[,6]
treat.7.sums <- both.run.sums[,7]

# remove NAs
all.1 <- treat.1.sums[!is.na(treat.1.sums)]
all.2 <- treat.2.sums[!is.na(treat.2.sums)]
all.3 <- treat.3.sums[!is.na(treat.3.sums)]
all.4 <- treat.4.sums[!is.na(treat.4.sums)]
all.5 <- treat.5.sums[!is.na(treat.5.sums)]
all.6 <- treat.6.sums[!is.na(treat.6.sums)]
all.7 <- treat.7.sums[!is.na(treat.7.sums)]
all.pts <- c(all.1, all.2, all.3, all.4, all.5, all.6, all.7)

# make longer vector of treatments
treat1 <- rep(1, length(all.1))
treat2 <- rep(0.8, length(all.2))
treat3 <- rep(0.7, length(all.3))
treat4 <- rep(0.6, length(all.4))
treat5 <- rep(0.4, length(all.5))
treat6 <- rep(0.2, length(all.6))
treat7 <- rep(0, length(all.7))
all.treat <- c(treat1, treat2, treat3, treat4, treat5, treat6, treat7)



# now fit the same model to these points
y2 <- all.pts
x2 <- all.treat

all.lm <- lm(y2 ~ x2)
seg.fit.all <- segmented(all.lm, seg.Z=~x2, psi=list(x2=c(0.6)),  # try other breakpoints (0.5 and 0.7)
                     control=seg.control(display=FALSE))
seg.line.all <- broken.line(seg.fit.all)


# plot all the data points & the best-fit line
plot(all.treat, all.pts, pch=16, xlab="treatment", ylab="# of eggs",
     main="Sum of eggs across each well \n for each treatment")
lines(all.treat, seg.line.all$fit, col="red", lwd=2)

# CIs
UL.all <- seg.line.all$fit + 1.96 * seg.line.all$se.fit
LL.all <- seg.line.all$fit - 1.96 * seg.line.all$se.fit

# add to plot
lines(all.treat, UL.all, col="red", lty=2, lwd=2)
lines(all.treat, LL.all, col="red", lty=2, lwd=2)

# points.segmented adds the breakpoint on the plot
points.segmented(seg.fit.all)

# check residuals
plot(all.treat, residuals(seg.fit.all))  # this doesn't show much.



#=======================================================================================
# Fitting model to just Run 1
#=======================================================================================
y3 <- treat.means.1
x3 <- as.numeric(colnames(run1.sums))

lm.1 <- lm(y3 ~ x3)
seg.fit.1 <- segmented(lm.1, seg.Z=~x3, psi=list(x3=c(0.6)),  # try other breakpoints (0.5 and 0.7)
                       control=seg.control(display=FALSE))
seg.line.1 <- broken.line(seg.fit.1)

# plot points & best-fit line
plot(colnames(run1.sums), treat.means.1, ylim=c(0, 80), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for Run 1", type="b", pch=16)
lines(colnames(run1.sums), seg.line.1$fit, col="red", lwd=2)

# CIs
UL.1 <- seg.line.1$fit + 1.96 * seg.line.1$se.fit
LL.1 <- seg.line.1$fit - 1.96 * seg.line.1$se.fit

# add to plot
lines(colnames(run1.sums), UL.1, col="red", lty=2, lwd=2)
lines(colnames(run1.sums), LL.1, col="red", lty=2, lwd=2)

# points.segmented adds the breakpoint on the plot
points.segmented(seg.fit.1)

# residuals
plot(colnames(run1.sums), residuals(seg.fit.1))




#=======================================================================================
# Fitting model to just Run 2
#=======================================================================================
y4 <- treat.means.2
x4 <- as.numeric(colnames(run2.sums))

lm.2 <- lm(y4 ~ x4)
seg.fit.2 <- segmented(lm.2, seg.Z=~x4, psi=list(x4=c(0.6)),  # try other breakpoints (0.5 and 0.7)
                       control=seg.control(display=FALSE))
seg.line.2 <- broken.line(seg.fit.2)

# plot points & best-fit line
plot(colnames(run2.sums), treat.means.2, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for Run 2", type="b", pch=16)
lines(colnames(run2.sums), seg.line.2$fit, col="red", lwd=2)

# CIs
UL.2 <- seg.line.2$fit + 1.96 * seg.line.2$se.fit
LL.2 <- seg.line.2$fit - 1.96 * seg.line.2$se.fit

# add to plot
lines(colnames(run2.sums), UL.2, col="red", lty=2, lwd=2)
lines(colnames(run2.sums), LL.2, col="red", lty=2, lwd=2)

# points.segmented adds the breakpoint on the plot
points.segmented(seg.fit.2)

# residuals
plot(colnames(run2.sums), residuals(seg.fit.2))