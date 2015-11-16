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
# function to calculate means of all wells for each treatment while removing any missing values
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
### Or, no, because I'm not doing any analysis of these data; these are just the means of each treatment.
# I guess I could add the std errors of each mean, but is it necessary?
#==========================================
# Plot of means of each treatment for Run 1
#==========================================
plot(colnames(run1.day1), run1.day1[7,], ylim=c(0, 20), xlab="% replete Fe", ylab="mean # eggs",
     main="Individual day egg production \n for run 1", sub="not pre-screened for egg production",
     type="b", pch=16, lty=1, lwd=2, col="red")
# each dp = mean of all wells for each treatment
points(colnames(run1.day2), run1.day2[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
       main="Run 1 - Day 2", type="b", pch=15, lty=2, lwd=2, col="blue")

points(colnames(run1.day3), run1.day3[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
       main="Run 1 - Day 3", type="b", pch=17, lty=6, lwd=2, col="green")

points(colnames(run1.day4), run1.day4[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
       main="Run 1 - Day 4", type="b", pch=18, lty=4, lwd=2, col="brown")

points(colnames(run1.day5), run1.day5[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
       main="Run 1 - Day 5", type="b", pch=25, lty=5, lwd=2, col="orange")

legend("topleft", c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5"),
       col=c("red", "blue", "green", "brown", "orange"), lwd=3, lty=c(1, 2, 6, 4, 5),
       pch=c(16, 15, 17, 18, 25))

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
plot(colnames(run2.day1), run2.day1[7,], ylim=c(0, 30), xlab="% replete Fe", ylab="mean # eggs",
     main="Individual day egg production \n for run 2", sub="pre-screened for egg production",
     type="b", pch=16, lty=1, lwd=2, col="red")
# each dp = mean of all wells for each treatment
points(colnames(run2.day2), run2.day2[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
       main="Run 2 - Day 2", type="b", pch=15, lty=2, lwd=2, col="blue")

points(colnames(run2.day3), run2.day3[7,], ylim=c(0, 8), xlab="treatment", ylab="avg # eggs",
       main="Run 2 - Day 3", type="b", pch=17, lty=6, lwd=2, col="green")

points(colnames(run2.day4), run2.day4[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
       main="Run 2 - Day 4", type="b", pch=18, lty=4, lwd=2, col="brown")

points(colnames(run2.day5), run2.day5[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
       main="Run 2 - Day 5", type="b", pch=25, lty=5, lwd=2, col="orange")

legend("topleft", c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5"),
       col=c("red", "blue", "green", "brown", "orange"), lwd=3, lty=c(1, 2, 6, 4, 5),
       pch=c(16, 15, 17, 18, 25))

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

#----------------------------

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
arrows(as.numeric(colnames(run1.sums)), treat.means.1-std.errs.1, as.numeric(colnames(run1.sums)), treat.means.1+std.errs.1, 
       length=0.05, angle=90, code=3)
# error bars are high for some most likely due to the high variability between individuals.
# this is why taking the mean of all individuals is the preferred option? it kind of softens the high stochasticity
#==============================================================================
# do we even need to care about the std errors of each mean for each treatment? by taking the average
# across all wells, are we "accounting" for these high standard errors and thus leaving them out
# when fitting a model to this 'new data'?

# yes, because, when fitting the model, the uncertainty now is the std error in fitting the line
# to these data points, so the original std errors of each mean "go away"
#==============================================================================




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
library(segmented)

Fe.ratios <- as.numeric(colnames(both.run.sums))

y <- treat.means
x <- Fe.ratios
out.lm <- lm(y ~ x)
seg.fit <- segmented(out.lm, seg.Z=~x, psi=list(x=c(0.6)),  # this finds the same ans when using any other breakpts
                     control=seg.control(display=FALSE))
AIC(seg.fit)

# 'x' in this is referring to the mean number of eggs. It is calculated to be 42.4 (p=0.0203)
summary(seg.fit)

# add best-fit line & confidence intervals
seg.line <- broken.line(seg.fit)  # find fitted values

plot(Fe.ratios, treat.means, ylim=c(0, 60), xlab="% replete Fe", ylab="mean # eggs",
     main="Effect of food Fe content on \n A. tonsa egg production", type="p", pch=16)
lines(Fe.ratios, seg.line$fit, col="red", lwd=2)

# CIs
UL <- seg.line$fit + 1.96 * seg.line$se.fit  # 1.96 = qnorm( 1-(0.05/2) )
LL <- seg.line$fit - 1.96 * seg.line$se.fit  # ..we ARE assuming eggs are normally distributed, right?

# add to plot
lines(Fe.ratios, UL, col="red", lty=2, lwd=2)
lines(Fe.ratios, LL, col="red", lty=2, lwd=2)

# points.segmented adds the breakpoint on the plot
points.segmented(seg.fit)



# Davies' test for breakpoint significance:
# for run 1 -- breakpoint is significant if you exclude 0.7 treatment
no.treat.7 <- x3[-3]
no.treat.7.eggs <- treat.means.1[-3]
no.treat.7.fit <- lm(no.treat.7.eggs ~ no.treat.7)
davies.test(no.treat.7.fit, seg.Z=~no.treat.7)

# test is not significant when combining both runs
davies.test(out.lm, seg.Z=~x)





# try doing it the same was as TO growth
after.bkpt <- coef(lm(treat.means[1:4] ~ Fe.ratios[1:4]))[2]
before.bkpt <- coef(lm(treat.means[5:7] ~ Fe.ratios[5:7]))[2]

after.bkpt.1 <- coef(lm(treat.means.1[1:4] ~ Fe.ratios[1:4]))[2]
before.bkpt.1 <- coef(lm(treat.means.1[5:7] ~ Fe.ratios[5:7]))[2]

after.bkpt.2 <- coef(lm(treat.means.2[1:4] ~ Fe.ratios[1:4]))[2]
before.bkpt.2 <- coef(lm(treat.means.2[5:7] ~ Fe.ratios[5:7]))[2]

# plot to check
plot(Fe.ratios[1:4], treat.means[1:4])
abline(38, 2.257)

plot(Fe.ratios[5:7], treat.means[5:7])
abline(14, 39)

slopes.after <- c(after.bkpt, after.bkpt.1, after.bkpt.2)
slopes.before <- c(before.bkpt, before.bkpt.1, before.bkpt.2)

t.test(slopes.after, slopes.before)

# uggggggggggghhhhhhhhhhhhhhhhhh
# maybe bootstrap all the slopes.after and slopes.before??!?!?!?!?
# do bootstrap on the egg counts for the combined runs to see if the breakpoint found for
# the dist. of eggs per treatment I found is just by chance or not


boot.eggs <- sample(treat.means, replace=F)
#x <- Fe.ratios
boot.lm <- lm(boot.eggs ~ Fe.ratios)
boot.seg.fit <- try(segmented(boot.lm, seg.Z=~Fe.ratios, psi=list(Fe.ratios=c(0.6)),  # this finds the same ans when using any other breakpts
                     control=seg.control(display=FALSE)), silent=T)
plot(Fe.ratios, boot.eggs)
plot(Fe.ratios, treat.means)







# add legend maybe?
# legend("topright", c("best fit", "CI", "break point"), col=c("red", "green", "black"), lty=c(1,2), lwd=2, pch=1, pt.cex=1.5)

#=========================
# Try plotting with ggplot
#=========================
d <- data.frame(Fe.ratios, treat.means)
f <- data.frame(Fe.ratios, seg.line$fit)
ggplot(data=d, aes(x=Fe.ratios, y=treat.means)) +  # add CIs & breakpoint to this eventually
  geom_point() +
  theme_bw() +
  geom_line(data=d, aes(x=Fe.ratios, y=seg.line$fit)) +
  ylim(0, 60) +
  xlab("Treatment (% Replete Fe)" ) + ylab("Mean Number of Eggs") +
  geom_point(x=seg.fit$psi.history[[5]], y=seg.line$fit[4])  # this is the breakpoint
# don't know how to make the breakpoint look the same as from 'points.segmented'
# maybe just use qplot?  


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


# check residuals
plot(Fe.ratios, residuals(seg.fit))
# analyze residuals; do a rank-order plot as well

#====================================
# to calculate breakpoint Fe:C ratio:
#====================================
#minimum + 0.6*(difference betwn max and min) where min=deplete and max=replete
# ≈ 20
# min and max values are from Sunda & Huntsman
# min and max are the Fe:C for ≈1nM and ≈100nM total Fe
# the min value does not have as much weight as the max value
Min <- 2
Max <- 34.3
Breakpoint <- summary(seg.fit)$psi[2]

FeC.Break <- Min + Breakpoint * (Max - Min)
FeC.1 <- Min + 1 * (Max - Min)
FeC.0.8 <- Min + 0.8 * (Max - Min)
FeC.0.7 <- Min + 0.7 * (Max - Min)
FeC.0.6 <- Min + 0.6 * (Max - Min)
FeC.0.4 <- Min + 0.4 * (Max - Min)
FeC.0.2 <- Min + 0.2 * (Max - Min)
FeC.0.0 <- Min + 0.0 * (Max - Min)

# for run 1
Min + summary(seg.fit.1)$psi[2] * (Max - Min)
# for run 2
Min + summary(seg.fit.2)$psi[2] * (Max - Min)

# table of all treatment ratios and their respective Fe:C ratios
FeC.table <- data.frame(row.names=c("1", "0.8", "0.7", "Brkpt", "0.6", "0.4", "0.2", "0.0"),
                        "Fe:C"=c(FeC.1, FeC.0.8, FeC.0.7, FeC.Break, FeC.0.6, FeC.0.4, FeC.0.2, FeC.0.0))

library(xtable)
xtable(FeC.table)

#=======================================================================================
# Plotting all the data points instead of just their means and fitting the model to that.
# This is a method to weight each treatment relative to how many observations there are
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
seg.fit.all <- segmented(all.lm, seg.Z=~x2, psi=list(x2=c(0.6)),
                     control=seg.control(display=FALSE))
seg.line.all <- broken.line(seg.fit.all)
AIC(seg.fit.all)

summary(seg.fit.all)

# plot all the data points & the best-fit line
plot(all.treat, all.pts, pch=16, xlab="% replete Fe", ylab="mean # eggs",
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
seg.fit.1 <- segmented(lm.1, seg.Z=~x3, psi=list(x3=c(0.6)),
                       control=seg.control(display=FALSE))
seg.line.1 <- broken.line(seg.fit.1)
AIC(seg.fit.1)

summary(seg.fit.1)

# plot points & best-fit line
plot(colnames(run1.sums), treat.means.1, ylim=c(0, 80), xlab="% replete Fe", ylab="mean # eggs",
     main="Mean of each treatment \n across all wells for Run 1", type="p", pch=16)
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



#============================================================
# Fit to all the data points instead of their means for Run 1
#============================================================
treat.1.sums.1 <- run1.sums[,1]
treat.2.sums.1 <- run1.sums[,2]
treat.3.sums.1 <- run1.sums[,3]
treat.4.sums.1 <- run1.sums[,4]
treat.5.sums.1 <- run1.sums[,5]
treat.6.sums.1 <- run1.sums[,6]
treat.7.sums.1 <- run1.sums[,7]

# remove NAs
all.1.1 <- treat.1.sums.1[!is.na(treat.1.sums.1)]
all.2.1 <- treat.2.sums.1[!is.na(treat.2.sums.1)]
all.3.1 <- treat.3.sums.1[!is.na(treat.3.sums.1)]
all.4.1 <- treat.4.sums.1[!is.na(treat.4.sums.1)]
all.5.1 <- treat.5.sums.1[!is.na(treat.5.sums.1)]
all.6.1 <- treat.6.sums.1[!is.na(treat.6.sums.1)]
all.7.1 <- treat.7.sums.1[!is.na(treat.7.sums.1)]
all.pts.1 <- c(all.1.1, all.2.1, all.3.1, all.4.1, all.5.1, all.6.1, all.7.1)

# make longer vector of treatments
treat1.1 <- rep(1, length(all.1.1))
treat2.1 <- rep(0.8, length(all.2.1))
treat3.1 <- rep(0.7, length(all.3.1))
treat4.1 <- rep(0.6, length(all.4.1))
treat5.1 <- rep(0.4, length(all.5.1))
treat6.1 <- rep(0.2, length(all.6.1))
treat7.1 <- rep(0, length(all.7.1))
all.treat.1 <- c(treat1.1, treat2.1, treat3.1, treat4.1, treat5.1, treat6.1, treat7.1)

# now fit the same model to these points
y5 <- all.pts.1
x5 <- all.treat.1

all.lm.1 <- lm(y5 ~ x5)
seg.fit.all.1 <- segmented(all.lm.1, seg.Z=~x5, psi=list(x5=c(0.6)),
                         control=seg.control(display=FALSE))
seg.line.all.1 <- broken.line(seg.fit.all.1)
AIC(seg.fit.all.1)

summary(seg.fit.all.1)

# plot all the data points & the best-fit line
plot(all.treat.1, all.pts.1, pch=16, xlab="treatment", ylab="# of eggs",
     main="Sum of eggs across each well for each \n treatment for Run 1")
lines(all.treat.1, seg.line.all.1$fit, col="red", lwd=2)

# CIs
UL.all.1 <- seg.line.all.1$fit + 1.96 * seg.line.all.1$se.fit
LL.all.1 <- seg.line.all.1$fit - 1.96 * seg.line.all.1$se.fit

# add to plot
lines(all.treat.1, UL.all.1, col="red", lty=2, lwd=2)
lines(all.treat.1, LL.all.1, col="red", lty=2, lwd=2)

# points.segmented adds the breakpoint on the plot
points.segmented(seg.fit.all.1)

# check residuals
plot(all.treat.1, residuals(seg.fit.all.1))






#=======================================================================================
# Fitting model to just Run 2
#=======================================================================================
y4 <- treat.means.2
x4 <- as.numeric(colnames(run2.sums))

lm.2 <- lm(y4 ~ x4)
seg.fit.2 <- segmented(lm.2, seg.Z=~x4, psi=list(x4=c(0.6)),
                       control=seg.control(display=FALSE))
seg.line.2 <- broken.line(seg.fit.2)
AIC(seg.fit.2)

summary(seg.fit.2)

# plot points & best-fit line
plot(colnames(run2.sums), treat.means.2, ylim=c(0, 60), xlab="% replete Fe", ylab="mean # eggs",
     main="Mean of each treatment \n across all wells for Run 2", type="p", pch=16)
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

# estimate for treatment breakpoint & its std error
summary(seg.fit.2)$psi


#============================================================
# Fit to all the data points instead of their means for Run 2
#============================================================
treat.1.sums.2 <- run2.sums[,1]
treat.2.sums.2 <- run2.sums[,2]
treat.3.sums.2 <- run2.sums[,3]
treat.4.sums.2 <- run2.sums[,4]
treat.5.sums.2 <- run2.sums[,5]
treat.6.sums.2 <- run2.sums[,6]
treat.7.sums.2 <- run2.sums[,7]

# remove NAs
all.1.2 <- treat.1.sums.2[!is.na(treat.1.sums.2)]
all.2.2 <- treat.2.sums.2[!is.na(treat.2.sums.2)]
all.3.2 <- treat.3.sums.2[!is.na(treat.3.sums.2)]
all.4.2 <- treat.4.sums.2[!is.na(treat.4.sums.2)]
all.5.2 <- treat.5.sums.2[!is.na(treat.5.sums.2)]
all.6.2 <- treat.6.sums.2[!is.na(treat.6.sums.2)]
all.7.2 <- treat.7.sums.2[!is.na(treat.7.sums.2)]
all.pts.2 <- c(all.1.2, all.2.2, all.3.2, all.4.2, all.5.2, all.6.2, all.7.2)

# make longer vector of treatments
treat1.2 <- rep(1, length(all.1.2))
treat2.2 <- rep(0.8, length(all.2.2))
treat3.2 <- rep(0.7, length(all.3.2))
treat4.2 <- rep(0.6, length(all.4.2))
treat5.2 <- rep(0.4, length(all.5.2))
treat6.2 <- rep(0.2, length(all.6.2))
treat7.2 <- rep(0, length(all.7.2))
all.treat.2 <- c(treat1.2, treat2.2, treat3.2, treat4.2, treat5.2, treat6.2, treat7.2)

# now fit the same model to these points
y6 <- all.pts.2
x6 <- all.treat.2

all.lm.2 <- lm(y6 ~ x6)
seg.fit.all.2 <- segmented(all.lm.2, seg.Z=~x6, psi=list(x6=c(0.6)),
                           control=seg.control(display=FALSE))
seg.line.all.2 <- broken.line(seg.fit.all.2)
AIC(seg.fit.all.2)

summary(seg.fit.all.2)

# plot all the data points & the best-fit line
plot(all.treat.2, all.pts.2, pch=16, xlab="treatment", ylab="# of eggs",
     main="Sum of eggs across each well for each \n treatment for Run 2", ylim=c(0,100))  # make ylim be c(0,100) to match the plots from Run 1 and both runs
lines(all.treat.2, seg.line.all.2$fit, col="red", lwd=2)

# CIs ---- Take sqrt of egg counts to transform into normal
UL.all.2 <- seg.line.all.2$fit + 1.96 * seg.line.all.2$se.fit
LL.all.2 <- seg.line.all.2$fit - 1.96 * seg.line.all.2$se.fit

# add to plot
lines(all.treat.2, UL.all.2, col="red", lty=2, lwd=2)
lines(all.treat.2, LL.all.2, col="red", lty=2, lwd=2)

# points.segmented adds the breakpoint on the plot
points.segmented(seg.fit.all.2)

# check residuals
plot(all.treat.2, residuals(seg.fit.all.2))





#=========================================================
# Try doing ML analysis to find the CI for the breakpoints
#=========================================================





### CIs on each breakpoint itself. uses the Delta method for the ratio of two random variables.
### how is the upper limit on this so high? ###
bp.comb <- confint.segmented(seg.fit)$x[1]
bp.r1 <- confint.segmented(seg.fit.1)$x[1]
bp.r2 <- confint.segmented(seg.fit.2)$x[1]

# break points for plots using all data points instead of means
bp.comb.all <- confint.segmented(seg.fit.all)$x[1]
bp.r1.all <- confint.segmented(seg.fit.all.1)$x[1]
bp.r2.all <- confint.segmented(seg.fit.all.2)$x[1]


# calculate actual CIs
comb_l <- confint.segmented(seg.fit)$x[2]
comb_u <- confint.segmented(seg.fit)$x[3]
comb.all_l <- confint.segmented(seg.fit.all)$x[2]
comb.all_u <- confint.segmented(seg.fit.all)$x[3]

r1_l <- confint.segmented(seg.fit.1)$x[2]
r1_u <- confint.segmented(seg.fit.1)$x[3]
r1.all_l <- confint.segmented(seg.fit.all.1)$x[2]
r1.all_u <- confint.segmented(seg.fit.all.1)$x[3]

r2_l <- confint.segmented(seg.fit.2)$x[2]
r2_u <- confint.segmented(seg.fit.2)$x[3]
r2.all_l <- confint.segmented(seg.fit.all.2)$x[2]
r2.all_u <- confint.segmented(seg.fit.all.2)$x[3]



# compare size of CIs of each breakpoint for run 1, run 2, and combined
comb <- comb_u - comb_l  # combined
r1 <- r1_u - r1_l  # run 1
r2 <- r2_u - r2_l  # run 2

# CIs for breakpoints using all data points
comb.all <- comb.all_u - comb.all_l  # combined
r1.all <- r1.all_u - r1.all_l  # run 1
r2.all <- r2.all_u - r2.all_l  # run 2



### make a table
CI.sizes <- data.frame("break point"=c(bp.comb, bp.r1, bp.r2), "CI size"=c(comb, r1, r2))
row.names(CI.sizes) <- c("both runs", "run 1", "run 2")
colnames(CI.sizes) <- c("break point", "CI size")
# Run 2 does have the smallest CI on its breakpoint, but the run 1 data still is informative
# (eg, the 0 Fe treatment has lower egg production, ie, might not have been as much effect from Fe
# contamination), so it should be included.

# Also, Run 1's large CI is mostly due to the 0.7 dp. It otherwise looks very good.



# table including plots using just means and all data points
CI.sizes.all <- data.frame("break point"=c(bp.comb, bp.comb.all, bp.r1, bp.r1.all, bp.r2, bp.r2.all),
                           "CI size"=c(comb, comb.all, r1, r1.all, r2, r2.all))
row.names(CI.sizes.all) <- c("both runs", "both runs all", "run 1", "run 1 all", "run 2", "run 2 all")
colnames(CI.sizes.all) <- c("break point", "CI size")


# table with all runs w/ all data points w/ actual CIs
CI.table.full <- data.frame("break point"=c(bp.comb, bp.comb.all, bp.r1, bp.r1.all, bp.r2, bp.r2.all),
                   "CI lower"=c(comb_l, comb.all_l, r1_l, r1.all_l, r2_l, r2.all_l),
                   "CI upper"=c(comb_u, comb.all_u, r1_u, r1.all_u, r2_u, r2.all_u))

row.names(CI.table.full) <- c("both runs", "both runs all", "run 1", "run 1 all", "run 2", "run 2 all")
colnames(CI.table.full) <- c("break point", "CI lower", "CI upper")


### table for plots for just means
CI.table.full.2 <- data.frame("break point"=c(bp.comb, bp.r1, bp.r2),
                            "CI lower"=c(comb_l, r1_l, r2_l),
                            "CI upper"=c(comb_u, r1_u, r2_u))

row.names(CI.table.full.2) <- c("both runs", "run 1", "run 2")
colnames(CI.table.full.2) <- c("break point", "CI lower", "CI upper")



# LaTeX table for CI.sizes:
xtable(CI.sizes, align=c("l","c","c"), digits=3)

# LaTeX table for CI sizes w/ all data point plots:
xtable(CI.sizes.all, align=c("l","c","c"), digits=3)

# LaTeX table for actuall CI for all plots:
xtable(CI.table.full, align=c("l","c","c","c"), digits=3)

# table for actual CI for just plots using means
xtable(CI.table.full.2, align=c("l","c","c","c"), digits=3)


### Try calculating the influence of the 0.7 dp for Run 1. ###



# Vector of AIC values
AICs <- c("all mean"=AIC(seg.fit), "run 1 mean"=AIC(seg.fit.1), "run 2 mean"=AIC(seg.fit.2),
          "all points"=AIC(seg.fit.all), "run 1 points"=AIC(seg.fit.all.1), "run 2 points"=AIC(seg.fit.all.2))
# Run 2 also has the lowest AIC value, but the combined model is probably better for the same reasons above

# AICs as a data frame
AIC.df <- as.data.frame(matrix(AICs, nrow=1, ncol=6))
colnames(AIC.df) <- c("all mean", "run 1 mean", "run 2 mean", "all points", "run 1 points", "run 2 points")
row.names(AIC.df) <- c("AIC")

# LaTeX table for AICs
xtable(AIC.df)

#===========
# C:N Ratios
#===========
# From Kiorboe
Egg.C <- 45.7  # ng/egg
Egg.N <- 9.1  # ng/egg
Egg.CtoN <- Egg.C / Egg.N

# From Sunda & Huntsman
# Fetot(nM) of 1.2:: Fe:C = should be 2 for 1 nM Fe
# Fetot(nM) of 101:: Fe:C = 34.3 ; Cell Fe(µmol/L): 336

#==================
# Expected Response
#==================
plot(c(1:7), c(1,2,3,4,4,4,4), ylim=c(1,5), type="l", lwd=2, xaxt="n", yaxt="n",
     main="Expected Response", xlab="Fe treatment", ylab="# eggs", mgp=c(2, 2, 2))
