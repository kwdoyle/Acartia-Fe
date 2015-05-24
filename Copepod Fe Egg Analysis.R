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













#===============
# testing models
#===============
x <- 1:7
c <- 40
k <- 0.001
y <- c*x / (k + x) # adding a coeficceitnt before X makes its slope steeper. use the slope value from fitting a straight line to the linear increase part of the mean egg production graph as a starting value
# this almost works, but x keeps increasing albeit less
# although I guess making K small makes it sort of level off?
# K is the concentration at which the reaction rate is c/2.
plot(x, y)

P <- 0
r <- 0.5
t <- 5
K <- 10
y <- K / (1 + exp(x + r * t))
plot(x, y)


# model I want:
y <- x * (something that makes x effectively 0 at a specific x value.)

#==========================================================
# using pdf about linear response stochastic plateau models
#==========================================================
x <- 1:7
B0 <- 0  # intercept here should be 0, ie, no egg production at 0 Fe
B1 <- 46.1  # slope is w/e the rate of linear increase is before the plateau
y <- B0 + B1*x
# y.max <- B0 + B1*x.max  # equation for plateau point, ie, ≈40 for my data
y.max <- mean(treat.means[c(1:4)])  # using the last 4 points on the plateau
x.max <- (y.max / B1) - (B0 / B1)  # rearrange to solve for x.max
# putting in the slope value found below, this gives x.max = 0.868
# which is approx to where it "should" be.
### although when using the mean of those plateau points, it gives x.max of 0.82 still
### which seems too high.
# I guess if these parameters were tweeked more (ie, the y.max value), 
# it could spit out more close to 0.6

# find slope of linear increase part of plot
plot(colnames(both.run.sums)[-c(1:3)], treat.means[-c(1:3)], ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of treatments where slope is increasing \n for both runs", type="b", pch=16)

summary(lm(treat.means[-c(1:3)] ~ as.numeric(colnames(both.run.sums)[-c(1:3)])))
abline(a=13.037, b=46.100)
# so slope for the linear increase part is 46 I guess

# plot the slope line over the entire plot
plot(colnames(both.run.sums), treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs", type="b", pch=16)
abline(a=13.037, b=46.100)

# now I guess if I plot y.max = B0 + B1 * x.max, I'll get the plateau line?
# what is the y.max predictted from the value of x.max just calculated?
# oh. it's 40 since I used that value to calculate x.max in the first place.


# so this all comes down to figuring out how to combine these things into a single model
# which I can fit the data to.



# maybe use ricker, gompertz, logistic (use change in treatment instead of time)

# gompertz attempt:
gomp <- function(a, b, c, x)
{
  a * exp(b * exp(c * x))
}

# find start values by looking at what function looks like w/ diff params
# can't use treatments as numeric values or categorical values. I guess I can just assign them
# as values from 1 to 7
plot(as.numeric(colnames(both.run.sums)), gomp(a=1, b=-4, c=-1, x=as.numeric(colnames(both.run.sums))))  # looks good

plot(x, gomp(a=40, b=-1, c=-1, x=x))  # these are probably good starting values.
# need to use a=40 so that the y-axis values are equivalent to the mean egg counts.

plot(rev(treat.means) ~ c(1:7))  # plotting it this way flips the plot. need to use rev()

fit.nls <- nls(rev(treat.means) ~ gomp(a, b, c, x=c(1:7)), 
               start=list(a=40, b=-1, c=-1))
summary(fit.nls)
model <- predict(fit.nls)  # this will be the fitted line

# should I try fitting the model while excluding the 0.7 treatment point?
#============================================
# plotting best fit line using Gompertz Model
#============================================
plot(colnames(both.run.sums), treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs",
     sub="fitting to 1:7 on x-axis", type="b", pch=16)
lines(colnames(both.run.sums), rev(model), col="red", lwd=3)



# try to make x-axis a continum of ratio values
r <- seq(0, 1, by=0.1)
plot(r, gomp(a=40, b=-3, c=-5, x=r))  # try c=5 or 6; makes a flat line

# make treatments numerical values
Fe.ratios <- as.numeric(colnames(both.run.sums))
treat.means

# this fit looks pretty much the same as the previous one
# this one is fitting to actual Fe ratio values while the other is fitting to just 1:7
fit2 <- nls(treat.means ~ gomp(a, b, c, x=Fe.ratios),
            start=list(a=40, b=-4, c=-5))
model2 <- predict(fit2)
plot(colnames(both.run.sums), treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs",
     sub="fitting to numeric ratios", type="b", pch=16)
lines(Fe.ratios, model2, col="red", lwd=3)

# fit to a continum of ratio values
# how does this give different results?
predict(fit2, newdata=r)
residuals(fit2)
summary(fit2)

## CIs using confint()?
library(MASS)  # adds support for nls and glm
confint(fit2, level=0.1)  # still gives error, but not with 10% CIs!!!



#============================================
# plotting best fit line using Logistic Model
#============================================
logi <- function(B0, B1, x) {
  1 / (1 + exp((-B0 + B1 * x)))
}

logi2 <- function(x) {
  1 / (1 + exp((-x)))
}
plot(x, logi(0, -1, x))  # y-axis is probability?

fit3 <- nls(treat.means ~ logi(B0, B1, x=Fe.ratios),
            start=list(B0=0, B1=-0.1))



#=======================================================
# plotting best fit line using Linear with Plateau Model
#=======================================================
lrp <- function(x, a, b, tx) { 
  ifelse(x > tx, a + b * tx, a + b * x)
}

plot(x, lrp(x=x, a=0, b=10, tx=5))
# a = start of y-axis
# b = incriment y-axis increases by
# tx = y val at which plot levels off
