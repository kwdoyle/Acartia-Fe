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
# y <- x * (something that makes x effectively 0 at a specific x value.)

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
# relationship you'd expect?
plot(Fe.ratios, gomp(a=44, b=-0.5, c=-3, x=Fe.ratios))


plot(colnames(both.run.sums), treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs",
     sub="fitting to 1:7 on x-axis", type="b", pch=16)
lines(colnames(both.run.sums), rev(model), col="red", lwd=3)



# try to make x-axis a continum of ratio values
r <- seq(0, 1, by=0.1)
plot(r, gomp(a=40, b=-3, c=-5, x=r), ylim=c(0,60))  # try c=5 or 6; makes a flat line

# make treatments numerical values
Fe.ratios <- as.numeric(colnames(both.run.sums))
treat.means

# this fit looks pretty much the same as the previous one
# this one is fitting to actual Fe ratio values while the other is fitting to just 1:7
#===================================================
fit2 <- nls(treat.means ~ gomp(a, b, c, x=Fe.ratios),
            start=list(a=40, b=-4, c=-5))
model2 <- predict(fit2)
plot(Fe.ratios, treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs",
     sub="fitting to numeric ratios", type="b", pch=16)
lines(Fe.ratios, model2, col="red", lwd=3)
#===================================================

# fit to a continum of ratio values
# how does this give different results?
predict(fit2, newdata=r)
residuals(fit2)
summary(fit2)

## CIs using confint()?
library(MASS)  # adds support for nls and glm
confint(fit2, level=0.1)  # still gives error, but not with 10% CIs!!!

#===========================================================================================
##### Try fitting gompertz while removing 5th data point and/or use robust regression? #####
#===========================================================================================
new.vals <- treat.means[-3]
new.ratios <- Fe.ratios[-3]
fit3 <- nls(new.vals ~ gomp(a, b, c, x=new.ratios),
            start=list(a=40, b=-4, c=-5))
summary(fit3)

# plot(new.ratios, new.vals)
# plot(new.ratios, gomp(40, -4, -7, x=new.ratios))
model3 <- predict(fit3)
plot(new.ratios, new.vals, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs",
     sub="fitting w/o 0.7 treatment", type="b", pch=16)
lines(new.ratios, model3, col="red", lwd=3)


#=====================================================================================
#### also try fitting this model to the treatment means from both Run 1 and Run 2 ####
#=====================================================================================
# Run 1
fit.run1 <- nls(treat.means.1 ~ gomp(a, b, c, x=Fe.ratios),
                start=list(a=40, b=-4, c=-5))

model.run1 <- predict(fit.run1)

plot(Fe.ratios, treat.means.1, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for Run 1", type="b", pch=16)
lines(Fe.ratios, model.run1, col="red", lwd=3)

# removing 5th data point
new.vals.1 <- treat.means.1[-3]
fit.run1.2 <- nls(new.vals.1 ~ gomp(a, b, c, x=new.ratios),
                  start=list(a=40, b=-4, c=-5))
model.run1.2 <- predict(fit.run1.2)
plot(new.ratios, new.vals.1, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for Run 1 - minus dp 5",
     type="b", pch=16)
lines(new.ratios, model.run1.2, col="red", lwd=3)
# looks as if the point where it starts leveling off doesn't change between
# including or excluding the outlier



# Run 2
fit.run2 <- nls(treat.means.2 ~ gomp(a, b, c, x=Fe.ratios),
                start=list(a=40, b=-4, c=-5))

model.run2 <- predict(fit.run2)

plot(Fe.ratios, treat.means.2, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for Run 2", type="b", pch=16)
lines(Fe.ratios, model.run2, col="red", lwd=3)





# Note: I haven't been able to fit any of these other models.
# ie, gompertz seems to be the nicest so far.
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
# All these attemps give a 'singular gradient matrix' error. Is this due to the fact that
# nls just can't fit the plateau part since there's no variability anymore once it plateaus?

lrp <- function(x, a, b, tx) { 
  ifelse(x > tx, a + b * tx, a + b * x)
}


### maybe this is more like something you'd expect to see? ###
plot(Fe.ratios, lrp(Fe.ratios, 13, 46, 0.6))

nls(treat.means ~ lrp(x=Fe.ratios, a, b, tx),
    start=list(a=13, b=46, tx=0.6))

plot(treat.means ~ lrp(x=Fe.ratios, a=13, b=46, tx=0.6))

# what does this give me?
gomp(a=44.2, b=-1.22, c=-3.12, x=Fe.ratios)
plot(treat.means ~ gomp(a=44.2, b=-1.22, c=-3.12, x=Fe.ratios))


plot(x, lrp(x=x, a=0, b=10, tx=5))
plot(lrp(x=Fe.ratios, a=0, b=10, tx=0.6), treat.means)
plot(Fe.ratios,  lrp(x=Fe.ratios, a=0, b=1, tx=0.6))
# a = start of y-axis
# b = incriment y-axis increases by
# tx = y val at which plot levels off
### can test different tx values & see which value gives the model that fits the data best ###

nls(treat.means ~ lrp(x=Fe.ratios, a, b, tx),
    start=list(a=0, b=46.1, tx=0.6))

# plotting this against the function works
plot(gomp(40, -1, -1, x=Fe.ratios), treat.means)

# this just starts smushing them all together
plot(lrp(x=Fe.ratios, a=13, b=46, tx=0.6), treat.means)

# plotting this w/ slope & intercept work because x-axis values are characters?
plot(colnames(both.run.sums), treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs", type="b", pch=16)
abline(a=13.037, b=46.100)

# nope
plot(colnames(both.run.sums), lrp(x=treat.means, a=0, b=2, tx=42.1))

# this doesn't work
plot(lrp(x=Fe.ratios, a=0, b=1, tx=0.6), treat.means)
# but this does, but axies are flipped
plot(treat.means, lrp(x=Fe.ratios, a=0, b=1, tx=0.6))
# this "works", but it doesn't keep applying the ifelse after the threshold is found
plot(Fe.ratios, lrp(x=treat.means, a=0, b=1, tx=40))

### maybe this model is not worth it anyway, since it's not exactly "fitting" anything
# it's just fitting a linear model until I tell it to stop and make a plateau ###


# trying to re-create the lrp function to work like the equations in that paper
B0 <- 0  # intercept here should be 0, ie, no egg production at 0 Fe
B1 <- 46.1  # slope is w/e the rate of linear increase is before the plateau
y <- B0 + B1*x
y.max <- B0 + B1*x.max  # equation for plateau point, ie, ≈40 for my data
y.max <- mean(treat.means[c(1:4)])  # using the last 4 points on the plateau
x.max <- (y.max / B1) - (B0 / B1)  # rearrange to solve for x.max


# new attempt with trying to make it plot just y.max
# ...it is not successful.
lrp2 <- function(x, a, b, tx, y.max) { 
  ifelse(x > tx, y.max, a + b * x)
}


plot(treat.means ~ lrp2(x=Fe.ratios, a=13, b=46, tx=0.6, y.max=40))




plot(Fe.ratios, treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs", type="b", pch=16)
abline(a=13.037, b=46.100)

# plotting this works
rev(lrp2(Fe.ratios, a=13.037, b=46.1, x.max=0.6, y.max=40))
plot(Fe.ratios, lrp2(Fe.ratios, a=13.037, b=46.1, x.max=0.6, y.max=40), ylim=c(0,60))

# try fitting it w/ nls
fit4 <- nls(treat.means ~ lrp2(x=Fe.ratios, a, b, x.max, y.max),
            start=list(a=13, b=46, x.max=0.6, y.max=40), trace=T)

fit4 <- nls(treat.means ~ lrp2(x=Fe.ratios, a, b, x.max, y.max),
            start=list(a=0, b=1, x.max=0.6, y.max=40), trace=T)
# these have to be like this to work
plot(lrp2(x=Fe.ratios, a=13, b=46, x.max=0.6, y.max=40) ~ treat.means)
# ie, the lrp values are a function of the treatment means


# do I have to flip this so it's computing y-values and not x-values?
lrp3 <- function(y, a, b, x.max, y.max) { 
  ifelse(y > y.max, x.max, a + b * x)
}

lrp3(treat.means, a=13, b=46, x.max=0.6, y.max=40)

# how does this work?
plot(treat.means ~ gomp(a=40, b=-4, c=-5, x=Fe.ratios), ylim=c(0,60))

# this doesn't?
lrp2(x=Fe.ratios, a=13, b=46, x.max=0.6, y.max=40)



## fit model using 'segmented' package to the data instead
library(segmented)

out.lm <- lm(treat.means[-c(1:3)] ~ as.numeric(colnames(both.run.sums)[-c(1:3)]))
segmented(out.lm, seg.Z=~Fe.ratios, psi=list(Fe.ratios=c(0.4, 0.8)), control=seg.control(display=FALSE))
