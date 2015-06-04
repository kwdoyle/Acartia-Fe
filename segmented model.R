# testing 'segmented'
xx<-1:100
zz<-runif(100)
yy<-2+1.5*pmax(xx-35,0)-1.5*pmax(xx-70,0)+15*pmax(zz-.5,0)+rnorm(100,0,2)
dati<-data.frame(x=xx,y=yy,z=zz)
out.lm.test<-lm(y~x,data=dati)
o<-segmented(out.lm.test,seg.Z=~x,psi=list(x=c(30,60)),
             control=seg.control(display=FALSE))





y <- treat.means
x <- as.numeric(colnames(both.run.sums))  # can use 'Fe.ratios' for this as well
out.lm <- lm(y ~ x)
seg.fit <- segmented(out.lm, seg.Z=~x, psi=list(x=c(0.6)),  # try other breakpoints (0.5 and 0.7)
          control=seg.control(display=FALSE))
# add weights to this fit?
# via plotting all the data points instead of their means (find that plot in the first script)
# and then find the residuals



#=========================================
# Add best-fit line & confidence Intervals
#=========================================
## Incorrect way
seg.line <- broken.line(seg.fit)
plot(Fe.ratios, treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs",
     sub="incorrect CI calculation", type="b", pch=16)
lines(Fe.ratios, seg.line$fit, col="red", lwd=2)
# use something like this line?
# no this plots the breakpoint at the 0.6 treatment too.
#points(Fe.ratios, broken.line(seg.fit,link=FALSE)$fit,col=2,pch=20, type="l")

# add confidence intervals
CI.plus <- seg.line$fit + seg.line$se.fit
CI.minus <- seg.line$fit - seg.line$se.fit

# add them to the plot
lines(Fe.ratios, CI.plus, col="red", lty=2, lwd=2)
lines(Fe.ratios, CI.minus, col="red", lty=2, lwd=2)



## Correct way
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
plot(Fe.ratios, treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs", type="b", pch=16)
points.segmented(seg.fit)

# plot just the fit
plot.segmented(seg.fit, main="plot.segmented(seg.fit)")

# need to incorporate the std error in the breakpoint value seen from:
summary(seg.fit)
summary(seg.fit)$psi  # estimate for treatment breakpoint & its std error
summary(seg.fit)$psi[2]  # estimate
summary(seg.fit)$psi[3]  # s.e.

# CI on the breakpoint itself. uses the Delta method for the ratio of two random variables.
confint.segmented(seg.fit)


# how do I make the slope of the line after the breakpoint 0?
# A: I don't really need to
#===============================================================================
# try to weight each data point by the number of observations there are for each
# analyze residuals; do a rank-order plot as well
#===============================================================================

# check residuals
plot(Fe.ratios, residuals(seg.fit))

# to calculate Fe:C ratio:
#minimum + 0.6*(difference betwn max and min) where min=deplete and max=replete
# ≈ 20