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
seg.fit <- segmented(out.lm, seg.Z=~x, psi=list(x=c(0.6)),
          control=seg.control(display=FALSE))
# works when I put in only one breakpoint (0.6 Fe treatment)



# add best-fit line
seg.line <- broken.line(seg.fit)
plot(Fe.ratios, treat.means, ylim=c(0, 60), xlab="treatment", ylab="mean # eggs",
     main="Mean of each treatment across all wells \n for both runs", type="b", pch=16)
lines(Fe.ratios, seg.line$fit, col="red", lwd=2)

# add confidence intervals
CI.plus <- seg.line$fit + seg.line$se.fit
CI.minus <- seg.line$fit - seg.line$se.fit
lines(Fe.ratios, CI.plus, col="red", lty=2, lwd=2)
lines(Fe.ratios, CI.minus, col="red", lty=2, lwd=2)

# need to incorporate the std error in the breakpoint value seen from:
summary(seg.fit)
