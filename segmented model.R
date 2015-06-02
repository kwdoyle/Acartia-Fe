# testing 'segmented'
xx<-1:100
zz<-runif(100)
yy<-2+1.5*pmax(xx-35,0)-1.5*pmax(xx-70,0)+15*pmax(zz-.5,0)+rnorm(100,0,2)
dati<-data.frame(x=xx,y=yy,z=zz)
out.lm.test<-lm(y~x,data=dati)
o<-segmented(out.lm.test,seg.Z=~x,psi=list(x=c(30,60)),
             control=seg.control(display=FALSE))




y <- treat.means
x <- as.numeric(colnames(both.run.sums))
out.lm <- lm(y ~ x)
seg.fit <- segmented(out.lm, seg.Z=~x, psi=list(x=c(0.6)),
          control=seg.control(display=FALSE))
# works when I put in only one breakpoint (0.6 Fe treatment)