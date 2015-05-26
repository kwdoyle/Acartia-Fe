growth <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO Fe Growth.csv",
                   row.names=1)

# remove fake 'diameter' column
growth <- growth[,-4]
# add column names
colnames(growth) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")

# take log of cells/mL
log.growth <- growth
log.growth$"1nmFe" <- log(log.growth$"1nmFe")
log.growth$"100nmFe" <- log(log.growth$"100nmFe")


# plot w/ logging cells/mL first
plot(log.growth$time, log.growth$"100nmFe", xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica growth curve", type="l", col="red")
points(log.growth$time, log.growth$"1nmFe", type="l", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


# plotting cells/mL on log-scale
# this gives a warning message that 'log is not a graphical parameter', but obviously it is
# otherwise it wouldn't be plotting it as a linear line on a log-scale y-axis
plot(growth$time, growth$"100nmFe", xlab="time (hours)", ylab="cells / mL",
     main="T. oceanica growth curve", type="b", col="red", log="y")
points(growth$time, growth$"1nmFe", type="b", col="blue", log="y")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


# test fitting an exponential model to growth curves
fit100 <- lm(log(growth$"100nmFe") ~ growth$time)
fit1 <- lm(log(growth$"1nmFe") ~ growth$time)
# growth rate of 100 nM Fe T. oceanica
coef(fit100)[2]  # 0.0507
# growth rate of 1 nM Fe T. oceanica
coef(fit1)[2]  # 0.0406




# plot just the parts that split off and compare the slopes to each other?
# or just do a t-test on the actual cells/mL
# plot just the last 3 points:
plot(growth$time[4:6], log(growth$"100nmFe"[4:6]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica growth curve \n last 3 points", type="b", col="red")
points(growth$time[4:6], log(growth$"1nmFe"[4:6]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)

# get slopes
slope100 <- lm(log(growth$"100nmFe"[4:6]) ~ growth$time[4:6])
slope1 <- lm(log(growth$"1nmFe"[4:6]) ~ growth$time[4:6])
summary(slope100)
summary(slope1)
coef(slope100)[2]  # 0.05219807 = slope of 100nmFe; se = 0.003922
coef(slope1)[2]  # 0.03279275 = slope of 1 nmFe; se = 0.0006307

# construct CIs around the slopes and see if they overlap
# I don't think they will at all because the std errors are so small

predict(slope1)
log(growth$"1nmFe"[4:6])

# add best fit & CI lines for 100 nm Fe
lines(growth$time[4:6], predict(slope100))
lines(growth$time[4:6], predict(slope100) + 1.96 * 0.003922, lwd=2, lty=2)
lines(growth$time[4:6], predict(slope100) - 1.96 * 0.003922, lwd=2, lty=2)


# add best fit & CI lines for 1 nm Fe
lines(growth$time[4:6], predict(slope1))
lines(growth$time[4:6], predict(slope1) + 1.96 * 0.0006307, lwd=2, lty=2)
lines(growth$time[4:6], predict(slope1) - 1.96 * 0.0006307, lwd=2, lty=2)

# yeah these CIs are barely visible, but that is a good thing!
# and due to the fact that these slopes & their CIs don't overlap at all, they are
# significantly different.



#==========================================================================================
# need to take slopes of growth curves where they begin to differentiate for all replicates
# and make a table and do a t-test between all the 100nM and 1nM slopes.
#==========================================================================================