Feb28 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 2-28.csv", row.names=1)
Mar9  <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 3-9.csv", row.names=1)
Mar13 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 3-13.csv", row.names=1)
Mar17 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 3-17.csv", row.names=1)
Mar21 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 3-21.csv", row.names=1)
Apr4  <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 4-4.csv", row.names=1)
Apr8  <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 4-8.csv", row.names=1)
Apr12 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 4-12.csv", row.names=1)
Apr28 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 4-28.csv", row.names=1)
May2  <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO 5-2.csv", row.names=1)
May6  <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/TO Growth/TO Fe Growth.csv", row.names=1)

# remove blank row with text indicating the values to the right are diameters
Feb28 <- Feb28[,-4]
Mar9 <- Mar9[,-4]
Mar13 <- Mar13[,-4]
Mar17 <- Mar17[,-4]
Mar21 <- Mar21[,-4]
Apr4 <- Apr4[,-4]
Apr8 <- Apr8[,-4]
Apr12 <- Apr12[,-4]
Apr28 <- Apr28[,-4]
May2 <- May2[,-4]
May6 <- May6[,-4]

colnames(Feb28) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(Mar9) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(Mar13) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(Mar17) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(Mar21) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(Apr4) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(Apr8) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(Apr12) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(Apr28) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(May2) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(May6) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")

### Try fitting the segmented model to all of these and take the slopes of before & after the breakpoint
# and take relative growth rates. RGR = µ/µ_max
### Or just find RGRs by using µ_max as the time where both are growing maximally and µ as the time
# when the 1 nM culture splits off.
### All points where the cultures reach stationary phase are not considered.
#==================================================================================
# take growth of both from ≈3rd day until before the 100nM reaches stationary phase
#==================================================================================
# Feb 28
plot(Feb28$time[c(3:5)], log(Feb28$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 2/28", type="b", col="red")
points(Feb28$time[c(3:5)], log(Feb28$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Feb28.100 <- coef(lm(log(Feb28$"100nmFe"[3:5]) ~ Feb28$time[3:5]))[2]
Feb28.1 <- coef(lm(log(Feb28$"1nmFe"[3:5]) ~ Feb28$time[3:5]))[2]
# slopes before limitation for relative growth rates (µ_max)
Feb28.100.max <- coef(lm(log(Feb28$"100nmFe"[1:3]) ~ Feb28$time[1:3]))[2]
Feb28.1.max <- coef(lm(log(Feb28$"1nmFe"[1:3]) ~ Feb28$time[1:3]))[2]
# s.e.
Feb28.100.se <- summary(lm(log(Feb28$"100nmFe"[3:5]) ~ Feb28$time[3:5]))$coefficients[, 2][2]
Feb28.1.se <- summary(lm(log(Feb28$"1nmFe"[3:5]) ~ Feb28$time[3:5]))$coefficients[, 2][2]
# s.e. (µ_max)
Feb28.100.max.se <- summary(lm(log(Feb28$"100nmFe"[1:3]) ~ Feb28$time[1:3]))$coefficients[, 2][2]
Feb28.1.max.se <- summary(lm(log(Feb28$"1nmFe"[1:3]) ~ Feb28$time[1:3]))$coefficients[, 2][2]

### µ / µ_max
Feb28.100.r <- Feb28.100 / Feb28.100.max
Feb28.1.r <- Feb28.1 / Feb28.1.max



# Mar 9
plot(Mar9$time[c(1:5)], log(Mar9$"100nmFe"[c(1:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 3/9", type="b", col="red")
points(Mar9$time[c(1:5)], log(Mar9$"1nmFe"[c(1:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Mar9.100 <- coef(lm(log(Mar9$"100nmFe"[3:5]) ~ Mar9$time[3:5]))[2]
Mar9.1 <- coef(lm(log(Mar9$"1nmFe"[3:5]) ~ Mar9$time[3:5]))[2]
# µ_max
Mar9.100.max <- coef(lm(log(Mar9$"100nmFe"[1:3]) ~ Mar9$time[1:3]))[2]
Mar9.1.max <- coef(lm(log(Mar9$"1nmFe"[1:3]) ~ Mar9$time[1:3]))[2]
# s.e.
Mar9.100.se <- summary(lm(log(Mar9$"100nmFe"[3:5]) ~ Mar9$time[3:5]))$coefficients[, 2][2]
Mar9.1.se <- summary(lm(log(Mar9$"1nmFe"[3:5]) ~ Mar9$time[3:5]))$coefficients[, 2][2]
# s.e. (µ_max)
Mar9.100.max.se <- summary(lm(log(Mar9$"100nmFe"[1:3]) ~ Mar9$time[1:3]))$coefficients[, 2][2]
Mar9.1.max.se <- summary(lm(log(Mar9$"1nmFe"[1:3]) ~ Mar9$time[1:3]))$coefficients[, 2][2]

### µ / µ_max
Mar9.100.r <- Mar9.100 / Mar9.100.max  # I guess the 100 started growing faster after the 3rd day?
Mar9.1.r <- Mar9.1 / Mar9.1.max



# Mar 13
plot(Mar13$time[c(3:5)], log(Mar13$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 3/13", type="b", col="red")
points(Mar13$time[c(3:5)], log(Mar13$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Mar13.100 <- coef(lm(log(Mar13$"100nmFe"[3:5]) ~ Mar13$time[3:5]))[2]
Mar13.1 <- coef(lm(log(Mar13$"1nmFe"[3:5]) ~ Mar13$time[3:5]))[2]
# µ_max
Mar13.100.max <- coef(lm(log(Mar13$"100nmFe"[1:3]) ~ Mar13$time[1:3]))[2]
Mar13.1.max <- coef(lm(log(Mar13$"1nmFe"[1:3]) ~ Mar13$time[1:3]))[2]
# s.e.
Mar13.100.se <- summary(lm(log(Mar13$"100nmFe"[3:5]) ~ Mar13$time[3:5]))$coefficients[, 2][2]
Mar13.1.se <- summary(lm(log(Mar13$"1nmFe"[3:5]) ~ Mar13$time[3:5]))$coefficients[, 2][2]
# s.e. (µ_max)
Mar13.100.max.se <- summary(lm(log(Mar13$"100nmFe"[1:3]) ~ Mar13$time[1:3]))$coefficients[, 2][2]
Mar13.1.max.se <- summary(lm(log(Mar13$"1nmFe"[1:3]) ~ Mar13$time[1:3]))$coefficients[, 2][2]

### µ / µ_max
Mar13.100.r <- Mar13.100 / Mar13.100.max
Mar13.1.r <- Mar13.1 / Mar13.1.max




# Mar 17
plot(Mar17$time[c(3:5)], log(Mar17$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 3/17", type="b", col="red")
points(Mar17$time[c(3:5)], log(Mar17$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Mar17.100 <- coef(lm(log(Mar17$"100nmFe"[3:5]) ~ Mar17$time[3:5]))[2]
Mar17.1 <- coef(lm(log(Mar17$"1nmFe"[3:5]) ~ Mar17$time[3:5]))[2]
# µ_max
Mar17.100.max <- coef(lm(log(Mar17$"100nmFe"[1:3]) ~ Mar17$time[1:3]))[2]
Mar17.1.max <- coef(lm(log(Mar17$"1nmFe"[1:3]) ~ Mar17$time[1:3]))[2]
# s.e.
Mar17.100.se <- summary(lm(log(Mar17$"100nmFe"[3:5]) ~ Mar17$time[3:5]))$coefficients[, 2][2]
Mar17.1.se <- summary(lm(log(Mar17$"1nmFe"[3:5]) ~ Mar17$time[3:5]))$coefficients[, 2][2]
# s.e. (µ_max)
Mar17.100.max.se <- summary(lm(log(Mar17$"100nmFe"[1:3]) ~ Mar17$time[1:3]))$coefficients[, 2][2]
Mar17.1.max.se <- summary(lm(log(Mar17$"1nmFe"[1:3]) ~ Mar17$time[1:3]))$coefficients[, 2][2]

### µ / µ_max
Mar17.100.r <- Mar17.100 / Mar17.100.max
Mar17.1.r <- Mar17.1 / Mar17.1.max





# Mar 21
plot(Mar21$time[c(1:5)], log(Mar21$"100nmFe"[c(1:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 3/21", type="b", col="red")
points(Mar21$time[c(1:5)], log(Mar21$"1nmFe"[c(1:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Mar21.100 <- coef(lm(log(Mar21$"100nmFe"[3:5]) ~ Mar21$time[3:5]))[2]
Mar21.1 <- coef(lm(log(Mar21$"1nmFe"[3:5]) ~ Mar21$time[3:5]))[2]
# µ_max
Mar21.100.max <- coef(lm(log(Mar21$"100nmFe"[1:3]) ~ Mar21$time[1:3]))[2]
Mar21.1.max <- coef(lm(log(Mar21$"1nmFe"[1:3]) ~ Mar21$time[1:3]))[2]
# s.e.
Mar21.100.se <- summary(lm(log(Mar21$"100nmFe"[3:5]) ~ Mar21$time[3:5]))$coefficients[, 2][2]
Mar21.1.se <- summary(lm(log(Mar21$"1nmFe"[3:5]) ~ Mar21$time[3:5]))$coefficients[, 2][2]
# s.e. (µ_max)
Mar21.100.max.se <- summary(lm(log(Mar21$"100nmFe"[1:3]) ~ Mar21$time[1:3]))$coefficients[, 2][2]
Mar21.1.max.se <- summary(lm(log(Mar21$"1nmFe"[1:3]) ~ Mar21$time[1:3]))$coefficients[, 2][2]

### µ / µ_max
Mar21.100.r <- Mar21.100 / Mar21.100.max
Mar21.1.r <- Mar21.1 / Mar21.1.max



# Apr 4
plot(Apr4$time[c(3:5)], log(Apr4$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 4/4", type="b", col="red")
points(Apr4$time[c(3:5)], log(Apr4$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Apr4.100 <- coef(lm(log(Apr4$"100nmFe"[3:5]) ~ Apr4$time[3:5]))[2]
Apr4.1 <- coef(lm(log(Apr4$"1nmFe"[3:5]) ~ Apr4$time[3:5]))[2]
# µ_max
Apr4.100.max <- coef(lm(log(Apr4$"100nmFe"[1:3]) ~ Apr4$time[1:3]))[2]
Apr4.1.max <- coef(lm(log(Apr4$"1nmFe"[1:3]) ~ Apr4$time[1:3]))[2]
# s.e.
Apr4.100.se <- summary(lm(log(Apr4$"100nmFe"[3:5]) ~ Apr4$time[3:5]))$coefficients[, 2][2]
Apr4.1.se <- summary(lm(log(Apr4$"1nmFe"[3:5]) ~ Apr4$time[3:5]))$coefficients[, 2][2]
# s.e. (µ_max)
Apr4.100.max.se <- summary(lm(log(Apr4$"100nmFe"[1:3]) ~ Apr4$time[1:3]))$coefficients[, 2][2]
Apr4.1.max.se <- summary(lm(log(Apr4$"1nmFe"[1:3]) ~ Apr4$time[1:3]))$coefficients[, 2][2]

### µ / µ_max
Apr4.100.r <- Apr4.100 / Apr4.100.max
Apr4.1.r <- Apr4.1 / Apr4.1.max


# Apr 8 
plot(Apr8$time[c(3:5)], log(Apr8$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 4/8", type="b", col="red")
points(Apr8$time[c(3:5)], log(Apr8$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Apr8.100 <- coef(lm(log(Apr8$"100nmFe"[3:5]) ~ Apr8$time[3:5]))[2]
Apr8.1 <- coef(lm(log(Apr8$"1nmFe"[3:5]) ~ Apr8$time[3:5]))[2]
# µ_max
Apr8.100.max <- coef(lm(log(Apr8$"100nmFe"[1:3]) ~ Apr8$time[1:3]))[2]
Apr8.1.max <- coef(lm(log(Apr8$"1nmFe"[1:3]) ~ Apr8$time[1:3]))[2]
# s.e.
Apr8.100.se <- summary(lm(log(Apr8$"100nmFe"[3:5]) ~ Apr8$time[3:5]))$coefficients[, 2][2]
Apr8.1.se <- summary(lm(log(Apr8$"1nmFe"[3:5]) ~ Apr8$time[3:5]))$coefficients[, 2][2]
# s.e. (µ_max)
Apr8.100.max.se <- summary(lm(log(Apr8$"100nmFe"[1:3]) ~ Apr8$time[1:3]))$coefficients[, 2][2]
Apr8.1.max.se <- summary(lm(log(Apr8$"1nmFe"[1:3]) ~ Apr8$time[1:3]))$coefficients[, 2][2]


### µ / µ_max
Apr8.100.r <- Apr8.100 / Apr8.100.max
Apr8.1.r <- Apr8.1 / Apr8.1.max



# Apr 12 
plot(Apr12$time[c(3:5)], log(Apr12$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 4/12", type="b", col="red")
points(Apr12$time[c(3:5)], log(Apr12$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Apr12.100 <- coef(lm(log(Apr12$"100nmFe"[3:5]) ~ Apr12$time[3:5]))[2]
Apr12.1 <- coef(lm(log(Apr12$"1nmFe"[3:5]) ~ Apr12$time[3:5]))[2]
# µ_max
Apr12.100.max <- coef(lm(log(Apr12$"100nmFe"[1:3]) ~ Apr12$time[1:3]))[2]
Apr12.1.max <- coef(lm(log(Apr12$"1nmFe"[1:3]) ~ Apr12$time[1:3]))[2]
# s.e.
Apr12.100.se <- summary(lm(log(Apr12$"100nmFe"[3:5]) ~ Apr12$time[3:5]))$coefficients[, 2][2]
Apr12.1.se <- summary(lm(log(Apr12$"1nmFe"[3:5]) ~ Apr12$time[3:5]))$coefficients[, 2][2]
# s.e. (µ_max)
Apr12.100.max.se <- summary(lm(log(Apr12$"100nmFe"[1:3]) ~ Apr12$time[1:3]))$coefficients[, 2][2]
Apr12.1.max.se <- summary(lm(log(Apr12$"1nmFe"[1:3]) ~ Apr12$time[1:3]))$coefficients[, 2][2]


### µ / µ_max
Apr12.100.r <- Apr12.100 / Apr12.100.max
Apr12.1.r <- Apr12.1 / Apr12.1.max


# Apr 28 
plot(Apr28$time[c(3:6)], log(Apr28$"100nmFe"[c(3:6)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 4/28", type="b", col="red")
points(Apr28$time[c(3:6)], log(Apr28$"1nmFe"[c(3:6)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
Apr28.100 <- coef(lm(log(Apr28$"100nmFe"[3:6]) ~ Apr28$time[3:6]))[2]
Apr28.1 <- coef(lm(log(Apr28$"1nmFe"[3:6]) ~ Apr28$time[3:6]))[2]
# µ_max
Apr28.100.max <- coef(lm(log(Apr28$"100nmFe"[1:3]) ~ Apr28$time[1:3]))[2]
Apr28.1.max <- coef(lm(log(Apr28$"1nmFe"[1:3]) ~ Apr28$time[1:3]))[2]
# s.e.
Apr28.100.se <- summary(lm(log(Apr28$"100nmFe"[3:6]) ~ Apr28$time[3:6]))$coefficients[, 2][2]
Apr28.1.se <- summary(lm(log(Apr28$"1nmFe"[3:6]) ~ Apr28$time[3:6]))$coefficients[, 2][2]
# s.e. (µ_max)
Apr28.100.max.se <- summary(lm(log(Apr28$"100nmFe"[1:3]) ~ Apr28$time[1:3]))$coefficients[, 2][2]
Apr28.1.max.se <- summary(lm(log(Apr28$"1nmFe"[1:3]) ~ Apr28$time[1:3]))$coefficients[, 2][2]


### µ / µ_max
Apr28.100.r <- Apr28.100 / Apr28.100.max
Apr28.1.r <- Apr28.1 / Apr28.1.max


# May 2
plot(May2$time[c(3:6)], log(May2$"100nmFe"[c(3:6)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 5/2", type="b", col="red")
points(May2$time[c(3:6)], log(May2$"1nmFe"[c(3:6)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
May2.100 <- coef(lm(log(May2$"100nmFe"[3:6]) ~ May2$time[3:6]))[2]
May2.1 <- coef(lm(log(May2$"1nmFe"[3:6]) ~ May2$time[3:6]))[2]
# µ_max
May2.100.max <- coef(lm(log(May2$"100nmFe"[1:3]) ~ May2$time[1:3]))[2]
May2.1.max <- coef(lm(log(May2$"1nmFe"[1:3]) ~ May2$time[1:3]))[2]
# s.e.
May2.100.se <- summary(lm(log(May2$"100nmFe"[3:6]) ~ May2$time[3:6]))$coefficients[, 2][2]
May2.1.se <- summary(lm(log(May2$"1nmFe"[3:6]) ~ May2$time[3:6]))$coefficients[, 2][2]
# s.e. (µ_max)
May2.100.max.se <- summary(lm(log(May2$"100nmFe"[1:3]) ~ May2$time[1:3]))$coefficients[, 2][2]
May2.1.max.se <- summary(lm(log(May2$"1nmFe"[1:3]) ~ May2$time[1:3]))$coefficients[, 2][2]


### µ / µ_max
May2.100.r <- May2.100 / May2.100.max
May2.1.r <- May2.1 / May2.1.max


# May 6
plot(May6$time[4:6], log(May6$"100nmFe"[4:6]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 5/6", type="b", col="red")
points(May6$time[4:6], log(May6$"1nmFe"[4:6]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes (µ)
May6.100 <- coef(lm(log(May6$"100nmFe"[4:6]) ~ May6$time[4:6]))[2]
May6.1 <- coef(lm(log(May6$"1nmFe"[4:6]) ~ May6$time[4:6]))[2]
# µ_max
May6.100.max <- coef(lm(log(May6$"100nmFe"[1:4]) ~ May6$time[1:4]))[2]
May6.1.max <- coef(lm(log(May6$"1nmFe"[1:4]) ~ May6$time[1:4]))[2]
# s.e.
May6.100.se <- summary(lm(log(May6$"100nmFe"[4:6]) ~ May6$time[4:6]))$coefficients[, 2][2]
May6.1.se <- summary(lm(log(May6$"1nmFe"[4:6]) ~ May6$time[4:6]))$coefficients[, 2][2]
# s.e. (µ_max)
May6.100.max.se <- summary(lm(log(May6$"100nmFe"[1:4]) ~ May6$time[1:4]))$coefficients[, 2][2]
May6.1.max.se <- summary(lm(log(May6$"1nmFe"[1:4]) ~ May6$time[1:4]))$coefficients[, 2][2]


### µ / µ_max
May6.100.r <- May6.100 / May6.100.max
May6.1.r <- May6.1 / May6.1.max



# Maybe do plots like on p.92 of Ecological Stoichiometry for TO growth & do relative growth rates
# So just calculate slops from points 1:3 and use them as the max growth rate.
#=============================================
# data frame of slopes & their standard errors
#=============================================
slopes.100 <- c(Feb28.100, Mar9.100, Mar13.100, Mar17.100, Mar21.100, Apr4.100, Apr8.100,
                Apr12.100, Apr28.100, May2.100, May6.100)

se.100 <- c(Feb28.100.se, Mar9.100.se, Mar13.100.se, Mar17.100.se, Mar21.100.se, Apr4.100.se,
            Apr8.100.se, Apr12.100.se, Apr28.100.se, May2.100.se, May6.100.se)

slopes.1 <- c(Feb28.1, Mar9.1, Mar13.1, Mar17.1, Mar21.1, Apr4.1, Apr8.1,
              Apr12.1, Apr28.1, May2.1, May6.1)

se.1 <- c(Feb28.1.se, Mar9.1.se, Mar13.1.se, Mar17.1.se, Mar21.1.se, Apr4.1.se,
          Apr8.1.se, Apr12.1.se, Apr28.1.se, May2.1.se, May6.1.se)

# relative growth rates
RGR.100 <- c(Feb28.100.r, Mar9.100.r, Mar13.100.r, Mar17.100.r, Mar21.100.r, Apr4.100.r, Apr8.100.r,
             Apr12.100.r, Apr28.100.r, May2.100.r, May6.100.r)

RGR.1 <- c(Feb28.1.r, Mar9.1.r, Mar13.1.r, Mar17.1.r, Mar21.1.r, Apr4.1.r, Apr8.1.r,
           Apr12.1.r, Apr28.1.r, May2.1.r, May6.1.r)





# make a nicer table of these slopes to put in the paper. use xtable instead of Rmarkdown.
TO.slopes <- data.frame(slopes.100, se.100, slopes.1, se.1)

row.names(TO.slopes) <- c("Feb28", "Mar9", "Mar13", "Mar17", "Mar21", "Apr4", "Apr8", "Apr12",
                          "Apr28", "May2", "May6")
colnames(TO.slopes) <- c("100 nM", "100 nM s.e.", "1 nM", "1 nM s.e.")





# table of RGRs
TO.RGR <- data.frame(RGR.100, RGR.1)

row.names(TO.RGR) <- c("Feb28", "Mar9", "Mar13", "Mar17", "Mar21", "Apr4", "Apr8", "Apr12",
                       "Apr28", "May2", "May6")

colnames(TO.RGR) <- c("100 nM RGR", "1 nM RGR")



### Significance Tests

# F-test to compare variances
var.test(slopes.100, slopes.1)  # p > 0.05; variances are not different

# t-test to compare slopes
t.test(slopes.100, slopes.1, alternative="greater")  # p < 0.01; slopes are different

# t-test to see if RGR.100 is different from 1
t.test(RGR.100, mu=1)  # p=0.8; RGR is not different from 1

# t-test to see if RGR.1 is different from 1
t.test(RGR.1, mu=1)  # p < 0.01; RGR is different from 1

# t-test to see if RGR.1 is less than 1
t.test(RGR.1, alternative="less", mu=1)  # p < 0.01; RGR is less than 1


## Make nice table
library(xtable)

# turn to scientific notation
TO.table <- format(TO.slopes, digits=3, scientific=T)

# generate LaTeX code for table
print(xtable(TO.table))

# table of RGRs
RGR.table <- format(TO.RGR, digits=3, scientific=T)

# generate LaTeX code for table
print(xtable(TO.RGR))

# the LaTeX for both tables are combined in the tex file.




# Try fitting model to each growth curve and see where the breakpoint (point of Fe limitation) is
# ...or maybe not, since I have to not include the part in stationary phase, and this would be a lot of
# potentially pointless model fitting to every single growth curve.