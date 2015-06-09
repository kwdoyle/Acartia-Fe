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


#==================================================================================
# take growth of both from ≈3rd day until before the 100nM reaches stationary phase
#==================================================================================
# Feb 28
plot(Feb28$time[c(3:5)], log(Feb28$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 2/28", type="b", col="red")
points(Feb28$time[c(3:5)], log(Feb28$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Feb28.100 <- coef(lm(log(Feb28$"100nmFe"[3:5]) ~ Feb28$time[3:5]))[2]
Feb28.1 <- coef(lm(log(Feb28$"1nmFe"[3:5]) ~ Feb28$time[3:5]))[2]
# s.e.
Feb28.100.se <- summary(lm(log(Feb28$"100nmFe"[3:5]) ~ Feb28$time[3:5]))$coefficients[, 2][2]
Feb28.1.se <- summary(lm(log(Feb28$"1nmFe"[3:5]) ~ Feb28$time[3:5]))$coefficients[, 2][2]



# Mar 9
plot(Mar9$time[c(3:5)], log(Mar9$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 3/9", type="b", col="red")
points(Mar9$time[c(3:5)], log(Mar9$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Mar9.100 <- coef(lm(log(Mar9$"100nmFe"[3:5]) ~ Mar9$time[3:5]))[2]
Mar9.1 <- coef(lm(log(Mar9$"1nmFe"[3:5]) ~ Mar9$time[3:5]))[2]
# s.e.
Mar9.100.se <- summary(lm(log(Mar9$"100nmFe"[3:5]) ~ Mar9$time[3:5]))$coefficients[, 2][2]
Mar9.1.se <- summary(lm(log(Mar9$"1nmFe"[3:5]) ~ Mar9$time[3:5]))$coefficients[, 2][2]



# Mar 13
plot(Mar13$time[c(3:5)], log(Mar13$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 3/13", type="b", col="red")
points(Mar13$time[c(3:5)], log(Mar13$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Mar13.100 <- coef(lm(log(Mar13$"100nmFe"[3:5]) ~ Mar13$time[3:5]))[2]
Mar13.1 <- coef(lm(log(Mar13$"1nmFe"[3:5]) ~ Mar13$time[3:5]))[2]
# s.e.
Mar13.100.se <- summary(lm(log(Mar13$"100nmFe"[3:5]) ~ Mar13$time[3:5]))$coefficients[, 2][2]
Mar13.1.se <- summary(lm(log(Mar13$"1nmFe"[3:5]) ~ Mar13$time[3:5]))$coefficients[, 2][2]



# Mar 17
plot(Mar17$time[c(3:5)], log(Mar17$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 3/17", type="b", col="red")
points(Mar17$time[c(3:5)], log(Mar17$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Mar17.100 <- coef(lm(log(Mar17$"100nmFe"[3:5]) ~ Mar17$time[3:5]))[2]
Mar17.1 <- coef(lm(log(Mar17$"1nmFe"[3:5]) ~ Mar17$time[3:5]))[2]
# s.e.
Mar17.100.se <- summary(lm(log(Mar17$"100nmFe"[3:5]) ~ Mar17$time[3:5]))$coefficients[, 2][2]
Mar17.1.se <- summary(lm(log(Mar17$"1nmFe"[3:5]) ~ Mar17$time[3:5]))$coefficients[, 2][2]



# Mar 21
plot(Mar21$time[c(3:5)], log(Mar21$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 3/21", type="b", col="red")
points(Mar21$time[c(3:5)], log(Mar21$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Mar21.100 <- coef(lm(log(Mar21$"100nmFe"[3:5]) ~ Mar21$time[3:5]))[2]
Mar21.1 <- coef(lm(log(Mar21$"1nmFe"[3:5]) ~ Mar21$time[3:5]))[2]
# s.e.
Mar21.100.se <- summary(lm(log(Mar21$"100nmFe"[3:5]) ~ Mar21$time[3:5]))$coefficients[, 2][2]
Mar21.1.se <- summary(lm(log(Mar21$"1nmFe"[3:5]) ~ Mar21$time[3:5]))$coefficients[, 2][2]



# Apr 4
plot(Apr4$time[c(3:5)], log(Apr4$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 4/4", type="b", col="red")
points(Apr4$time[c(3:5)], log(Apr4$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Apr4.100 <- coef(lm(log(Apr4$"100nmFe"[3:5]) ~ Apr4$time[3:5]))[2]
Apr4.1 <- coef(lm(log(Apr4$"1nmFe"[3:5]) ~ Apr4$time[3:5]))[2]
# s.e.
Apr4.100.se <- summary(lm(log(Apr4$"100nmFe"[3:5]) ~ Apr4$time[3:5]))$coefficients[, 2][2]
Apr4.1.se <- summary(lm(log(Apr4$"1nmFe"[3:5]) ~ Apr4$time[3:5]))$coefficients[, 2][2]



# Apr 8 
plot(Apr8$time[c(3:5)], log(Apr8$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 4/8", type="b", col="red")
points(Apr8$time[c(3:5)], log(Apr8$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Apr8.100 <- coef(lm(log(Apr8$"100nmFe"[3:5]) ~ Apr8$time[3:5]))[2]
Apr8.1 <- coef(lm(log(Apr8$"1nmFe"[3:5]) ~ Apr8$time[3:5]))[2]
# s.e.
Apr8.100.se <- summary(lm(log(Apr8$"100nmFe"[3:5]) ~ Apr8$time[3:5]))$coefficients[, 2][2]
Apr8.1.se <- summary(lm(log(Apr8$"1nmFe"[3:5]) ~ Apr8$time[3:5]))$coefficients[, 2][2]



# Apr 12 
plot(Apr12$time[c(3:5)], log(Apr12$"100nmFe"[c(3:5)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 4/12", type="b", col="red")
points(Apr12$time[c(3:5)], log(Apr12$"1nmFe"[c(3:5)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Apr12.100 <- coef(lm(log(Apr12$"100nmFe"[3:5]) ~ Apr12$time[3:5]))[2]
Apr12.1 <- coef(lm(log(Apr12$"1nmFe"[3:5]) ~ Apr12$time[3:5]))[2]
# s.e.
Apr12.100.se <- summary(lm(log(Apr12$"100nmFe"[3:5]) ~ Apr12$time[3:5]))$coefficients[, 2][2]
Apr12.1.se <- summary(lm(log(Apr12$"1nmFe"[3:5]) ~ Apr12$time[3:5]))$coefficients[, 2][2]



# Apr 28 
plot(Apr28$time[c(3:6)], log(Apr28$"100nmFe"[c(3:6)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 4/28", type="b", col="red")
points(Apr28$time[c(3:6)], log(Apr28$"1nmFe"[c(3:6)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
Apr28.100 <- coef(lm(log(Apr28$"100nmFe"[3:6]) ~ Apr28$time[3:6]))[2]
Apr28.1 <- coef(lm(log(Apr28$"1nmFe"[3:6]) ~ Apr28$time[3:6]))[2]
# s.e.
Apr28.100.se <- summary(lm(log(Apr28$"100nmFe"[3:6]) ~ Apr28$time[3:6]))$coefficients[, 2][2]
Apr28.1.se <- summary(lm(log(Apr28$"1nmFe"[3:6]) ~ Apr28$time[3:6]))$coefficients[, 2][2]



# May 2
plot(May2$time[c(3:6)], log(May2$"100nmFe"[c(3:6)]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 5/2", type="b", col="red")
points(May2$time[c(3:6)], log(May2$"1nmFe"[c(3:6)]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
May2.100 <- coef(lm(log(May2$"100nmFe"[3:6]) ~ May2$time[3:6]))[2]
May2.1 <- coef(lm(log(May2$"1nmFe"[3:6]) ~ May2$time[3:6]))[2]
# s.e.
May2.100.se <- summary(lm(log(May2$"100nmFe"[3:6]) ~ May2$time[3:6]))$coefficients[, 2][2]
May2.1.se <- summary(lm(log(May2$"1nmFe"[3:6]) ~ May2$time[3:6]))$coefficients[, 2][2]



# May 6
plot(May6$time[4:6], log(May6$"100nmFe"[4:6]), xlab="time (hours)", ylab="log(cells / mL)",
     main="T. oceanica 5/6", type="b", col="red")
points(May6$time[4:6], log(May6$"1nmFe"[4:6]), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
# slopes
May6.100 <- coef(lm(log(May6$"100nmFe"[4:6]) ~ May6$time[4:6]))[2]
May6.1 <- coef(lm(log(May6$"1nmFe"[4:6]) ~ May6$time[4:6]))[2]
# s.e.
May6.100.se <- summary(lm(log(May6$"100nmFe"[4:6]) ~ May6$time[4:6]))$coefficients[, 2][2]
May6.1.se <- summary(lm(log(May6$"1nmFe"[4:6]) ~ May6$time[4:6]))$coefficients[, 2][2]





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


# make a nicer table of these slopes to put in the paper. use R Markdown.
TO.slopes <- data.frame(slopes.100, se.100, slopes.1, se.1)
row.names(TO.slopes) <- c("Feb28", "Mar9", "Mar13", "Mar17", "Mar21", "Apr4", "Apr8", "Apr12",
                          "Apr28", "May2", "May6")
# add column of lables
#TO.slopes$Day <- as.factor(row.names(TO.slopes))

# change order of factor levels
#TO.slopes$Day <- factor(TO.slopes$Day, levels(TO.slopes$Day)[c(5,9,6,7,8,3,4,1,2,10,11)])

# F-test to compare variances
var.test(slopes.100, slopes.1)  # p > 0.05; variances are not different

# t-test to compare slopes
t.test(slopes.100, slopes.1, alternative="greater")  # p < 0.01; slopes are different


## Make nice table
library(xtable)

# turn to scientific notation
TO.table <- format(TO.slopes, digits=3, scientific=T)

print(xtable(TO.table), floating=T)











## Old table making bad-code
# none of this really worked
library(tables)

tabular(Species ~ Format(digits=2) * (Sepal.Length + Sepal.Width) * (mean + sd), data=iris)
tabular(Species ~ Format(digits=2) * (Sepal.Length + Sepal.Width), data=iris)
tabular(TO.slopes$Day ~ Format(digits=3) * (TO.slopes$slopes.100 + TO.slopes$slopes.1) * (mean))

# this is the closest I can get.
latex(tabular(Day ~ Format(digits=3) * (slopes.100 + se.100
                                            + slopes.1 + se.1) * (print),
                                            data=TO.slopes))
# maybe paste the latex code and just remove the 'paste' text?



tabular(Day ~ All(TO.slopes), data=TO.slopes)
as.tabular(TO.slopes)
