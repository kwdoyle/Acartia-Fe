# Import cultures used in the experiment
exp1 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture1.csv", row.names=1)
exp2 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture2.csv", row.names=1)
exp3 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture3.csv", row.names=1)
exp4 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture4.csv", row.names=1)
exp5 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture5.csv", row.names=1)
exp6 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture6.csv", row.names=1)
exp7 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture7.csv", row.names=1)
exp8 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture8.csv", row.names=1)
exp9 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture9.csv", row.names=1)
exp10 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture10.csv", row.names=1)
exp11 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture11.csv", row.names=1)
exp12 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture12.csv", row.names=1)
exp13 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture13.csv", row.names=1)
exp14 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture14.csv", row.names=1)
exp15 <- read.csv("/Users/kevin/Research/Experiment/Data/New CSVs/Exp Cultures/ExpCulture15.csv", row.names=1)

# remove blank 4th column
exp1 <- exp1[,-4]
exp2 <- exp2[,-4]
exp3 <- exp3[,-4]
exp4 <- exp4[,-4]
exp5 <- exp5[,-4]
exp6 <- exp6[,-4]
exp7 <- exp7[,-4]
exp8 <- exp8[,-4]
exp9 <- exp9[,-4]
exp10 <- exp10[,-4]
exp11 <- exp11[,-4]
exp12 <- exp12[,-4]
exp13 <- exp13[,-4]
exp14 <- exp14[,-4]
exp15 <- exp15[,-4]

# rename columns
colnames(exp1) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp2) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp3) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp4) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp5) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp6) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp7) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp8) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp9) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp10) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp11) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp12) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp13) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp14) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")
colnames(exp15) <- c("time", "1nmFe", "100nmFe", "1nmDiameter", "100nmDiameter")


#plot them
plot(exp1$time, log(exp1$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 1", type="b", col="red")
points(exp1$time, log(exp1$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


plot(exp2$time, log(exp2$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 2", type="b", col="red")
points(exp2$time, log(exp2$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


plot(exp3$time, log(exp3$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 3", type="b", col="red")
points(exp3$time, log(exp3$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


plot(exp4$time, log(exp4$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 4", type="b", col="red", ylim=c(7,12))
points(exp4$time, log(exp4$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


plot(exp5$time, log(exp5$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 5", type="b", col="red")
points(exp5$time, log(exp5$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


plot(exp6$time, log(exp6$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 6", type="b", col="red")
points(exp6$time, log(exp6$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


plot(exp7$time, log(exp7$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 7", type="b", col="red")
points(exp7$time, log(exp7$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


## Second Run

# 5/28
plot(exp9$time, log(exp9$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 9", type="b", col="red",
     sub="used on 5/28")
points(exp9$time, log(exp9$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


# 5/29
plot(exp10$time, log(exp10$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 10", type="b", col="red",
     sub="used on 5/29")
points(exp10$time, log(exp10$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


# 5/30
plot(exp11$time, log(exp11$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 11", type="b", col="red",
     sub="used on 5/30")
points(exp11$time, log(exp11$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


# 5/31
plot(exp8$time, log(exp8$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 8", type="b", col="red",
     sub="used on 5/31")
points(exp8$time, log(exp8$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


# 6/1
plot(exp12$time, log(exp12$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 12", type="b", col="red",
     sub="used on 6/1")
points(exp12$time, log(exp12$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


plot(exp13$time, log(exp13$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 13", type="b", col="red")
points(exp13$time, log(exp13$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


# 6/3
plot(exp14$time, log(exp14$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 14", type="b", col="red",
     sub="used on 6/3")
points(exp14$time, log(exp14$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)


# 6/4
plot(exp15$time, log(exp15$"100nmFe"), xlab="time (hours)", ylab="log(cells / mL)",
     main="Experiment Culture # 15", type="b", col="red",
     sub="used on 6/4")
points(exp15$time, log(exp15$"1nmFe"), type="b", col="blue")
legend("topleft", c("1 nM Fe", "100 nM Fe"), col=c("blue", "red"), lwd=2)
