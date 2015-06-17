# mean of run1.day1
plot(colnames(run1.day1), run1.day1[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
     main="Run 1 - Day 1", type="b", pch=16)

# mean of run1.day2
plot(colnames(run1.day2), run1.day2[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
     main="Run 1 - Day 2", type="b", pch=16)

# mean of run1.day3
plot(colnames(run1.day3), run1.day3[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
     main="Run 1 - Day 3", type="b", pch=16)

# mean of run1.day4
plot(colnames(run1.day4), run1.day4[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
     main="Run 1 - Day 4", type="b", pch=16)

# mean of run1.day5
plot(colnames(run1.day5), run1.day5[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
     main="Run 1 - Day 5", type="b", pch=16)







# mean of run2.day1
plot(colnames(run2.day1), run2.day1[7,], ylim=c(0, 30), xlab="treatment", ylab="avg # eggs",
     main="Run 2 - Day 1", type="b", pch=16)

# mean of run2.day2
plot(colnames(run2.day2), run2.day2[7,], ylim=c(0, 20), xlab="treatment", ylab="avg # eggs",
     main="Run 2 - Day 2", type="b", pch=16)

# mean of run2.day3
plot(colnames(run2.day3), run2.day3[7,], ylim=c(0, 8), xlab="treatment", ylab="avg # eggs",
     main="Run 2 - Day 3", type="b", pch=16)

# mean of run2.day4
plot(colnames(run2.day4), run2.day4[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
     main="Run 2 - Day 4", type="b", pch=16)

# mean of run2.day5
plot(colnames(run2.day5), run2.day5[7,], ylim=c(0, 10), xlab="treatment", ylab="avg # eggs",
     main="Run 2 - Day 5", type="b", pch=16)





total <- list(run1day1=run1.day1, run1day2=run1.day2, run1day3=run1.day3, run1day4=run1.day4,
              run1day5=run1.day5, run2day1=run2.day1, run2day2=run2.day2, run2day3=run2.day3,
              run2day4=run2.day4, run2day5=run2.day5)

total.means <- aaply(laply(total, as.matrix), c(2, 3), mean, na.rm=TRUE)




# stuff from All TO Growth Curves:
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
