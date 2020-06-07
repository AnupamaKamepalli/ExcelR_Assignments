#Question - 01 # Dataset : cutlets.csv
cutlets_d1 <- read.csv(file.choose())
View(cutlets_d1)
attach(cutlets_d1)
#Normality Check
#H0: Data is normally distributed
#H1: Data is not normally distributed

shapiro.test(Unit.A)
#p-value = 0.32 > 0.05, then fail to reject H0. Data is normally distributed.

shapiro.test(Unit.B)
#p-value = 0.5225 > 0.05, then fail to reject H0. Data is normally distributed.

#variance check.
#H0:var(Unit.A)=var(Unit.B)
#H1:var(Unit.A)!=var(Unit.B)
var.test(Unit.A,Unit.B)
#p-value = 0.3136 > 0.05, we fail to reject H0. variace of 2 variables are same.

#2 sameple T- test for equal variances.
#H0:mean(unit.A) = mean(unit.B)
#H1:mean(unit.A)!= mean(Unit.B)
t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.05,correct=TRUE)
#p-value = 0.472 > 0.05, we fail to reject H0. so there is no between b/w 2 units of measure.








