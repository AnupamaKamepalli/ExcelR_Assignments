# Question 4(b) ,dataset = fantaloons
fatoons_d1 <- read.csv(file.choose())
View(fatoons_d1)
attach(fatoons_d1)
table_fatooms <- table(Weekdays,Weekend)
View(table_fatooms)
#2proportion test
#H0: the proportion of males = proportion of females
#H1: the proportion of males != proportion of females
prop.test(x=c(47,167),n=c(167,233),conf.level = 0.05,correct=FALSE,alternative = "two.sided")
#p-value < 2.2e-16 which is < than 0.05, we could be able to reject H0.

#H0: the propotion of males > the proportion of females
#H1: the proportion of males < the proportion of females
prop.test(x=c(47,167),n=c(167,233),conf.level = 0.05,correct=FALSE,alternative = "less")
#p-value < 2.2e-16 which is < than 0.05, we could be able to reject H0.
#Conclusion : males > females in both weekday and Weekend
