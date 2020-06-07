#dataset : buyer ratio Question # : 03.
install.packages("readxl")
library(readxl)
buyerratio_d1 <- read_excel(file.choose())
View(buyerratio_d1)
colnames(buyerratio_d1)[1]=("country")
View(buyerratio_d1)
attach(buyerratio_d1)
#chisq test.
#H0: All countries have equal proportion of males
#H1:All countries have unequal proportion of males
chisq.test(country,males)
# p-value = 0.2133 >0.05 , fail to reject H0.
#conclusion: all countries have equal proportion of males.

chisq.test(country,females)
# p-value = 0.2133 >0.05 , fail to reject H0.
#conclusion: all countries have equal proportion of males.