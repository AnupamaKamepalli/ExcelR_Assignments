getwd()
ye.sal <- read.csv("Salary_Data.csv")
View(ye.sal)
attach(ye.sal)

#Graphical Representation.
dotplot(YearsExperience)
dotplot(Salary)

#Boxplot
boxplot(YearsExperience,horizontal = TRUE,col = "red") # No outliers, right skewed, data could not be symmetrical.
boxplot(Salary,horizontal = TRUE,col = "green") # No outliers, right skewed,so data could not be symmetrical.

#Histogram
hist(YearsExperience,col = "red") # Could be normally distributed
hist(Salary,col = "green") # could be normally distributed.

#QQplot
qqnorm(YearsExperience,col="red")
qqline(YearsExperience) # could be normally distributed.
qqnorm(Salary,col="green")
qqline(Salary) # could not be normally distributed.
shapiro.test(YearsExperience) # p-value = 0.1034 > 0.05,so data is normally distributed.
shapiro.test(Salary) #  p-value = 0.01516 < 0.05,so data is not normally distributed.

#Model Building.
reg_model <- lm(Salary~YearsExperience)
summary(reg_model) # Multiple R-squared:  0.957 >0.80. Accuracy of the model is good and coefficients are significant to use in our model.



