getwd()
sh.cr <- read.csv("emp_data.csv")
View(sh.cr)
attach(sh.cr)

#Graphical representation
dotplot(Salary_hike)
dotplot(Churn_out_rate)

#Boxplot
boxplot(Salary_hike,horizontal = TRUE,col = "red") # No outliers, right skewed, not symmetrical.
boxplot(Churn_out_rate,horizontal = TRUE,col = "green") # No Outliers existed,  right skewed so the data could not be symmetrical.

#Histogram
hist(Salary_hike,col = "red") # Right Skewed
hist(Churn_out_rate,col = "green") # Right Skewed

#qq plot
qqnorm(Salary_hike,col="red")
qqline(Salary_hike) # Data could not be normally distributed

qqnorm(Churn_out_rate,col="green")
qqline(Churn_out_rate) # Data could not be normally distributed

#H0 : Data is normally distributed
#H1 : Data is not normally distributed

shapiro.test(Salary_hike) # p-value = 0.5018 > 0.05, data is normally distributed.
shapiro.test(Churn_out_rate) #p-value = 0.7342> 0.05, data is normally distributed.

#Scatterplot 
plot(Salary_hike,Churn_out_rate,col="red",col.lab="green",main = "Scatter Plot",col.main="red")
lines(lowess(Salary_hike,Churn_out_rate)) # Negative moderate correlation.
cor(Salary_hike,Churn_out_rate) #-0.9117216 : strong negative correlation.

#Model Building
reg_model <- lm(Churn_out_rate~Salary_hike,data = sh.cr)
summary(reg_model) #Multiple R-squared:  0.8312 , so the accuracy of the model is good.
confint(reg_model,level = 0.95)
pred_model <-predict(reg_model,interval = "predict") 
pred_model <- as.data.frame(pred_model)
View(pred_model)
cor(pred_model$fit,sh.cr$Salary_hike) # -1, Perfect -ve correlation











