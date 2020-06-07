getwd()
setwd("C:/Users/anupa/Desktop/ExcelR/Assignment/04_linear_Regression")
dt.st <- read.csv("delivery_time.csv")
View(dt.st)
attach(dt.st)
#Graphical Representaion.
#input : Sorting Time; output : delivery time. Need to predict the delivery time by using the sorting time.
install.packages("lattice")
library("lattice")
dotplot(Sorting.Time)
dotplot(Delivery.Time)
#Box Plot - To check outlier,skewness
boxplot(Sorting.Time,col="red") # No outliers found, could be normally distributed.
boxplot(Delivery.Time,col = "green",horizontal = TRUE) # No outliers found, could be Right skewed and not symmetric.

# To check the distribution of the data.

qqnorm(Sorting.Time)
qqline(Sorting.Time,col="red") # Data could not be normally distributed.

qqnorm(Delivery.Time)
qqline(Delivery.Time,col="green") # Data could be normally distributed.

#H0: Data is normally distributed.
#H1: Data is not norrmally distributed.
shapiro.test(Sorting.Time) #p-value = 0.1881 > 0.05, Data is normally distributed.
shapiro.test(Delivery.Time)#p-value = 0.8963 > 0.05, Data is normally distributed.

#Scatter plot.
plot(Sorting.Time,Delivery.Time,main = "ScatterPlot",col="Orange",xlab = "SortingTime",
     ylab = "DeliveryTime",col.lab="Red",col.main="Green",pch=06)
lines(lowess(Sorting.Time,Delivery.Time)) # Moderate Postive correlation between sorting time and delivery time.
cor(Sorting.Time,Delivery.Time) #  0.8259973 < 0.85 , so conclude that moderate positive correlation.

#Model Building.
reg_M1 <- lm(Delivery.Time~Sorting.Time,data = dt.st)
summary(reg_M1) # Parameters are significant enough to use in our model.
                # Multiple R-squared:  0.6823 < 0.80, so it could not be a good model
confint(reg_M1,level=0.95)
pred_M1 <- as.data.frame(predict(reg_M1,interval = "predict"))
View(pred_M1)
cor(pred_M1$fit,dt.st$Delivery.Time) # 0.8259973

# Apply Transformation to increase the model accuracy.
#Log Transformation on input variable.
reg_log <- lm(Delivery.Time~log(Sorting.Time),data = dt.st)
summary(reg_log) #Adjusted R-squared:  0.6794, and intercept becomes insignificant due to this transformation.
confint(reg_log,level = 0.95)
pred_log <- as.data.frame(predict(reg_log,interval = "predict"))
View(pred_log)
cor(pred_log$fit,dt.st$Delivery.Time) #  0.8339325 incresed when compaed to first model.
#log transformation on output variable.
reg_log_op <- lm(log(Delivery.Time)~Sorting.Time,data = dt.st)
summary(reg_log_op) # Adjusted R-squared:  0.6957 raised a  bit when compared to earlier 2 models.
                    # Parameters are significant to use in our model.
confint(reg_log_op,level = 0.95)
pred_log_op <- as.data.frame(predict(reg_log_op,interval = "predict"))
cor(pred_log_op$fit,dt.st$Delivery.Time) #0.8259973

#Sqrt transformation using input variable 'sorting time'.
reg_sqrt <- lm(Delivery.Time~sqrt(Sorting.Time),data=dt.st)
summary(reg_sqrt) #Adjusted R-squared:  0.6798,Intercept becomes insignificant.
confint(reg_sqrt,level = 0.95)
pred_sqrt <- as.data.frame(predict(reg_sqrt,interval = "predict"))
cor(pred_sqrt$fit,dt.st$Delivery.Time) #0.83415

#sqrt transformation using ouput variable 'delivery time'.
reg_sqrt_op <- lm(sqrt(Delivery.Time)~Sorting.Time,data = dt.st)
summary(reg_sqrt_op)#Adjusted R-squared:  0.6885 ,parameters are significant to use in our model.
confint(reg_sqrt_op,level = 0.95)
pred_sqrt_op <- as.data.frame(predict(reg_sqrt_op,interval = "predict"))
cor(pred_sqrt_op$fit,dt.st$Delivery.Time) #0.8259973

#other transformation
reg_other <- lm(log(Delivery.Time)~Sorting.Time,I(Sorting.Time*Sorting.Time),data = dt.st)
summary(reg_other) # Adjusted R-squared:  0.4316 ; bad model.


