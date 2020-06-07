getwd()
setwd("C:/Users/anupa/Desktop/ExcelR/Assignment/04_linear_Regression")
wg.cc <- read.csv("calories_consumed.csv")
View(wg.cc)
attach(wg.cc)
summary(wg.cc)
colnames(wg.cc) <- c("WeightGained","CaloriesConsumed")
View(wg.cc)
attach(wg.cc)

#Boxplot 
boxplot(WeightGained,col = "red",horizontal = TRUE) #No outliers existed,Right Skewed,could not be normally distributed.
boxplot(CaloriesConsumed,col="green",horizontal = TRUE) #No outliers existed, Right skewed, could not be normally distributed.

#Histogram
hist(WeightGained,col = "red") #No outlier existed,Right Skewed ie mean > median so data are not symmentric.
hist(CaloriesConsumed,col = "green") # No outliers existed, Right skewed ie mean > median so data are not symmentric.

#qqline,qqnorm - To test whether data is distributed normally or not.
qqnorm(WeightGained) # Data Could not be normally distributed
qqline(WeightGained,col = "red")

qqnorm(CaloriesConsumed)# Data could not be normally distributed
qqline(CaloriesConsumed,col = "green")

#H0  : Data is normally distributed
#H1  : Data is not normally distributed
shapiro.test(WeightGained) # p-value = 0.006646 < 0.05 So we can reject reject H0.Data is not normally distributed.
shapiro.test(CaloriesConsumed)#p-value = 0.4887 > 0.05, we fail to reject H0. so data is normally distributed.

#
install.packages("e1071")
library("e1071") 
kurtosis(CaloriesConsumed) #-0.9277095
skewness(CaloriesConsumed) # 0.5212708

#Used Standardization 
#wg.cc_scale <- scale(wg.cc)
#summary(wg.cc_scale)
#wg.cc_scale <- as.data.frame(wg.cc_scale)
#attach(wg.cc_scale)
#View(wg.cc_scale)
#shapiro.test(wg.cc_scale$WeightGained)
#kurtosis(wg.cc_scale$CaloriesConsumed)

#Scatter plot
plot(wg.cc$CaloriesConsumed,wg.cc$WeightGained,col="red")
plot(wg.cc_scale$CaloriesConsumed,wg.cc_scale$WeightGained,col="purple")
lines(lowess(wg.cc_scale$CaloriesConsumed,wg.cc_scale$WeightGained)) # could be Moderate,positive relationship.
cor(wg.cc_scale$CaloriesConsumed,wg.cc_scale$WeightGained) #0.946991, so strong positive correlation after standardization
cor(wg.cc$CaloriesConsumed,wg.cc$WeightGained) #  0.946991 > 0.85,so strong positive correlation before standardization

#Model Building

wt_model1 <- lm(wg.cc$WeightGained~wg.cc$CaloriesConsumed,data = wg.cc)
summary(wt_model1) # Multiple R-squared:  0.8968 > 0.80 : so conclude that the accuracy of the model is good.
                   # Parameters are significant enough to use in our model. because the corresponding p-values are < 0.05.
confint(wt_model1,level = 0.95)
pred1 <- predict(wt_model1,interval = "predict")
pred1 <- as.data.frame(pred1)
cor(pred1$fit,wg.cc$WeightGained) #0.946991 

wt_model2 <- lm(wg.cc_scale$WeightGained~wg.cc_scale$CaloriesConsumed)
summary(wt_model2) # Multiple R-squared:  0.8968
                   # B0 becomes insignificant to use in our model.
confint(wt_model2,level = 0.95)
pred2 <- as.data.frame(predict(wt_model2,interval = "predict"))
cor(pred2$fit,wg.cc_scale$WeightGained) # 0.946991
