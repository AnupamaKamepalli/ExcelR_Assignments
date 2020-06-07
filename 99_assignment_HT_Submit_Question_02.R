# Question - 2, Topic : Hypthesis Testing

###### Anova Test ################################################################################
# Step01: Check for Normality
#H0 : Data is normally distributed
#H1 : Data is not Normally Distributed

# File Reading.
TatD1 <- read.csv(file.choose(),header = TRUE)
View(TatD1)

shapiro.test(TatD1$Laboratory.1)
# p-value = 0.5508 > 0.05, Fail to reject H0. Data is normally distributed

shapiro.test(TatD1$Laboratory.2)
#p-value = 0.8637 > 0.05, Fail to Reject H0. Data is normally distributed.

shapiro.test(TatD1$Laboratory.3)
# p-value = 0.4205 > 0.05,Fail to reject H0. Data is normally distributed

shapiro.test(TatD1$Laboratory.4)
# p-value = 0.6619 > 0.05,Fail to reject H0. Data is normally distributed.

#*******Insight : All Datapoints are normally distributed

#step02:  Check for variance.
#H0 : The variance b/w the variables are equal
#h1 : The variance b/w the variables are not equal
# performing variance test b/w more than 2 variables so will chhose for bartlett test.

#a) perform stack function.
TatD1_stack <- stack(TatD1)
View(TatD1_stack)

#b)bartlett test
bartlett.test(values~ind,data=TatD1_stack)
# p-value = 0.1069 > 0.05, fail to reject H0. The variance between the variables are equal.

#Step03 : Perform Anova Test
#H0 : There is no difference in the TAT among different laboratory.
#H1 : There is diffrence in the TAT among diffrent laboratory.

Anova_results <- aov(values~ind,data = TatD1_stack)
summary(Anova_results)
# p value = 2e-16 < 0.05, Reject H0.

#Conclusion : There is a diffrence in the TAT among diffrebt laboratory.





