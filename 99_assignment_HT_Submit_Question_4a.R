#Question - 4(a) : Customer Order form.
library(readxl)
cust_order <- read.csv(file.choose())
View(cust_order)
cust_order_stack <- stack(cust_order)
table(cust_order$Country,cust_order$Defective)
chisq.test(cust_order$Country,cust_order$Defective)
#H0: customer order form doesnot have Defective
#H1: customer order form have defectives.
#p-value = 0.2771> 0.05, fail to reject null.
