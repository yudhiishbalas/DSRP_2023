library(dplyr)
data_Set <- read.csv("data/Numeric.csv")
View(data_Set)
glimpse(data_Set)
str(data_Set)
pw <- data_Set$PriceinUKPounds # num of elements is 1:309
pw <- na.omit(pw) # num of elements is 1:198
mean(pw) # = 64255.2
median(pw) # = 52682.5
max(pw) - min(pw) # = range = 324005
var(pw) # = 1404879444
sd(pw) # = 37481.72
IQR(pw) # = 37460.25
outliers1 <- mean(pw) - 3*sd(pw) # = -48189.9674442543
outliers2 <- mean(pw) + 3*sd(pw) # = 176700.361383648
outliers3 <- as.numeric(quantile(pw,0.25) - 1.5*IQR(pw)) # = -17314.125
outliers4 <- as.numeric(quantile(pw,0.75) + 1.5*IQR(pw)) #= 13.2526.875
#pw < outliers1 # none
#pw > outliers2 # 2 elements
#pw < outliers3 # none
#pw > outliers4 # 9 elements

pw1 <- pw[pw < outliers2] # num of elements is 1:196
pw2 <- pw[pw < outliers4] # num of elements is 1:189, all outliers removed

mean(pw2) # = 58901.05
median(pw2) # = 52110

abs(mean(pw) - mean(pw2)) # = 5354.144
abs(median(pw) - median(pw2)) # = 572.5
