

# head(X, 20)
# 
# 
# # x = matrix(data(c(NA,1:length(data$Temperature)), 1:length(data), replace = TRUE), length(data))
# # data =
# # colnames(data)
# #
# # rf <- randomForest(data,y)
# 
# 
# 
# 
# sort(importance(rf_first), decreasing = TRUE)
# # importance(rf_first)
# varImpPlot(rf_first)

# count = 0
# for(val in X$CPI){
#   if(is.na(val)){
#     print("NA")
#     count = count + 1
#     X$CPI[count] = 1
#   }
#   
# }
# 
# x <- rnorm(100)
# x[c(7, 21, 33)] <- NA
# 

















# 714 Final Project
# Kaggle - Retail Data Analytics Dataset

#_______________________________________________To-Do_____________________________________________________
# sales per square foot?
# Weekly.Sales cannot be the output variable (must be 0 <= y <= 1) - glm()
# drop date
# predict weekly sales/square foot
# add binary for whether or not it is the weekend?
#_________________________________________________________________________________________________________
rm(list = ls())
# install.packages('randomForest')

X = read.csv("clean_data.csv")
head(X,5)
colnames(X)

X$Date = as.Date(X$Date, format = "%m/%d/%y")
head(X, 10)
remove_pre_NA = X[,2] >= "2011-06-11"
remove_post_NA = X[,2] <= "2012-10-21"
X = X[remove_pre_NA,]
X = X[remove_post_NA,]
colnames(X)
str(X)
head(X, 10)

#____________________________________Test and Validation Data Split_______________________________________________
# Separating Test and Validation Data using a random seed - data is not dependent on time
set.seed(2345)
trainfract<-0.75
validfract<-0.25

sampleSizeTraining <- floor(trainfract*nrow(X))
sampleSizeValidation <- floor(validfract*nrow(X))

indicesTraining <- sort(sample(seq_len(nrow(X)), size=sampleSizeTraining))
indicesValidation <- setdiff(seq_len(nrow(X)), indicesTraining)
train_data <- X[indicesTraining, ]
test_data <- X[indicesValidation, ]

#___________________________________Logistic Regression___________________________________________________________
# # glm() logistic regression - Output is AIC
# cor(X)
# LRfit<-glm(formula=Weekly.Sales~Store+Date+Temperature+Fuel_Price+MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5+CPI+Unemployment+IsHoliday+StoreType,Size..SF., data=X, family=binomial())
# # Weekly.Sales cannot be the output variable (must be 0 <= y <= 1)
# summary(LRfit) #AIC = 724.19


#___________________________________Random Forest_________________________________________________________________
library(rpart)
library(rpart.plot)
library(randomForest)
library(na.tools)

X$Date = NULL
y = X$Weekly.Sales

library('randomForest')

X = read.csv("clean_data.csv")
head(X,5)
colnames(X)


# fill NA values in CPI using moveing average of previous XXXXX values
# na.tools.ma(X$CPI, k=4, weighting = exponential)
# X$roll_mean_4 = X$CPI.rolling(4,center=True,min_periods=1).mean()



X$Date = as.Date(X$Date, format = "%m/%d/%y")
remove_pre_NA = X[,2] >= "2011-11-11"
remove_post_NA = X[,2] <= "2012-10-21"
X = X[remove_pre_NA,]
X = X[remove_post_NA,]

# filling NAs using simple weighting, a moving average window of 4, for both CPI and unemployment
# I figure that this is an alright way to handle these but we'll have to rationalize it in our presentation
maUnemploy = imputeTS::na.ma(X$Unemployment, k = 4, weighting = "simple")
maCPI = imputeTS::na.ma(X$CPI, k = 4, weighting = "simple")
# no good but just to try but just to fill in NA
maWS = imputeTS::na.ma(X$Weekly.Sales, k = 4, weighting = "simple")
maGP = imputeTS::na.ma(X$Fuel_Price, k = 4, weighting = "simple")


X$CPI = maCPI
X$Unemployment = maUnemploy
X$Weekly.Sales = maWS
X$Fuel_Price = maGP

first_RF = randomForest(Weekly.Sales ~ CPI+Unemployment+StoreType, data = X)
first_RF
colnames(X)

X$Weekly.Sales
