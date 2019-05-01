# 714 Final Project
# Kaggle - Retail Data Analytics Dataset

#_________________________________________________________________________________________________________
# bring libraries into play
library(rpart)
library(rpart.plot)
library(randomForest)
library(na.tools)
library(imputeTS)

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



#_________________________________________________________________________________________________________
# RANDOM FOREST WITH ROWS OMITTED THAT CONTAIN NA VALUES - I omitted any rows that contain an NA - we are left
# with 1395 observations out of the 8190 observations that we had in the initial dataframe
# remove any rows that contain an na value
x_no_na = na.omit(X)
first_RF = randomForest(Weekly.Sales ~ CPI+Unemployment+StoreType+MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5, data = x_no_na)
first_RF
str(x_no_na)

#_________________________________________________________________________________________________________
# Now, rather than omitting every single row, I'm going to fill values in unemployment, CPI, and Gas Prices
maUnemploy = imputeTS::na.ma(X$Unemployment, k = 4, weighting = "simple")
maCPI = imputeTS::na.ma(X$CPI, k = 4, weighting = "simple")
# no good but just to try but just to fill in NA
maGP = imputeTS::na.ma(X$Fuel_Price, k = 4, weighting = "simple")

X$CPI = maCPI
X$Unemployment = maUnemploy
X$Fuel_Price = maGP
x_no_na_MA = na.omit(X)

second_RF = randomForest(Weekly.Sales ~ CPI+Unemployment+StoreType+MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5, data = x_no_na_MA)
second_RF
str(x_no_na_MA)


str(X)
na.omit(X)
str(X)






