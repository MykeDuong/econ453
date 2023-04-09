## ----setup, include=FALSE--------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------
rm(list = ls()) # clear the workspace

getwd()
setwd("G:/chromeDownload/R453/PS3")

library(readxl)
library(car) # loads car library that does tests of linear restrictions 

data1<- read_excel("pset3_data.xlsx")
summary(data1)
str(data1)

#set.seed(1) 
#index=sample(c(1:58),50, replace = F);

data1=data1[c(1:50), ]
str(data1)
data1$Price <- as.numeric(data1$Price)      
data1$Area <- as.numeric(data1$Area)   
str(data1)
data1$D_Pool=ifelse((data1$Pool =="yes"), 1, 0)
data1$D_Location=ifelse((data1$Location =="North"), 1, 0)
str(data1)
var(data1$Price)
mean(data1$Price)
data1$Age=2022-(1900+data1$Year)

## premise that size of house (living area in square feet), 
## age of the house, location and swimming pool are important
## characteristics that explain the selling price

# 1a)
model1 <-lm(data=data1, Price~Area+Age+D_Pool+D_Location)
summary(model1,digits=5)

## Area is referring to the effect of size of a house on the price of the house.
## Age is referring to the effect of age of a house on the price of the house.
## D_Pool is referring to the effect of whether to have a swimming pool,
## on the price of the house.
## D_Location is referring to the effect of location of a house on the price of
## the house.

# 1b)
model1$coefficients["D_Location"]

# 1c)
model1$coefficients["D_Pool"]


# 1e)
model1$coefficients["Age"]

## Very significant.

## Year indicates the year of the construction of the house, not the age of the
## house.

# 1f)

coefs <- names(coef(model1))

linearHypothesis(model1, coefs[-1])

linearHypothesis(model1, c("Age = 0","Area=0","D_Pool=0","D_Location=0"))

## Conclude ...

# 1g)

## Reminders:
## Need to add the intercept term.
## And need to predict for the cases with different locations. 

# 1j)

model2=lm(data=data1, Price~Area+Age+D_Pool+D_Location+Area*D_Location)
#
summary(model2)

data1$inter= data1$Area*data1$D_Location

model3 <-lm(data=data1, Price~Age+Area+D_Pool+D_Location+inter)
summary(model3)

## H0: coefficient of the interaction is zero.
## Report the p-value and conclude.

# 1k)
data1$PriceK  =data1$Price/1000

## Then run the regression using this new dependent variable


