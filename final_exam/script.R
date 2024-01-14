### Minh Duong - ECON 453 - Final Exam Part 2 - R Source code

## ----setup, include=FALSE-------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

rm(list = ls()) # clear the workspace

getwd()
setwd("./")

library(readxl)
library(forecast)
library(car) # loads car library that does tests of linear restrictions 

###--------------------------------------------------------------------------
### Question 1
## 1A
data1 = read_excel(
  "./data/final_exam_part2_data.xlsx", 
  sheet="Housing_Starts"
)
summary(data1)
# Date                        HStarts      
# Min.   :2011-01-01 00:00:00   Min.   : 35.40  
# 1st Qu.:2013-12-24 06:00:00   1st Qu.: 77.83  
# Median :2016-12-16 12:00:00   Median : 97.50  
# Mean   :2016-12-15 17:00:00   Mean   : 96.38  
# 3rd Qu.:2019-12-08 18:00:00   3rd Qu.:114.88  
# Max.   :2022-12-01 00:00:00   Max.   :164.30  
monthly_data <- ts(
  data1$HStarts, 
  start = c(2011, 1), end = c(2022, 12), 
  frequency = 12 # Monthly
)

model1_1 = tslm(monthly_data ~ trend)
summary(model1_1)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 56.12329    2.24587   24.99   <2e-16 ***
#   trend        0.55532    0.02687   20.66   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.41 on 142 degrees of freedom
# Multiple R-squared:  0.7504,	Adjusted R-squared:  0.7487 
# F-statistic:   427 on 1 and 142 DF,  p-value: < 2.2e-16
forecast(model1_1, h=6)
# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Jan 2023       136.6448 119.1446 154.1450 109.7761 163.5135
# Feb 2023       137.2001 119.6949 154.7053 110.3237 164.0765
# Mar 2023       137.7554 120.2452 155.2657 110.8713 164.6396
# Apr 2023       138.3107 120.7953 155.8261 111.4187 165.2027
# May 2023       138.8660 121.3455 156.3866 111.9661 165.7660
# Jun 2023       139.4214 121.8955 156.9472 112.5133 166.3294

## 1B
model1_2 = tslm(monthly_data ~ trend + season)
summary(model1_2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 43.22445    3.19335  13.536  < 2e-16 ***
#   trend        0.55424    0.02016  27.487  < 2e-16 ***
#   season2     -0.94590    4.09208  -0.231  0.81756    
#   season3     12.86653    4.09223   3.144  0.00206 ** 
#   season4     20.12062    4.09247   4.916 2.59e-06 ***
#   season5     20.36639    4.09282   4.976 2.00e-06 ***
#   season6     24.24548    4.09327   5.923 2.62e-08 ***
#   season7     22.89124    4.09382   5.592 1.25e-07 ***
#   season8     17.97034    4.09446   4.389 2.32e-05 ***
#   season9     17.71610    4.09521   4.326 2.99e-05 ***
#   season10    16.52853    4.09605   4.035 9.22e-05 ***
#   season11     6.15763    4.09699   1.503  0.13526    
#   season12    -2.18827    4.09803  -0.534  0.59426    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.02 on 131 degrees of freedom
# Multiple R-squared:  0.8713,	Adjusted R-squared:  0.8595 
# F-statistic: 73.89 on 12 and 131 DF,  p-value: < 2.2e-16
forecast(model1_2, h=6)
# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Jan 2023       123.5888 109.9992 137.1784 102.7173 144.4603
# Feb 2023       123.1972 109.6075 136.7868 102.3257 144.0687
# Mar 2023       137.5638 123.9742 151.1534 116.6923 158.4353
# Apr 2023       145.3722 131.7825 158.9618 124.5007 166.2437
# May 2023       146.1722 132.5825 159.7618 125.3007 167.0437
# Jun 2023       150.6055 137.0159 164.1951 129.7340 171.4770

###--------------------------------------------------------------------------
### Question 2
## 2A
data2 = read_excel(
  "./data/final_exam_part2_data.xlsx", 
  sheet="gym"
)

summary(data2)
# Renewed           Age            Income           Plan          
# Min.   :0.000   Min.   :20.00   Min.   : 30.00   Length:200        
# 1st Qu.:0.000   1st Qu.:34.00   1st Qu.: 56.00   Class :character  
# Median :1.000   Median :50.00   Median : 96.50   Mode  :character  
# Mean   :0.575   Mean   :49.17   Mean   : 91.85                     
# 3rd Qu.:1.000   3rd Qu.:65.00   3rd Qu.:122.00                     
# Max.   :1.000   Max.   :80.00   Max.   :150.00                     

data2$Single = ifelse(grepl("Single",data2$Plan, fixed = TRUE), 1, 0)

model2_linear = lm(data = data2, Renewed ~ Age + Income + Single)
summary(model2_linear)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -0.3380805  0.1107921  -3.051  0.00259 ** 
#   Age          0.0054229  0.0016099   3.368  0.00091 ***
#   Income       0.0077886  0.0007725  10.082  < 2e-16 ***
#   Single      -0.1289184  0.0562271  -2.293  0.02292 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3952 on 196 degrees of freedom
# Multiple R-squared:  0.3735,	Adjusted R-squared:  0.3639 
# F-statistic: 38.95 on 3 and 196 DF,  p-value: < 2.2e-16

TestValues <- data.frame(
  Age = c(50),
  Income = c(70),
  Single = c(0)
)

predict(model2_linear, TestValues)
# Result: 0.4782644

## 2B
model2_log <- glm(
  data = data2, 
  family = binomial,
  Renewed ~ Age + Income + Single
)

# 2B - 1,2
summary(model2_log)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -4.926652   0.863719  -5.704 1.17e-08 ***
#   Age          0.036994   0.011378   3.251  0.00115 ** 
#   Income       0.043722   0.006124   7.139 9.40e-13 ***
#   Single      -0.859315   0.380345  -2.259  0.02386 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 272.74  on 199  degrees of freedom
# Residual deviance: 185.71  on 196  degrees of freedom
# AIC: 193.71
# 
# Number of Fisher Scoring iterations: 5
p = predict(model2_log, TestValues, type = "response")
p
# Result: 0.4959009

# 2B - 3
odds = p/(1-p)
odds
# Result: 0.9837371 

# 2C - Accuracy
pHatLin = predict(model2_linear)
yHatLin <- ifelse(pHatLin >= 0.5, 1,0)
accuracy_lin = mean(data2$Renewed == yHatLin)

pHatLog = predict(model2_log, type = "response")
yHatLog <- ifelse(pHatLog >= 0.5, 1,0)
accuracy_log = mean(data2$Renewed == yHatLog)

accuracy_lin
# Result: 79.5%
accuracy_log
# Result: 78.5%
# -> accuracy of LPM is more than of logistic model

###--------------------------------------------------------------------------
### Question 3
## 3A
data3 = read_excel(
  "./data/final_exam_part2_data.xlsx", 
  sheet="Houses"
)

model3 = lm(data = data3, Price ~ Sqft + Beds + Baths + Ranch)

summary(model3)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 227635.79   48419.88   4.701 2.47e-05 ***
#   Sqft            98.24      27.50   3.572 0.000857 ***
#   Beds           359.28   14087.82   0.026 0.979767    
#   Baths        67373.21   20160.74   3.342 0.001682 ** 
#   Ranch       -66711.79   23542.36  -2.834 0.006864 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 64080 on 45 degrees of freedom
# Multiple R-squared:  0.7773,	Adjusted R-squared:  0.7576 
# F-statistic: 39.28 on 4 and 45 DF,  p-value: 3.88e-14

forecastHomeValue <- data.frame(
  Sqft = c(2500),
  Beds = c(3),
  Baths = c(2),
  Ranch = c(1)
)

predict(model3, forecastHomeValue, interval = "confidence")
# fit      lwr      upr
# 542348.5 495527.8 589169.1
