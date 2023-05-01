## ----setup, include=FALSE-------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

rm(list = ls()) # clear the workspace

getwd()
setwd("./")

library(readxl)
library(car) # loads car library that does tests of linear restrictions 

###--------------------------------------------------------------------------
### Question 1
data1 = read_excel("./data/pset1_data.xlsx", sheet="medical_expenses")

## 1A
data1$D_RURAL=ifelse((data1$location =="RURAL"), 1, 0)
data1$D_USA=ifelse((data1$country =="USA"), 1, 0)
data1$D_CANADA=ifelse((data1$country =="CANADA"), 1, 0)

model1 <-lm(
  data = data1, 
  medicalexpn ~ income + education + D_RURAL + D_USA + D_CANADA
)

summary(model1)

## 1B
linearHypothesis(model1, c("D_RURAL = 0","D_USA=0","D_CANADA=0"))
# 2.301e-09< 0.05 => Reject the Null hypothesis

## 1C

predict(
  model1, 
  data.frame(income=50000, education=12, D_RURAL=1, D_USA=0, D_CANADA=0), 
  interval="confidence", level = 0.95
) 

predict(
  model1, 
  data.frame(income=50000, education=12, D_RURAL=0, D_USA=1, D_CANADA=0), 
  interval="confidence", level = 0.95
) 

###--------------------------------------------------------------------------
### Question 2 
data2 = read_excel("./data/pset1_data.xlsx", sheet="scores")

data2$dken=ifelse((data2$school =="Kennedy"), 1, 0)
data2$d2016=ifelse((data2$year ==2016), 1, 0)

## 2A
# Kennedy
model2_ak = lm(
  data = data2[data2$dken==1,],
  score ~ d2016
)

summary(model2_ak)
Output <- summary(model2_ak)
coef(Output)

pt(coef(Output)[2,3], 98, lower = FALSE)

# Lincoln

model2_al = lm(
  data = data2[data2$dken==0,],
  score ~ d2016
)

summary(model2_al)
Output <- summary(model2_al)
coef(Output)

pt(coef(Output)[2,3], 98, lower = FALSE)
# 0.00943 < 0.05, reject the Null even at 5% significance level

## 2B
# 2014
model2_b2014 = lm(
  data = data2[data2$d2016 == 0,],
  score ~ dken
)

linearHypothesis(model2_b2014, c("dken = 0"))

summary(model2_b2014)
Output <- summary(model2_b2014)
coef(Output)
2 * (1 - pt(coef(Output)[2, 3], 98))
# 0.02915<0.05, reject the Null even at 5% significance level

# 2016
model2_b2016 = lm(
  data = data2[data2$d2016 == 1,],
  score ~ dken
)

linearHypothesis(model2_b2016, c("dken = 0"))

summary(model2_b2016)
Output <- summary(model2_b2016)
coef(Output)
2 * (1 - pt(coef(Output)[2, 3], 98))
# 0.02855 < 0.05, reject the Null even at 5% significance level

## 2C

model2_c = lm(
  data = data2,
  score ~ dken + d2016
)
summary(model2_c)
linearHypothesis(model2_c, c("dken = 0", "d2016 = 0"))
# 3.762e-05< 0.05, reject the Null.

###--------------------------------------------------------------------------
### Question 3
data3<- read_excel("./data/jaggia_ba_2e_ch08_data.xlsx", sheet = "Wages")

## 3A
scatterplot(data3$Wage~data3$Age)
# The plot is not linear

## 3B
data3_T = data3[1:140,]
data3_V = data3[141:160,]

model3_1 = lm(
  Wage~ Graduate + Age , 
  data = data3_T
)
summary(model3_1);

model3_2 = lm(
  Wage  ~ Graduate + Age + I(Age^2),
  data = data3_T
)
summary(model3_2);

## 3C
predict.lm(model3_1, data.frame(Graduate = 1, Age = 30)) 

predict.lm(model3_2, data.frame(Graduate = 1, Age = 30)) 

## 3D
Output = summary(model3_2)
-coef(Output)[3,1]/(2*coef(Output)[4,1])

## 3E
evaluate_model <- function(model, data) {
  summary(model)
  
  data$yhat = predict.lm(model, data) 
  data$res = data$Wage - data$yhat
  
  print("Correlation-Square:")
  print((cor(data$Wage, data$yhat)) ^ 2)
  
  MSE = (sum((data$Wage - data$yhat) ^ 2) / (140))
  print("MSE:")
  print(MSE)
  
  RMSE = MSE ^ 0.5
  print("RMSE: ")
  print(RMSE)
  
  MAE = mean(abs(data$Wage - data$yhat))
  print("MAE: ")
  print(MAE)
  mean(abs(data$res));
  
  MAPE = mean(abs(data$Wage - data$yhat) / data$Wage * 100)
  print("MAPE: ") 
  print(MAPE)
  
  RSE = (sum((data$Wage - data$yhat) ^ 2) / (140 - 4)) ^ 0.5
  print("RSE: ")
  print(RSE)
}

evaluate_model(model3_1, data3_T)
evaluate_model(model3_2, data3_T)
evaluate_model(model3_1, data3_V)
evaluate_model(model3_2, data3_V)

