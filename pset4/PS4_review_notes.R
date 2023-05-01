## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------------------------
#### Notice: these are NOT the complete answers for the problem sets,
#### I omitted some of the questions.
#### Please make sure your submission of the answers are complete.


rm(list = ls()) # clear the workspace

getwd()
setwd("G:/chromeDownload/R453/PS4")

library(readxl)
library(car) # loads car library that does tests of linear restrictions 


### Q1


data1<- read_excel("G:/chromeDownload/R453/PS2/pset1_data.xlsx", 
                   sheet = "medical_expenses")



## ----------------------------------------------------------------------------------------------------

str(data1)

##### a)

data1$D_RURAL=ifelse((data1$location =="RURAL"), 1, 0)
data1$D_USA=ifelse((data1$country =="USA"), 1, 0)
data1$D_CANADA=ifelse((data1$country =="CANADA"), 1, 0)

model1 <-lm(data=data1, medicalexpn~income+education+D_RURAL+D_USA+D_CANADA)

summary(model1)

##### b)

linearHypothesis(model1, c("D_RURAL = 0","D_USA=0","D_CANADA=0"))

## 2.301e-09< 0.05, reject the null

##### c)
predict(model1, data.frame(income=50000, education=12, D_RURAL=1, D_USA=0, D_CANADA=0), 
           interval="confidence", level = 0.95) 

predict(model1, data.frame(income=50000, education=12, D_RURAL=0, D_USA=1, D_CANADA=0), 
           interval="confidence", level = 0.95) 

## Reference:
## pizza_prediction_intervals_edited.pdf
## https://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval

### Q2
data2<- read_excel("G:/chromeDownload/R453/PS2/pset1_data.xlsx", 
                   sheet = "scores")

str(data2)
data2$dken=ifelse((data2$school =="Kennedy"), 1, 0)
data2$d2016=ifelse((data2$year ==2016), 1, 0)

###### a)
model2_1=lm(data=data2[data2$dken==1,],score~ d2016)

summary(model2_1)

Output <- summary(model2_1)
coef(Output)

pt(coef(Output)[2,3], 98 )
1-pt(coef(Output)[2,3], 98)

## or:
pt(coef(Output)[2,3], 98, lower = FALSE)


### 0.005984738<0.05, reject the Null even at 5% significance  level

##### ref:
##### https://bookdown.org/ccolonescu/RPoE4/multiplereg.html
####
df=100-2

qt(0.95, 98)
# plot the t distribution density on the domain [-4,4]
curve(dt(x, df=98),
      xlim = c(-4, 4),
      main = "Rejection Region of a Right-Sided Test",
      yaxs = "i",
      xlab = "t-statistic",
      ylab = "",
      lwd = 2,
      axes = "F")

# add the x-axis
axis(1, 
     at = c(-4,-qt(0.994, 98), 0, qt(0.994, 98), 4), 
     padj = 0.5,
     labels = c("",expression(t^-1~(.006)==-2.56), 0, expression(t^-1~(.994)==2.56), ""))

# shade the rejection region in the left tail
polygon(x = c(2.56, seq(2.56, 4, 0.01), 4),
        y = c(0, dt(seq(2.56, 4, 0.01),df=df), 0), 
        col = "darkred")

####

#####


### two-sided, just for your interest
### 
2*(1-pt(coef(Output)[2,3], 98))

coef(Output)[2,4]

curve(dt(x, df=df),
      xlim = c(-4, 4),
      main = "Rejection Region of a Right-Sided Test",
      yaxs = "i",
      xlab = "t-statistic",
      ylab = "",
      lwd = 2,
      axes = "F")

# add the x-axis
axis(1, 
     at = c(-4,-2.560779, 0, 2.560779, 4), 
     padj = 0.5,
     labels = c("",expression(t^-1~(0.006)==-2.56), 0, expression(t^-1~(0.994)==2.56), ""))

# shade the rejection region in the left tail
polygon(x = c(2.56, seq(2.56, 4, 0.01), 4),
        y = c(0, dt(seq(2.56, 4, 0.01), df=df),0), 
        col = "darkred")

polygon(x = c(-4, seq(-4, -2.56, 0.01), -2.56),
        y = c(0, dt(seq(-4, -2.56, 0.01), df=df), 0), 
        col = "darkred")

#####

# You need to finish the question by testing the null hypothesis for Lincoln
model2_2=lm(data=data2[data2$dken==0,],score~ d2016)

####
###### b)
model2_3=lm(data=data2[data2$d2016==0,],score~ dken)
linearHypothesis(model2_3, c("dken = 0"))

summary(model2_3)
Output <- summary(model2_3)
coef(Output)
2*(1-pt(coef(Output)[2,3], 98))

### 0.02915<0.05, reject the Null even at 5% significance level

# You need to finish the question by testing the null hypothesis for 2016
model2_4=lm(data=data2[data2$d2016==1,],score~ dken)

######
###### c)

model2_5=lm(data=data2,score~ dken+d2016)
summary(model2_5)
linearHypothesis(model2_5, c("dken = 0", "d2016=0"))

## 3.762e-05< 0.05, reject the Null.

###### Q3

data3<- read_excel("jaggia_ba_2e_ch08_data.xlsx", 
                                             sheet = "wages")

summary(data3)
str(data3)

###### a)
scatterplot(data3$Wage~data3$Age)

## non-linearity

###### b)
data3_T=data3[1:140,];
data3_V=data3[141:160,];

model3_1=lm( Wage  ~ Graduate+Age,data=data3_T)
summary(model3_1);

model3_2=lm( Wage  ~ Graduate+Age+ I(Age^2),data=data3_T)
summary(model3_2);

### can also use this way to include the 2nd order term

data3_T$aa=data3_T$Age*data3_T$Age
model3_3=lm( Wage  ~ Graduate+Age+ aa,data=data3_T)

###### c)
predict.lm(model3_1, data.frame(Graduate=1, Age=30)) 

predict.lm(model3_2, data.frame(Graduate=1, Age=30)) 

predict.lm(model3_3, data.frame(Graduate=1, Age=30, aa=30^2)) 

####
###### d)
Output=summary(model3_2)
### 2.00885*Age+(-0.0205)*Age^2
### taking 1st order derivative and set it to be 0:
### 2.00885+( -2*0.0205)*Age=0
-coef(Output)[3,1]/(2*coef(Output)[4,1])
##

###### e)
## Mean Squared Error (MSE)
## Root Mean Squared Error (RMSE)
## Mean Absolute Error (MAE)
## Mean Absolute Percent Error (MAPE),

summary(model3_2)

data3_T$yhat=predict.lm(model3_2, data3_T) 
data3_T$res=resid(model3_2)

(cor(data3_T$Wage,data3_T$yhat))^2

MSE=(sum((data3_T$Wage-data3_T$yhat)^2)/(140))
MSE

RMSE=MSE^0.5
RMSE

MAE=mean(abs(data3_T$Wage-data3_T$yhat))
MAE
mean(abs(data3_T$res));

MAPE=mean(abs(data3_T$Wage-data3_T$yhat)/data3_T$Wage*100)
MAPE

RSE=(sum((data3_T$Wage-data3_T$yhat)^2)/(140-4))^0.5
RSE

knitr::purl("PS4_review_notes.Rmd")
 

