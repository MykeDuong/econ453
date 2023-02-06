
# Q1
## a) 
rm(list = ls()) # clear the workspace
library(readxl)

setwd("G:/chromeDownload/R4532/PS1")
# you may want to set the directory where you store the data.

# choose the sheet we want:
data1<- read_excel("pset1_data.xlsx", sheet="scores")

summary(data1)

str(data1)

## to save it into a table
### do summary statistics for the third column: score
summary(data1[,3]);
df1_summary<-as.data.frame(apply(data1[,3],2,summary))
df1_summary
### add more summray statistics:
df1_summary=rbind(df1_summary,
                  var(data1$score ),
                  sd(data1$score),
                  sd(data1$score)/mean(data1$score),
                  mean(abs(data1$score-mean(data1$score))),
                  IQR(data1$score)
)
### name these summary statistics:
row.names(df1_summary)[7] <- "Var"  
row.names(df1_summary)[8] <- "SD"  
row.names(df1_summary)[9] <- "coefficient of variation"  
row.names(df1_summary)[10] <- "mean absolute deviation"  
row.names(df1_summary)[11] <- "IQR" 
### report the table:
df1_summary

### You don't have to use this method. You can create the table from EXCEL, and include it
### in the PDF (or Doc) file you want to submit.

## b) Draw a Box and whisker plot for scored.
 
boxplot(data1$score )


## c) Calculate summary statistics (this time only sample mean, and sample  standard deviation) for scores by year and by school.  Present these numbers in well formatted table and a diagram (a bar chart would be good). 

# get the mean, and the sd of score by year and school
mean_S=aggregate(data1$score, list(data1$school,data1$year), FUN = function(x)
  c(mean=mean(x), sd=sd(x)))

mean_S

# draw it in a bar chart
barplot(x[,"mean"]~Group.2+Group.1 ,data=mean_S, beside=T, 
        col=c("dodgerblue2","firebrick2"),
        main = "Averagr Score by Year and School", ylab = "Average Score",
        xlab = "School")
legend("topleft", c("2014","2016"), pch=15, bty="n", 
       col=c("dodgerblue2","firebrick2"))

barplot(x[,"sd"]~Group.2+Group.1 ,data=mean_S, beside=T, 
        col=c("dodgerblue2","firebrick2"),
        main = "SD  by Year and School", ylab = "SD",
        xlab = "School")
legend("topleft", c("2014","2016"), pch=15, bty="n", 
       col=c("dodgerblue2","firebrick2"))

# another way, do it one by one:
## calculate the mean:
mean_Ss=tapply(data1$score,  data1[,c(1,2)], mean)  

mean_Ss
## plot the mean:
barplot(mean_Ss, beside=T, 
        col=c("dodgerblue2","firebrick2"),
        main = "Averagr Score by Year and School", ylab = "Average Score",
        xlab = "School")
legend("topleft", c("2014","2016"), pch=15, bty="n", 
       col=c("dodgerblue2","firebrick2"))

## calculate the sd:
sd_Ss=tapply(data1$score,  data1[,c(1,2)], sd) 
## plot the sd:
barplot(sd_Ss, beside=T, 
        col=c("dodgerblue2","firebrick2"),
        main = "SD by Year and School", ylab = "SD",
        xlab = "School")
legend("topleft", c("2014","2016"), pch=15, bty="n", 
       col=c("dodgerblue2","firebrick2"))



# reference: https://statisticsglobe.com/draw-grouped-barplot-in-r

# special thanks to: Jordan Carlton, for his suggestions.

## d) Based on these numbers, how do the schools compare?
  
#  Hint: may want to choose the one with higher average score and lower sd.

# Question 2

# Another way to read the data: type the complete path where you store the data:
data2<- read_excel("G:/chromeDownload/R4532/PS1/pset1_data.xlsx", sheet="medical_expenses")

## a) Provide summary statistics for all numeric variables. Provide min, max, sample mean, sample variance, sample standard deviation, coefficient of variation, mean absolute deviation, Q1, median, Q3, and IQR. Present your results in well formatted table with a title.

summary(data2)
str(data2)
## to save it into a table

## Omitted
  
## b) Are there any outliers in the data for medical expenses? 


# get the IQR length
IQR_med=quantile(data2$medicalexpn,0.75)-quantile(data2$medicalexpn,0.25)
# get the lower and higher bound
low_med=quantile(data2$medicalexpn,0.25)-1.5*IQR_med
high_med=quantile(data2$medicalexpn,0.75)+1.5*IQR_med
# identify the (possible) outliers
# the value
data2$medicalexpn[which(data2$medicalexpn <low_med | data2$medicalexpn > high_med)]
# which observation
row.names(data2)[which(data2$medicalexpn <low_med | data2$medicalexpn > high_med)]
# So, yes. There is one outlier.


## extra, FYI: to drop the outliers
## https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/
kept<- subset(data2, data2$medicalexpn >=low_med & 
                data2$medicalexpn <= high_med)

## c) Calculate summary statistics for all numeric variables (this time only sample mean, and sample standard deviation) by country and by location (urban/rural). Present these numbers in well formatted table and a diagram (a bar chart would be good). You may consider three separate diagrams for the three numeric variables. 


mean_S=aggregate(data2$medicalexpn, list(data2$country,data2$location), FUN = function(x)
  c(mean=mean(x), sd=sd(x)))
# get the mean of score by year and school


# I omit other results which shall be part of the complete answers, for you to fill in the gaps.



## e)  Draw a scatter plot of medical expenses (on y-axis) and income (on x-axis).

plot(medicalexpn~income,data=data2)

plot(data2$income,data2$medicalexpn)


# pay attention to the different orders of the variables.

## f) Calculate sample correlations between all numeric variables  and present them in a table. 


cor(data2[,c(3,4,5)])

cor(cbind(data2$medicalexpn,data2$income,data2$education))