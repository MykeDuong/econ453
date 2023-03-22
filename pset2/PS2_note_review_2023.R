## ----setup, include=FALSE------------------------------------------------------------------------------
# knitr::opts_chunk$set(echo = TRUE)
# knitr::purl("PS2_note_review_2023.Rmd")

## ------------------------------------------------------------------------------------------------------
rm(list = ls()) # clear the workspace

getwd()
setwd("./data")

library(readxl)

data1<- read_excel("pset1_data.xlsx", sheet="scores")
#View(data1)

summary(data1)

var(data1$score )
mean(data1$score )
n <- nrow(data1)


## ------------------------------------------------------------------------------------------------------

qt(0.025, 199)
qt(0.975, 199)

mean(data1$score )+(sd(data1$score )/sqrt(200))*qt(0.025, 199)
mean(data1$score )+(sd(data1$score )/sqrt(200))*qt(0.975, 199)

t.test(data1$score,conf.level = 0.95)$"conf.int"



## ------------------------------------------------------------------------------------------------------

# one way: calculate it manually

t_stat <- (mean(data1$score) - 63) / (sd(data1$score) / sqrt(n))
t_stat

# t_stat (Z_calc in the notes)  smaller than 0.

2*pt(t_stat, n-1)

# the other way: use t.test function

t.test(data1$score, mu = 63,conf.level = 0.95, alternative = "two.sided")

# p-value >5% DO NOT reject the null hypothesis that overall average 
# test scores (for both schools and for both years combined) is 63 

# plot the t distribution density on the domain [-4,4]
qt(0.975, 199)
qt(0.025, 199)

curve(dt(x, df=n-1),
      xlim = c(-4, 4),
      main = "Rejection Region of a Two-Sided Test",
      yaxs = "i",
      xlab = "t-statistic",
      ylab = "",
      lwd = 2,
      axes = "F")

# add the x-axis
axis(1, 
     at = c(-4,-1.972, 0, 1.972, 4), 
     padj = 0.5,
     labels = c("",expression(Phi^-1~(.025)==-1.972), 0, expression(Phi^-1~(.975)==1.972), ""))

# shade the rejection region in the left tail
polygon(x = c(1.972, seq(1.972, 4, 0.01), 4),
        y = c(0, dt(seq(1.972, 4, 0.01), df=n-1),0), 
        col = "darkred")

polygon(x = c(-4, seq(-4, -1.972, 0.01), -1.972),
        y = c(0, dt(seq(-4, -1.972, 0.01), df=n-1), 0), 
        col = "darkred")




## ------------------------------------------------------------------------------------------------------

S1=data1$score[data1$school=='Lincoln' & data1$year==2014]

S2=data1$score[data1$school=='Lincoln' & data1$year==2016]

S3=data1$score[data1$year==2014]

S4=data1$score[data1$year==2016]

S5=data1$score[data1$school=='Kennedy' & data1$year==2014]

S6=data1$score[data1$school=='Kennedy' & data1$year==2016]

t_stat <- (mean(S2) - mean(S1))/sqrt((var(S2)/length(S2)+(var(S1)/length(S1)))) 

t_stat



## ------------------------------------------------------------------------------------------------------

df=(((var(S2)/length(S2)+(var(S1)/length(S1))))^2)/
  (((var(S2)/length(S2))^2/(length(S2)-1))+((var(S1)/length(S1))^2/(length(S1)-1)))

df

1-pt(t_stat, df)


t.test(S2, S1, conf.level = 0.95, alternative = "greater")


# p-value= 0.009<5%, REJECT the null hypothesis that  average test scores
# for Lincoln for 2016 is no greater than  that for 2014; against the
# alternative hypothesis that scores  for Lincoln for 2016 is greater
# than  that for 2014.


# You need to finish the question by testing the null hypothesis for Kennedy



# plot the t distribution density on the domain [-4,4]
curve(dt(x, df=length(S2)+length(S1)-2),
      xlim = c(-4, 4),
      main = "Rejection Region of a Right-Sided Test",
      yaxs = "i",
      xlab = "t-statistic",
      ylab = "",
      lwd = 2,
      axes = "F")

# add the x-axis
axis(1, 
     at = c(-4,-1.64, 0, 1.64, 4), 
     padj = 0.5,
     labels = c("",expression(Phi^-1~(.05)==-1.66), 0, expression(Phi^-1~(.95)==1.66), ""))

# shade the rejection region in the left tail
polygon(x = c(1.66, seq(1.66, 4, 0.01), 4),
        y = c(0, dt(seq(1.66, 4, 0.01),df=df), 0), 
        col = "darkred")



## ------------------------------------------------------------------------------------------------------

data1$pass =ifelse((data1$score >=40), 1, 0)


S7=data1$pass[data1$school=='Kennedy' ]

S8=data1$pass[data1$school=='Lincoln' ]

# one way:
t_stat <- (mean(S7) - mean(S8))/sqrt((var(S7)/length(S7)+(var(S8)/length(S8)))) 

t_stat

# the other way from lecture 15:
p0=mean(S7)
p1=mean(S8)

t_stat2 <- (mean(S7) - mean(S8))/sqrt(((p0*(1-p0))/length(S7)+((p1*(1-p1))/length(S8)))) 

t_stat2

#explanation: 
var(S7)*((length(S7)-1)/length(S7))

p0*(1-p0)


# t_stat (Z_calc in the notes) larger than 0.

df=(((var(S7)/length(S7)+(var(S8)/length(S8))))^2)/
  (((var(S7)/length(S7))^2/(length(S7)-1))+((var(S8)/length(S8))^2/(length(S8)-1)))


2*(1-pt(t_stat, df))

t.test(S7, S8, conf.level = 0.95, alternative = "two.sided")

# p-value= 0.0003<5%, REJECT the null hypothesis that  proportion of students 
# who pass the exam is the same for both schools.




