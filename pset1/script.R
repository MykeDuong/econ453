# Minh Duong, ECON 453, pset 1

# Packages Install && Import
library(readxl)

# Clear workspace
rm(list = ls())

# Relative directory
# Put data to the data directory inside the project directory
# setwd("data")
getwd()

# Problem 1
data1<- read_excel("pset1_data.xlsx", sheet="scores")

summary(data1)

str(data1)

## 1a
#df1_summary <- as.data.frame(apply(data1, 2, summary))
#df1_summary
### add more summray statistics:
df1_summary = rbind(
  # df1_summary,
  var(data1$score),
  sd(data1$score),
  sd(data1$score) / mean(data1$score),
  mean(abs(data1$score - mean(data1$score))),
  IQR(data1$score)
)
df1_summary
length(df1_summary)
### name these summary statistics:
row.names(df1_summary)[1] <- "Var"  
row.names(df1_summary)[2] <- "SD"  
row.names(df1_summary)[3] <- "Coefficient of Variation"  
row.names(df1_summary)[4] <- "Mean Absolute Deviation"  
row.names(df1_summary)[5] <- "IQR" 
### report the table:
df1_summary



