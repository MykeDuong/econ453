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

## 1A
# Add necessary summary statistics:
summary(data1)
data1_summary <- as.data.frame(
  apply(data1, 2, summary)
)
data1_summary
data1_summary = rbind(
  data1_summary,
  min(data1$score),
  max(data1$score),
  mean(data1$score),
  var(data1$score),
  sd(data1$score),
  sd(data1$score) / mean(data1$score),
  mean(abs(data1$score - mean(data1$score))),
  IQR(data1$score)
)

# Provide Row names
rownames(data1_summary) <- c(
  "Length",
  "Class",
  "Mode",
  "Min",
  "Max",
  "Sample Mean",
  "Sample Variance",
  "Standard Deviation",
  "Coefficient of Variance",
  "Mean Average Deviation",
  "Interquartile Range (IQR)"
)

# Provide Column names
colnames(data1_summary) <- c(
  "Year",
  "School",
  "Score"
)

# Report the table:
data1_summary

# 1A
boxplot(data1$score)

# 1C
# Sample Mean, Sample SD aggregated by year & school
aggregated_data1 = aggregate(
  data1$score, 
  list(
    Year = data1$year, 
    School = data1$school
  ), 
  FUN = function(x) c(
    "Sample Mean" = mean(x), 
    "Sample SD" = sd(x)
  )
)

aggregated_data1

# Bar Chart Draw
# Sample Mean Chart
barplot(
  x[,"Sample Mean"] ~ Year + School,
  data = aggregated_data1, 
  beside = T, 
  col = c("dodgerblue2", "firebrick2"),
  main = "Averagr Score by Year and School", 
  ylab = "Average Score",
  xlab = "School"
)

legend(
  "topleft", 
  c("2014", "2016"), 
  pch = 15, 
  bty = "n", 
  col = c("dodgerblue2", "firebrick2")
)

# Standard Deviation Chart
barplot(
  x[,"Sample SD"] ~ Year + School ,
  data = aggregated_data1,
  beside = T,
  col = c("dodgerblue2", "firebrick2"),
  main = "Standard Deviation  by Year and School", 
  ylab = "SD",
  xlab = "School"
)

legend(
  "topleft",
  c("2014", "2016"), 
  pch = 15, 
  bty = "n", 
  col = c("dodgerblue2", "firebrick2")
)

# Question 2
data2<- read_excel("pset1_data.xlsx", sheet="medical_expenses");

# 2A
summary(data2)

data2_summary <- as.data.frame(
  apply(data2, 2, summary)
)

data2_summary

data2_summary = rbind(
  data2_summary,
  
  # Medical Experience
  min(data2$medicalexpn),
  max(data2$medicalexpn),
  mean(data2$medicalexpn),
  var(data2$medicalexpn),
  sd(data2$medicalexpn),
  sd(data2$medicalexpn) / mean(data2$medicalexpn),
  mean(abs(data2$medicalexpn - mean(data2$medicalexpn))),
  quantile(data2$medicalexpn, 0.25),
  quantile(data2$medicalexpn, 0.5),
  quantile(data2$medicalexpn, 0.75),
  IQR(data2$medicalexpn),
  
  # Income
  min(data2$income),
  max(data2$income),
  mean(data2$income),
  var(data2$income),
  sd(data2$income),
  sd(data2$income) / mean(data2$income),
  mean(abs(data2$income - mean(data2$income))),
  quantile(data2$income, 0.25),
  quantile(data2$income, 0.5),
  quantile(data2$income, 0.75),
  IQR(data2$income),
  
  # Education
  min(data2$education),
  max(data2$education),
  mean(data2$education),
  var(data2$education),
  sd(data2$education),
  sd(data2$education) / mean(data2$education),
  mean(abs(data2$education - mean(data2$education))),
  quantile(data2$education, 0.25),
  quantile(data2$education, 0.5),
  quantile(data2$education, 0.75),
  IQR(data2$education)
)

nrow(data2_summary)

# Provide Row names
rownames(data2_summary) <- c(
  "Length",
  "Class",
  "Mode",
  
  # Medical Experience
  "Medical Experience - Min",
  "Medical Experience - Max",
  "Medical Experience - Sample Mean",
  "Medical Experience - Sample Variance",
  "Medical Experience - sample standard deviation", 
  "Medical Experience - coefficient of variation",
  "Medical Experience - mean absolute deviation", 
  "Medical Experience - Q1", 
  "Medical Experience - median",
  "Medical Experience - Q3",
  "Medical Experience - IQR",
  
  # Income
  "Income - Min",
  "Income - Max",
  "Income - Sample Mean",
  "Income - Sample Variance",
  "Income - sample standard deviation", 
  "Income - coefficient of variation",
  "Income - mean absolute deviation", 
  "Income - Q1", 
  "Income - median",
  "Income - Q3",
  "Income - IQR",
  
  # Education
  "Education - Min",
  "Education - Max",
  "Education - Sample Mean",
  "Education - Sample Variance",
  "Education - sample standard deviation", 
  "Education - coefficient of variation",
  "Education - mean absolute deviation", 
  "Education - Q1", 
  "Education - median",
  "Education - Q3",
  "Education - IQR"
)

data2_summary

# 2B
IQR_med = quantile(data2$medicalexpn, 0.75) - quantile(data2$medicalexpn, 0.25)
# get the lower and higher bound
low_med = quantile(data2$medicalexpn, 0.25) - 1.5 * IQR_med
high_med = quantile(data2$medicalexpn, 0.75) + 1.5 * IQR_med

# identify the (possible) outliers

# the value
data2$medicalexpn[
  which(data2$medicalexpn < low_med | data2$medicalexpn > high_med)
]

# which observation
row.names(data2)[
  which(data2$medicalexpn < low_med | data2$medicalexpn > high_med)
]
# => The outlier is the Value 62.231 of row 27


