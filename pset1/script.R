# Minh Duong, ECON 453, pset 1

# Packages Install && Import
library(readxl)

# Clear workspace
rm(list = ls())

# Relative directory
# Put data to the data directory inside the project directory
setwd("data")
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
  main = "Average Score by Year and School", 
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
  ylab = "Standard Deviation",
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


# Provide Row names
rownames(data2_summary) <- c(
  "Length",
  "Class",
  "Mode",
  
  # Medical Experience
  "Medical Expenses - Min",
  "Medical Expenses - Max",
  "Medical Expenses - Sample Mean",
  "Medical Expenses - Sample Variance",
  "Medical Expenses - sample standard deviation", 
  "Medical Expenses - coefficient of variation",
  "Medical Expenses - mean absolute deviation", 
  "Medical Expenses - Q1", 
  "Medical Expenses - median",
  "Medical Expenses - Q3",
  "Medical Expenses - IQR",
  
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
# Outliers not in the range [Q1 - 1.5 * IQR, Q3 + 1.5 * IQR]

IQR_med = quantile(data2$medicalexpn, 0.75) - quantile(data2$medicalexpn, 0.25)
IQR_med

# get the lower and higher bound
low_med = quantile(data2$medicalexpn, 0.25) - 1.5 * IQR_med
high_med = quantile(data2$medicalexpn, 0.75) + 1.5 * IQR_med

# identify the outliers

# Outlier value(s)
data2$medicalexpn[
  which(data2$medicalexpn < low_med | data2$medicalexpn > high_med)
]

# Outlier Record(s) (Observations(s))
row.names(data2)[
  which(data2$medicalexpn < low_med | data2$medicalexpn > high_med)
]
# => The outlier is the Value 62.231 of row 27

# 2C
aggregated_medicalexpn = aggregate(
  data2$medicalexpn,
  list(
    Country = data2$country, 
    Location = data2$location
  ), 
  FUN = function(x) c(
    "Sample Mean" = mean(x), 
    "Sample SD" = sd(x)
  )
)

# Bar Chart Draw
# Sample Mean Chart
barplot(
  x[,"Sample Mean"] ~ Country + Location,
  data = aggregated_medicalexpn, 
  beside = T, 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1"),
  main = "", 
  ylab = "Average Medical Expenses",
  xlab = "Country"
)

legend(
  "topleft", 
  c("Canada", "Mexico", "USA"), 
  pch = 15, 
  bty = "n", 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1")
)

# Standard Deviation Chart
barplot(
  x[,"Sample SD"] ~ Country + Location,
  data = aggregated_medicalexpn, 
  beside = T, 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1"),
  main = "", 
  ylab = "Standard Deviation of Medical Expenses",
  xlab = "Country"
)

legend(
  "topleft", 
  c("Canada", "Mexico", "USA"), 
  pch = 15, 
  bty = "n", 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1")
)


aggregated_income = aggregate(
  data2$income,
  list(
    Country = data2$country, 
    Location = data2$location
  ), 
  FUN = function(x) c(
    "Sample Mean" = mean(x), 
    "Sample SD" = sd(x)
  )
)

# Bar Chart Draw
# Sample Mean Chart
barplot(
  x[,"Sample Mean"] ~ Country + Location,
  data = aggregated_income, 
  beside = T, 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1"),
  main = "", 
  ylab = "Average Income",
  xlab = "Country"
)

legend(
  "topleft", 
  c("Canada", "Mexico", "USA"), 
  pch = 15, 
  bty = "n", 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1")
)

# Standard Deviation Chart
barplot(
  x[,"Sample SD"] ~ Country + Location,
  data = aggregated_income, 
  beside = T, 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1"),
  main = "", 
  ylab = "Standard Deviation of Income",
  xlab = "Country"
)

legend(
  "topleft", 
  c("Canada", "Mexico", "USA"), 
  pch = 15, 
  bty = "n", 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1")
)


aggregated_education = aggregate(
  data2$education,
  list(
    Country = data2$country, 
    Location = data2$location
  ), 
  FUN = function(x) c(
    "Sample Mean" = mean(x), 
    "Sample SD" = sd(x)
  )
)

# Bar Chart Draw
# Sample Mean Chart
barplot(
  x[,"Sample Mean"] ~ Country + Location,
  data = aggregated_education, 
  beside = T, 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1"),
  main = "", 
  ylab = "Average Education",
  xlab = "Country"
)

legend(
  "topleft", 
  c("Canada", "Mexico", "USA"), 
  pch = 15, 
  bty = "n", 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1")
)

# Standard Deviation Chart
barplot(
  x[,"Sample SD"] ~ Country + Location,
  data = aggregated_education, 
  beside = T, 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1"),
  main = "", 
  ylab = "Standard Deviation of Education",
  xlab = "Country"
)

legend(
  "topleft", 
  c("Canada", "Mexico", "USA"), 
  pch = 15, 
  bty = "n", 
  col = c("dodgerblue2", "firebrick2", "darkgoldenrod1")
)


# 2E - Draw a scatter plot of medical expenses (on y-axis) and income (on x-axis).

plot(medicalexpn ~ income,data = data2)

plot(
  data2$income, 
  data2$medicalexpn,
  "Income",
  "Medical Expertise"
)

