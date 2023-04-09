## ----setup, include=FALSE-----------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------
rm(list = ls()) # clear the workspace

getwd()
setwd("./")

library(readxl)
library(car) # loads car library that does tests of linear restrictions 

data <- read_excel("./data/pset3_data.xlsx")

# only get data from row 2-51 (-1 for 0-indexed)
data = data[c(1:50), ]
# Convert to numerical data typee
data$Price <- as.numeric(data$Price)      
data$Area <- as.numeric(data$Area)
# Convert dummy variables
data$D_Pool = ifelse((data$Pool == "yes"), 1, 0)
data$D_Location = ifelse((data$Location == "North"), 1, 0)
# Convert year (19xx) to age
data$Age= 2023 - (1900 + data$Year) # Change to 2023

summary(data)

# 1A. Estimated equation
model <- lm(data = data, Price ~ Area + Age + D_Pool + D_Location)
summary(model, digits = 5)

# 1B. House location
model$coefficients["D_Location"]

# 1C. Swimming pool
model$coefficients["D_Pool"]

# 1D. House Size
model$coefficients["Area"]

# 1E. House Age
model$coefficients["Age"]
## 1-tailed test
pt(coef(summary(model))[, 3], model$df, lower = TRUE)

# 1F. Overall significance
coefs <- names(coef(model))

linearHypothesis(model, coefs[-1])

linearHypothesis(model, c(
  "Age = 0", 
  "Area = 0", 
  "D_Pool = 0", 
  "D_Location = 0"
))

# 1G. Forecast of the price for a 2,000 square foot house built in 1990 with 
#     a swimming pool, a fireplace, and a garage?
forecastNorthernValue <- data.frame(
  Age = c(2023 - 1990),
  Area = c(2000),
  D_Pool = c(1),
  D_Location = c(1)
)

predict(model, forecastNorthernValue)

forecastNonNorthernValue <- data.frame(
  Area = c(2000),
  Age = c(2023 - 1990),
  D_Pool = c(1),
  D_Location = c(0)
)

predict(model, forecastNonNorthernValue)

# 1H. Reasonable

# 1I.

# 1J.
data$Area_Location = data$Area * data$D_Location

priceAreaModel = lm(
  data = data, 
  Price ~ Area + Age + D_Pool + D_Location + Area_Location
)

summary(priceAreaModel)

# 1K.
data$PriceK = data$Price / 1000
kModel = lm(data = data, PriceK ~ Area + Age + D_Pool + D_Location)
summary(kModel)
 
