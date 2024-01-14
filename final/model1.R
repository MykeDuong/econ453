## ----setup, include=FALSE-----------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------
rm(list = ls()) # clear the workspace

getwd()
setwd("./")

library(car) # loads car library that does tests of linear restrictions 

library(ISLR)
library(dplyr)
library(broom)
library(modelr)
library(purrr)
library(tidyr)
library(caret)

count_item <- function(column) {
  ifelse(
    grepl("NA", column, fixed = TRUE), 
    0,
    lengths(regmatches(column, gregexpr(";", column)))
  )
}
data <- read.csv("./data/survey_results_public.csv")

data$USA <- ifelse(grepl("United States of America", data$Country, fixed = TRUE), 1,0)

data$YearsCodePro <- ifelse(grepl("More than 50 years", data$YearsCodePro, fixed = TRUE), 51, data$YearsCodePro)

data$YearsCodePro <- ifelse(grepl("Less than 1 year", data$YearsCodePro, fixed = TRUE), 0, data$YearsCodePro)

data$YearsCodePro <- as.integer(data$YearsCodePro)

data$India <- ifelse(grepl("India", data$Country, fixed = TRUE), 1,0)

data$VerySmallComp <- ifelse( 
  grepl("2 to 9 employees", data$OrgSize, fixed = TRUE) | 
  grepl("10 to 19 employees", data$OrgSize, fixed = TRUE) | 
  grepl("Just me - I am a freelancer, sole proprietor, etc.", data$OrgSize, fixed = TRUE),
  1, 0
)

data$FullInPerson <- ifelse( grepl("Fully remote", data$RemoteWork, fixed = TRUE), 1, 0)

data$MasterEdu <- ifelse(
  grepl("Master", data$EdLevel, fixed = TRUE)
, 1, 0)

data$Britain <- ifelse(
  grepl("Britain", data$Country, fixed = TRUE), 1, 0
)

data$Brazil <- ifelse(
  grepl("Brazil", data$Country, fixed = TRUE), 1, 0
)

data$NumOfDevType <- count_item(data$DevType)

data$VeryLargeComp <- ifelse( 
  grepl("5,000 to 9,999 employees", data$OrgSize, fixed = TRUE) |
  grepl("10,000 or more employees", data$OrgSize, fixed = TRUE), 1, 0)

data$Canada <- ifelse(
  grepl("Canada", data$Country, fixed = TRUE), 1, 0
)

data$NumOfLanguages <- count_item(data$LanguageHaveWorkedWith)

data$Germany <- ifelse(
  grepl("Germany", data$Country, fixed = TRUE), 1, 0
)

data$FullyRemote <-ifelse(
  grepl("Fully remote", data$RemoteWork, fixed = TRUE), 1, 0
)

data$College <- ifelse(
  grepl("School (i.e., University, College, etc)", data$LearnCode, fixed = TRUE), 
  1, 0
)

data$SmallComp <- ifelse( grepl("20 to 99 employees", data$OrgSize, fixed = TRUE), 1, 0)

data$LargeComp <- ifelse( grepl("1,000 to 4,999 employees", data$OrgSize, fixed = TRUE), 1, 0)

data$Hybrid <- ifelse(grepl("Hybrid (some remote, some in-person)", data$RemoteWork, fixed = TRUE), 1, 0)

data$DataScientist <- ifelse(
  grepl("Data scientist", data$DevType, ignore.case = TRUE, fixed = TRUE), 1, 0)

data$Executive <- ifelse(
  grepl("Senior Executive", ignore.case = TRUE, data$DevType, fixed = TRUE), 1, 0)

data$ConvertedCompYearly <- ifelse(
  data$ConvertedCompYearly < 1000 |
  data$ConvertedCompYearly > 500000,
  NA, data$ConvertedCompYearly
)

data = data[c("ConvertedCompYearly",
              "USA",
              "YearsCodePro",
              "India",
              "VerySmallComp",
              "FullInPerson",
              "MasterEdu",
              "Britain",
              "Brazil",
              "NumOfDevType",
              "VeryLargeComp",
              "Canada",
              "NumOfLanguages",
              "Germany",
              "FullyRemote",
              "College",
              "SmallComp",
              "LargeComp",
              "Hybrid",
              "DataScientist",
              "Executive"
)]

data = data[complete.cases(data), ]

train_control <- trainControl(method = "repeatedcv", number = 10)

# train the model on training set
model <- train(ConvertedCompYearly ~., 
               data = data,
               trControl = train_control,
               method = "lm")
# print cv scores
summary(model)

# Results with Errors
model$results