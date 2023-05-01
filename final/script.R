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

data <- read.csv("./data/survey_results_public.csv")

data$blockchainFavour = ifelse(
  grepl("Very favorable", ignore.case = FALSE, data$Blockchain, fixed = TRUE) |
  grepl("Favorable", ignore.case = FALSE, data$Blockchain, fixed = TRUE),
  1, 0
)
data$blockchainFavour = as.factor(data$blockchainFavour)

count_item <- function(column) {
  ifelse(
    grepl("NA", column, fixed = TRUE), 
    0,
    lengths(regmatches(column, gregexpr(";", column)))
  )
}

data$NumOfTech = 
  count_item(data$LanguageHaveWorkedWith) +
  count_item(data$DatabaseHaveWorkedWith) + 
  count_item(data$PlatformHaveWorkedWith) + 
  count_item(data$WebframeHaveWorkedWith) +
  count_item(data$MiscTechHaveWorkedWith)
data$NumOfLanguages <- count_item(data$LanguageHaveWorkedWith)
data$NumOfDatabases <- count_item(data$DatabaseHaveWorkedWith)
data$NumOfPlatform <- count_item(data$PlatformHaveWorkedWith)
data$NumOfWebframe <- count_item(data$WebframeHaveWorkedWith)
data$NumOfMisc <- count_item(data$MiscTechHaveWorkedWith)

data$Hobby <- ifelse(grepl("Hobby", data$CodingActivities, fixed = TRUE), 1,0)

data$Young <- ifelse(grepl("18-24 years old", data$Age, fixed = TRUE), 1,0)
data$Normal <- ifelse(grepl("25-34 years old", data$Age, fixed = TRUE), 1,0)
data$Middle <- ifelse(grepl("35-44 years old", data$Age, fixed = TRUE), 1,0)
data$Retiring <- ifelse(grepl("45-54 years old", data$Age, fixed = TRUE), 1,0)
data$Senior <- ifelse(grepl("55-64 years old", data$Age, fixed = TRUE), 1,0)
data$BlockchainDev <- ifelse(grepl("Blockchain", data$DevType, fixed = TRUE), 1,0)

data$SmallComp <- ifelse(
  grepl("2 to 9 employees", data$OrgSize, fixed = TRUE) |
  grepl("20 to 99 employees", data$OrgSize, fixed = TRUE),
  1, 0
)

data$MediumComp <- ifelse(
  grepl("100 to 499 employees", data$OrgSize, fixed = TRUE) |
    grepl("500 to 999 employees", data$OrgSize, fixed = TRUE),
  1, 0
)

data$SolidityWantTo <- ifelse(
  grepl("Solidity", data$LanguageWantToWorkWith, fixed = TRUE), 1, 0
)

data$WorkWithSolidity <- ifelse(
  grepl("Solidity", data$LanguageHaveWorkedWith, fixed = TRUE), 1, 0
)

data$BackendExposure <- ifelse(
  grepl("back-end", data$DevType, fixed = TRUE)
  , 1, 0
)

data$Entr <- ifelse(
  grepl("Bootstrapping a business", data$CodingActivities, fixed = TRUE)
  , 1, 0
)

data$NumOfPos <- count_item(data$DevType)

data = data[c("blockchainFavour", 
              "NumOfTech",
              #"NumOfLanguages", 
              #"NumOfDatabases", 
              #"NumOfPlatform",
              "NumOfWebframe", 
              #"NumOfMisc", 
              #"WorkExp", 
              "ConvertedCompYearly", 
              "Young",
              "Retiring",
              "Senior",
              "Normal",
              #"Middle",
              "SmallComp",
              #"MediumComp",
              "BackendExposure",
              #"Hobby",
              "SolidityWantTo",
              "WorkWithSolidity",             
              "Entr",
              "BlockchainDev")]

data = data[complete.cases(data), ]

train_control <- trainControl(method = "repeatedcv", number = 10)

# train the model on training set
model <- train(blockchainFavour ~., 
               data = data,
               trControl = train_control,
               method = "glm",
               family=binomial())

# print cv scores
summary(model)
  

# accuracy from mod$results$Accuracy
model.accuracy = max(model$results$Accuracy)

model.accuracy