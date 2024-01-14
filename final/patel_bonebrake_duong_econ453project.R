## ECON 453 - Final Project
## Patel - Duong - Bonebrake

## ------------------------------- MODEL 1 ------------------------------------
knitr::opts_chunk$set(echo = TRUE)

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
data <- read.csv("./patel_bonebrake_duong_econ453project.csv")

data$USA <- ifelse(
  grepl("United States of America", data$Country, fixed = TRUE), 1,0
)

data$YearsCodePro <- ifelse(
  grepl("More than 50 years", data$YearsCodePro, fixed = TRUE), 
  51, data$YearsCodePro
)

data$YearsCodePro <- ifelse(
  grepl("Less than 1 year", data$YearsCodePro, fixed = TRUE), 
  0, data$YearsCodePro
)

data$YearsCodePro <- as.integer(data$YearsCodePro)

data$India <- ifelse(grepl("India", data$Country, fixed = TRUE), 1,0)

data$VerySmallComp <- ifelse( 
  grepl("2 to 9 employees", data$OrgSize, fixed = TRUE) | 
  grepl("10 to 19 employees", data$OrgSize, fixed = TRUE) | 
  grepl("Just me - I am a freelancer, sole proprietor, etc.", 
        data$OrgSize, fixed = TRUE
  ),
  1, 0
)

data$FullInPerson <- ifelse(
  grepl("Fully remote", data$RemoteWork, fixed = TRUE), 1, 0)

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
  grepl("School (i.e., University, College, etc)", 
        data$LearnCode, fixed = TRUE), 
  1, 0
)

data$SmallComp <- ifelse(
  grepl("20 to 99 employees", data$OrgSize, fixed = TRUE), 1, 0)

data$LargeComp <- ifelse(
  grepl("1,000 to 4,999 employees", data$OrgSize, fixed = TRUE), 1, 0)

data$Hybrid <- ifelse(
  grepl("Hybrid (some remote, some in-person)", data$RemoteWork, fixed = TRUE), 
  1, 0)

data$DataScientist <- ifelse(
  grepl("Data scientist", data$DevType, ignore.case = TRUE, fixed = TRUE), 
  1, 0)

data$Executive <- ifelse(
  grepl("Senior Executive", ignore.case = TRUE, data$DevType, fixed = TRUE), 
  1, 0)

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
              #"FullyRemote",
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

## ------------------------------- MODEL 2 ------------------------------------
knitr::opts_chunk$set(echo = TRUE)
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
    NA,
    lengths(regmatches(column, gregexpr(";", column)))
  )
}
data <- read.csv("./patel_bonebrake_duong_econ453project.csv")

data$USA <- ifelse(
  grepl("United States of America", data$Country, fixed = TRUE), 1,0)

data$YearsCodePro <- ifelse(
  grepl("More than 50 years", data$YearsCodePro, fixed = TRUE), 
  51, data$YearsCodePro
)

data$YearsCodePro <- ifelse(
  grepl("Less than 1 year", data$YearsCodePro, fixed = TRUE), 
  0, data$YearsCodePro
)

data$YearsCodePro <- as.integer(data$YearsCodePro)

data$India <- ifelse(grepl("India", data$Country, fixed = TRUE), 1,0)

data$VerySmallComp <- ifelse( 
  grepl("2 to 9 employees", data$OrgSize, fixed = TRUE) | 
  grepl("10 to 19 employees", data$OrgSize, fixed = TRUE) | 
  grepl("Just me - I am a freelancer, sole proprietor, etc.", 
        data$OrgSize, fixed = TRUE
  ),
  1, 0
)

data$FullInPerson <- ifelse(
  grepl("Fully remote", data$RemoteWork, fixed = TRUE), 1, 0
)

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
  grepl("School (i.e., University, College, etc)", 
        data$LearnCode, fixed = TRUE), 
  1, 0
)

data$SmallComp <- ifelse(
  grepl("20 to 99 employees", data$OrgSize, fixed = TRUE), 1, 0
)

data$LargeComp <- ifelse(
  grepl("1,000 to 4,999 employees", data$OrgSize, fixed = TRUE), 1, 0
)

data$Hybrid <- ifelse(
  grepl("Hybrid (some remote, some in-person)", data$RemoteWork, fixed = TRUE), 
  1, 0
)

data$DataScientist <- ifelse(
  grepl("Data scientist", data$DevType, ignore.case = TRUE, fixed = TRUE), 
  1, 0
)

data$Executive <- ifelse(
  grepl("Senior Executive", ignore.case = TRUE, data$DevType, fixed = TRUE), 
  1, 0
)

data$ConvertedCompYearly <- ifelse(
  data$ConvertedCompYearly < 1000 |
    data$ConvertedCompYearly > 500000,
  NA, data$ConvertedCompYearly
)

data$YearsSq = data$YearsCodePro ^ 2

data = data[c("ConvertedCompYearly",
              "USA",
              "YearsCodePro",
              "YearsSq",
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
              #"FullyRemote",
              "College",
              "SmallComp",
              "LargeComp",
              "Hybrid",
              "DataScientist",
              "Executive"
)]

data = data[complete.cases(data), ]

scatterplot(data$ConvertedCompYearly ~ data$NumOfLanguages, data = data)

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

## ------------------------------- MODEL 3 ------------------------------------
knitr::opts_chunk$set(echo = TRUE)
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

data <- read.csv("./patel_bonebrake_duong_econ453project.csv")

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
data$StrictlyWork <- ifelse(
  grepl("code outside of work", data$CodingActivities, fixed = TRUE), 1,0)

data$Young <- ifelse(grepl("18-24 years old", data$Age, fixed = TRUE), 1,0)
data$Normal <- ifelse(grepl("25-34 years old", data$Age, fixed = TRUE), 1,0)
data$Middle <- ifelse(grepl("35-44 years old", data$Age, fixed = TRUE), 1,0)
data$Retiring <- ifelse(grepl("45-54 years old", data$Age, fixed = TRUE), 1,0)
data$Senior <- ifelse(grepl("55-64 years old", data$Age, fixed = TRUE), 1,0)
data$BlockchainDev <- ifelse(
  grepl("Blockchain", data$DevType, fixed = TRUE), 1,0)

data$NewComp <- ifelse(
  grepl("2 to 9 employees", data$OrgSize, fixed = TRUE) |
  grepl("10 to 19 employees", data$OrgSize, fixed = TRUE) | 
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

data$SqComp <- data$ConvertedCompYearly ^ 2

data$Aspiring = data$Young * data$SolidityWantTo

data$EntrWithBlockchain = data$Entr * data$SolidityWantTo

data$BlockchainHobby = data$SolidityWantTo * data$Hobby


data = data[c("blockchainFavour", 
              "NumOfTech",
              #"NumOfLanguages", 
              #"NumOfDatabases", 
              #"NumOfPlatform",
              "NumOfWebframe", 
              #"NumOfMisc", 
              #"WorkExp", 
              "ConvertedCompYearly",
              #"SqComp",
              #"Aspiring",
              #"BlockchainHobby",
              #"EntrWithBlockchain",
              "Young",
              "Retiring",
              "Senior",
              "Normal",
              #"Middle",
              "NewComp",
              #"SmallCompWithBlockchain",
              #"MediumComp",
              "BackendExposure",
              #"Hobby",
              #"StrictlyWork",
              "SolidityWantTo",
              "WorkWithSolidity",             
              "Entr",
              "BlockchainDev")]

data = data[complete.cases(data), ]

train_control <- trainControl(method = "repeatedcv", number = 10)

# train the model on training set
model <- train(blockchainFavour ~., 
               data = data,
               #metric="Accuracy",
               trControl = train_control,
               method = "glm",
               family=binomial())

# print cv scores
summary(model)
  

# accuracy from mod$results$Accuracy
model.accuracy = max(model$results$Accuracy)

model.accuracy

confusionMatrix(model)

## ------------------------------- MODEL 4 ------------------------------------
knitr::opts_chunk$set(echo = TRUE)
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

data <- read.csv("./patel_bonebrake_duong_econ453project.csv")

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
data$StrictlyWork <- ifelse(
  grepl("code outside of work", data$CodingActivities, fixed = TRUE), 1,0)

data$Young <- ifelse(grepl("18-24 years old", data$Age, fixed = TRUE), 1,0)
data$Normal <- ifelse(grepl("25-34 years old", data$Age, fixed = TRUE), 1,0)
data$Middle <- ifelse(grepl("35-44 years old", data$Age, fixed = TRUE), 1,0)
data$Retiring <- ifelse(grepl("45-54 years old", data$Age, fixed = TRUE), 1,0)
data$Senior <- ifelse(grepl("55-64 years old", data$Age, fixed = TRUE), 1,0)
data$BlockchainDev <- ifelse(
  grepl("Blockchain", data$DevType, fixed = TRUE), 1,0)

data$NewComp <- ifelse(
  grepl("2 to 9 employees", data$OrgSize, fixed = TRUE) |
  grepl("10 to 19 employees", data$OrgSize, fixed = TRUE) | 
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

data$SqComp <- data$ConvertedCompYearly ^ 2

data$Aspiring = data$Young * data$SolidityWantTo

data$EntrWithBlockchain = data$Entr * data$SolidityWantTo

data$BlockchainHobby = data$SolidityWantTo * data$Hobby

data$NewCompWithBlockchain = data$NewComp * data$WorkWithSolidity

data = data[c("blockchainFavour", 
              "Aspiring",
              "BlockchainHobby",
              "EntrWithBlockchain",
              "Retiring",
              "Senior",
              "Normal",
              "NewComp",
              "NewCompWithBlockchain",
              "MediumComp",
              "BackendExposure",
              "Hobby",
              "StrictlyWork",
              "SolidityWantTo",
              "WorkWithSolidity",             
              "Entr",
              "BlockchainDev")]

data = data[complete.cases(data), ]

train_control <- trainControl(method = "repeatedcv", number = 10)

# train the model on training set
model <- train(blockchainFavour ~., 
               data = data,
               #metric="Accuracy",
               trControl = train_control,
               method = "nb")

# print cv scores
summary(model)


# accuracy from mod$results$Accuracy
model.accuracy = max(model$results$Accuracy)

model.accuracy

confusionMatrix(model)