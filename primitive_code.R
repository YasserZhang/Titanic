> colnames(train)
[1] "PassengerId" "Survived"    "Pclass"      "Name"       
[5] "Sex"         "Age"         "SibSp"       "Parch"      
[9] "Ticket"      "Fare"        "Cabin"       "Embarked"   

getwd()
# windows working directory
setwd("~/R_Projects/Titantic")
# ubuntu working directory
setwd("~/R_Projects/Titanic")
getwd()

train = read.csv("train.csv", stringsAsFactor = FALSE)
test = read.csv("test.csv", stringsAsFactor = FALSE)

dim(train)
head(train)

library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

class(train$Name)
train$Name[1]
