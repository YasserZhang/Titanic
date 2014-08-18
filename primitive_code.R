> colnames(train)
[1] "PassengerId" "Survived"    "Pclass"      "Name"       
[5] "Sex"         "Age"         "SibSp"       "Parch"      
[9] "Ticket"      "Fare"        "Cabin"       "Embarked"   

getwd()
setwd("~/R_Projects/Titantic")
getwd()

train = read.csv("train.csv", stringsAsFactor = FALSE)
test = read.csv("test.csv", stringsAsFactor = FALSE)

dim(train)
head(train)
