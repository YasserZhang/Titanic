getwd()
# set working Directory in ubuntu
setwd("~/R_Projects/Titanic")
getwd()
# load train and test data
train <- read.csv("train.csv", header =TRUE, stringsAsFactor = FALSE)
test  <- read.csv("test.csv", header =TRUE, stringsAsFactor = FALSE)
# review the first six rows in the train data
head(train)
# We’ll also take a look at a few values and plots to get a better understanding of our data.
"""
By first plotting the density we’re able to get a sense of how the overall data feel
and get a few vague answers: where is the general center? Is there a skew? Does is
generally take higher values? Where are most of the values concentrated?
"""
plot(density(train$Age, na.rm=TRUE))
plot(density(train$Fare, na.rm=TRUE))
barplot(table(train$Sex))
table(train$Cabin)
sum(train$Cabin !="")
barplot(table(train$Pclass))
barplot(table(train$Survived))
table(train$Survived)
plot(train$Age,train$Fare, na.rm=TRUE)
plot(density(train$Age, na.rm=TRUE)$y, )
# Survival Rate by Sex Barplot
counts <- table(train$Survived, train$Sex)
counts
class(counts)
counts[2]
counts[1]
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")
counts[2] / (counts[1] + counts[2])
counts[4] / (counts[3] + counts[4])
# Survival Rate by Passenger Class Barplot
Pclass_survival <- table(train$Survived, train$Pclass)
Pclass_survival
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People",
        main = "survived and deceased between male and female")
Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2])
Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4])
Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6])




###

















































# explore the Cabin attribute, and it seems that passengers with Cabin marks have higer survival rate than those without Cabin marks
unique(train$Cabin, na.rm=TRUE)
#plot the density of the fares of tickets with Cabin numbers
plot(density(train$Fare[train$Cabin != ""]))
# boxplot to see quantiles of the fares of tickets with Cabin numbers
boxplot(train$Fare[train$Cabin != ""])
# median of the fares of tickets with Cabin marks
median(train$Fare[train$Cabin != ""])
# mean value of the fares of tickets with Cabin marks
mean(train$Fare[train$Cabin != ""])
# check out quantile distribution of the fares of tickets with Cabin marks
quantile(train$Fare[train$Cabin != ""])
# check out mean value of all ticket fares in the train data
mean(train$Fare,na.rm=TRUE)
# see the survival rate of passengers having Cabin marks
table(train$Survived[train$Cabin != ""])
# see the proportion of death and survival among passengers with Cabin marks
prop.table(table(train$Survived[train$Cabin != ""]))
# set a new value Cabin_Death to restore a vector consisting of Cabin marks whose holders are all dead.
Cabin_Death <- train$Cabin[train$Survived == 0 & train$Cabin != ""]
# set a new value Cabin_Survived to restore a vector consisting of Cabin marks whose holders are all survived.
Cabin_Survived <- train$Cabin[train$Survived == 1 & train$Cabin != ""]
# check out Cabin classes, or Pclass, for all Passengers having Cabin marks.
train$Pclass[train$Cabin %in% Cabin_Death]
# calculate the proportion of passengers who are in the first class among the dead passengers having Cabin marks.
sum(train$Pclass[train$Cabin %in% Cabin_Death]==1)/length(train$Pclass[train$Cabin %in% Cabin_Death])
# check out these death Cabin, which category of cabins are most fatal?
C <- grep("C", Cabin_Death)
Cabin_Death[C]
A <- grep("A", Cabin_Death)
Cabin_Death[A]
B <- grep("B", Cabin_Death)
Cabin_Death[B]
# check out these survival Cabin, which category of cabins have higher survival rate
Cabin_Survived[grep("C", Cabin_Survived)]
Cabin_Death[C]
Cabin_Survived[grep("A", Cabin_Survived)]
Cabin_Survived[grep("B", Cabin_Survived)]
# subset survived passengers who have cabin marks.
train_cabin_sur <- train[train$Cabin %in% Cabin_Survived & train$Survived == 1,]
# subset all passengers who have cabin marks.
train_cabin <- train[train$Cabin != "",]
dim(train_cabin)
# subset all passengers who don't have cabin marks.
train_no_cabin <- train[train$Cabin == "",]
# check out survived passengers with cabin marks in terms of Pclass.
train_cabin_sur[train_cabin_sur$Pclass == 3,]
train_cabin_sur[train_cabin_sur$Pclass == 2,]
train_cabin_sur[train_cabin_sur$Pclass == 1,]
# check out survival rate of all passengers with cabin marks in terms of pclass.
train_cabin[train_cabin$Pclass == 3,]
train_cabin[train_cabin$Pclass == 2, ]
table(train_cabin$Survived[train_cabin$Pclass == 1])
table(train_cabin$Survived[train_cabin$Pclass == 2])
table(train_cabin$Survived[train_cabin$Pclass == 3])
# check out survival rate of all passengers in in terms of Pclass.
table(train$Survived[train$Pclass == 1])
table(train$Survived[train$Pclass == 2])
table(train$Survived[train$Pclass == 3])
# check out survival rate of all passengers with no cabin marks in terms of Pclass.
table(train_no_cabin$Survived[train_no_cabin$Pclass == 1])
table(train_no_cabin$Survived[train_no_cabin$Pclass == 3])
table(train_no_cabin$Survived[train_no_cabin$Pclass == 2])
dim(train_no_cabin)
""" 
after exploring the Cabin attribute, we can confirm the attribute contributes to the classification model we want ot fit on Survival.
"""