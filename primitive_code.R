> colnames(train)
[1] "PassengerId" "Survived"    "Pclass"      "Name"       
[5] "Sex"         "Age"         "SibSp"       "Parch"      
[9] "Ticket"      "Fare"        "Cabin"       "Embarked"   

# setting workding directory
getwd()
# windows working directory
setwd("~/R_Projects/Titantic")
# ubuntu working directory
setwd("~/R_Projects/Titanic")
# mac working directory
setwd("/Users/yaseru2003/R-PROJECT/Titantic")
getwd()

# loading data
train = read.csv("train.csv", stringsAsFactor = FALSE)
test = read.csv("test.csv", stringsAsFactor = FALSE)

#summary
dim(train)
head(train)

# relation between survived and sex
prop.table(table(train$Sex, train$Survived),1)

# explore children's survival proportion
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# explore the ticket fare's influence on survival and also Pcalss and sex
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# using decision tree
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fancyRpartPlot(fit)
# customize the decision tree by manually chopping off branches you think not appropriate
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class")
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
"""
An interactive version of the decision tree will appear in the plot tab where you simply click
 on the nodes that you want to kill. Once you’re satisfied with the tree, hit ‘quit’ and it will
 be stored to the new.fit object. Try to look for overly complex decisions being made, and kill 
 the nodes that appear to go to far.
"""
# predict Survival or not to test data based on fit
Prediction <- predict(fit, test, type = "class")
# build a new data frame that only include PassengerId and Survived for the use of submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write the submit data into a csv file
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# manipulate text information
class(train$Name)
train$Name[1]
# adding a column of Survived to "test" dataframe, with all values being "NA".
test$Survived <- NA
# join train and test dataframe
combi <- rbind(train, test)

# split a passenger's name into parts, the result is a list
strsplit(combi$Name[1], split='[,.]')
# get the title of a passenger
strsplit(combi$Name[1], split='[,.]')[[1]][2]
# apply split function to each passenger's name, and load the title into a new column "Title".
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
# strip off the space before each title with sub() function
combi$Title <- sub(' ', '', combi$Title)
# Let's check out the new column's summary content with table function
table(combi$Title)

"""
there are a few very rare titles in here that won’t give our model much to work with, 
so let’s combine a few of the most unusual ones. We’ll begin with the French. 
Mademoiselle and Madame are pretty similar (so long as you don’t mind offending) 
so let’s combine them into a single category:
"""

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
# note: The %in% operator checks to see if a value is part of the vector we’re comparing it to.
# combine several male titles whose holders are military men or rich or noble people.
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
# combine several female titles whose holders are noble or rich women.
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
# Our final step is to change the variable type back to a factor, as these are essentially categories that we have created:
combi$Title <- factor(combi$Title)
# Let's look at SibSp (siblings) and Parch (parents and children), and calculate each passenger's family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1
# extract each passenger's last name, which is the first part in the name column
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
head(combi$Surname)
# combine FamilySize and Surname to build a new column FamilyID
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
head(combi$FamilyID)
# check out an example of FamilyID
combi[combi$FamilyID == "3Brown",]
# build a new data frame holding the table of FamilyID
famIDs <- data.frame(table(combi$FamilyID))
# customize the famIDs data frame to a new one which only holds rows whose Var1 frequency is less than or equal to 2
famIDs <- famIDs[famIDs$Freq <= 2,]
# reset values to the FamilyID in the combi data frame, setting each row's FamilyID's value to "small" if its FamilyID can be found in famIDs$Var1.
combi$FamilyID[combi$FamilyID %in% famIDs$Var1]  <- "small"
# change the class of FamilyID to factor
combi$FamilyID <- factor(combi$FamilyID)
# To this point, we finish the manipulation on names and titles.
#########################
# split combi and bring back the train data
train1  <- combi[1:891,]
# see the relation between the title and survival rate
table(train1$Survived)
table(train1$Title)
table(train1$Title, train1$Survived)
# split combi and bring back the test data
test <- combi[892:1309,]
# build a new decision tree based on existing columns and others that we just generate
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
# predict Survival or not to test data based on fit
Prediction <- predict(fit, test, type = "class")
# build a new data frame that only include PassengerId and Survived for the use of submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write the submit data into a csv file
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
###############
# use Random Forest!!!
"""
R’s Random Forest algorithm has a few restrictions that we did not have with our decision trees.
The big one has been the elephant in the room until now, we have to clean up the missing values
in our dataset.
"""
# build a decision tree used to predict age
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
# predict an age value for age-deficit rows based on the decision tree.
combi$Age[is.na(combi$Age)] <- round(predict(Agefit, combi[is.na(combi$Age),]))
summary(combi)




data.frame(SibSp = train$SibSp, Parch = train$Parch)


