library(mice)
library(ggplot2)
library(tidyr)
library(dplyr)
library(randomForest)
library(rpart)


train <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)
test$Survived <- NA
test <- test[, c(1,12,2,3,4,5,6,7,8,9,10,11)]
all <- rbind(train,test)
all$Sex <- as.factor(all$Sex)
all$Embarked <- as.factor(all$Embarked)
all[all$Cabin == "",]$Cabin <- NA


summary(all)

#263 NAs in Age, 1 NA in Fare, 2 blanks in Embarked
all[all$Embarked == "",]$Embarked <- NA

#Completing Age
prop.table((table(is.na(all$Age), all$Sex)), 2)
prop.table((table(is.na(all$Age), all$Pclass)), 2)
prop.table((table(is.na(all$Age), all$Embarked)), 2)

#There is discrepancy between the ratio of missing ages for females vs males (16.7% vs 21.9%).
#However more noteworthy are the discrepancies between different classes and embarked ports.
#Pclass 3 has a missing Age ratio of 29.3% whereas it is 5.8% for Pclass 2. Likewise, Embarked Q has a missing Age ratio of 59.3%, much higher than other ports. Also, 91.9% of passengers who embarked from Port Q were Class 3.
#Missing-at-Random decided, we can use predictive mean matching to complete the Age data.

tempAge <- mice(all, m=8, meth="pmm", seed=500)

#Plotting all imputation together to identify which one represents original data the best
age_df <- data.frame(org <- all$Age)

while (ncol(age_df) < 9) {
  age_df <- cbind(age_df, data.frame(mice::complete(tempAge, (ncol(age_df)))$Age))
}

colnames(age_df) <- c("org", 1:8)

ggplot(gather(age_df), aes(x=value, color=key)) + geom_density(aes(group=key))
ggplot(all, aes(x=Age)) + geom_density(fill="green", alpha = 0.3) + geom_density(data=mice::complete(tempAge,3), aes(fill="red"), alpha=0.3)

all$Age <- mice::complete(tempAge,3)$Age #Upon inspection imputed dataset 3 selected for further analysis


#Combine SibSp and Parch under family size
all$FamilySize <- all$SibSp + all$Parch + 1

#Creating IsSingle
all$IsSingle <- FALSE
all[all$FamilySize==1,]$IsSingle <- TRUE


#Creating a FamilyName column
all$FamilyName <- as.factor(sapply(all$Name, function(x){strsplit(x, ', ')}[[1]][1]))

#There are 875 unqiue FamilyNames. There seems to be 14 families with more than 5 members.
#When we look into some of these families we can see they are usually made up of either a big family or a big family mixed up with several single travellers with the same family name
#We can also see that, with a few exceptions, families travel under the same ticker number. So we can use FamilyName+TicketNo as a unique identifier for families.
all$Family <- paste(all$FamilyName, all$Ticket, sep="-")


#Creating a Deck column
all$Deck <- as.factor(sapply(all$Cabin, function(x){substr(x, 1,1)}[1]))

#Extracting titles from names
all$Titles <- sapply(strsplit(all$Name, "[,|.]"), "[[", 2)
all$Titles[all$Titles %in% c(' Mlle', ' Ms')] <- ' Miss'
all$Titles[all$Titles %in% c(' Mme')] <- ' Mrs'
all$Titles[all$Titles %in% c(" Capt", " Don", " Major", " Sir", " Col")] <- " Sir"
all$Titles[all$Titles %in% c(' Dona', ' Lady', ' the Countess', ' Jonkheer')] <- ' Lady'
all$Titles[all$Titles %in% c(' Dr', ' Rev')] <- ' DrRev'
all$Titles <- as.factor(all$Titles)
#We can see several titles with a handful of cases each. We could deal with them to avoid overfitting, however I think this 'rare' titles might indicative of a persons social class, which I believe would be a important factor for survival.

#Creating FamilyType


#Completing Embarked: There are only two missing values. Southampton has the significant majority in our data so we assign S for the missing values.
all[which(is.na(all$Embarked)),]$Embarked <- "S"

#Completing Fare data by putting the median Fare value to the only missing case
all$Fare[is.na(all$Fare)] <- median(all$Fare, na.rm = TRUE)

#Completing Cabin: Cabin column has 1014 missing values, a ratio of almost 75%. Normally I would remove this column from analysis but I have a few extra ideas that I want to check first.
#I think it would be safe to assume that family members stay in the same deck if not the same room. Let's see how many family members have NA cabins with a family member with a known cabin
familiesWithCabin <- spread(as.data.frame(prop.table(table(all$Family,is.na(all$Cabin)), 1)), Var2, Freq)
colnames(familiesWithCabin) <- c("family","knownCabin"," noKnownCabin")
summary(familiesWithCabin[familiesWithCabin$knownCabin > "0", ])
#There are 214 families with known cabins for at least one member. Let's extract which deck their room is on, assign all family members to the same deck.
familyDecks <- unique(all[!is.na(all$Deck) & all$SibSp + all$Parch > 0, c("Family", "Deck")])

deckAssigner <- function(Family, Deck) {
  ifelse (Family %in% familyDecks$Family, familyDecks$Deck, Deck)
}

familyDecks$Deck <- as.character(familyDecks$Deck) #above function doesn't work as intended if .$Deck is of class 'factor'.

all[is.na(all$Deck),]$Deck = apply(all[ is.na(all$Deck),c("Family", "Deck")], 1, function(y) deckAssigner(y["Family"], y["Deck"]))
#Well, unfortunately this whole procedure helped us with just 4 missing values. Let's dive further to see if we can find anything else.

prop.table(table(is.na(all$Deck), all$Pclass), 1) #Most of the missing cabin info is related with Pclass 3 (68.4%). Pclass 1 has very few missing cabins with only 6.4%.
prop.table(table(all$Deck, all$Pclass), 1) #The distribution of classes two decks doesn't look random. We can see that each deck has a signifcant preference for a Pclass. Let's assign decks to passengers according the the Pclass distributions.


all[which(is.na(all$Deck) & all$Pclass==1), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==1),]), size=5)), "Deck"] <- "A"
all[which(is.na(all$Deck) & all$Pclass==1), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==1),]), size=16)), "Deck"] <- "B"
all[which(is.na(all$Deck) & all$Pclass==1), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==1),]), size=25)), "Deck"] <- "C"
all[which(is.na(all$Deck) & all$Pclass==1), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==1),]), size=9)), "Deck"] <- "D"
all[which(is.na(all$Deck) & all$Pclass==1), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==1),]), size=8)),"Deck"] <- "E"
all[which(is.na(all$Deck) & all$Pclass==1), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==1),]), size=2)),"Deck"] <- "T"

all[which(is.na(all$Deck) & all$Pclass==2), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==2),]), size=66)),"Deck"] <- "D"
all[which(is.na(all$Deck) & all$Pclass==2), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==2),]), size=44)),"Deck"] <- "E"
all[which(is.na(all$Deck) & all$Pclass==2), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==2),]), size=144)),"Deck"] <- "F"

all[which(is.na(all$Deck) & all$Pclass==3), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==3),]), size=77)),"Deck"] <- "C"
all[which(is.na(all$Deck) & all$Pclass==3), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==3),]), size=115)),"Deck"] <- "E"
all[which(is.na(all$Deck) & all$Pclass==3), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==3),]), size=307)),"Deck"] <- "F"
all[which(is.na(all$Deck) & all$Pclass==3), ][(sample(nrow(all[which(is.na(all$Deck) & all$Pclass==3),]), size=192)),"Deck"] <- "G"

#Let's seperate test & train data sets
all$Deck <- as.factor(all$Deck)

#Creating Age2 and completing using decision tree to see if it performs better
all$Age2 <- rbind(train,test)$Age 
age2_fit <- rpart(Age2 ~ Pclass + Sex + Parch + SibSp + Embarked + Titles + Deck + FamilySize, data=all[which(!is.na(all$Age2)),], method="anova")
all[which(is.na(all$Age2)), "Age2"] <- predict(age2_fit, all[which(is.na(all$Age2)),])

#Creating AgeBrackets because I suspect that having each different age might lead to overfitting
#A quick google search suggests average life expectancy in UK in the year 1912 was 51(M) and 55(F).
#Let's assume the wealth of titanic passengers give them about +20% life expectancy and set it to 60 years of age.

all$AgeBrackets <- ""
all$AgeBrackets <- as.character(all$AgeBrackets)
all[all$Age >= 50,]$AgeBrackets <- "Senior"
all[all$Age < 50,]$AgeBrackets <- "MiddleAge"
all[all$Age < 30,]$AgeBrackets <- "Young"
all[all$Age < 20,]$AgeBrackets <- "Teeanger" 
all[all$Age < 12,]$AgeBrackets <- "Kid" 
all[all$Age < 4,]$AgeBrackets <- "Toddler" 
all$AgeBrackets <- as.factor(all$AgeBrackets)

#Looking at a table with AgeBrackets and Parch, we can see that there are quite a bit of minors without a parent
table(all$AgeBrackets, all$Parch)
all$NoParentMinor <- FALSE
all[all$AgeBrackets %in% c("Kid","Toddler","Teeanger") & all$Parch == 0,]$NoParentMinor <- TRUE

#Let's create Fare Brackets as well while we are at it
hist(all$Fare, breaks=40)
all$FareBrackets <- ""
all$FareBrackets <- as.character(all$FareBrackets)
all[all$Fare >= 80,]$FareBrackets <- "High"
all[all$Fare < 80,]$FareBrackets <- "Medium"
all[all$Fare <= 30,]$FareBrackets <- "Low"
all[all$Fare <= 10,]$FareBrackets <- "Lowest"
all$FareBrackets <- as.factor(all$FareBrackets)

#Also, FamilySize Brackets, because why not?
all$FamilysizeBrackets <- ""
all$FamilysizeBrackets <- as.character(all$FamilysizeBrackets)
all[all$FamilySize >= 5,]$FamilysizeBrackets <- "Large"
all[all$FamilySize < 5,]$FamilysizeBrackets <- "Medium"
all[all$FamilySize ==2,]$FamilysizeBrackets <- "Duo"
all[all$FamilySize ==1,]$FamilysizeBrackets <- "Solo"
all$FamilysizeBrackets <- as.factor(all$FamilysizeBrackets)

#Since in R Random Forests can't work with factor levels above 53, let's decrease the number of our levels by grouping single people under "solo" and families with 3 or less members under "small"
all[all$FamilySize==1,]$Family <- "Solo" 
all[all$FamilySize < 4,]$Family <- "Small" 
all$Family <- as.factor(all$Family)

train <- all[1:891,]
test <- all[892:1309,]

surv_fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Embarked + Family + Deck + Titles + FamilySize, data=train, importance=TRUE, ntree=2000)
#0.77990

varImpPlot(surv_fit)

surv_fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Embarked + Deck + Titles + FamilySize, data=train, importance=TRUE, ntree=2000)
#0.78468 (-Family)

varImpPlot(surv_fit)

surv_fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Embarked + Titles + FamilySize + Fare, data=train, importance=TRUE, ntree=2000)
#0.79425 (-Family -Deck +Fare)

surv_fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + AgeBrackets + Embarked + Titles + FamilySize + Fare + IsSingle + NoParentMinor, data=train, ntree=2000)
#0.79425 (-Age +AgeBrackets +IsSingle +NoParentMinor)
#Score didn't improve.

surv_fit_logres <- glm(as.factor(Survived)~ Pclass + Sex + Age + Embarked + Titles + FamilySize + Fare, family = binomial, data=train)
#0.77033 (-Family -Deck + Fare, Logistic Regression)

surv_fit_cart <- rpart(as.factor(Survived) ~ Pclass + Sex + Age + Embarked + Titles + FamilySize + Fare, data=train, method="class")
#0.79904

#Logistic Regressions Prediction
summary(surv_fit_logres)
Prediction_logres <- predict(surv_fit_logres, type="response", newdata=test)
test$Survived <- as.numeric(Prediction_logres >= 0.5)
submission_file_logres <- data.frame(test[c("PassengerId","Survived")])
write.csv(submission_file_logres, file = "submission_logres.csv", row.names = FALSE)

#Random Forest Prediction
Prediction <- predict(surv_fit, test)
submission_file <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submission_file, file = "submission.csv", row.names = FALSE)

#Decision Tree Prediction
Prediction <- predict(surv_fit_cart, test, type="class")
submission_file <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submission_file, file = "submission_cart.csv", row.names = FALSE)
