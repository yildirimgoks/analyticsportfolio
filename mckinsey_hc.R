library(ggplot2)
library(tidyr)
library(rpart)
library(randomForest)
library(party)
library(mice)
library(party)
library(xgboost)
library(Matrix)

train <- read.csv("train_mk_hc.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv("test_mk_hc.csv", header = TRUE, stringsAsFactors = TRUE)

test$stroke <- NA
all <- rbind(train,test)

all$hypertension <- as.factor(all$hypertension)
all$heart_disease <- as.factor(all$heart_disease)
all$stroke <- as.factor(all$stroke)

#-----EXPLORATORY ANALYSIS-----

table(is.na(all))
all[all$smoking_status=="", "smoking_status"] <- NA

#bmi & smoking_status have NA / blank values. They respectively have about 3.3% and 31% missing value ratio.
#Beware, because my initial doubt is people with unhealthy bmi might have chosen not to disclose this information, indicating a case of 'Missing Not at Random' (MNAR).

all[all$gender == "Other",]
#Among people with the 'Other' gender we have 4 children which makes me think if 'Other' partially or fully used to indicate missing gender data, rather than a gender besides male/female.

ggplot(all, aes(x=age)) + 
  geom_density(data=all[all$gender=="Female",], fill="green", alpha=0.2) + 
  geom_density(data=all[all$gender=="Male",], fill="blue", alpha=0.2)
#In 20-50 age bracket we have more cases for Female patients while until ~20 the amount of cases for Male patients is superior.

prop.table(table(all$heart_disease, all$smoking_status),2)
prop.table(table(all$hypertension, all$smoking_status),2)
#Looking at the relation between smoking status and having hypertension/heart_disease, results are pretty counter-intuitive.
#For both health problems, former smokers seem to have the highest ratio of sick people. One explanation of this could be a high number of smokers who had to quit after diagnosis.
#For hypertension, people who never smoked and people who smoke have a very similar ratio of sick people.
prop.table(table(train$smoking_status, train$stroke),1)
#Former smokers have the highest ratio of stroke patients as well

prop.table(table(train$smoking_status, train$stroke),1)

prop.table(table(train$ever_married, train$stroke),1)
prop.table(table(all$ever_married, all$smoking_status),1)

ggplot(all, aes(x=age)) +
 geom_density(data=all[all$smoking_status=="formerly smoked",], fill="orange", alpha=0.2) +
 geom_density(data=all[all$smoking_status=="never smoked",], fill="green", alpha=0.2) +
 geom_density(data=all[all$smoking_status=="smokes",], fill="red", alpha=0.2)
#Above graph shows that amount of smokers tend to fall with age after age of 50 while the amount of former smokers reach it's peak.


ggplot(train, aes(x=age)) + 
  geom_density(data=train[train$stroke=="1",], fill="red", alpha=0.2) + 
  geom_density(data=train[train$stroke=="0",], fill="green", alpha=0.2)
#This plot shows that stroke possibility dramatically increases with age. However, interestingly, it starts falling after about 76 years old.

ggplot(train, aes(x=bmi)) + 
  geom_density(data=train[train$stroke=="1",], fill="red", alpha=0.2) + 
  geom_density(data=train[train$stroke=="0",], fill="green", alpha=0.2)
#This plot shows that stroke possibility dramatically increases with age. However, interestingly, it starts falling after about 76 years old.

ggplot(all, aes(x=age)) + 
  geom_density(data=all[all$ever_married=="No",], fill="red", alpha=0.2) + 
  geom_density(data=all[all$ever_married=="Yes",], fill="green", alpha=0.2)
#Amount of patient who were ever married surpass who didn't by the age of 30.

ggplot(all, aes(x=age)) + 
  geom_density(data=all[all$hypertension=="1",], fill="red", alpha=0.2) + 
  geom_density(data=all[all$hypertension=="0",], fill="green", alpha=0.2)

ggplot(all, aes(x=age)) + 
  geom_density(data=all[all$heart_disease=="1",], fill="red", alpha=0.2) + 
  geom_density(data=all[all$heart_disease=="0",], fill="green", alpha=0.2)
#As expected hypertension & heart disease is more prominent among older people

ggplot(all, aes(x=bmi, y=avg_glucose_level)) + 
  geom_point()
#No obvious relation between avg_glucose_level vs bmi

#Also, there are several very high bmi values, up to 97.60 (~260 kg, assuming 165 cm height).
#I will be leaving these values in the dataset as is, because I believe them to be plausible.

#----MISSING VALUES----

#looking at bmi
prop.table(table(is.na(all$bmi), all$gender),2)
prop.table(table(is.na(all$bmi), all$ever_married),2)
prop.table(table(is.na(all$bmi), all$work_type),2) 
prop.table(table(is.na(all$bmi), all$smoking_status),2) 
ggplot(all, aes(x=age)) +
  geom_density(data=all[is.na(all$bmi),], fill="red", alpha=0.2) +
  geom_density(data=all[!is.na(all$bmi),], fill="green", alpha=0.2)
  
#Missing bmi gets more frequent with increasing age, thus I conclude again on a missing-at-random case.
temp_bmi <- mice(all[, !names(all) %in% c("stroke", "id", "name", "bmi_rpart")], m=5, maxit = 5, method="pmm", seed=17)

ggplot(all, aes(x=bmi)) +
  geom_density(fill="black", alpha=0.2) +
  geom_density(data=data.frame(all$id, mice::complete(temp_bmi,1)), fill="blue", alpha=0.2) +
  geom_density(data=data.frame(all$id, mice::complete(temp_bmi,2)), fill="red", alpha=0.2) +
  geom_density(data=data.frame(all$id, mice::complete(temp_bmi,3)), fill="green", alpha=0.2) +
  geom_density(data=data.frame(all$id, mice::complete(temp_bmi,4)), fill="yellow", alpha=0.2) +
  geom_density(data=data.frame(all$id, mice::complete(temp_bmi,5)), fill="purple", alpha=0.2)
  
all$bmi <- mice::complete(temp_bmi,2)$bmi

#use rpart to impute bmi to create bmi2
#bmi_fit <- rpart(bmi ~ gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + smoking_status, data=all[!is.na(all$b)])

#looking at smoking_status
prop.table(table(is.na(all$smoking_status), all$gender),2)
prop.table(table(is.na(all$smoking_status), all$Residence_type),2)
prop.table(table(is.na(all$smoking_status), all$work_type),2) 
hist(all[all$work_type=="Never_worked", "age"]) 
hist(all[is.na(all$smoking_status), "age"])

#The missing value ratio for smoking_status is higher for people under 20. This indicates a missing-at-random case.
smoking_fit <- rpart(smoking_status ~ gender + age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + bmi, data=all[!is.na(all$smoking_status),], method="class")
all$smoking_status[is.na(all$smoking_status)] <- predict(smoking_fit, all[is.na(all$smoking_status),], type="class")

#----FEATURE ENGINEERING----
#Let's create a feature indicating people who has both heart_disease and hypertension
all$hasboth <- FALSE
all[train$heart_disease=="1" & train$hypertension=="1", "hasboth"] <- TRUE
all$hasboth <- as.factor(all$hasboth)

#Likewise, let's tag obese people with hypertension
all$obese_hyper <- FALSE
all[all$bmi_category=="obese" & all$hypertension=="1", "obese_hyper"] <- TRUE
all$obese_hyper <- as.factor(all$obese_hyper)

#Create isObese
all$isObese <- FALSE
all[all$bmi_category=="obese", "isObese"] <- TRUE
all$isObese <- as.factor(all$isObese)

all$isOverweight <- FALSE
all[all$bmi_category %in% c("obese","overweight"), "isOverweight"] <- TRUE
all$isOverweight <- as.factor(all$isOverweight)

#Create age_brackets
all$age_bracket <- ""
all[all$age >= 55, "age_bracket"] <- "Elder"
all[all$age < 55, "age_bracket"] <- "Middle Aged"
all[all$age < 35, "age_bracket"] <- "Young"
all[all$age < 20, "age_bracket"] <- "Teenager"
all[all$age < 7, "age_bracket"] <- "Kid"
all$age_bracket <- as.factor(all$age_bracket)

#Create isElder
all$isElder <- FALSE
all[all$age_bracket == "Elder", "isElder"] <- TRUE

#Creating BMI categories 
#BMI interval for each category is taken from U.S. Department of Health & Human Serices - National Heart, Lung and Blood Institute
all$bmi_category <- ""
all[all$bmi >= 30, "bmi_category"] <- "obese"
all[all$bmi < 30, "bmi_category"] <- "overweight"
all[all$bmi < 25, "bmi_category"] <- "normal"
all[all$bmi < 18.5, "bmi_category"] <- "underweight"
all$bmi_category <- as.factor(all$bmi_category)

#Although I'm no medical expert, after some Google search, I believe it would be safe to diagnose people with >=200 avg_glucose_level with diabetes.
all$diabetes <- FALSE
all[all$avg_glucose_level >= 200, "diabetes"] <- TRUE
all$diabetes <- as.factor(all$diabetes)

#Create high_stress
all$high_stress <- FALSE
all[all$work_type %in% c("private", "Self-employed") & all$Residence_type=="Rural", "high_stress"] <- TRUE
all$high_stress <-as.factor(all$high_stress)

#Create condition_count
all$condition_count <- 0
all[all$hypertension==1, "condition_count"] <- all[all$hypertension==1,"condition_count"]+1
all[all$heart_disease==1, "condition_count"] <- all[all$heart_disease==1, "condition_count"]+1
all[all$diabetes==1, "condition_count"] <- all[all$diabetes==1, "condition_count"]+1
all[all$smoking_status %in% c("formerly smoked", "smokes"), "condition_count"] <- all[all$smoking_status %in% c("formerly smoked", "smokes"), "condition_count"]+1
all$condition_count <- as.factor(all$condition_count)

#----PREDICTION-----

train <- all[1:43400,]
test <- all[43401:62001,]

#---Random Forest--- #0.7994
rf <- randomForest(stroke ~ gender + age + ever_married + work_type + 
                     avg_glucose_level + bmi + hypertension + heart_disease + 
                     Residence_type +smoking_status + diabetes + isOverweight ,data=train, ntree=1000, importance=TRUE)

rf <- randomForest(stroke ~ gender + age + ever_married + work_type + 
                     avg_glucose_level + bmi + hypertension + heart_disease + 
                     Residence_type + smoking_status + diabetes + isOverweight + condition_count, data=train, ntree=1000, importance=TRUE)


Prediction <- predict(rf,test, type="prob")[,2]
submission_file <- data.frame(id = test$id, stroke = Prediction)
write.csv(submission_file, file = "rf.csv", row.names = FALSE)
#---/RandomForest---



#---Conditional Random Forest--- #0.81744
crf <- cforest(stroke ~ gender + age + ever_married + work_type + 
                 avg_glucose_level + bmi + hypertension + heart_disease + 
                 Residence_type + smoking_status + diabetes + isOverweight + condition_count, data=train, controls=cforest_unbiased(ntree=50, mtry=3))

Prediction_crf <- predict(crf, test, OOB=TRUE, type = "prob")
submission_file_crf <- data.frame(id = test$id, stroke = sapply(Prediction_crf, "[[", 2))
write.csv(submission_file_crf, file = "crf.csv", row.names = FALSE)
#---/Conditional Random Forest---



#---XGBoost--- #0.81744
train.label  <- train$stroke
test.label   <- test$stroke

options(na.action='na.pass')
dtrain  <- sparse.model.matrix(stroke ~ .-1, data=train[,!names(train) %in% c("id","hypertension","heart_disease","smoking_status","hasboth","bmi_category","obese_hyper","avg_glucose_level")])
dtest   <- sparse.model.matrix(stroke ~ .-1, data=test[,!names(test) %in% c("id","hypertension","heart_disease","smoking_status","hasboth","bmi_category","obese_hyper","avg_glucose_level")])
options(na.action='na.omit')

dim(dtrain)
dim(dtest)

param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 7,
              eta         = 0.1,
              gammma      = 1,
              colsample_bytree = 0.5,
              min_child_weight = 1)

xgb <- xgboost(params  = param,
                           data    = dtrain,
                           label   = as.numeric(as.character(train.label)), 
                           nrounds = 3000,
                           print_every_n = 100,
                           verbose = 1)

pred <- predict(xgb, dtest, outputmargin=TRUE)
submission_file_xgb <- data.frame(id = test$id, stroke = pred)
write.csv(submission_file_crf, file = "xgb2.csv", row.names = FALSE)

# Get the trained model
model <- xgb.dump(xgb, with_stats=TRUE)

# Get the feature real names
names <- dimnames(dtrain)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model=xgb)[0:20] # View top 20 most important features

# Plot
xgb.plot.importance(importance_matrix)
#---/XGBoost---