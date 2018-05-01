#XGBoost with 10-fold Cross Validation
library(Amelia)
library(mice)
library(ggplot2)
library(rpart)
library(car)
library(Matrix)
library(xgboost)
library(caret)
library(randomForest)
library(dplyr)
library(Ckmeans.1d.dp)
library(corrplot)

test <- read.csv("test.csv", header = TRUE)
train <- read.csv("train.csv", header = TRUE)
test <- cbind(test, SalePrice = NA)
all <- rbind(train, test)

all$BsmtFullBath <- as.factor(all$BsmtFullBath)
all$BsmtHalfBath <- as.factor(all$BsmtHalfBath)
all$HalfBath <- as.factor(all$HalfBath)
all$FullBath <- as.factor(all$FullBath)
all$BedroomAbvGr <- as.factor(all$BedroomAbvGr)
all$KitchenAbvGr <- as.factor(all$KitchenAbvGr)
all$TotRmsAbvGrd <- as.factor(all$TotRmsAbvGrd)
all$GarageCars <- as.factor(all$GarageCars)
all$OverallCond <- as.factor(all$OverallCond)
all$OverallQual <- as.factor(all$OverallQual)
all$YearRemodAdd <- as.factor(all$YearRemodAdd)
all$GarageYrBlt <- as.factor(all$GarageYrBlt)
all$MoSold <- as.factor(all$MoSold)
all$YrSold <- as.factor(all$YrSold)
all$YearBuilt <- as.factor(all$YearBuilt)
all$YearRemodAdd <- as.factor(all$YearRemodAdd)

missmap(all)
#We have quite bit of columns with no or very few missing data
#PoolQC, MiscFeature, Alley, Fence seem to be missing over half of their values
#There are several columns that seem to be missing together
#One such group is: GarageCond, -Qual, -Finish, -YrBlt, -Type. The missing cases are probably houses without garages.
#The second group is: BsmtCond, - Qual- FinType2, -FiType1. So, probably houses without basement.

#--HANDLING MISSING VALUES--

#Let's start with the easier stuff...
#--MSZoning--
all[is.na(all$MSZoning), "MSZoning"] <- "RL"

#--Utilities--
summary(all$MSZoning)
all[is.na(all$Utilities), "Utilities"] <- "AllPub"

#--Exterior1st--
all[is.na(all$Exterior1st), "Exterior1st"] <- "VinylSd"

#--Exterior2nd--
all[is.na(all$Exterior2nd), "Exterior2nd"] <- "VinylSd"

#--BsmtFinSF1--
all[is.na(all$BsmtFinSF1), "BsmtFinSF1"] <- mean(all[!is.na(all$BsmtFinSF1), "BsmtFinSF1"])

#--BsmtFinSF2--
all[is.na(all$BsmtFinSF2), "BsmtFinSF2"] <- mean(all[!is.na(all$BsmtFinSF2), "BsmtFinSF2"])

#--BsmtUnfSF--
all[is.na(all$BsmtUnfSF), "BsmtUnfSF"] <- mean(all[!is.na(all$BsmtUnfSF), "BsmtUnfSF"])

#--TotalBsmtSF--
all$TotalBsmtSF <- all$BsmtFinSF1 + all$BsmtFinSF2 + all$BsmtUnfSF

#--Electrical--
all[is.na(all$Electrical), "Electrical"] <- "SBrkr"

#--BsmtFullBath--
all[is.na(all$BsmtFullBath), "BsmtFullBath"] <- "0"

#--BsmtHalfBath--
all[is.na(all$BsmtHalfBath), "BsmtHalfBath"] <- "0"

#--KitchenQual--
#P.S. At first I thought the "TA" category in KitchenQual might be another wording for
#"not available". But a check to amstat.org showed that it actually meant "Typical".
all[is.na(all$KitchenQual), "KitchenQual"] <- "TA"

#--Functional--
all[is.na(all$Functional), "Functional"] <- "Typ"

#--GarageCars--
all[is.na(all$GarageCars), "GarageCars"] <- "2"

#--GarageArea--
all[is.na(all$GarageArea), "GarageArea"] <- mean(all[!is.na(all$GarageArea), "GarageArea"])

#--SaleType--
all[is.na(all$SaleType), "SaleType"] <- "WD"

#Now onto the kinkier ones...

#--Garage-related Columns--
all[is.na(all$GarageYrBlt), c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond")]

all$GarageType <- as.character(all$GarageType)
all$GarageYrBlt <- as.character(all$GarageYrBlt)
all$GarageFinish <- as.character(all$GarageFinish)
all$GarageQual <- as.character(all$GarageQual)
all$GarageCond <- as.character(all$GarageCond)

all[is.na(all$GarageType), c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond")] <- "None"

all$GarageType <- as.factor(all$GarageType)
all$GarageYrBlt <- as.factor(all$GarageYrBlt)
all$GarageFinish <- as.factor(all$GarageFinish)
all$GarageQual <- as.factor(all$GarageQual)
all$GarageCond <- as.factor(all$GarageCond)

#Almost all garage related columns have the same number of NAs. 
#The above first line shows that those NAs indeed belong to same cases.
#My assumption was that those houses didn't have a garage.

all[is.na(all$GarageFinish), "GarageFinish"] <- "Unf"
all[is.na(all$GarageCond), "GarageCond"] <- "TA"
all[is.na(all$GarageQual), "GarageQual"] <- "TA"
all[is.na(all$GarageYrBlt), "GarageYrBlt"] <- all[is.na(all$GarageYrBlt), "YearBuilt"]

#--Basement-related Columns--
all[is.na(all$BsmtCond), c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] 

all$BsmtQual <- as.character(all$BsmtQual)
all$BsmtCond <- as.character(all$BsmtCond)
all$BsmtExposure <- as.character(all$BsmtExposure)
all$BsmtFinType1 <- as.character(all$BsmtFinType1)
all$BsmtFinType2 <- as.character(all$BsmtFinType2)

all[is.na(all$BsmtCond) & is.na(all$BsmtQual) & is.na(all$BsmtExposure) & is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2), c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] <- "None" 

all$BsmtQual <- as.factor(all$BsmtQual)
all$BsmtCond <- as.factor(all$BsmtCond)
all$BsmtExposure <- as.factor(all$BsmtExposure)
all$BsmtFinType1 <- as.factor(all$BsmtFinType1)
all$BsmtFinType2 <- as.factor(all$BsmtFinType2)

all[is.na(all$BsmtCond), "BsmtCond"] <- "TA"
all[is.na(all$BsmtQual), "BsmtQual"] <- "TA"
all[is.na(all$BsmtExposure), "BsmtExposure"] <- "No"
all[is.na(all$BsmtFinType2), "BsmtFinType2"] <- "Unf"

#--MasVnrType & MasVnrArea--
all[is.na(all$MasVnrArea), "MasVnrType"] <- "None"
all[is.na(all$MasVnrArea), "MasVnrArea"] <- 0
all[is.na(all$MasVnrType), "MasVnrType"] <- "BrkFace"

#--FireplaceQu--
sum((all[is.na(all$FireplaceQu), "Fireplaces"]))
all$FireplaceQu <- as.character(all$FireplaceQu)
all[is.na(all$FireplaceQu), "FireplaceQu"] <- "None"
all$FireplaceQu <- as.factor(all$FireplaceQu)

#--PoolQC--
length(all[!all$PoolArea==0, "PoolArea"]) #Only 13 houses have pools
all[(sample(nrow(all[is.na(all$PoolQC),]), 1)), "PoolQC"] <- "Ex"
all[(sample(nrow(all[is.na(all$PoolQC),]), 1)), "PoolQC"] <- "Fa"
all[(sample(nrow(all[is.na(all$PoolQC),]), 1)), "PoolQC"] <- "Gd"

all$PoolQC <- as.character(all$PoolQC)
all[is.na(all$PoolQC), "PoolQC"] <- "None"
all$PoolQC <- as.factor(all$PoolQC)

#--LotFrontage--
prop.table(table(is.na(all$LotFrontage), all$MSZoning), 2)
prop.table(table(is.na(all$LotFrontage), all$LotShape), 2)
#LotFrontage seems like it might be related to LotShape as the most common Shape has a very significanly few missing values.
lotf_fit <- rpart(LotFrontage~., data=all[!is.na(all$LotFrontage),], method="anova")
all[is.na(all$LotFrontage),"LotFrontage"] <- predict(lotf_fit, all[is.na(all$LotFrontage),])

#--MiscFeature & MiscVaal--
all[all$MiscVal > 0, c("MiscFeature", "MiscVal")]
#My assumption is that a lack of info on MiscFeature means there are no miscfeatures for the house.
#This assumption is supported by that almost each MiscFeature has an associated MiscVal and NAs are met
# with a 0 in MiscValue. There are barely any MiscFeatures without a positive value.

all[all$MiscVal==17000, "MiscFeature"] <- "Gar2"
#There is a single MiscFeature missing to which a MiscValue is defined. The value suggests it's a Gar2.

all$MiscFeature <- as.character(all$MiscFeature)
all[is.na(all$MiscFeature), "MiscFeature"] <- "None"
all$MiscFeature <- as.factor(all$MiscFeature)

#I will be leaving "Fence" and "Alley" out of the model since they are missing over 80% of their values.
#And I believe we can not safely assume the nature of their absfrence.
storage <- data.frame(Fence = all$Fence, Alley = all$Alley)
all$Fence <- c()
all$Alley <- c()


#--FEATURE ENGINEERING--

#--Total Area--
all$TotalArea <- all$LotArea + all$TotalBsmtSF + all$X1stFlrS + all$GarageArea + all$WoodDeckSF + all$OpenPorchSF + 
  all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch + all$PoolArea

#--Total Closed 'House' Area--
all$HouseArea <- all$TotalBsmtSF + all$X2ndFlrSF + all$GarageArea + all$EnclosedPorch 

#--Room per Avaialbe Bathrooms--

all$RoomPerBath <- as.numeric(as.character(all$TotRmsAbvGrd)) / (as.numeric(as.character(all$BsmtFullBath)) + 0.5*as.numeric(all$BsmtHalfBath))

#--Age of the building at the time of sale--
all$Age <- (as.numeric(as.character(all$YrSold)) - as.numeric(as.character(all$YearBuilt)))

#--Has the house been remodelled?--
all$YearRemodAdd <- as.numeric(as.character(all$YearRemodAdd))
all$YearBuilt <- as.numeric(as.character(all$YearBuilt))

all$IsRemodelled <- TRUE
all[all$YearBuilt == all$YearRemodAdd, "IsRemodelled"] <- FALSE

#--How old was the house before remodelling?--
all$AgeBeforeRemodel <- all$YearRemodAdd - all$YearBuilt
summary(all$AgeBeforeRemodel)
#Oops, we got a house that was remodelled before it was built :)
all[all$AgeBeforeRemodel == -1,]
#Most plausible option is that building year and remodelling year got mixed up
all[all$AgeBeforeRemodel == -1, "YearBuilt"] <- "2001"
all[all$AgeBeforeRemodel == -1, "YearRemodAdd"] <- "2002"
all$YearBuilt <- as.factor(all$YearBuilt)
all$YearRemodAdd <- as.factor(all$YearRemodAdd)

#LotShape
all$LotShape <- as.character(all$LotShape)
all[all$LotShape %in% c("IR2", "IR3"), "LotShape"] <- "Other"
all$LotShape <- as.factor(all$LotShape)

#LotConfig
all$LotConfig <- as.character(all$LotConfig)
all[all$LotConfig %in% c("CulDSac", "FR2", "FR3"), "LotConfig"] <- "Other"
all$LotConfig <- as.factor(all$LotConfig)

#SlopeIsGTL
all$SlopeIsGtl <- FALSE
all[all$LandSlope == "Gtl", "SlopeIsGtl"] <- TRUE

#IsLevel
all$IsLevel <- FALSE
all[all$LandContour == "Lvl", "IsLevel"] <- TRUE

#Is1Story
all$Is1Story <- FALSE
all[all$HouseStyle == "1Story", "Is1Story"] <- TRUE

#RoofStyle
all$RoofStyle <- as.character(all$RoofStyle)
all[!all$RoofStyle %in% c("Gable", "Hip"), "RoofStyle"] <- "Other"
all$RoofStyle <- as.factor(all$RoofStyle)

#RoofMatlComShg
all$RoofMatlComShg <- FALSE
all[all$RoofMatl == "ComShg", "RoofMatlComShg"] <- TRUE

#Foundation
all$Foundation <- as.character(all$Foundation)
all[all$Foundation %in% c("Slab", "Stone", "Wood"), "Foundation"] <- "Other"
all$Foundation <- as.factor(all$Foundation)

#IsHeatingGasA
all$IsHeatingGasA <- FALSE
all[all$Heating == "GasA", "IsHeatingGasA"] <- TRUE


#HasFireplace
all$HasFireplace <- FALSE
all[all$Fireplaces > 0, "HasFireplace"] <- TRUE

#HasMasVnr
all$HasMasVnr <- FALSE
all[all$MasVnrArea > 0, "HasMasVnr"] <- TRUE

#IsCondition1Norm
all$IsCondition1Norm <- FALSE
all[all$Condition1 == "Norm", "IsCondition1Norm"] <- TRUE

#IsCondition2Norm
all$IsCondition2Norm <- FALSE
all[all$Condition2 == "Norm", "IsCondition2Norm"] <- TRUE

#IsBldgType1Fam
all$IsBldgType1Fam <- FALSE
all[all$BldgType == "1Fam", "IsBldgType1Fam"] <- TRUE

#Exterior1st
all$Exterior1st <- as.character(all$Exterior1st)
all[!all$Exterior1st %in% c("CemntBd", "HdBoard", "MetalSd", "Plywood", "VinylSd", "Wd Sdng"), "Exterior1st"] <- "Other"
all$Exterior1st <- as.factor(all$Exterior1st)

#Exterior2nd
all$Exterior2nd <- as.character(all$Exterior2nd)
all[!all$Exterior2nd %in% c("CmentBd", "HdBoard", "MetalSd", "Plywood", "VinylSd", "Wd Sdng"), "Exterior2nd"] <- "Other"
all$Exterior2nd <- as.factor(all$Exterior2nd)

#PorchSF
all$PorchSF <- all$OpenPorchSF + all$X3SsnPorch + all$EnclosedPorch + all$EnclosedPorch

#HasPool
all$HasPool <- FALSE
all[!all$PoolArea == 0, "HasPool"] <- TRUE

#BuyingSeason
hist(as.numeric(all$MoSold)) 
#We could say that months 4 to 6 is the hottest season to buy a house
#followed buy months 1 to 3 (+7)
all$BuyingSeason <- 1
all[all$MoSold %in% c(1,2,3,7), "BuyingSeason"] <- 2
all[all$MoSold %in% c(4,5,6), "BuyingSeason"] <- 3


#--Some more processing--
all$OverallQual <- as.numeric(as.character(all$OverallQual))
all$OverallCond <- as.numeric(as.character(all$OverallCond))
all$BsmtFullBath <- as.numeric(as.character(all$BsmtFullBath))
all$BsmtHalfBath <- as.numeric(as.character(all$BsmtHalfBath))
all$FullBath <- as.numeric(as.character(all$FullBath))
all$HalfBath <- as.numeric(as.character(all$HalfBath))
all$BedroomAbvGr <- as.numeric(as.character(all$BedroomAbvGr))
all$KitchenAbvGr <- as.numeric(as.character(all$KitchenAbvGr))
all$TotRmsAbvGrd <- as.numeric(as.character(all$TotRmsAbvGrd))
all$GarageCars <- as.numeric(as.character(all$GarageCars))
all$ExterQual<- recode(all$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$ExterCond<- recode(all$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$BsmtQual<- recode(all$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$BsmtCond<- recode(all$BsmtCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$BsmtExposure<- recode(all$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)
all$BsmtFinType1<- recode(all$BsmtFinType1,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
all$BsmtFinType2<- recode(all$BsmtFinType2,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
all$HeatingQC<- recode(all$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$KitchenQual<- recode(all$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$Functional<- recode(all$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
all$FireplaceQu<- recode(all$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$GarageFinish<- recode(all$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
all$GarageQual<- recode(all$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$GarageCond<- recode(all$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
all$PoolQC<- recode(all$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)



all_backup <- all

predictor_all <- all[!names(all) %in% c("Id", "Street", "Utilities", "LandSlope", "BldgType", "HouseStyle", "RoofMatl", 
                                            "Heating", "Electrical", "Condition1", "Condition2", "BsmtFinSF1", "BsmtFinSF2", 
                                            "BsmtUnfSF", "1stFlrSF", "2ndFlrSF", "LowQualFinSF", "BsmtFullBath", "BsmtHalfBath", 
                                            "FullBath", "HalfBath", "GarageYrBlt", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", 
                                            "ScreenPorch", "PoolArea", "MiscFeature", "MoSold")]

train <- all[!is.na(all$SalePrice),]
test <- all[is.na(all$SalePrice),]
numerics <- all[,lapply(all, class)=="numeric"]
categorics <- all[,lapply(all, class)=="factor"]

#RandomForest for Feature Importance
train2 <- train[, !names(train) %in% c("YrSold", "YearBuilt", "GarageYrBlt", "YearRemodAdd")]
rf_feature_importance <- randomForest(SalePrice ~ . , data=train2, ntree=500, importance=TRUE)
varImpPlot(rf_feature_importance)

rf_top20 <- c("HouseArea", "Neighborhood", "OverallQual", "GrLivArea", "X1stFlrSF", 
              "TotalBsmtSF", "CentralAir", "ExterQual", "Age", "X2ndFlrSF", "GarageCars", 
              "GarageType", "TotalArea", "KitchenQual", "BsmtFinSF1", "GarageArea", "MSZoning",
              "BsmtFinType1", "BsmtQual", "FireplaceQu")

all_top20 <- all[,names(all) %in% rf_top20]
train_top20 <- all_top20[!is.na(all$SalePrice),]
test_top20 <- all_top20[is.na(all$SalePrice),]
numerics_top20 <- all_top20[,lapply(all_top20, class)=="numeric"]
categorics_top20 <- all_top20[,lapply(all_top20, class)=="factor"]

#XGBoost Prediction
catToDummy <- as.data.frame(model.matrix(~.-1, categorics))
all_dummied <- cbind(numerics, catToDummy)
train_xgb <- all_dummied[!is.na(all$SalePrice),]
test_xgb <- all_dummied[is.na(all$SalePrice),]

train_label <- all$SalePrice[!is.na(all$SalePrice)]

dtrain <- xgb.DMatrix(data = as.matrix(train_xgb), label= train_label)
dtest <- xgb.DMatrix(data = as.matrix(test_xgb))

default_param <-list (
        objective = "reg:linear",
        booster = "gbtree",
        eta=0.1, #default = 0.3
        gamma=0,
        max_depth=6,
        min_child_weight=1,
        subsample=1,
        colsample_bytree=1 
      )

xgbcv <- xgb.cv(params = default_param, data = dtrain, nrounds = 500, nfold = 10, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)

xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 124)
XGBpred <- predict(xgb_mod, dtest)
Submission <- data.frame(Id = 1461:2919, SalePrice = XGBpred)
write.csv(Submission, file="submission_xgb.csv", row.names = FALSE)

mat <- xgb.importance (feature_names = colnames(train),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)
