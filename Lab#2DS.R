sample <- read.csv(file="sample_submission.csv", header=TRUE)
test <- read.csv(file="test.csv", header=TRUE)
train <- read.csv(file="train.csv", header=TRUE)

#simple random sample for 60% train and 40% test. based on train 
porcentaje<-0.6

corte <- sample(nrow(train),nrow(train)*porcentaje)
train<-train[corte,]
test<-train[-corte,]
#poniento uno con solo numericas 


cuantitativas<-data.frame(
  
  "LotFrontage"=train$LotFrontage,
  "LotArea"= train$LotArea,
  "YearBuilt"=train$YearBuilt,
  "YearRemodAdd"=train$YearRemodAdd,
  "MasVnrArea"=train$MasVnrArea,
  "BsmtFinSF1"=train$BsmtFinSF1,
  "BsmtFinSF2"=train$BsmtFinSF2,
  "BsmtUnfSF"=train$BsmtUnfSF,
  "TotalBsmtSF"=train$TotalBsmtSF,
  "BsmtFinSF1"=train$BsmtFinSF1,
  "X1stFlrSF"=train$X1stFlrSF,
  "X2ndFlrSF"=train$X2ndFlrSF, 
  "GrLivArea"=train$GrLivArea,
  "BsmtFullBath"=train$BsmtFullBath ,
  "BsmtHalfBath"=train$BsmtHalfBath,
  "BedroomAbvGr"=train$BedroomAbvGr,
  "KitchenAbvGr"=train$KitchenAbvGr, 
  "TotRmsAbvGrd"=train$TotRmsAbvGrd,
  "Fireplaces"=train$Fireplaces,
  "GarageYrBlt"=train$GarageYrBlt,
  "GarageCars"=train$GarageCars,
  "GarageArea"=train$GarageArea,
  "WoodDeckSF"=train$WoodDeckSF,
  "OpenPorchSF"=train$OpenPorchSF ,
  "EnclosedPorch"=train$EnclosedPorch,
  "X3SsnPorch"=train$X3SsnPorch,
  "ScreenPorch"=train$ScreenPorch,
  "PoolArea"=train$PoolArea,
  "MiscVal"=train$MiscVal,
  "YrSold"=train$YrSold,
  "SalePrice"=train$SalePrice
)

#-------------------------------------------------
# Regresi�n Lineal Simple 
#-------------------------------------------------

rls <- lm()
fitLMPW<-lm(Petal.Length~Petal.Width, data = train)