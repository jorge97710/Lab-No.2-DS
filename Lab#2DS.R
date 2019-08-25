sample <- read.csv(file="sample_submission.csv", header=TRUE)
test <- read.csv(file="test.csv", header=TRUE)
train <- read.csv(file="train.csv", header=TRUE)

#simple random sample for 60% train and 40% test. based on train 
porcentaje<-0.6
set.seed(8)
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
# Regresión Lineal Múltiple 
#-------------------------------------------------
library(caret)
##  install.packages("caret")
fitLM<-lm(SalePrice~TotalBsmtSF+X1stFlrSF+GrLivArea+GarageCars+GarageArea+GarageYrBlt+YearBuilt+YearRemodAdd +MasVnrArea+Fireplaces,data = train)
1/(1-summary(fitLM)$r.squared)
summary(fitLM)
plot(fitLM)
confint(fitLM)
##prueba 
predicted<-predict(fitLM,newdata = train,na.action = na.pass)
train$prediccion <- predicted
train$prediccionModeloAjustado<-round(train$prediccion,0)
train$SalePriceajustado <-round(train$SalePrice,0)

install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")

#regresion lineal
trainSet = read.csv("train.csv") 
testSet = read.csv("test.csv")
trainSetData <- data.frame(trainSet)
testSetData <- data.frame(testSet)
all <- merge(trainSetData, testSetData, all = TRUE)
set.seed(452)
filatrain <- sample(1:nrow(all), 0.6*nrow(all))
datatrain <-all[filatrain,]
prueba <- all[-filatrain,]
datatrain <- datatrain[complete.cases(datatrain),]
prueba <- prueba[complete.cases(prueba),]
datatrain <- within(datatrain, rm("TotalBsmtSF","GrLivArea"))
prueba <- within(prueba,rm("TotalBsmtSF","GrLivArea"))
lmodel <- lm(SalePrice ~ ., data = datatrain)
summary(lmodel)
sigma(lmodel)/mean(datatrain$SalePrice)
predL<-predict(lmodel, newdata = prueba, interval = "confidence", level = .95)
predL
resultados<-data.frame(prueba$SalePrice,predL)
resultado<-abs(resultados$prueba.SalePrice-resultados$predL)
resultado
summary(resultado)
