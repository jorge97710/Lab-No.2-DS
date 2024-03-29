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
# Regresi�n Lineal M�ltiple 
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

library(class)
predKnn<-knn(trainSet[,c(1:8,10:11)],testSet[,c(1:8,10:11)],as.factor(trainSet$SalePrice),k=3)
cfm<-confusionMatrix(as.factor(testSet$TotalBsmtSF),predKnn)
cfm

#Con caret usando validaci???n cruzada
set.seed(123)
trctrl <- trainControl(method = "cv",
                       number = 10)

train$am<-as.factor(train$SalePrice)
test$am<-as.factor(test$SalePrice)

#-----
train <- complete.cases(train)
model_caret <- train(SalePrice~TotalBsmtSF+X1stFlrSF+GrLivArea+GarageCars+GarageArea+GarageYrBlt+YearBuilt+YearRemodAdd +MasVnrArea+Fireplaces,   # model to fit
                     data = train,                        
                     trControl = trctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)   
model_caret

model_caret$finalModel

test$pred <-predict (model_caret, newdata = test, na.action = na.pass)
summary(test$pred)


model_caret$resample

sd(model_caret$resample$Rsquared)

summary(model_caret$resample)


#-----

#--
fit <- train(SalePrice ~.,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trctrl,
             
             data       = train,na.action = na.pass)


#--

knnTrain <- train(SalePrice ~., data = trainSet, method = "knn",
                  trControl=trctrl,
                  preProcess = c("center", "scale"), tuneLength=10)
predknn<-predict(knnTrain,newdata = testSet[,c(1:8,10:11)])
summary(knnTrain)
cfm<-confusionMatrix(as.factor(testSet$am),predKnn)
cfm