#------------------------------------------------
## Model 1 : LR
#------------------------------------------------
# load cleaned data
setwd("C:\\Users\\dattatray.s.shinde\\Desktop\\ngGIT\\assignment\\project\\src\\r")
ddir = "../../data/"

#source("fe.R")
load(file = paste(ddir,'model_ready_data.rda',sep=""))
load(file = paste(ddir,'final_data.rda',sep=""))

# [1] "index"             "index.1"           "Name"              "Year"              "Genre"            
# [6] "Publisher"         "NorthAmericaSales" "EuropeSales"       "JapanSales"        "RowSales"         
# [11] "TotalSales"        "Platform_score"    "VGScore"           "CriticScore"       "UserScore" 

str(df_train)
str(df_test)

require(caret)
set.seed(666)

# lm_model <- train(TotalSales~Year+Genre+avgSales+Publisher,
#                 weights=exp(df_train$Year-min(df_train$Year)+1),
#                 data=df_train,
#                 method="lm")
lm_model <- train(TotalSales~Year+Genre+avgSales,
                  #weights=exp(df_train$Year-min(df_train$Year)+1),
                  data=df_train,
                  method="lm")
summary(lm_model)
predicted <- predict(lm_model,test_x)
RMSE(predicted, test_y) 

# Predict Total sales for given blind data
Genre <- c('RolePlaying')
Publisher <- c('Square EA')
avgSales <- c(0)
Year <- c(2018)
test_record = data.frame(Genre, avgSales, Year, Publisher)
predicted <- predict(lm_model,test_record)
predicted




















# Feature importance

#Check the importance of the features before running the model
# set.seed(123)
# boruta.train <- Boruta(x=x_train, y = x_train$income,doTrace = 2)
# 
# plot(boruta.train, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
#   boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta.train$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#        at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

formula = "income~."
logit.fit=glm(formula,family=binomial(logit),data=x_train)

print("GLM fit:")
summary(logit.fit)

features=names(x_train)
print("GLM vif for colliniarity(more than 2.5 is not good):")
vif.fit = vif(logit.fit)
print(vif.fit)

print("GLM Performance:")
## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
logit.preds=predict(logit.fit,newdata=x_test,type ="response") 
pred <- prediction(logit.preds,x_test$income)
perf <- performance(pred,"tpr","fpr")
plot(perf)

## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

#Confusion Matrix
preds=ifelse(predict(logit.fit,newdata=x_test,type="response")>=0.5,1,0)
table(x_test$income,preds)
confusionMatrix(data = preds, reference = x_test$income)