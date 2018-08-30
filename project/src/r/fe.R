#-------------------------------------------------------------
# Features Engineering
#-------------------------------------------------------------
print("Feature Engineering Started")
setwd("C:\\Users\\dattatray.s.shinde\\Desktop\\ngGIT\\assignment\\project\\src\\r")
ddir = "../../data/"

require(dplyr)
require(caret)
require(corrplot)

# Keep the main dataframes as it is .. may require in future
df_main_games = read.csv(paste(ddir,'games_data.csv',sep=""),header = TRUE)
colnames(df_main_games)
df_main_pf = read.csv(paste(ddir,'platforms.csv',sep=""),header = TRUE)
colnames(df_main_games)
df_main_games$Year

# [1] "index"             "index.1"           "Name"              "Year"              "Genre"            
# [6] "Publisher"         "NorthAmericaSales" "EuropeSales"       "JapanSales"        "RowSales"         
# [11] "TotalSales"        "Platform_score"    "VGScore"           "CriticScore"       "UserScore" 

vg <- df_main_games
vg = vg[3:15]
vg$Platform = vg$Platform_score
vg = vg[-10]

vg$Year <- as.numeric(as.character(vg$Year))
vg$VGScore <- as.numeric(as.character(vg$VGScore))
vg$CriticScore <- as.numeric(as.character(vg$CriticScore))
vg$UserScore <- as.numeric(as.character(vg$UserScore))
# vg = vg[1:100,]

# 1. mean median mode year for imputaion
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_year = Mode(vg$Year[!is.na(vg$Year)])
vg$Year[is.na(vg$Year)] =  mode_year
vg[vg==""] <-NA
vg$Genre <- as.character(vg$Genre)
vg$Genre[vg$Genre=="Role-Playing"] <-"RolePlaying"
#vg <- vg %>% filter(vg$Year_of_Release<=2017)
vg <- vg%>% arrange(desc(Year))

# Duplicate processed data
vg_model_data = vg

# Check missing values
missing_data_per<-apply(vg_model_data,2, function(x){length(which(is.na(x)))/dim(vg_model_data)[1]})
missing_data_per

# 2. impute publisher
length(unique(vg_model_data[["Publisher"]]))
length(unique(vg_model_data[["Genre"]]))
length(unique(vg_model_data[["Name"]]))
mode_publisher = Mode(vg_model_data$Publisher[!is.na(vg_model_data$Publisher)])
vg_model_data$Publisher[is.na(vg_model_data$Publisher)] =  mode_publisher


# 3. Impute scores
mean_CriticScore = mean(vg_model_data$CriticScore[!is.na(vg_model_data$CriticScore)])
mean_CriticScore
mean_UserScore = mean(vg_model_data$UserScore[!is.na(vg_model_data$UserScore)])
mean_UserScore
mean_VGScore = mean(vg_model_data$VGScore[!is.na(vg_model_data$VGScore)])
mean_VGScore
vg_model_data$CriticScore[is.na(vg_model_data$CriticScore)] =  mean_CriticScore
vg_model_data$UserScore[is.na(vg_model_data$UserScore)] =  mean_UserScore
vg_model_data$VGScore[is.na(vg_model_data$VGScore)] =  mean_VGScore

# create a feature publisher average sale
publisher_avg_Sales  <-vg_model_data%>%
  filter(Year>=max(Year)-3)%>% 
  group_by(Publisher)%>% 
  mutate(avgSales = mean(TotalSales))%>%ungroup()%>%
  select(Publisher,avgSales)%>% unique()
publisher_avg_Sales
vg_model_data<- left_join(vg_model_data,publisher_avg_Sales,by="Publisher")
vg_model_data$avgSales[is.na(vg_model_data$avgSales)] =0

# check for correlation
colnames(vg_model_data)
num_cols = c('VGScore','CriticScore','UserScore','avgSales','TotalSales')
vg_model_data_numeric = vg_model_data[,num_cols]
vg_model_data_non_numeric = vg_model_data %>% select(Name, Year, Genre,Publisher)
str(vg_model_data_non_numeric)
str(vg_model_data_numeric)
# cormax <- cor(vg_model_data_numeric)
# corrplot(cormax,method="number")

# Data Transformation for numeric data
cs <- preProcess(vg_model_data_numeric,method=c("center","scale"))
vg_model_data_numeric1 <- predict(cs,vg_model_data_numeric)
cormax <- cor(vg_model_data_numeric1)
corrplot(cormax,method="number")
colnames(vg_model_data_numeric1)

#combine numeric and nonnumeric data
final_vg_model_data = cbind(vg_model_data_non_numeric,vg_model_data_numeric1)
write.csv(vg_model_data, file = paste(ddir,'final_vg_model_data.csv',sep=""), row.names=FALSE)
colnames(final_vg_model_data)
require(caret)
#do the partition of data in train and test
inTrain <- createDataPartition(y=final_vg_model_data$Publisher, p=0.80, list=FALSE)  
summary(final_vg_model_data[inTrain,])
summary(final_vg_model_data[-inTrain,])

#merge data with train_only flag
df_train = final_vg_model_data[inTrain,]
colnames(df_train)
df_test = final_vg_model_data[-inTrain,]
class_var =c( "TotalSales")
train_x = df_train[-9]
train_y = df_train$TotalSales
test_x = df_test[-9]
test_y = df_test$TotalSales
save(train_x, train_y, test_x, test_y, file=paste(ddir,'model_ready_data.rda',sep=""))
save(df_train, df_test, file=paste(ddir,'final_data.rda',sep=""))

print("Feature Engineering complete.")

