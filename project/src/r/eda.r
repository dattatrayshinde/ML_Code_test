#------------------------------------------------
## EDA
#------------------------------------------------
setwd("C:\Users\dattatray.s.shinde\Desktop\ngGIT\assignment\project\src\r")
ddir = "../../data/"
#library("mlbench")
#import data
#install.packages("gdata")
# require(gdata)
df_main_games = read.csv(paste(ddir,'games_data.csv',sep=""),header = TRUE)
df_main_pf = read.csv(paste(ddir,'platforms.csv',sep=""),header = TRUE)

#descriptive statistics
summary(df_main_pf)
dim(df_main_pf)
head(df_main_pf)
sapply(df_main_pf, class)
str(df_main_pf)

summary(df_main_games)
dim(df_main_games)
head(df_main_games)
sapply(df_main_games, class)
str(df_main_games)

# missing values
library(VIM)
aggr_plot <- aggr(df_main_games, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df_main_games), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#outliers
library(ggplot2)
colnames(df_main_pf)
ggplot(df_main_pf, aes(x = Platform, y = Games)) +
  geom_boxplot() +
  theme_classic()

library(ggplot2)
library(purrr)
library(tidyr)

# Histograms
df_main_games %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# density
df_main_games %>%
  keep(is.numeric) %>%                   
  gather() %>%                           
  ggplot(aes(value)) +                    
  facet_wrap(~ key, scales = "free") +  
  geom_density()                       

#correlation matrix
require(corrplot)
df_main_games %>%
  keep(is.numeric) %>%                   
  gather()-> temp_df_main_games
dim(temp_df_main_games)
corrplot(df_main_games, method = "number") # Display the correlation coefficient


 
