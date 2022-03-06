##########################################################
# Install Required packages, download data
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(maptree)) install.packages("maptree", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(caTools)
library(randomForest)
library(party)
library(rpart)
library(rpart.plot)
library(maptree)

# Housing Price dataset:
# https://www.kaggle.com/harlfoxem/housesalesprediction
# Data moved to personal github for the sake of the project
# https://github.com/martinchan127/House-Price-prediction/blob/main/kc_house_data.csv

# dl <- tempfile()

download.file("https://raw.githubusercontent.com/martinchan127/House-Price-prediction/main/kc_house_data.csv", "house_data.csv")

MainData <- read.csv('house_data.csv')

# Basic data inspection
head(MainData)

dim(MainData)     
str(MainData)     
summary(MainData)  
length(MainData)   

# Correlation
corr = round(cor(MainData[-2]),3)
head(corr)
ggcorrplot(corr, type = "lower",
           lab = TRUE, lab_size = 2.5)

# Data Plots
options(repr.plot.width = 12, repr.plot.height = 6)

contVars <- c("price", "sqft_living", "sqft_lot", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15")
forplot <- MainData[,contVars]
forplot <- as.data.frame(melt(forplot))

p <- ggplot(forplot , aes(value)) +
  geom_density(aes(fill = variable)) +
  facet_wrap(~variable, scales = "free") +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

p
# Data Preparation
sum(is.na(MainData)) 

# Data trimming for better handling
ModelData <- subset(MainData, select=-c(id,date,lat,long,sqft_living15,sqft_lot15))
ModelData <- ModelData[-which(ModelData$bedrooms == 0),]
ModelData <- ModelData[-which(ModelData$bathrooms == 0),]

## Split training/test data with caTools
split <- sample.split(ModelData, SplitRatio = 0.9) 
split

traindata <- subset(ModelData, split == "TRUE") 
testdata <- subset(ModelData, split == "FALSE")

dim(traindata)
dim(testdata)

# Construction of RMSE Function, R_sq function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

R_sq <- function(true, predicted, df){
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  1 - SSE / SST
}



# Regression Model
lr_model = lm(price~., data = traindata)
summary(lr_model)

lr_predict_test = predict(lr_model,testdata[,-1])

result <- tibble(Method = "M1 - Linear Regression", 
                 RMSE = RMSE(testdata$price, lr_predict_test ), R2 = R_sq(testdata$price, lr_predict_test,testdata[,-1]))


result

# Decision Tree
dt_model <- rpart(price ~ .,data= traindata, method= "anova",control=list(maxdepth = 4))


rpart.plot(dt_model,cex = 0.6)

dt_predict_test = predict(dt_model,testdata[,-1])

result <- bind_rows(result, tibble(Method = "M2 - Decision Tree", 
                                   RMSE = RMSE(testdata$price, dt_predict_test), R2 = R_sq(testdata$price, dt_predict_test,testdata[,-1])))

# Random Forest
set.seed(1234)

rf_house_price <- randomForest(x = traindata[,-1], 
                               y = as.vector(traindata$price), 
                               ntree = 100,  
                               mtry = round(length(colnames(traindata))/3), # predictors/3 for regression random forests 
                               importance = TRUE)


rf_predict_test <- predict(object = rf_house_price, 
                              newdata = testdata[,-1])

result <- bind_rows(result, tibble(Method = "M3 - Random Forest", 
                           RMSE = RMSE(testdata$price, rf_predict_test), R2 = R_sq(testdata$price, rf_predict_test,testdata[,-1])))


result

