#Importing the data that was cleaned and prepared in Python
#### Setting up the Environment ####

#declare working directory
setwd("C:/University/Grad School/Thesis/Data")
getwd()
#loading libraries
#install.packages("tidyverse")
library(tidyverse)
library(caret)
library(glmnet)
#install.packages("zoo")
require(zoo)
library(jtools)
library(MASS)
library(matrixcalc)
#install.packages('mosaic')
library(mosaic)
#install.packages('caTools')
library(caTools)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('ggpubr')
library(ggpubr)
#install.packages('randomForest')
library(randomForest)
#install.packages('e1071')
library(e1071)
set.seed(0)
#importing data
temp_data = read.csv("final_data_negative_gradient.csv")

#Creating monthly variables out of quarterly data using a spline approach. 
GDP_quarterly <- temp_data$GDP_quarterly
GDP_monthly <- na.spline(GDP_quarterly)
temp_data$GDP_monthly <- GDP_monthly
GDP_monthly_FD <- diff(GDP_monthly,1)


nonfarm_productivity_quarterly <- temp_data$nonfarm_productivity
nonfarm_productivity_monthly <- na.spline(nonfarm_productivity_quarterly)
temp_data$nonfarm_productivity_monthly <- nonfarm_productivity_monthly
nonfarm_productivity_monthly_FD <- diff(nonfarm_productivity_monthly,1)


unit_labor_cost_quarterly <- temp_data$unit_labor_cost
unit_labor_cost_monthly <- na.spline(unit_labor_cost_quarterly)
temp_data$unit_labor_cost_monthly <- unit_labor_cost_monthly
unit_labor_cost_monthly_FD <- diff(unit_labor_cost_monthly,1)


current_account_balance_quarterly <- temp_data$current_account_balance
current_account_balance_monthly <- na.spline(current_account_balance_quarterly)
temp_data$current_account_balance_monthly <- current_account_balance_monthly
current_account_balance_monthly_FD <- diff(current_account_balance_monthly,1)
temp_data <- temp_data[-1,]
temp_data$current_account_balance_monthly_FD <- current_account_balance_monthly_FD


temp_data$GDP_monthly_FD <- GDP_monthly_FD
temp_data$nonfarm_productivity_monthly_FD <- nonfarm_productivity_monthly_FD
temp_data$unit_labor_cost_monthly_FD <- unit_labor_cost_monthly_FD
temp_data$current_account_balance_monthly_FD <- current_account_balance_monthly_FD


#Altering data measured in billions to be measured in millions. 
temp_data$personal_income <- temp_data$personal_income*1000
temp_data$personal_income_FD <- temp_data$personal_income_FD*1000

temp_data$consumer_credit <- temp_data$consumer_credit*1000
temp_data$consumer_credit_FD <- temp_data$consumer_credit_FD*1000

#adjusting FFR to be measured in a basis point term
temp_data$FFR_FD_BP <- temp_data$FFR_FD*100

#control variable for ZIRP period index = 180:264
temp_data$ZIRP <- 0
ZIRP <- temp_data$ZIRP
ZIRP[180:264] <- 1
ZIRP
temp_data$ZIRP <- ZIRP
#dropping observations after the end of 2014 Index = 252

#adjusting values to be measured in constant 2006 dollars

temp_data$GDP_monthly <- temp_data$GDP_monthly/temp_data$consumer_price_index_rescale*100
temp_data$GDP_monthly_FD <- temp_data$GDP_monthly/temp_data$consumer_price_index_rescale*100

temp_data$personal_income <- temp_data$personal_income/temp_data$consumer_price_index_rescale*100
personal_income_FD <- diff(temp_data$personal_income,1)
personal_income_FD <-prepend(personal_income_FD, 0, before =1)
temp_data$personal_income_FD <- personal_income_FD

temp_data$business_inventories <- temp_data$business_inventories/temp_data$consumer_price_index_rescale*100
business_inventories_FD <- diff(temp_data$business_inventories,1)
business_inventories_FD <-prepend(business_inventories_FD, 0, before =1)
temp_data$business_inventories_FD <- business_inventories_FD

temp_data$monthly_budget_statement <- temp_data$monthly_budget_statement/temp_data$consumer_price_index_rescale*100
monthly_budget_statement_FD <- diff(temp_data$monthly_budget_statement,1)
monthly_budget_statement_FD <-prepend(monthly_budget_statement_FD, 0, before =1)
temp_data$monthly_budget_statement_FD <- monthly_budget_statement_FD

temp_data$durable_goods_orders <- temp_data$durable_goods_orders/temp_data$consumer_price_index_rescale*100
durable_goods_orders_FD <- diff(temp_data$durable_goods_orders,1)
durable_goods_orders_FD <-prepend(durable_goods_orders_FD, 0, before =1)
temp_data$durable_goods_orders_FD <- durable_goods_orders_FD

temp_data$wholesale_inventories <- temp_data$wholesale_inventories/temp_data$consumer_price_index_rescale*100
wholesale_inventories_FD <- diff(temp_data$wholesale_inventories,1)
wholesale_inventories_FD <-prepend(wholesale_inventories_FD, 0, before =1)
temp_data$wholesale_inventories_FD <- wholesale_inventories_FD

temp_data$average_weekly_hours_of_production <- temp_data$average_weekly_hours_of_production/temp_data$consumer_price_index_rescale*100
average_weekly_hours_of_production_FD <- diff(temp_data$average_weekly_hours_of_production,1)
average_weekly_hours_of_production_FD <-prepend(average_weekly_hours_of_production_FD, 0, before =1)
temp_data$average_weekly_hours_of_production_FD <- average_weekly_hours_of_production_FD

temp_data$GDP_monthly_FD <- temp_data$GDP_monthly/temp_data$consumer_price_index_rescale*100
temp_data$GDP_monthly_FD <- temp_data$GDP_monthly/temp_data$consumer_price_index_rescale*100
temp_data$GDP_monthly_FD <- temp_data$GDP_monthly/temp_data$consumer_price_index_rescale*100
temp_data$GDP_monthly_FD <- temp_data$GDP_monthly/temp_data$consumer_price_index_rescale*100


full_period_data <- temp_data
temp_data <- subset(temp_data, Index <= 253)
full_data <- temp_data

#normalizing the data
full_data$business_outlook_survey <- scale(full_data$business_outlook_survey)
full_data$housing_starts <- scale(full_data$housing_starts)
full_data$industrial_production_index_rescaled_FD <- scale(full_data$industrial_production_index_rescaled_FD)
full_data$consumer_confidence_index <- scale(full_data$consumer_confidence_index)
full_data$manufacturing_payrolls_FD <- scale(full_data$manufacturing_payrolls_FD)
full_data$import_price_index_rescale_FD <- scale(full_data$import_price_index_rescale_FD)
full_data$new_home_sales <- scale(full_data$new_home_sales)
full_data$non_farm_payrolls_FD <- scale(full_data$non_farm_payrolls_FD)
full_data$u_michigan_confidence_index <- scale(full_data$u_michigan_confidence_index)
full_data$capacity_utilization_FD <- scale(full_data$capacity_utilization_FD)
full_data$consumer_price_index_rescale <- scale(full_data$consumer_price_index_rescale)
full_data$consumer_price_index_rescale_FD <- scale(full_data$consumer_price_index_rescale_FD)
full_data$leading_indicators_FD <- scale(full_data$leading_indicators_FD)
full_data$average_hourly_earnings_FD <- scale(full_data$average_hourly_earnings_FD)
full_data$producer_price_index_rescale_FD <- scale(full_data$producer_price_index_rescale_FD)
full_data$average_weekly_hours_of_production_FD <- scale(full_data$average_weekly_hours_of_production_FD)
full_data$unemployment_rate <- scale(full_data$unemployment_rate)
full_data$domestic_vehicle_sales <- scale(full_data$domestic_vehicle_sales)
full_data$personal_income_FD <- scale(full_data$personal_income_FD)
full_data$business_inventories_FD <- scale(full_data$business_inventories_FD)
full_data$CPI_excluding_food_and_energy_rescale_FD <- scale(full_data$CPI_excluding_food_and_energy_rescale_FD)
full_data$factory_orders_FD <- scale(full_data$factory_orders_FD)
full_data$trade_balance_FD <- scale(full_data$trade_balance_FD)
full_data$consumer_credit_FD <- scale(full_data$consumer_credit_FD)
full_data$monthly_budget_statement <- scale(full_data$monthly_budget_statement)
full_data$durable_goods_orders_FD <- scale(full_data$durable_goods_orders_FD)
full_data$wholesale_inventories_FD <- scale(full_data$wholesale_inventories_FD)
full_data$GDP_monthly_FD <- scale(full_data$GDP_monthly_FD)
full_data$nonfarm_productivity_monthly_FD <- scale(full_data$nonfarm_productivity_monthly_FD)
full_data$unit_labor_cost_monthly_FD <- scale(full_data$unit_labor_cost_monthly_FD)
full_data$current_account_balance_monthly_FD <- scale(full_data$current_account_balance_monthly_FD)


#Note: Apply na.spline to the quarterly data in temp data, replace this data with the splined data. 
GUM_variables <- c('business_outlook_survey',
                   'housing_starts',
                   'industrial_production_index_rescaled_FD',
                   'consumer_confidence_index',
                   'manufacturing_payrolls_FD',
                   'import_price_index_rescale_FD',
                   'new_home_sales',
                   'non_farm_payrolls_FD',
                   'u_michigan_confidence_index',
                   'capacity_utilization_FD',
                   'consumer_price_index_rescale',
                   'consumer_price_index_rescale_FD',
                   'leading_indicators_FD',
                   'average_hourly_earnings_FD',
                   'producer_price_index_rescale_FD',
                   'average_weekly_hours_of_production_FD',
                   'unemployment_rate',
                   'domestic_vehicle_sales',
                   'personal_income_FD',
                   'business_inventories_FD',
                   'CPI_excluding_food_and_energy_rescale_FD',
                   'factory_orders_FD',
                   'trade_balance_FD',
                   'consumer_credit_FD',
                   'monthly_budget_statement',
                   'durable_goods_orders_FD',
                   'wholesale_inventories_FD',
                   'GDP_monthly_FD',
                   'nonfarm_productivity_monthly_FD',
                   'unit_labor_cost_monthly_FD',
                   'current_account_balance_monthly_FD',
                   'ZIRP')


GUM_data <- subset(full_data, select = GUM_variables)
GUM_data <- na.omit(GUM_data)

FFR_FD <- GUM_data$FFR_FD

#### Creating Sentiment variable lists ####
# MNB_0_1
# paragraphs
sentiment_paragraph_MNB_0_1 <- list('Sentiment_0_Paragraphs_MNB_0_1','Sentiment_1_Paragraphs_MNB_0_1','Sentiment_2_Paragraphs_MNB_0_1',
                                 'Sentiment_3_Paragraphs_MNB_0_1','Sentiment_4_Paragraphs_MNB_0_1','Sentiment_5_Paragraphs_MNB_0_1',
                                 'Sentiment_6_Paragraphs_MNB_0_1','Sentiment_7_Paragraphs_MNB_0_1')

# sentences
sentiment_sentences_MNB_0_1 <- list('Sentiment_0_Sentences_MNB_0_1','Sentiment_1_Sentences_MNB_0_1','Sentiment_2_Sentences_MNB_0_1',
                                 'Sentiment_3_Sentences_MNB_0_1','Sentiment_4_Sentences_MNB_0_1','Sentiment_5_Sentences_MNB_0_1',
                                 'Sentiment_6_Sentences_MNB_0_1','Sentiment_7_Sentences_MNB_0_1')


# MNB_0_4
# paragraphs
sentiment_paragraph_MNB_0_4 <- list('Sentiment_0_Paragraphs_MNB_0_4','Sentiment_1_Paragraphs_MNB_0_4','Sentiment_2_Paragraphs_MNB_0_4',
                                 'Sentiment_3_Paragraphs_MNB_0_4','Sentiment_4_Paragraphs_MNB_0_4','Sentiment_5_Paragraphs_MNB_0_4',
                                 'Sentiment_6_Paragraphs_MNB_0_4','Sentiment_7_Paragraphs_MNB_0_4')

# sentences
sentiment_sentences_MNB_0_4 <- list('Sentiment_0_Sentences_MNB_0_4','Sentiment_1_Sentences_MNB_0_4','Sentiment_2_Sentences_MNB_0_4',
                                 'Sentiment_3_Sentences_MNB_0_4','Sentiment_4_Sentences_MNB_0_4','Sentiment_5_Sentences_MNB_0_4',
                                 'Sentiment_6_Sentences_MNB_0_4','Sentiment_7_Sentences_MNB_0_4')


# LR_0_1
# paragraphs
sentiment_paragraph_LR_0_1 <- list('Sentiment_0_Paragraphs_LR_0_1','Sentiment_1_Paragraphs_LR_0_1','Sentiment_2_Paragraphs_LR_0_1',
                                'Sentiment_3_Paragraphs_LR_0_1','Sentiment_4_Paragraphs_LR_0_1','Sentiment_5_Paragraphs_LR_0_1',
                                'Sentiment_6_Paragraphs_LR_0_1','Sentiment_7_Paragraphs_LR_0_1')

# sentences
sentiment_sentences_LR_0_1 <- list('Sentiment_0_Sentences_LR_0_1','Sentiment_1_Sentences_LR_0_1','Sentiment_2_Sentences_LR_0_1',
                                'Sentiment_3_Sentences_LR_0_1','Sentiment_4_Sentences_LR_0_1','Sentiment_5_Sentences_LR_0_1',
                                'Sentiment_6_Sentences_LR_0_1','Sentiment_7_Sentences_LR_0_1')

# OLR_0_4
# paragraphs
sentiment_paragraph_OLR_0_4 <- list('Sentiment_0_Paragraphs_OLR_0_4','Sentiment_1_Paragraphs_OLR_0_4','Sentiment_2_Paragraphs_OLR_0_4',
                                 'Sentiment_3_Paragraphs_OLR_0_4','Sentiment_4_Paragraphs_OLR_0_4','Sentiment_5_Paragraphs_OLR_0_4',
                                 'Sentiment_6_Paragraphs_OLR_0_4','Sentiment_7_Paragraphs_OLR_0_4')

# sentences
sentiment_sentences_OLR_0_4 <- list('Sentiment_0_Sentences_OLR_0_4','Sentiment_1_Sentences_OLR_0_4','Sentiment_2_Sentences_OLR_0_4',
                                 'Sentiment_3_Sentences_OLR_0_4','Sentiment_4_Sentences_OLR_0_4','Sentiment_5_Sentences_OLR_0_4',
                                 'Sentiment_6_Sentences_OLR_0_4','Sentiment_7_Sentences_OLR_0_4')


# BERT_0_1
# paragraphs
sentiment_paragraph_BERT_0_1 <- list('Sentiment_0_Paragraphs_BERT_0_1','Sentiment_1_Paragraphs_BERT_0_1','Sentiment_2_Paragraphs_BERT_0_1',
                                  'Sentiment_3_Paragraphs_BERT_0_1','Sentiment_4_Paragraphs_BERT_0_1','Sentiment_5_Paragraphs_BERT_0_1',
                                  'Sentiment_6_Paragraphs_BERT_0_1','Sentiment_7_Paragraphs_BERT_0_1')

# sentences
sentiment_sentences_BERT_0_1 <- list('Sentiment_0_Sentences_BERT_0_1','Sentiment_1_Sentences_BERT_0_1','Sentiment_2_Sentences_BERT_0_1',
                                  'Sentiment_3_Sentences_BERT_0_1','Sentiment_4_Sentences_BERT_0_1','Sentiment_5_Sentences_BERT_0_1',
                                  'Sentiment_6_Sentences_BERT_0_1','Sentiment_7_Sentences_BERT_0_1')

# BERT_0_4
# paragraphs
sentiment_paragraph_BERT_0_4 <- list('Sentiment_0_Paragraphs_BERT_0_4','Sentiment_1_Paragraphs_BERT_0_4','Sentiment_2_Paragraphs_BERT_0_4',
                                  'Sentiment_3_Paragraphs_BERT_0_4','Sentiment_4_Paragraphs_BERT_0_4','Sentiment_5_Paragraphs_BERT_0_4',
                                  'Sentiment_6_Paragraphs_BERT_0_4','Sentiment_7_Paragraphs_BERT_0_4')

# sentences
sentiment_sentences_BERT_0_4 <- list('Sentiment_0_Sentences_BERT_0_4','Sentiment_1_Sentences_BERT_0_4','Sentiment_2_Sentences_BERT_0_4',
                                  'Sentiment_3_Sentences_BERT_0_4','Sentiment_4_Sentences_BERT_0_4','Sentiment_5_Sentences_BERT_0_4',
                                  'Sentiment_6_Sentences_BERT_0_4','Sentiment_7_Sentences_BERT_0_4')

sentiment_set <- list(sentiment_paragraph_MNB_0_1,sentiment_sentences_MNB_0_1,
                      sentiment_paragraph_MNB_0_4,sentiment_sentences_MNB_0_4,
                      sentiment_paragraph_LR_0_1,sentiment_sentences_LR_0_1,
                      sentiment_paragraph_OLR_0_4,sentiment_sentences_OLR_0_4,
                      sentiment_paragraph_BERT_0_1,sentiment_sentences_BERT_0_1,
                      sentiment_paragraph_BERT_0_4,sentiment_sentences_BERT_0_4)
                      

#result labels ----
sentiment_names <- c('complete_MSE_set_no_sentiment',
                     'complete_MSE_set_paragraph_MNB_0_1','complete_MSE_set_sentences_MNB_0_1',
                     'complete_MSE_set_paragraph_MNB_0_4','complete_MSE_set_sentences_MNB_0_4',
                     'complete_MSE_set_paragraph_LR_0_1','complete_MSE_set_sentences_LR_0_1',
                     'complete_MSE_set_paragraph_OLR_0_4','complete_MSE_set_sentences_OLR_0_4',
                     'complete_MSE_set_paragraph_BERT_0_1','complete_MSE_set_sentences_BERT_0_1',
                     'complete_MSE_set_paragraph_BERT_0_4','complete_MSE_set_sentences_BERT_0_4')

sentiment_names_paragraph <- c('complete_MSE_set_no_sentiment',
                               'complete_MSE_set_paragraph_MNB_0_1',
                               'complete_MSE_set_paragraph_MNB_0_4',
                               'complete_MSE_set_paragraph_LR_0_1',
                               'complete_MSE_set_paragraph_OLR_0_4',
                               'complete_MSE_set_paragraph_BERT_0_1',
                               'complete_MSE_set_paragraph_BERT_0_4')
sentiment_names_paragraph_0_1 <- c('complete_MSE_set_no_sentiment',
                                   'complete_MSE_set_paragraph_MNB_0_1',
                                   'complete_MSE_set_paragraph_LR_0_1',
                                   'complete_MSE_set_paragraph_BERT_0_1')
sentiment_names_paragraph_0_4 <- c('complete_MSE_set_no_sentiment',
                                   'complete_MSE_set_paragraph_MNB_0_4',
                                   'complete_MSE_set_paragraph_OLR_0_4',
                                   'complete_MSE_set_paragraph_BERT_0_4')

sentiment_names_sentences <- c('complete_MSE_set_no_sentiment',
                               'complete_MSE_set_sentences_MNB_0_1',
                               'complete_MSE_set_sentences_MNB_0_4',
                               'complete_MSE_set_sentences_LR_0_1',
                               'complete_MSE_set_sentences_OLR_0_4',
                               'complete_MSE_set_sentences_BERT_0_1',
                               'complete_MSE_set_sentences_BERT_0_4')
sentiment_names_sentences_0_1 <- c('complete_MSE_set_no_sentiment',
                                   'complete_MSE_set_sentences_MNB_0_1',
                                   'complete_MSE_set_sentences_LR_0_1',
                                   'complete_MSE_set_sentences_BERT_0_1')
sentiment_names_sentences_0_4 <- c('complete_MSE_set_no_sentiment',
                                   'complete_MSE_set_sentences_MNB_0_4',
                                   'complete_MSE_set_sentences_OLR_0_4',
                                   'complete_MSE_set_sentences_BERT_0_4')



#### GUM ####
FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_varlist <- varlist
temp_dataset <- subset(full_data, select = temp_varlist)

results_GUM <- c()
set.rseed(0)
temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
#getting R2
temp_R2 <- summary(temp_OLS)$r.squared 
results_GUM <- append(results_GUM, temp_R2, after = length(results_GUM))
#getting adj R2
temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
results_GUM <- append(results_GUM, temp_adj_R2, after = length(results_GUM))
#getting MSE
temp_MSE <- mean(temp_OLS$residuals^2)
results_GUM <- append(results_GUM, temp_MSE, after = length(results_GUM))
#for 0.8 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_80 <- mean(MSFE)
results_GUM <- append(results_GUM, MSFE_mean_80, after = length(results_GUM))
MSFE_variance_80 <- var(MSFE)
results_GUM <- append(results_GUM, MSFE_variance_80, after = length(results_GUM))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_GUM <- append(results_GUM, MAFE_mean_80, after = length(results_GUM))
MAFE_variance_80 <- var(MAFE)
results_GUM <- append(results_GUM, MAFE_variance_80, after = length(results_GUM))
#for 0.9 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_90 <- mean(MSFE)
results_GUM <- append(results_GUM, MSFE_mean_90, after = length(results_GUM))
MSFE_variance_90 <- var(MSFE)
results_GUM <- append(results_GUM, MSFE_variance_90, after = length(results_GUM))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_GUM <- append(results_GUM, MAFE_mean_90, after = length(results_GUM))
MAFE_variance_90 <- var(MAFE)
results_GUM <- append(results_GUM, MAFE_variance_90, after = length(results_GUM))

# Incorporating sentiments
for(i in 1:length(sentiment_set)){
  set.rseed(0)
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
  #getting R2
  temp_R2 <- summary(temp_OLS)$r.squared 
  results_GUM <- append(results_GUM, temp_R2, after = length(results_GUM))
  #getting adj R2
  temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
  results_GUM <- append(results_GUM, temp_adj_R2, after = length(results_GUM))
  #getting MSE
  temp_MSE <- mean(temp_OLS$residuals^2)
  results_GUM <- append(results_GUM, temp_MSE, after = length(results_GUM))
  #for 0.8 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_80 <- mean(MSFE)
  results_GUM <- append(results_GUM, MSFE_mean_80, after = length(results_GUM))
  MSFE_variance_80 <- var(MSFE)
  results_GUM <- append(results_GUM, MSFE_variance_80, after = length(results_GUM))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_GUM <- append(results_GUM, MAFE_mean_80, after = length(results_GUM))
  MAFE_variance_80 <- var(MAFE)
  results_GUM <- append(results_GUM, MAFE_variance_80, after = length(results_GUM))
  #for 0.9 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_90 <- mean(MSFE)
  results_GUM <- append(results_GUM, MSFE_mean_90, after = length(results_GUM))
  MSFE_variance_90 <- var(MSFE)
  results_GUM <- append(results_GUM, MSFE_variance_90, after = length(results_GUM))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_GUM <- append(results_GUM, MAFE_mean_90, after = length(results_GUM))
  MAFE_variance_90 <- var(MAFE)
  results_GUM <- append(results_GUM, MAFE_variance_90, after = length(results_GUM))
}

#### Stepwise regressions ####

#variables to include exogenously 
exogenous_variables <- c('ZIRP', 
                         'unemployment_rate',
                         'consumer_price_index_rescale')

#finding the stepwise variable sets

FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_varlist <- varlist
temp_dataset <- subset(full_data, select = temp_varlist)
temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
p_values <- summary(temp_OLS)$coefficients[,4]

#getting the sets of variables using backwards stepwise regressions
stepwise_10_variables <- c()
stepwise_5_variables <- c()
stepwise_1_variables <- c()
temp_varlist <- varlist
for(i in 1:30){
  temp_dataset <- subset(full_data, select = temp_varlist)
  temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
  p_values <- summary(temp_OLS)$coefficients[,4]
  #drops intercept from consideration
  p_values <- p_values[-1]
  p_values <- p_values[p_values < max(p_values)]
  temp_varlist <- names(p_values)
  if(max(p_values)>= 0.1){
    stepwise_10_variables <- temp_varlist
  }
  if(max(p_values)>=0.05){
    stepwise_5_variables <- temp_varlist
  }
  if(max(p_values)>=0.01){
    stepwise_1_variables <- temp_varlist
  }
}

stepwise_10_variables <- stepwise_10_variables[!stepwise_10_variables %in% exogenous_variables]
stepwise_10_variables <- append(stepwise_10_variables, exogenous_variables, after = length(stepwise_10_variables))

stepwise_5_variables <- stepwise_5_variables[!stepwise_5_variables %in% exogenous_variables]
stepwise_5_variables <- append(stepwise_5_variables, exogenous_variables, after = length(stepwise_5_variables))

stepwise_1_variables <- stepwise_1_variables[!stepwise_1_variables %in% exogenous_variables]
stepwise_1_variables <- append(stepwise_1_variables, exogenous_variables, after = length(stepwise_1_variables))


#variables at 10% level

# values at the 0.10 level
temp_dataset <- subset(full_data, select = stepwise_10_variables)
varlist <- stepwise_10_variables

results_stepwise_10 <- c()
set.rseed(0)
temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
#getting R2
temp_R2 <- summary(temp_OLS)$r.squared 
results_stepwise_10 <- append(results_stepwise_10, temp_R2, after = length(results_stepwise_10))
#getting adj R2
temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
results_stepwise_10 <- append(results_stepwise_10, temp_adj_R2, after = length(results_stepwise_10))
#getting MSE
temp_MSE <- mean(temp_OLS$residuals^2)
results_stepwise_10 <- append(results_stepwise_10, temp_MSE, after = length(results_stepwise_10))
#for 0.8 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_80 <- mean(MSFE)
results_stepwise_10 <- append(results_stepwise_10, MSFE_mean_80, after = length(results_stepwise_10))
MSFE_variance_80 <- var(MSFE)
results_stepwise_10 <- append(results_stepwise_10, MSFE_variance_80, after = length(results_stepwise_10))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_stepwise_10 <- append(results_stepwise_10, MAFE_mean_80, after = length(results_stepwise_10))
MAFE_variance_80 <- var(MAFE)
results_stepwise_10 <- append(results_stepwise_10, MAFE_variance_80, after = length(results_stepwise_10))
#for 0.9 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_90 <- mean(MSFE)
results_stepwise_10 <- append(results_stepwise_10, MSFE_mean_90, after = length(results_stepwise_10))
MSFE_variance_90 <- var(MSFE)
results_stepwise_10 <- append(results_stepwise_10, MSFE_variance_90, after = length(results_stepwise_10))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_stepwise_10 <- append(results_stepwise_10, MAFE_mean_90, after = length(results_stepwise_10))
MAFE_variance_90 <- var(MAFE)
results_stepwise_10 <- append(results_stepwise_10, MAFE_variance_90, after = length(results_stepwise_10))

# Incorporating sentiments
for(i in 1:length(sentiment_set)){
  set.rseed(0)
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
  #getting R2
  temp_R2 <- summary(temp_OLS)$r.squared 
  results_stepwise_10 <- append(results_stepwise_10, temp_R2, after = length(results_stepwise_10))
  #getting adj R2
  temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
  results_stepwise_10 <- append(results_stepwise_10, temp_adj_R2, after = length(results_stepwise_10))
  #getting MSE
  temp_MSE <- mean(temp_OLS$residuals^2)
  results_stepwise_10 <- append(results_stepwise_10, temp_MSE, after = length(results_stepwise_10))
  #for 0.8 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_80 <- mean(MSFE)
  results_stepwise_10 <- append(results_stepwise_10, MSFE_mean_80, after = length(results_stepwise_10))
  MSFE_variance_80 <- var(MSFE)
  results_stepwise_10 <- append(results_stepwise_10, MSFE_variance_80, after = length(results_stepwise_10))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_stepwise_10 <- append(results_stepwise_10, MAFE_mean_80, after = length(results_stepwise_10))
  MAFE_variance_80 <- var(MAFE)
  results_stepwise_10 <- append(results_stepwise_10, MAFE_variance_80, after = length(results_stepwise_10))
  #for 0.9 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_90 <- mean(MSFE)
  results_stepwise_10 <- append(results_stepwise_10, MSFE_mean_90, after = length(results_stepwise_10))
  MSFE_variance_90 <- var(MSFE)
  results_stepwise_10 <- append(results_stepwise_10, MSFE_variance_90, after = length(results_stepwise_10))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_stepwise_10 <- append(results_stepwise_10, MAFE_mean_90, after = length(results_stepwise_10))
  MAFE_variance_90 <- var(MAFE)
  results_stepwise_10 <- append(results_stepwise_10, MAFE_variance_90, after = length(results_stepwise_10))
}

# values at 0.05 level
temp_dataset <- subset(full_data, select = stepwise_5_variables)
varlist <- stepwise_5_variables

results_stepwise_05 <- c()
set.rseed(0)
temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
#getting R2
temp_R2 <- summary(temp_OLS)$r.squared 
results_stepwise_05 <- append(results_stepwise_05, temp_R2, after = length(results_stepwise_05))
#getting adj R2
temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
results_stepwise_05 <- append(results_stepwise_05, temp_adj_R2, after = length(results_stepwise_05))
#getting MSE
temp_MSE <- mean(temp_OLS$residuals^2)
results_stepwise_05 <- append(results_stepwise_05, temp_MSE, after = length(results_stepwise_05))
#for 0.8 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_80 <- mean(MSFE)
results_stepwise_05 <- append(results_stepwise_05, MSFE_mean_80, after = length(results_stepwise_05))
MSFE_variance_80 <- var(MSFE)
results_stepwise_05 <- append(results_stepwise_05, MSFE_variance_80, after = length(results_stepwise_05))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_stepwise_05 <- append(results_stepwise_05, MAFE_mean_80, after = length(results_stepwise_05))
MAFE_variance_80 <- var(MAFE)
results_stepwise_05 <- append(results_stepwise_05, MAFE_variance_80, after = length(results_stepwise_05))
#for 0.9 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_90 <- mean(MSFE)
results_stepwise_05 <- append(results_stepwise_05, MSFE_mean_90, after = length(results_stepwise_05))
MSFE_variance_90 <- var(MSFE)
results_stepwise_05 <- append(results_stepwise_05, MSFE_variance_90, after = length(results_stepwise_05))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_stepwise_05 <- append(results_stepwise_05, MAFE_mean_90, after = length(results_stepwise_05))
MAFE_variance_90 <- var(MAFE)
results_stepwise_05 <- append(results_stepwise_05, MAFE_variance_90, after = length(results_stepwise_05))

# Incorporating sentiments
for(i in 1:length(sentiment_set)){
  set.rseed(0)
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
  #getting R2
  temp_R2 <- summary(temp_OLS)$r.squared 
  results_stepwise_05 <- append(results_stepwise_05, temp_R2, after = length(results_stepwise_05))
  #getting adj R2
  temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
  results_stepwise_05 <- append(results_stepwise_05, temp_adj_R2, after = length(results_stepwise_05))
  #getting MSE
  temp_MSE <- mean(temp_OLS$residuals^2)
  results_stepwise_05 <- append(results_stepwise_05, temp_MSE, after = length(results_stepwise_05))
  #for 0.8 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_80 <- mean(MSFE)
  results_stepwise_05 <- append(results_stepwise_05, MSFE_mean_80, after = length(results_stepwise_05))
  MSFE_variance_80 <- var(MSFE)
  results_stepwise_05 <- append(results_stepwise_05, MSFE_variance_80, after = length(results_stepwise_05))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_stepwise_05 <- append(results_stepwise_05, MAFE_mean_80, after = length(results_stepwise_05))
  MAFE_variance_80 <- var(MAFE)
  results_stepwise_05 <- append(results_stepwise_05, MAFE_variance_80, after = length(results_stepwise_05))
  #for 0.9 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_90 <- mean(MSFE)
  results_stepwise_05 <- append(results_stepwise_05, MSFE_mean_90, after = length(results_stepwise_05))
  MSFE_variance_90 <- var(MSFE)
  results_stepwise_05 <- append(results_stepwise_05, MSFE_variance_90, after = length(results_stepwise_05))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_stepwise_05 <- append(results_stepwise_05, MAFE_mean_90, after = length(results_stepwise_05))
  MAFE_variance_90 <- var(MAFE)
  results_stepwise_05 <- append(results_stepwise_05, MAFE_variance_90, after = length(results_stepwise_05))
}

# values at the 0.01 level
temp_dataset <- subset(full_data, select = stepwise_1_variables)
varlist <- stepwise_1_variables

results_stepwise_01 <- c()
set.rseed(0)
temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
#getting R2
temp_R2 <- summary(temp_OLS)$r.squared 
results_stepwise_01 <- append(results_stepwise_01, temp_R2, after = length(results_stepwise_01))
#getting adj R2
temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
results_stepwise_01 <- append(results_stepwise_01, temp_adj_R2, after = length(results_stepwise_01))
#getting MSE
temp_MSE <- mean(temp_OLS$residuals^2)
results_stepwise_01 <- append(results_stepwise_01, temp_MSE, after = length(results_stepwise_01))
#for 0.8 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_80 <- mean(MSFE)
results_stepwise_01 <- append(results_stepwise_01, MSFE_mean_80, after = length(results_stepwise_01))
MSFE_variance_80 <- var(MSFE)
results_stepwise_01 <- append(results_stepwise_01, MSFE_variance_80, after = length(results_stepwise_01))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_stepwise_01 <- append(results_stepwise_01, MAFE_mean_80, after = length(results_stepwise_01))
MAFE_variance_80 <- var(MAFE)
results_stepwise_01 <- append(results_stepwise_01, MAFE_variance_80, after = length(results_stepwise_01))
#for 0.9 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_90 <- mean(MSFE)
results_stepwise_01 <- append(results_stepwise_01, MSFE_mean_90, after = length(results_stepwise_01))
MSFE_variance_90 <- var(MSFE)
results_stepwise_01 <- append(results_stepwise_01, MSFE_variance_90, after = length(results_stepwise_01))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_stepwise_01 <- append(results_stepwise_01, MAFE_mean_90, after = length(results_stepwise_01))
MAFE_variance_90 <- var(MAFE)
results_stepwise_01 <- append(results_stepwise_01, MAFE_variance_90, after = length(results_stepwise_01))

# Incorporating sentiments
for(i in 1:length(sentiment_set)){
  set.rseed(0)
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
  #getting R2
  temp_R2 <- summary(temp_OLS)$r.squared 
  results_stepwise_01 <- append(results_stepwise_01, temp_R2, after = length(results_stepwise_01))
  #getting adj R2
  temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
  results_stepwise_01 <- append(results_stepwise_01, temp_adj_R2, after = length(results_stepwise_01))
  #getting MSE
  temp_MSE <- mean(temp_OLS$residuals^2)
  results_stepwise_01 <- append(results_stepwise_01, temp_MSE, after = length(results_stepwise_01))
  #for 0.8 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_80 <- mean(MSFE)
  results_stepwise_01 <- append(results_stepwise_01, MSFE_mean_80, after = length(results_stepwise_01))
  MSFE_variance_80 <- var(MSFE)
  results_stepwise_01 <- append(results_stepwise_01, MSFE_variance_80, after = length(results_stepwise_01))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_stepwise_01 <- append(results_stepwise_01, MAFE_mean_80, after = length(results_stepwise_01))
  MAFE_variance_80 <- var(MAFE)
  results_stepwise_01 <- append(results_stepwise_01, MAFE_variance_80, after = length(results_stepwise_01))
  #for 0.9 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_90 <- mean(MSFE)
  results_stepwise_01 <- append(results_stepwise_01, MSFE_mean_90, after = length(results_stepwise_01))
  MSFE_variance_90 <- var(MSFE)
  results_stepwise_01 <- append(results_stepwise_01, MSFE_variance_90, after = length(results_stepwise_01))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_stepwise_01 <- append(results_stepwise_01, MAFE_mean_90, after = length(results_stepwise_01))
  MAFE_variance_90 <- var(MAFE)
  results_stepwise_01 <- append(results_stepwise_01, MAFE_variance_90, after = length(results_stepwise_01))
}
#### AIC ####

#Getting the AIC model specification
set.rseed(0)
FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_varlist <- varlist
temp_dataset <- subset(full_data, select = varlist)
temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
AIC <- stepAIC(temp_OLS, direction = "both", trace = FALSE)

#getting model specification from AIC
temp_varlist <- colnames(AIC$model)
varlist <- temp_varlist
temp_dataset <- subset(full_data, select = varlist)
#running the model with the AIC specification 

results_AIC <- c()
set.rseed(0)
temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
#getting R2
temp_R2 <- summary(temp_OLS)$r.squared 
results_AIC <- append(results_AIC, temp_R2, after = length(results_AIC))
#getting adj R2
temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
results_AIC <- append(results_AIC, temp_adj_R2, after = length(results_AIC))
#getting MSE
temp_MSE <- mean(temp_OLS$residuals^2)
results_AIC <- append(results_AIC, temp_MSE, after = length(results_AIC))
#for 0.8 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_80 <- mean(MSFE)
results_AIC <- append(results_AIC, MSFE_mean_80, after = length(results_AIC))
MSFE_variance_80 <- var(MSFE)
results_AIC <- append(results_AIC, MSFE_variance_80, after = length(results_AIC))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_AIC <- append(results_AIC, MAFE_mean_80, after = length(results_AIC))
MAFE_variance_80 <- var(MAFE)
results_AIC <- append(results_AIC, MAFE_variance_80, after = length(results_AIC))
#for 0.9 train:test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_90 <- mean(MSFE)
results_AIC <- append(results_AIC, MSFE_mean_90, after = length(results_AIC))
MSFE_variance_90 <- var(MSFE)
results_AIC <- append(results_AIC, MSFE_variance_90, after = length(results_AIC))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_AIC <- append(results_AIC, MAFE_mean_90, after = length(results_AIC))
MAFE_variance_90 <- var(MAFE)
results_AIC <- append(results_AIC, MAFE_variance_90, after = length(results_AIC))

# Incorporating sentiments
for(i in 1:length(sentiment_set)){
  set.rseed(0)
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  temp_OLS <- lm(FFR_FD ~., data = temp_dataset)
  #getting R2
  temp_R2 <- summary(temp_OLS)$r.squared 
  results_AIC <- append(results_AIC, temp_R2, after = length(results_AIC))
  #getting adj R2
  temp_adj_R2 <-summary(temp_OLS)$adj.r.squared
  results_AIC <- append(results_AIC, temp_adj_R2, after = length(results_AIC))
  #getting MSE
  temp_MSE <- mean(temp_OLS$residuals^2)
  results_AIC <- append(results_AIC, temp_MSE, after = length(results_AIC))
  #for 0.8 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_80 <- mean(MSFE)
  results_AIC <- append(results_AIC, MSFE_mean_80, after = length(results_AIC))
  MSFE_variance_80 <- var(MSFE)
  results_AIC <- append(results_AIC, MSFE_variance_80, after = length(results_AIC))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_AIC <- append(results_AIC, MAFE_mean_80, after = length(results_AIC))
  MAFE_variance_80 <- var(MAFE)
  results_AIC <- append(results_AIC, MAFE_variance_80, after = length(results_AIC))
  #for 0.9 train:test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- lm(FFR_FD_train ~., data = temp_dataset_train)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_90 <- mean(MSFE)
  results_AIC <- append(results_AIC, MSFE_mean_90, after = length(results_AIC))
  MSFE_variance_90 <- var(MSFE)
  results_AIC <- append(results_AIC, MSFE_variance_90, after = length(results_AIC))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_AIC <- append(results_AIC, MAFE_mean_90, after = length(results_AIC))
  MAFE_variance_90 <- var(MAFE)
  results_AIC <- append(results_AIC, MAFE_variance_90, after = length(results_AIC))
}

#### Random Forest ####
# only for MSFE and MAFE
# Mean of the results
#mean squared forecasting error 
set.rseed(0)
FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_varlist <- varlist
temp_dataset <- subset(full_data, select = varlist)


#used to fill the results for r2, adj r2, and MSE, which can't be generated by RF
NA_vector <- c(NA,NA,NA)
results_RF_10 <- c()
results_RF_10 <- append(results_RF_10, NA_vector, after = length(results_RF_10))
#0.9 train test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 10, importance = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
MSFE_mean_80 <- mean(MSFE)
results_RF_10 <- append(results_RF_10, MSFE_mean_80, after = length(results_RF_10))
MSFE_variance_80 <- var(MSFE)
results_RF_10 <- append(results_RF_10, MSFE_variance_80, after = length(results_RF_10))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_RF_10 <- append(results_RF_10, MAFE_mean_80, after = length(results_RF_10))
MAFE_variance_80 <- var(MAFE)
results_RF_10 <- append(results_RF_10, MAFE_variance_80, after = length(results_RF_10))
#0.8 train test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 10, importance = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
MSFE_mean_90 <- mean(MSFE)
results_RF_10 <- append(results_RF_10, MSFE_mean_90, after = length(results_RF_10))
MSFE_variance_90 <- var(MSFE)
results_RF_10 <- append(results_RF_10, MSFE_variance_90, after = length(results_RF_10))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_RF_10 <- append(results_RF_10, MAFE_mean_90, after = length(results_RF_10))
MAFE_variance_90 <- var(MAFE)
results_RF_10 <- append(results_RF_10, MAFE_variance_90, after = length(results_RF_10))

results_RF_10

for(i in 1:length(sentiment_set)){
  set.rseed(0)
  results_RF_10 <- append(results_RF_10, NA_vector, after = length(results_RF_10))
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  NA_vector <- c(NA,NA,NA)
  #0.9 train test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 10, importance = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  MSFE_mean_80 <- mean(MSFE)
  results_RF_10 <- append(results_RF_10, MSFE_mean_80, after = length(results_RF_10))
  MSFE_variance_80 <- var(MSFE)
  results_RF_10 <- append(results_RF_10, MSFE_variance_80, after = length(results_RF_10))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_RF_10 <- append(results_RF_10, MAFE_mean_80, after = length(results_RF_10))
  MAFE_variance_80 <- var(MAFE)
  results_RF_10 <- append(results_RF_10, MAFE_variance_80, after = length(results_RF_10))
  #0.9 train test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 10, importance = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  MSFE_mean_90 <- mean(MSFE)
  results_RF_10 <- append(results_RF_10, MSFE_mean_90, after = length(results_RF_10))
  MSFE_variance_90 <- var(MSFE)
  results_RF_10 <- append(results_RF_10, MSFE_variance_90, after = length(results_RF_10))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_RF_10 <- append(results_RF_10, MAFE_mean_90, after = length(results_RF_10))
  MAFE_variance_90 <- var(MAFE)
  results_RF_10 <- append(results_RF_10, MAFE_variance_90, after = length(results_RF_10))
}

# for mtry = 15
#used to fill the results for r2, adj r2, and MSE, which can't be generated by RF
set.rseed(0)
FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_varlist <- varlist
temp_dataset <- subset(full_data, select = varlist)
NA_vector <- c(NA,NA,NA)
results_RF_15 <- c()
results_RF_15 <- append(results_RF_15, NA_vector, after = length(results_RF_15))
#0.8 train test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 15, importance = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
MSFE_mean_80 <- mean(MSFE)
results_RF_15 <- append(results_RF_15, MSFE_mean_80, after = length(results_RF_15))
MSFE_variance_80 <- var(MSFE)
results_RF_15 <- append(results_RF_15, MSFE_variance_80, after = length(results_RF_15))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_RF_15 <- append(results_RF_15, MAFE_mean_80, after = length(results_RF_15))
MAFE_variance_80 <- var(MAFE)
results_RF_15 <- append(results_RF_15, MAFE_variance_80, after = length(results_RF_15))
#0.9 train test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 15, importance = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
MSFE_mean_90 <- mean(MSFE)
results_RF_15 <- append(results_RF_15, MSFE_mean_90, after = length(results_RF_15))
MSFE_variance_90 <- var(MSFE)
results_RF_15 <- append(results_RF_15, MSFE_variance_90, after = length(results_RF_15))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_RF_15 <- append(results_RF_15, MAFE_mean_90, after = length(results_RF_15))
MAFE_variance_90 <- var(MAFE)
results_RF_15 <- append(results_RF_15, MAFE_variance_90, after = length(results_RF_15))

results_RF_15

for(i in 1:length(sentiment_set)){
  set.rseed(0)
  results_RF_15 <- append(results_RF_15, NA_vector, after = length(results_RF_15))
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  NA_vector <- c(NA,NA,NA)
  #0.9 train test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 15, importance = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  MSFE_mean_80 <- mean(MSFE)
  results_RF_15 <- append(results_RF_15, MSFE_mean_80, after = length(results_RF_15))
  MSFE_variance_80 <- var(MSFE)
  results_RF_15 <- append(results_RF_15, MSFE_variance_80, after = length(results_RF_15))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_RF_15 <- append(results_RF_15, MAFE_mean_80, after = length(results_RF_15))
  MAFE_variance_80 <- var(MAFE)
  results_RF_15 <- append(results_RF_15, MAFE_variance_80, after = length(results_RF_15))
  #0.8 train test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 15, importance = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  MSFE_mean_90 <- mean(MSFE)
  results_RF_15 <- append(results_RF_15, MSFE_mean_90, after = length(results_RF_15))
  MSFE_variance_90 <- var(MSFE)
  results_RF_15 <- append(results_RF_15, MSFE_variance_90, after = length(results_RF_15))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_RF_15 <- append(results_RF_15, MAFE_mean_90, after = length(results_RF_15))
  MAFE_variance_90 <- var(MAFE)
  results_RF_15 <- append(results_RF_15, MAFE_variance_90, after = length(results_RF_15))
}

# for mtry = 6
#used to fill the results for r2, adj r2, and MSE, which can't be generated by RF
set.rseed(0)
FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_varlist <- varlist
temp_dataset <- subset(full_data, select = varlist)
NA_vector <- c(NA,NA,NA)
results_RF_6 <- c()
results_RF_6 <- append(results_RF_6, NA_vector, after = length(results_RF_6))
#0.8 train test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 6, importance = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
MSFE_mean_80 <- mean(MSFE)
results_RF_6 <- append(results_RF_6, MSFE_mean_80, after = length(results_RF_6))
MSFE_variance_80 <- var(MSFE)
results_RF_6 <- append(results_RF_6, MSFE_variance_80, after = length(results_RF_6))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_RF_6 <- append(results_RF_6, MAFE_mean_80, after = length(results_RF_6))
MAFE_variance_80 <- var(MAFE)
results_RF_6 <- append(results_RF_6, MAFE_variance_80, after = length(results_RF_6))
#0.9 train test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 6, importance = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
MSFE_mean_90 <- mean(MSFE)
results_RF_6 <- append(results_RF_6, MSFE_mean_90, after = length(results_RF_6))
MSFE_variance_90 <- var(MSFE)
results_RF_6 <- append(results_RF_6, MSFE_variance_90, after = length(results_RF_6))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_RF_6 <- append(results_RF_6, MAFE_mean_90, after = length(results_RF_6))
MAFE_variance_90 <- var(MAFE)
results_RF_6 <- append(results_RF_6, MAFE_variance_90, after = length(results_RF_6))

results_RF_6

for(i in 1:length(sentiment_set)){
  set.rseed(0)
  results_RF_6 <- append(results_RF_6, NA_vector, after = length(results_RF_6))
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  NA_vector <- c(NA,NA,NA)
  #0.9 train test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 6, importance = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  MSFE_mean_80 <- mean(MSFE)
  results_RF_6 <- append(results_RF_6, MSFE_mean_80, after = length(results_RF_6))
  MSFE_variance_80 <- var(MSFE)
  results_RF_6 <- append(results_RF_6, MSFE_variance_80, after = length(results_RF_6))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_RF_6 <- append(results_RF_6, MAFE_mean_80, after = length(results_RF_6))
  MAFE_variance_80 <- var(MAFE)
  results_RF_6 <- append(results_RF_6, MAFE_variance_80, after = length(results_RF_6))
  #0.8 train test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 6, importance = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  MSFE_mean_90 <- mean(MSFE)
  results_RF_6 <- append(results_RF_6, MSFE_mean_90, after = length(results_RF_6))
  MSFE_variance_90 <- var(MSFE)
  results_RF_6 <- append(results_RF_6, MSFE_variance_90, after = length(results_RF_6))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_RF_6 <- append(results_RF_6, MAFE_mean_90, after = length(results_RF_6))
  MAFE_variance_90 <- var(MAFE)
  results_RF_6 <- append(results_RF_6, MAFE_variance_90, after = length(results_RF_6))
}

#### Random Bagging #### 

set.rseed(0)
FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_varlist <- varlist
temp_dataset <- subset(full_data, select = varlist)
NA_vector <- c(NA,NA,NA)
results_Bagging <- c()
results_Bagging <- append(results_Bagging, NA_vector, after = length(results_Bagging))
#0.8 train test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 32, importance = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
MSFE_mean_80 <- mean(MSFE)
results_Bagging <- append(results_Bagging, MSFE_mean_80, after = length(results_Bagging))
MSFE_variance_80 <- var(MSFE)
results_Bagging <- append(results_Bagging, MSFE_variance_80, after = length(results_Bagging))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_Bagging <- append(results_Bagging, MAFE_mean_80, after = length(results_Bagging))
MAFE_variance_80 <- var(MAFE)
results_Bagging <- append(results_Bagging, MAFE_variance_80, after = length(results_Bagging))
#0.9 train test split
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  #getting MSFE and MAFE
  temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 32, importance = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
}
MSFE_mean_90 <- mean(MSFE)
results_Bagging <- append(results_Bagging, MSFE_mean_90, after = length(results_Bagging))
MSFE_variance_90 <- var(MSFE)
results_Bagging <- append(results_Bagging, MSFE_variance_90, after = length(results_Bagging))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_Bagging <- append(results_Bagging, MAFE_mean_90, after = length(results_Bagging))
MAFE_variance_90 <- var(MAFE)
results_Bagging <- append(results_Bagging, MAFE_variance_90, after = length(results_Bagging))

results_Bagging

for(i in 1:length(sentiment_set)){
  set.rseed(0)
  results_Bagging <- append(results_Bagging, NA_vector, after = length(results_Bagging))
  temp_varlist <- varlist
  temp_sentiment <- unlist(sentiment_set[i])
  temp_varlist <- append(temp_varlist, temp_sentiment, after = length(temp_varlist))
  temp_dataset <- subset(full_data, select = temp_varlist)
  NA_vector <- c(NA,NA,NA)
  #0.9 train test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 40, importance = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  MSFE_mean_80 <- mean(MSFE)
  results_Bagging <- append(results_Bagging, MSFE_mean_80, after = length(results_Bagging))
  MSFE_variance_80 <- var(MSFE)
  results_Bagging <- append(results_Bagging, MSFE_variance_80, after = length(results_Bagging))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_Bagging <- append(results_Bagging, MAFE_mean_80, after = length(results_Bagging))
  MAFE_variance_80 <- var(MAFE)
  results_Bagging <- append(results_Bagging, MAFE_variance_80, after = length(results_Bagging))
  #0.8 train test split
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    #getting MSFE and MAFE
    temp_train <- randomForest(FFR_FD_train ~ ., data = temp_dataset_train, ntree = 1000, mtry = 40, importance = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  }
  MSFE_mean_90 <- mean(MSFE)
  results_Bagging <- append(results_Bagging, MSFE_mean_90, after = length(results_Bagging))
  MSFE_variance_90 <- var(MSFE)
  results_Bagging <- append(results_Bagging, MSFE_variance_90, after = length(results_Bagging))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_Bagging <- append(results_Bagging, MAFE_mean_90, after = length(results_Bagging))
  MAFE_variance_90 <- var(MAFE)
  results_Bagging <- append(results_Bagging, MAFE_variance_90, after = length(results_Bagging))
}



#### SVM ####
#### radial ####
results_SVM_radial <- c()

FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_dataset <- subset(full_data, select = varlist)

temp_dataset <- data.matrix(temp_dataset, rownames.force = NA)

temp_SVM <- svm(temp_dataset, y = FFR_FD, scale = TRUE, type = NULL, 
                kernel = "radial", degree = 3, 
                coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                probability = FALSE, fitted = TRUE)



NA_vector <- c(NA,NA)
results_SVM_radial <- append(results_SVM_radial, NA_vector, after = length(results_SVM_radial))
#getting MSE
temp_MSE <- mean(temp_SVM$residuals^2)
results_SVM_radial <- append(results_SVM_radial, temp_MSE, after = length(results_SVM_radial))
#for 0.8 train:test split
set.rseed(0)
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  temp_dataset <- subset(full_data, select = varlist)
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  temp_dataset_train <- data.matrix(temp_dataset_train)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  temp_dataset_test <- data.matrix(temp_dataset_test)
  #getting MSFE and MAFE
  temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                    kernel = "radial", degree = 3, 
                    coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                    tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                    probability = FALSE, fitted = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_80 <- mean(MSFE)
results_SVM_radial <- append(results_SVM_radial, MSFE_mean_80, after = length(results_SVM_radial))
MSFE_variance_80 <- var(MSFE)
results_SVM_radial <- append(results_SVM_radial, MSFE_variance_80, after = length(results_SVM_radial))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_SVM_radial <- append(results_SVM_radial, MAFE_mean_80, after = length(results_SVM_radial))
MAFE_variance_80 <- var(MAFE)
results_SVM_radial <- append(results_SVM_radial, MAFE_variance_80, after = length(results_SVM_radial))

for(j in 1:99){
  temp_dataset <- subset(full_data, select = varlist)
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  temp_dataset_train <- data.matrix(temp_dataset_train)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  temp_dataset_test <- data.matrix(temp_dataset_test)
  #getting MSFE and MAFE
  temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                    kernel = "radial", degree = 3, 
                    coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                    tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                    probability = FALSE, fitted = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_90 <- mean(MSFE)
results_SVM_radial <- append(results_SVM_radial, MSFE_mean_90, after = length(results_SVM_radial))
MSFE_variance_90 <- var(MSFE)
results_SVM_radial <- append(results_SVM_radial, MSFE_variance_90, after = length(results_SVM_radial))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_SVM_radial <- append(results_SVM_radial, MAFE_mean_90, after = length(results_SVM_radial))
MAFE_variance_90 <- var(MAFE)
results_SVM_radial <- append(results_SVM_radial, MAFE_variance_90, after = length(results_SVM_radial))


for(i in 1:length(sentiment_set)){
  NA_vector <- c(NA,NA)
  set.rseed(0)
  varlist <- GUM_variables
  temp_sentiment <- unlist(sentiment_set[i])
  varlist <- append(varlist, temp_sentiment,after = length(varlist))
  temp_dataset <- subset(full_data, select = varlist)
  
  temp_dataset <- data.matrix(temp_dataset, rownames.force = NA)
  
  temp_SVM <- svm(temp_dataset, y = FFR_FD, scale = TRUE, type = NULL, 
                  kernel = "radial", degree = 3, 
                  coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                  tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                  probability = FALSE, fitted = TRUE)
  
  
  
  
  results_SVM_radial <- append(results_SVM_radial, NA_vector, after = length(results_SVM_radial))
  #getting MSE
  temp_MSE <- mean(temp_SVM$residuals^2)
  results_SVM_radial <- append(results_SVM_radial, temp_MSE, after = length(results_SVM_radial))
  #for 0.8 train:test split
  set.rseed(0)
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    temp_dataset <- subset(full_data, select = varlist)
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    temp_dataset_train <- data.matrix(temp_dataset_train)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    temp_dataset_test <- data.matrix(temp_dataset_test)
    #getting MSFE and MAFE
    temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                      kernel = "radial", degree = 3, 
                      coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                      tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                      probability = FALSE, fitted = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
    
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_80 <- mean(MSFE)
  results_SVM_radial <- append(results_SVM_radial, MSFE_mean_80, after = length(results_SVM_radial))
  MSFE_variance_80 <- var(MSFE)
  results_SVM_radial <- append(results_SVM_radial, MSFE_variance_80, after = length(results_SVM_radial))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_SVM_radial <- append(results_SVM_radial, MAFE_mean_80, after = length(results_SVM_radial))
  MAFE_variance_80 <- var(MAFE)
  results_SVM_radial <- append(results_SVM_radial, MAFE_variance_80, after = length(results_SVM_radial))
  
  for(j in 1:99){
    temp_dataset <- subset(full_data, select = varlist)
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    temp_dataset_train <- data.matrix(temp_dataset_train)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    temp_dataset_test <- data.matrix(temp_dataset_test)
    #getting MSFE and MAFE
    temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                      kernel = "radial", degree = 3, 
                      coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                      tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                      probability = FALSE, fitted = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
    
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_90 <- mean(MSFE)
  results_SVM_radial <- append(results_SVM_radial, MSFE_mean_90, after = length(results_SVM_radial))
  MSFE_variance_90 <- var(MSFE)
  results_SVM_radial <- append(results_SVM_radial, MSFE_variance_90, after = length(results_SVM_radial))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_SVM_radial <- append(results_SVM_radial, MAFE_mean_90, after = length(results_SVM_radial))
  MAFE_variance_90 <- var(MAFE)
  results_SVM_radial <- append(results_SVM_radial, MAFE_variance_90, after = length(results_SVM_radial))
}

#### sigmoid ####
results_SVM_sigmoid <- c()

FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_dataset <- subset(full_data, select = varlist)

temp_dataset <- data.matrix(temp_dataset, rownames.force = NA)

temp_SVM <- svm(temp_dataset, y = FFR_FD, scale = TRUE, type = NULL, 
                kernel = "sigmoid", degree = 3,
                coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                probability = FALSE, fitted = TRUE)



NA_vector <- c(NA,NA)
results_SVM_sigmoid <- append(results_SVM_sigmoid, NA_vector, after = length(results_SVM_sigmoid))
#getting MSE
temp_MSE <- mean(temp_SVM$residuals^2)
results_SVM_sigmoid <- append(results_SVM_sigmoid, temp_MSE, after = length(results_SVM_sigmoid))
#for 0.8 train:test split
set.rseed(0)
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  temp_dataset <- subset(full_data, select = varlist)
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  temp_dataset_train <- data.matrix(temp_dataset_train)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  temp_dataset_test <- data.matrix(temp_dataset_test)
  #getting MSFE and MAFE
  temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                    kernel = "sigmoid", degree = 3, 
                    coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                    tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                    probability = FALSE, fitted = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_80 <- mean(MSFE)
results_SVM_sigmoid <- append(results_SVM_sigmoid, MSFE_mean_80, after = length(results_SVM_sigmoid))
MSFE_variance_80 <- var(MSFE)
results_SVM_sigmoid <- append(results_SVM_sigmoid, MSFE_variance_80, after = length(results_SVM_sigmoid))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_SVM_sigmoid <- append(results_SVM_sigmoid, MAFE_mean_80, after = length(results_SVM_sigmoid))
MAFE_variance_80 <- var(MAFE)
results_SVM_sigmoid <- append(results_SVM_sigmoid, MAFE_variance_80, after = length(results_SVM_sigmoid))

for(j in 1:99){
  temp_dataset <- subset(full_data, select = varlist)
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  temp_dataset_train <- data.matrix(temp_dataset_train)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  temp_dataset_test <- data.matrix(temp_dataset_test)
  #getting MSFE and MAFE
  temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                    kernel = "sigmoid", degree = 3, 
                    coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                    tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                    probability = FALSE, fitted = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_90 <- mean(MSFE)
results_SVM_sigmoid <- append(results_SVM_sigmoid, MSFE_mean_90, after = length(results_SVM_sigmoid))
MSFE_variance_90 <- var(MSFE)
results_SVM_sigmoid <- append(results_SVM_sigmoid, MSFE_variance_90, after = length(results_SVM_sigmoid))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_SVM_sigmoid <- append(results_SVM_sigmoid, MAFE_mean_90, after = length(results_SVM_sigmoid))
MAFE_variance_90 <- var(MAFE)
results_SVM_sigmoid <- append(results_SVM_sigmoid, MAFE_variance_90, after = length(results_SVM_sigmoid))


for(i in 1:length(sentiment_set)){
  set.rseed(0)
  varlist <- GUM_variables
  temp_sentiment <- unlist(sentiment_set[i])
  varlist <- append(varlist, temp_sentiment,after = length(varlist))
  temp_dataset <- subset(full_data, select = varlist)
  
  temp_dataset <- data.matrix(temp_dataset, rownames.force = NA)
  
  temp_SVM <- svm(temp_dataset, y = FFR_FD, scale = TRUE, type = NULL, 
                  kernel = "sigmoid", degree = 3, 
                  coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                  tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                  probability = FALSE, fitted = TRUE)
  
  
  
  NA_vector <- c(NA,NA)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, NA_vector, after = length(results_SVM_sigmoid))
  #getting MSE
  temp_MSE <- mean(temp_SVM$residuals^2)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, temp_MSE, after = length(results_SVM_sigmoid))
  #for 0.8 train:test split
  set.rseed(0)
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    temp_dataset <- subset(full_data, select = varlist)
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    temp_dataset_train <- data.matrix(temp_dataset_train)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    temp_dataset_test <- data.matrix(temp_dataset_test)
    #getting MSFE and MAFE
    temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                      kernel = "sigmoid", degree = 3, 
                      coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                      tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                      probability = FALSE, fitted = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
    
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_80 <- mean(MSFE)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, MSFE_mean_80, after = length(results_SVM_sigmoid))
  MSFE_variance_80 <- var(MSFE)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, MSFE_variance_80, after = length(results_SVM_sigmoid))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, MAFE_mean_80, after = length(results_SVM_sigmoid))
  MAFE_variance_80 <- var(MAFE)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, MAFE_variance_80, after = length(results_SVM_sigmoid))
  
  for(j in 1:99){
    temp_dataset <- subset(full_data, select = varlist)
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    temp_dataset_train <- data.matrix(temp_dataset_train)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    temp_dataset_test <- data.matrix(temp_dataset_test)
    #getting MSFE and MAFE
    temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                      kernel = "sigmoid", degree = 3, 
                      coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                      tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                      probability = FALSE, fitted = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
    
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_90 <- mean(MSFE)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, MSFE_mean_90, after = length(results_SVM_sigmoid))
  MSFE_variance_90 <- var(MSFE)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, MSFE_variance_90, after = length(results_SVM_sigmoid))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, MAFE_mean_90, after = length(results_SVM_sigmoid))
  MAFE_variance_90 <- var(MAFE)
  results_SVM_sigmoid <- append(results_SVM_sigmoid, MAFE_variance_90, after = length(results_SVM_sigmoid))
}

#### polynomial ####
results_SVM_polynomial <- c()

FFR_FD <- full_data$FFR_FD
varlist <- GUM_variables
temp_dataset <- subset(full_data, select = varlist)

temp_dataset <- data.matrix(temp_dataset, rownames.force = NA)

temp_SVM <- svm(temp_dataset, y = FFR_FD, scale = TRUE, type = NULL, 
                kernel = "polynomial", degree = 3, 
                coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                probability = FALSE, fitted = TRUE)



NA_vector <- c(NA,NA)
results_SVM_polynomial <- append(results_SVM_polynomial, NA_vector, after = length(results_SVM_polynomial))
#getting MSE
temp_MSE <- mean(temp_SVM$residuals^2)
results_SVM_polynomial <- append(results_SVM_polynomial, temp_MSE, after = length(results_SVM_polynomial))
#for 0.8 train:test split
set.rseed(0)
MSFE <- c()
MAFE <- c()
for(j in 1:99){
  temp_dataset <- subset(full_data, select = varlist)
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  temp_dataset_train <- data.matrix(temp_dataset_train)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  temp_dataset_test <- data.matrix(temp_dataset_test)
  #getting MSFE and MAFE
  temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                    kernel = "polynomial", degree = 3, 
                    coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                    tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                    probability = FALSE, fitted = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_80 <- mean(MSFE)
results_SVM_polynomial <- append(results_SVM_polynomial, MSFE_mean_80, after = length(results_SVM_polynomial))
MSFE_variance_80 <- var(MSFE)
results_SVM_polynomial <- append(results_SVM_polynomial, MSFE_variance_80, after = length(results_SVM_polynomial))
#same but for MAFE
MAFE_mean_80 <- mean(MAFE)
results_SVM_polynomial <- append(results_SVM_polynomial, MAFE_mean_80, after = length(results_SVM_polynomial))
MAFE_variance_80 <- var(MAFE)
results_SVM_polynomial <- append(results_SVM_polynomial, MAFE_variance_80, after = length(results_SVM_polynomial))

for(j in 1:99){
  temp_dataset <- subset(full_data, select = varlist)
  #incorporating FFR_FD for the purpose of split
  temp_dataset$FFR_FD <- full_data$FFR_FD
  temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
  #creating training sub sample
  temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
  FFR_FD_train <- temp_dataset_train$FFR_FD
  
  #dropping FFR_FR from the list of considered variables
  temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
  temp_dataset_train <- data.matrix(temp_dataset_train)
  #creating testing sub sample
  temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
  FFR_FD_test <- temp_dataset_test$FFR_FD
  #dropping FFR_FR from the list of considered variables
  temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
  temp_dataset_test <- data.matrix(temp_dataset_test)
  #getting MSFE and MAFE
  temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                    kernel = "polynomial", degree = 3, 
                    coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                    tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                    probability = FALSE, fitted = TRUE)
  temp_test <- predict(temp_train, newdata = temp_dataset_test)
  temp_difference <- FFR_FD_test - temp_test
  #getting MSFE
  temp_MSFE <- mean(temp_difference^2)
  MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
  #getting MAFE
  temp_MAFE <- mean(abs(temp_difference))
  MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
  
}
#getting the mean and variance of MSFE for this specification from the results generated in the loop
MSFE_mean_90 <- mean(MSFE)
results_SVM_polynomial <- append(results_SVM_polynomial, MSFE_mean_90, after = length(results_SVM_polynomial))
MSFE_variance_90 <- var(MSFE)
results_SVM_polynomial <- append(results_SVM_polynomial, MSFE_variance_90, after = length(results_SVM_polynomial))
#same but for MAFE
MAFE_mean_90 <- mean(MAFE)
results_SVM_polynomial <- append(results_SVM_polynomial, MAFE_mean_90, after = length(results_SVM_polynomial))
MAFE_variance_90 <- var(MAFE)
results_SVM_polynomial <- append(results_SVM_polynomial, MAFE_variance_90, after = length(results_SVM_polynomial))


for(i in 1:length(sentiment_set)){
  set.rseed(0)
  varlist <- GUM_variables
  temp_sentiment <- unlist(sentiment_set[i])
  varlist <- append(varlist, temp_sentiment,after = length(varlist))
  temp_dataset <- subset(full_data, select = varlist)
  
  temp_dataset <- data.matrix(temp_dataset, rownames.force = NA)
  
  temp_SVM <- svm(temp_dataset, y = FFR_FD, scale = TRUE, type = NULL, 
                  kernel = "polynomial", degree = 3, 
                  coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                  tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                  probability = FALSE, fitted = TRUE)
  
  
  
  NA_vector <- c(NA,NA)
  results_SVM_polynomial <- append(results_SVM_polynomial, NA_vector, after = length(results_SVM_polynomial))
  #getting MSE
  temp_MSE <- mean(temp_SVM$residuals^2)
  results_SVM_polynomial <- append(results_SVM_polynomial, temp_MSE, after = length(results_SVM_polynomial))
  #for 0.8 train:test split
  set.rseed(0)
  MSFE <- c()
  MAFE <- c()
  for(j in 1:99){
    temp_dataset <- subset(full_data, select = varlist)
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.8)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    temp_dataset_train <- data.matrix(temp_dataset_train)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    temp_dataset_test <- data.matrix(temp_dataset_test)
    #getting MSFE and MAFE
    temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                      kernel = "polynomial", degree = 3, 
                      coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                      tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                      probability = FALSE, fitted = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
    
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_80 <- mean(MSFE)
  results_SVM_polynomial <- append(results_SVM_polynomial, MSFE_mean_80, after = length(results_SVM_polynomial))
  MSFE_variance_80 <- var(MSFE)
  results_SVM_polynomial <- append(results_SVM_polynomial, MSFE_variance_80, after = length(results_SVM_polynomial))
  #same but for MAFE
  MAFE_mean_80 <- mean(MAFE)
  results_SVM_polynomial <- append(results_SVM_polynomial, MAFE_mean_80, after = length(results_SVM_polynomial))
  MAFE_variance_80 <- var(MAFE)
  results_SVM_polynomial <- append(results_SVM_polynomial, MAFE_variance_80, after = length(results_SVM_polynomial))
  
  for(j in 1:99){
    temp_dataset <- subset(full_data, select = varlist)
    #incorporating FFR_FD for the purpose of split
    temp_dataset$FFR_FD <- full_data$FFR_FD
    temp_sample <- sample.split(temp_dataset[,1], SplitRatio = 0.9)
    #creating training sub sample
    temp_dataset_train <- subset(temp_dataset, temp_sample == TRUE)
    FFR_FD_train <- temp_dataset_train$FFR_FD
    
    #dropping FFR_FR from the list of considered variables
    temp_dataset_train <- subset(temp_dataset_train, select = -FFR_FD)
    temp_dataset_train <- data.matrix(temp_dataset_train)
    #creating testing sub sample
    temp_dataset_test <- subset(temp_dataset, temp_sample == FALSE)
    FFR_FD_test <- temp_dataset_test$FFR_FD
    #dropping FFR_FR from the list of considered variables
    temp_dataset_test <- subset(temp_dataset_test, select = -FFR_FD)
    temp_dataset_test <- data.matrix(temp_dataset_test)
    #getting MSFE and MAFE
    temp_train <- svm(temp_dataset_train, y = FFR_FD_train, scale = TRUE, type = NULL, 
                      kernel = "polynomial", degree = 3,
                      coef0 = 0, cost = 1, nu = 0.5, class.weights = NULL, cachesize = 40, 
                      tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
                      probability = FALSE, fitted = TRUE)
    temp_test <- predict(temp_train, newdata = temp_dataset_test)
    temp_difference <- FFR_FD_test - temp_test
    #getting MSFE
    temp_MSFE <- mean(temp_difference^2)
    MSFE <- append(MSFE, temp_MSFE, after = length(MSFE))
    #getting MAFE
    temp_MAFE <- mean(abs(temp_difference))
    MAFE <- append(MAFE, temp_MAFE, after = length(MAFE))
    
  }
  #getting the mean and variance of MSFE for this specification from the results generated in the loop
  MSFE_mean_90 <- mean(MSFE)
  results_SVM_polynomial <- append(results_SVM_polynomial, MSFE_mean_90, after = length(results_SVM_polynomial))
  MSFE_variance_90 <- var(MSFE)
  results_SVM_polynomial <- append(results_SVM_polynomial, MSFE_variance_90, after = length(results_SVM_polynomial))
  #same but for MAFE
  MAFE_mean_90 <- mean(MAFE)
  results_SVM_polynomial <- append(results_SVM_polynomial, MAFE_mean_90, after = length(results_SVM_polynomial))
  MAFE_variance_90 <- var(MAFE)
  results_SVM_polynomial <- append(results_SVM_polynomial, MAFE_variance_90, after = length(results_SVM_polynomial))
}

#### Results dataframe ####

column_names <- c('R2_no_sentiment','adj_R2_no_sentiment','MSE_no_sentiment','MSFE_mean_80_no_sentiment','MSFE_variance_80_no_sentiment',
                  'MAFE_mean_80_no_sentiment','MAFE_variance_80_no_sentiment','MSFE_mean_90_no_sentiment','MSFE_variance_90_no_sentiment',
                  'MAFE_mean_90_no_sentiment','MAFE_variance_90_no_sentiment',
                  'R2_Paragraphs_MNB_0_1','adj_R2_Paragraphs_MNB_0_1','MSE_Paragraphs_MNB_0_1','MSFE_mean_80_Paragraphs_MNB_0_1','MSFE_variance_80_Paragraphs_MNB_0_1',
                  'MAFE_mean_80_Paragraphs_MNB_0_1','MAFE_variance_80_Paragraphs_MNB_0_1','MSFE_mean_90_Paragraphs_MNB_0_1','MSFE_variance_90_Paragraphs_MNB_0_1',
                  'MAFE_mean_90_Paragraphs_MNB_0_1','MAFE_variance_90_Paragraphs_MNB_0_1',
                  'R2_Sentences_MNB_0_1','adj_R2_Sentences_MNB_0_1','MSE_Sentences_MNB_0_1','MSFE_mean_80_Sentences_MNB_0_1','MSFE_variance_80_Sentences_MNB_0_1',
                  'MAFE_mean_80_Sentences_MNB_0_1','MAFE_variance_80_Sentences_MNB_0_1','MSFE_mean_90_Sentences_MNB_0_1','MSFE_variance_90_Sentences_MNB_0_1',
                  'MAFE_mean_90_Sentences_MNB_0_1','MAFE_variance_90_Sentences_MNB_0_1',
                  'R2_Paragraphs_MNB_0_4','adj_R2_Paragraphs_MNB_0_4','MSE_Paragraphs_MNB_0_4','MSFE_mean_80_Paragraphs_MNB_0_4','MSFE_variance_80_Paragraphs_MNB_0_4',
                  'MAFE_mean_80_Paragraphs_MNB_0_4','MAFE_variance_80_Paragraphs_MNB_0_4','MSFE_mean_90_Paragraphs_MNB_0_4','MSFE_variance_90_Paragraphs_MNB_0_4',
                  'MAFE_mean_90_Paragraphs_MNB_0_4','MAFE_variance_90_Paragraphs_MNB_0_4',
                  'R2_Sentences_MNB_0_4','adj_R2_Sentences_MNB_0_4','MSE_Sentences_MNB_0_4','MSFE_mean_80_Sentences_MNB_0_4','MSFE_variance_80_Sentences_MNB_0_4',
                  'MAFE_mean_80_Sentences_MNB_0_4','MAFE_variance_80_Sentences_MNB_0_4','MSFE_mean_90_Sentences_MNB_0_4','MSFE_variance_90_Sentences_MNB_0_4',
                  'MAFE_mean_90_Sentences_MNB_0_4','MAFE_variance_90_Sentences_MNB_0_4',
                  'R2_Paragraphs_LR_0_1','adj_R2_Paragraphs_LR_0_1','MSE_Paragraphs_LR_0_1','MSFE_mean_80_Paragraphs_LR_0_1','MSFE_variance_80_Paragraphs_LR_0_1',
                  'MAFE_mean_80_Paragraphs_LR_0_1','MAFE_variance_80_Paragraphs_LR_0_1','MSFE_mean_90_Paragraphs_LR_0_1','MSFE_variance_90_Paragraphs_LR_0_1',
                  'MAFE_mean_90_Paragraphs_LR_0_1','MAFE_variance_90_Paragraphs_LR_0_1',
                  'R2_Sentences_LR_0_1','adj_R2_Sentences_LR_0_1','MSE_Sentences_LR_0_1','MSFE_mean_80_Sentences_LR_0_1','MSFE_variance_80_Sentences_LR_0_1',
                  'MAFE_mean_80_Sentences_LR_0_1','MAFE_variance_80_Sentences_LR_0_1','MSFE_mean_90_Sentences_LR_0_1','MSFE_variance_90_Sentences_LR_0_1',
                  'MAFE_mean_90_Sentences_LR_0_1','MAFE_variance_90_Sentences_LR_0_1',
                  'R2_Paragraphs_OLR_0_4','adj_R2_Paragraphs_OLR_0_4','MSE_Paragraphs_OLR_0_4','MSFE_mean_80_Paragraphs_OLR_0_4','MSFE_variance_80_Paragraphs_OLR_0_4',
                  'MAFE_mean_80_Paragraphs_OLR_0_4','MAFE_variance_80_Paragraphs_OLR_0_4','MSFE_mean_90_Paragraphs_OLR_0_4','MSFE_variance_90_Paragraphs_OLR_0_4',
                  'MAFE_mean_90_Paragraphs_OLR_0_4','MAFE_variance_90_Paragraphs_OLR_0_4',
                  'R2_Sentences_OLR_0_4','adj_R2_Sentences_OLR_0_4','MSE_Sentences_OLR_0_4','MSFE_mean_80_Sentences_OLR_0_4','MSFE_variance_80_Sentences_OLR_0_4',
                  'MAFE_mean_80_Sentences_OLR_0_4','MAFE_variance_80_Sentences_OLR_0_4','MSFE_mean_90_Sentences_OLR_0_4','MSFE_variance_90_Sentences_OLR_0_4',
                  'MAFE_mean_90_Sentences_OLR_0_4','MAFE_variance_90_Sentences_OLR_0_4',
                  'R2_Paragraphs_BERT_0_1','adj_R2_Paragraphs_BERT_0_1','MSE_Paragraphs_BERT_0_1','MSFE_mean_80_Paragraphs_BERT_0_1','MSFE_variance_80_Paragraphs_BERT_0_1',
                  'MAFE_mean_80_Paragraphs_BERT_0_1','MAFE_variance_80_Paragraphs_BERT_0_1','MSFE_mean_90_Paragraphs_BERT_0_1','MSFE_variance_90_Paragraphs_BERT_0_1',
                  'MAFE_mean_90_Paragraphs_BERT_0_1','MAFE_variance_90_Paragraphs_BERT_0_1',
                  'R2_Sentences_BERT_0_1','adj_R2_Sentences_BERT_0_1','MSE_Sentences_BERT_0_1','MSFE_mean_80_Sentences_BERT_0_1','MSFE_variance_80_Sentences_BERT_0_1',
                  'MAFE_mean_80_Sentences_BERT_0_1','MAFE_variance_80_Sentences_BERT_0_1','MSFE_mean_90_Sentences_BERT_0_1','MSFE_variance_90_Sentences_BERT_0_1',
                  'MAFE_mean_90_Sentences_BERT_0_1','MAFE_variance_90_Sentences_BERT_0_1',
                  'R2_Paragraphs_BERT_0_4','adj_R2_Paragraphs_BERT_0_4','MSE_Paragraphs_BERT_0_4','MSFE_mean_80_Paragraphs_BERT_0_4','MSFE_variance_80_Paragraphs_BERT_0_4',
                  'MAFE_mean_80_Paragraphs_BERT_0_4','MAFE_variance_80_Paragraphs_BERT_0_4','MSFE_mean_90_Paragraphs_BERT_0_4','MSFE_variance_90_Paragraphs_BERT_0_4',
                  'MAFE_mean_90_Paragraphs_BERT_0_4','MAFE_variance_90_Paragraphs_BERT_0_4',
                  'R2_Sentences_BERT_0_4','adj_R2_Sentences_BERT_0_4','MSE_Sentences_BERT_0_4','MSFE_mean_80_Sentences_BERT_0_4','MSFE_variance_80_Sentences_BERT_0_4',
                  'MAFE_mean_80_Sentences_BERT_0_4','MAFE_variance_80_Sentences_BERT_0_4','MSFE_mean_90_Sentences_BERT_0_4','MSFE_variance_90_Sentences_BERT_0_4',
                  'MAFE_mean_90_Sentences_BERT_0_4','MAFE_variance_90_Sentences_BERT_0_4')

options(scipen = 999)
results_aggregated_final <- data.frame(column_names,results_GUM,results_stepwise_10,results_stepwise_05,results_stepwise_01,results_AIC,results_RF_6, results_RF_10, results_RF_15, results_Bagging, results_SVM_polynomial, results_SVM_radial, results_SVM_sigmoid)
results_aggregated_final <- as.data.frame(t(results_aggregated_final))
colnames(results_aggregated_final) <- results_aggregated_final[1,]
results_aggregated_final <- results_aggregated_final[-1,]


save(results_aggregated_final, file = "results_aggregated_normalized_negative_gradient_final.Rda")

#delete this before submission

load("results_aggregated_normalized_negative_gradient_final.Rda")

temp_dataframe <- as.data.frame(t(results_aggregated_final))

results_GUM <- temp_dataframe$results_GUM
results_GUM <- as.numeric(results_GUM)
results_stepwise_10 <- temp_dataframe$results_stepwise_10
results_stepwise_10 <- as.numeric(results_stepwise_10)
results_stepwise_05 <- temp_dataframe$results_stepwise_05
results_stepwise_05 <- as.numeric(results_stepwise_05)
results_stepwise_01 <- temp_dataframe$results_stepwise_01
results_stepwise_01 <- as.numeric(results_stepwise_01)
results_AIC <- temp_dataframe$results_AIC
results_AIC <- as.numeric(results_AIC)
results_RF_6 <- temp_dataframe$results_RF_6
results_RF_6 <- as.numeric(results_RF_6)
results_RF_10 <- temp_dataframe$results_RF_10
results_RF_10 <- as.numeric(results_RF_10)
#results_RF_15 <- temp_dataframe$results_RF_15
#results_RF_15 <- as.numeric(results_RF_15)
results_Bagging <- temp_dataframe$results_Bagging
results_Bagging <- as.numeric(results_Bagging)
results_SVM_polynomial <- temp_dataframe$results_SVM_polynomial
results_SVM_polynomial <- as.numeric(results_SVM_polynomial)
results_SVM_radial <- temp_dataframe$results_SVM_radial
results_SVM_radial <- as.numeric(results_SVM_radial)
results_SVM_sigmoid <- temp_dataframe$results_SVM_sigmoid
results_SVM_sigmoid <- as.numeric(results_SVM_sigmoid)


results_SVM_polynomial, results_SVM_radial, results_SVM_sigmoid


test <- results_aggregated_final[-c(8,9,10),]
install.packages("miscTools")
library(miscTools)
test <- rbind(test,results_RF_15)
rownames(test)[8] <- "results_RF_15"
test <- rbind(test,results_aggregated_final[c(9,10),])
test <- test[-10,]
test <- rbind(test,results_SVM_polynomial)
