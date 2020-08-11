#declare working directory
setwd('C:/University/Grad School/Thesis/Data')
getwd()
#loading libraries
#install.packages('tidyverse')
library(tidyverse)
library(caret)
library(glmnet)
#install.packages('zoo')
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
#install.packages('reshape2')
library(reshape2)
#install.packages('expss')
library(expss)
#install.packages('grid')
library(grid)
#install.packages('gridExtra')
library(gridExtra)
#install.packages('stargazer')
library(stargazer)
#install.packages('sjPlot')
library(sjPlot)
library(dplyr)

#Set seed
set.seed(0)
options(digits = 6)
options(scipen = 999)

load("results_aggregated_normalized_negative_gradient_final.Rda")

results_aggregated <- results_aggregated_final
#changing row variables for readability 
rownames(results_aggregated)[rownames(results_aggregated) == 'results_GUM'] = 'GUM'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_stepwise_10'] = 'Stepwise 10%'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_stepwise_05'] = 'Stepwise 5%'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_stepwise_01'] = 'Stepwise 1%'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_AIC'] = 'AIC'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_RF_6'] = 'Random Forest: 6'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_RF_10'] = 'Random Forest: 10'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_RF_15'] = 'Random Forest: 15'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_Bagging'] = 'Bagging'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_SVM_radial'] = 'Support Vector Regression: Radial'
rownames(results_aggregated)[rownames(results_aggregated) == 'results_SVM_polynomial'] = 'Support Vector Regression: Polynomial'
#Dropping sigmoid due to it providing unambiguously worse results than any other model considered 
results_aggregated <- results_aggregated[-12,]
#rownames(results_aggregated)[rownames(results_aggregated) == 'results_SVM_sigmoid'] = 'Support Vector Machine: Sigmoid'


setwd('C:/University/Grad School/Thesis/Plots Negative Gradient Normalized')
#### R2 ####
#sentences 0_1
png(file='R2 Sentences 0_1.png')

R2_sentences_0_1 <- c('R2_no_sentiment', 'R2_Sentences_MNB_0_1', 'R2_Sentences_LR_0_1', 'R2_Sentences_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = R2_sentences_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'R Squared Sentences -1_1', file = 'R Squared Sentences 1_1.doc')


temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$R2 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable



#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_R2_Sentences_0_1 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
                         coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('R Squared Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
                         labs(y = 'R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')
print(plot_R2_Sentences_0_1)
dev.off()

plot_R2_Sentences_0_1_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = R2, shape = sentiment_approach, size = 1.5), show.legend = FALSE) +
                                  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('R Squared Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
                                  labs(y = 'R Squared', x = 'Sentiment Quantification Approach')


#Paragraphs 0_1
png(file='R2 Paragraphs 0_1.png')

R2_Paragraphs_0_1 <- c('R2_no_sentiment', 'R2_Paragraphs_MNB_0_1', 'R2_Paragraphs_LR_0_1', 'R2_Paragraphs_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = R2_Paragraphs_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'R Squared Paragraphs -1_1', file = 'R Squared Paragraphs 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$R2 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_R2_Paragraphs_0_1 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = R2, shape = sentiment_approach, size = 1.5)) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('R Squared Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')
print(plot_R2_Paragraphs_0_1)
dev.off()

plot_R2_Paragraphs_0_1_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
                                   coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('R Squared Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
                                   labs(y = 'R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')

png(file='R2 0_1 Combined.png', width = 900, height = 400)
combined_R2_0_1 <- ggarrange(plot_R2_Sentences_0_1_combined,plot_R2_Paragraphs_0_1_combined, ncol = 2, nrow = 1, widths = c(1,1.35))
print(combined_R2_0_1)
dev.off()

#Sentences 0_4
png(file='R2 Sentences 0_4.png')

R2_sentences_0_4 <- c('R2_no_sentiment', 'R2_Sentences_MNB_0_4', 'R2_Sentences_OLR_0_4', 'R2_Sentences_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = R2_sentences_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'R Squared Sentences -2_2', file = 'R Squared Sentences 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$R2 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_R2_Sentences_0_4 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('R Squared Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')
print(plot_R2_Sentences_0_4)
dev.off()

plot_R2_Sentences_0_4_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = R2, shape = sentiment_approach, size = 1.5), show.legend = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('R Squared Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'R Squared', x = 'Sentiment Quantification Approach')

#Paragraphs 0_4
png(file='R2 Paragraphs 0_4.png')

R2_Paragraphs_0_4 <- c('R2_no_sentiment', 'R2_Paragraphs_MNB_0_4', 'R2_Paragraphs_OLR_0_4', 'R2_Paragraphs_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = R2_Paragraphs_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'R Squared Paragraphs -2_2', file = 'R Squared Paragraphs 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$R2 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_R2_Paragraphs_0_4 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('R Squared Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')
print(plot_R2_Paragraphs_0_4)
dev.off()

plot_R2_Paragraphs_0_4_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('R Squared Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')

png(file='R2 0_4 Combined.png', width = 900, height = 400)
combined_R2_0_4 <- ggarrange(plot_R2_Sentences_0_4_combined,plot_R2_Paragraphs_0_4_combined, ncol = 2, nrow = 1, widths = c(1,1.35))
combined_R2_0_4
print(combined_R2_0_4)
dev.off()

#### adj_R2 ####
#sentences 0_1
png(file='adj_R2 Sentences 0_1.png')

adj_R2_sentences_0_1 <- c('adj_R2_no_sentiment', 'adj_R2_Sentences_MNB_0_1', 'adj_R2_Sentences_LR_0_1', 'adj_R2_Sentences_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = adj_R2_sentences_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Adjusted R Squared Sentences -1_1', file = 'Adjusted R Squared Sentences 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$adj_R2 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_adj_R2_Sentences_0_1 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = adj_R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('Adjusted R Squared Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Adjusted R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')
print(plot_adj_R2_Sentences_0_1)
dev.off()

plot_adj_R2_Sentences_0_1_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = adj_R2, shape = sentiment_approach, size = 1.5), show.legend = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('Adjusted R Squared Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Adjusted R Squared', x = 'Sentiment Quantification Approach')


#Paragraphs 0_1
png(file='adj_R2 Paragraphs 0_1.png')

adj_R2_Paragraphs_0_1 <- c('adj_R2_no_sentiment', 'adj_R2_Paragraphs_MNB_0_1', 'adj_R2_Paragraphs_LR_0_1', 'adj_R2_Paragraphs_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = adj_R2_Paragraphs_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Adjusted R Squared Paragraphs -1_1', file = 'Adjusted R Squared Paragraphs 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$adj_R2 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_adj_R2_Paragraphs_0_1 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = adj_R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('Adjusted R Squared Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Adjusted R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')
print(plot_adj_R2_Paragraphs_0_1)
dev.off()

plot_adj_R2_Paragraphs_0_1_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = adj_R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('Adjusted R Squared Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Adjusted R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')

png(file='adj_R2 0_1 Combined.png', width = 900, height = 400)
combined_adj_R2_0_1 <- ggarrange(plot_adj_R2_Sentences_0_1_combined,plot_adj_R2_Paragraphs_0_1_combined, ncol = 2, nrow = 1, widths = c(1,1.35))
print(combined_adj_R2_0_1)
dev.off()

#Sentences 0_4
png(file='adj_R2 Sentences 0_4.png')

adj_R2_sentences_0_4 <- c('adj_R2_no_sentiment', 'adj_R2_Sentences_MNB_0_4', 'adj_R2_Sentences_OLR_0_4', 'adj_R2_Sentences_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = adj_R2_sentences_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Adjusted R Squared Sentences -2_2', file = 'Adjusted R Squared Sentences 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$adj_R2 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_adj_R2_Sentences_0_4 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = adj_R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('Adjusted R Squared Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Adjusted R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')
print(plot_adj_R2_Sentences_0_4)
dev.off()

plot_adj_R2_Sentences_0_4_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = adj_R2, shape = sentiment_approach, size = 1.5), show.legend = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('Adjusted R Squared Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Adjusted R Squared', x = 'Sentiment Quantification Approach')

#Paragraphs 0_4
png(file='adj_R2 Paragraphs 0_4.png')

adj_R2_Paragraphs_0_4 <- c('adj_R2_no_sentiment', 'adj_R2_Paragraphs_MNB_0_4', 'adj_R2_Paragraphs_OLR_0_4', 'adj_R2_Paragraphs_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = adj_R2_Paragraphs_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Adjusted R Squared Paragraphs -2_2', file = 'Adjusted R Squared Paragraphs 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$adj_R2 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_adj_R2_Paragraphs_0_4 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = adj_R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('Adjusted R Squared Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Adjusted R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')
print(plot_adj_R2_Paragraphs_0_4)
dev.off()

plot_adj_R2_Paragraphs_0_4_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = adj_R2, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  coord_cartesian(ylim=c(0.25,0.6)) + ggtitle('Adjusted R Squared Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Adjusted R Squared', x = 'Sentiment Quantification Approach', shape = 'Method Used')

png(file='adj_R2 0_4 Combined.png', width = 900, height = 400)
combined_adj_R2_0_4 <- ggarrange(plot_adj_R2_Sentences_0_4_combined,plot_adj_R2_Paragraphs_0_4_combined, ncol = 2, nrow = 1, widths = c(1,1.35))
combined_adj_R2_0_4
print(combined_adj_R2_0_4)
dev.off()

#### MSE ####

#sentences 0_1
png(file='MSE Sentences 0_1.png')

MSE_sentences_0_1 <- c('MSE_no_sentiment', 'MSE_Sentences_MNB_0_1', 'MSE_Sentences_LR_0_1', 'MSE_Sentences_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MSE_sentences_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Error Sentences -1_1', file = 'Mean Squared Error Sentences 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSE <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MSE_Sentences_0_1 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSE, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  ggtitle('Mean Squared Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Mean Squared Error', x = 'Sentiment Quantification Approach', shape = 'Method Used')+ 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9))
print(plot_MSE_Sentences_0_1)
dev.off()

plot_MSE_Sentences_0_1_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSE, shape = sentiment_approach, size = 1.5), show.legend = FALSE) +
  ggtitle('Mean Squared Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Mean Squared Error', x = 'Sentiment Quantification Approach')+ 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9)) + ylim(0.006,0.024)


#Paragraphs 0_1
png(file='MSE Paragraphs 0_1.png')

MSE_Paragraphs_0_1 <- c('MSE_no_sentiment', 'MSE_Paragraphs_MNB_0_1', 'MSE_Paragraphs_LR_0_1', 'MSE_Paragraphs_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MSE_Paragraphs_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Error Paragraphs -1_1', file = 'Mean Squared Error Paragraphs 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSE <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MSE_Paragraphs_0_1 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSE, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  ggtitle('Mean Squared Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Mean Squared Error', x = 'Sentiment Quantification Approach', shape = 'Method Used')+ 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9))
print(plot_MSE_Paragraphs_0_1)
dev.off()

plot_MSE_Paragraphs_0_1_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSE, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  ggtitle('Mean Squared Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Mean Squared Error', x = 'Sentiment Quantification Approach', shape = 'Method Used')+ 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9))+ ylim(0.006,0.024)

png(file='MSE 0_1 Combined.png', width = 1000, height = 400)
combined_MSE_0_1 <- ggarrange(plot_MSE_Sentences_0_1_combined,plot_MSE_Paragraphs_0_1_combined, ncol = 2, nrow = 1, widths = c(1,1.5))
print(combined_MSE_0_1)
dev.off()

#Sentences 0_4
png(file='MSE Sentences 0_4.png')

MSE_sentences_0_4 <- c('MSE_no_sentiment', 'MSE_Sentences_MNB_0_4', 'MSE_Sentences_OLR_0_4', 'MSE_Sentences_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MSE_sentences_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Error Sentences -2_2', file = 'Mean Squared Error Sentences 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSE <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MSE_Sentences_0_4 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSE, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  ggtitle('Mean Squared Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Mean Squared Error', x = 'Sentiment Quantification Approach', shape = 'Method Used')+ 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9))
print(plot_MSE_Sentences_0_4)
dev.off()

plot_MSE_Sentences_0_4_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSE, shape = sentiment_approach, size = 1.5), show.legend = FALSE) +
  ggtitle('Mean Squared Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'MSE', x = 'Sentiment Quantification Approach')+ 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9)) + ylim(0.006,0.024)

#Paragraphs 0_4
png(file='MSE Paragraphs 0_4.png')

MSE_Paragraphs_0_4 <- c('MSE_no_sentiment', 'MSE_Paragraphs_MNB_0_4', 'MSE_Paragraphs_OLR_0_4', 'MSE_Paragraphs_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MSE_Paragraphs_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Error Paragraphs -2_2', file = 'Mean Squared Error Paragraphs 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSE <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MSE_Paragraphs_0_4 <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSE, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  ggtitle('Mean Squared Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Mean Squared Error', x = 'Sentiment Quantification Approach', shape = 'Method Used')+ 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9))
print(plot_MSE_Paragraphs_0_4)
dev.off()

plot_MSE_Paragraphs_0_4_combined <- ggplot() + geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSE, shape = sentiment_approach, size = 1.5)) + guides(size = FALSE) +
  ggtitle('Mean Squared Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Mean Squared Error', x = 'Sentiment Quantification Approach', shape = 'Method Used')+ 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9)) + ylim(0.006,0.024)

png(file='MSE 0_4 Combined.png', width = 1000, height = 400)
combined_MSE_0_4 <- ggarrange(plot_MSE_Sentences_0_4_combined,plot_MSE_Paragraphs_0_4_combined, ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MSE_0_4
print(combined_MSE_0_4)
dev.off()

### MSFE Mean 90 ####

#sentences 0_1
png(file='MSFE_mean_90 Sentences 0_1.png', width = 500, height = 400)

MSFE_mean_90_sentences_0_1 <- c('MSFE_mean_90_no_sentiment', 'MSFE_mean_90_Sentences_MNB_0_1', 'MSFE_mean_90_Sentences_LR_0_1', 'MSFE_mean_90_Sentences_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_90_sentences_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Forecasting Error 90 Sentences -1_1', file = 'Mean Squared Forecasting Error 90 Sentences 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

test <- temp_dataset

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable


#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MSFE_mean_90_Sentences_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_90_Sentences_0_1 <- plot_MSFE_mean_90_Sentences_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_90_Sentences_0_1)
plot_MSFE_mean_90_Sentences_0_1
dev.off()

plot_MSFE_mean_90_Sentences_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.01,0.030)
plot_MSFE_mean_90_Sentences_0_1_combined <- plot_MSFE_mean_90_Sentences_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))


#Paragraphs 0_1
png(file='MSFE_mean_90 Paragraphs 0_1.png')

MSFE_mean_90_Paragraphs_0_1 <- c('MSFE_mean_90_no_sentiment', 'MSFE_mean_90_Paragraphs_MNB_0_1', 'MSFE_mean_90_Paragraphs_LR_0_1', 'MSFE_mean_90_Paragraphs_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_90_Paragraphs_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Forecasting Error 90 Paragraphs -1_1', file = 'Mean Squared Forecasting Error 90 Paragraphs 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MSFE_mean_90_Paragraphs_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_90_Paragraphs_0_1 <- plot_MSFE_mean_90_Paragraphs_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_90_Paragraphs_0_1)
plot_MSFE_mean_90_Paragraphs_0_1
dev.off()

plot_MSFE_mean_90_Paragraphs_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.01,0.030)
plot_MSFE_mean_90_Paragraphs_0_1_combined <- plot_MSFE_mean_90_Paragraphs_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MSFE_mean_90 0_1 Combined.png', width = 1000, height = 400)
combined_MSFE_mean_90_0_1 <- ggarrange(plot_MSFE_mean_90_Sentences_0_1_combined,
                                       plot_MSFE_mean_90_Paragraphs_0_1_combined, 
                                       ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MSFE_mean_90_0_1 <- combined_MSFE_mean_90_0_1 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MSFE_mean_90_0_1)
dev.off()

#sentences 0_4
png(file='MSFE_mean_90 Sentences 0_4.png', width = 500, height = 400)

MSFE_mean_90_sentences_0_4 <- c('MSFE_mean_90_no_sentiment', 'MSFE_mean_90_Sentences_MNB_0_4', 'MSFE_mean_90_Sentences_OLR_0_4', 'MSFE_mean_90_Sentences_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_90_sentences_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Forecasting Error 90 Sentences -2_2', file = 'Mean Squared Forecasting Error 90 Sentences 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

nlevels(temp_dataset$sentiment_approach)

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MSFE_mean_90_Sentences_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) 
plot_MSFE_mean_90_Sentences_0_4 <- plot_MSFE_mean_90_Sentences_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_90_Sentences_0_4)
plot_MSFE_mean_90_Sentences_0_4
dev.off()

plot_MSFE_mean_90_Sentences_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.01,0.030)
plot_MSFE_mean_90_Sentences_0_4_combined <- plot_MSFE_mean_90_Sentences_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

#Paragraphs 0_4
png(file='MSFE_mean_90 Paragraphs 0_4.png')

MSFE_mean_90_Paragraphs_0_4 <- c('MSFE_mean_90_no_sentiment', 'MSFE_mean_90_Paragraphs_MNB_0_4', 'MSFE_mean_90_Paragraphs_OLR_0_4', 'MSFE_mean_90_Paragraphs_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_90_Paragraphs_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Forecasting Error 90 Paragraphs -2_2', file = 'Mean Squared Forecasting Error 90 Paragraphs 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MSFE_mean_90_Paragraphs_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_90_Paragraphs_0_4 <- plot_MSFE_mean_90_Paragraphs_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_90_Paragraphs_0_4)
plot_MSFE_mean_90_Paragraphs_0_4
dev.off()

plot_MSFE_mean_90_Paragraphs_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.01,0.030)
plot_MSFE_mean_90_Paragraphs_0_4_combined <- plot_MSFE_mean_90_Paragraphs_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MSFE_mean_90 0_4 Combined.png', width = 1000, height = 400)
combined_MSFE_mean_90_0_4 <- ggarrange(plot_MSFE_mean_90_Sentences_0_4_combined,
                                       plot_MSFE_mean_90_Paragraphs_0_4_combined, 
                                       ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MSFE_mean_90_0_4 <- combined_MSFE_mean_90_0_4 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MSFE_mean_90_0_4)
dev.off()


#### MSFE Mean 90 Reduced Scale ####


#sentences 0_1
png(file='MSFE_mean_90 Sentences 0_1.png', width = 500, height = 400)

MSFE_mean_90_sentences_0_1 <- c('MSFE_mean_90_no_sentiment', 'MSFE_mean_90_Sentences_MNB_0_1', 'MSFE_mean_90_Sentences_LR_0_1', 'MSFE_mean_90_Sentences_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_90_sentences_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')
temp_dataset$sentiment_models <- sentiment_models

test <- temp_dataset

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable


#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MSFE_mean_90_reduced_scale_Sentences_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_90_reduced_scale_Sentences_0_1 <- plot_MSFE_mean_90_reduced_scale_Sentences_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_90_reduced_scale_Sentences_0_1)
plot_MSFE_mean_90_reduced_scale_Sentences_0_1
dev.off()

plot_MSFE_mean_90_reduced_scale_Sentences_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.015,0.025)
plot_MSFE_mean_90_reduced_scale_Sentences_0_1_combined <- plot_MSFE_mean_90_reduced_scale_Sentences_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))


#Paragraphs 0_1
png(file='MSFE_mean_90 Paragraphs 0_1.png')

MSFE_mean_90_Paragraphs_0_1 <- c('MSFE_mean_90_no_sentiment', 'MSFE_mean_90_Paragraphs_MNB_0_1', 'MSFE_mean_90_Paragraphs_LR_0_1', 'MSFE_mean_90_Paragraphs_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_90_Paragraphs_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')
temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1 <- plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1)
plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1
dev.off()

plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.015,0.025)
plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1_combined <- plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MSFE_mean_90 0_1 Combined Reduced Scale.png', width = 1000, height = 400)
combined_MSFE_mean_90_reduced_scale_0_1 <- ggarrange(plot_MSFE_mean_90_reduced_scale_Sentences_0_1_combined,
                                                     plot_MSFE_mean_90_reduced_scale_Paragraphs_0_1_combined, 
                                                     ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MSFE_mean_90_reduced_scale_0_1 <- combined_MSFE_mean_90_reduced_scale_0_1 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MSFE_mean_90_reduced_scale_0_1)
dev.off()

#sentences 0_4
png(file='MSFE_mean_90 Sentences 0_4.png', width = 500, height = 400)

MSFE_mean_90_sentences_0_4 <- c('MSFE_mean_90_no_sentiment', 'MSFE_mean_90_Sentences_MNB_0_4', 'MSFE_mean_90_Sentences_OLR_0_4', 'MSFE_mean_90_Sentences_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_90_sentences_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')
temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

nlevels(temp_dataset$sentiment_approach)

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MSFE_mean_90_reduced_scale_Sentences_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) 
plot_MSFE_mean_90_reduced_scale_Sentences_0_4 <- plot_MSFE_mean_90_reduced_scale_Sentences_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_90_reduced_scale_Sentences_0_4)
plot_MSFE_mean_90_reduced_scale_Sentences_0_4
dev.off()

plot_MSFE_mean_90_reduced_scale_Sentences_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.01225,0.0225)
plot_MSFE_mean_90_reduced_scale_Sentences_0_4_combined <- plot_MSFE_mean_90_reduced_scale_Sentences_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

#Paragraphs 0_4
png(file='MSFE_mean_90 Paragraphs 0_4.png')

MSFE_mean_90_Paragraphs_0_4 <- c('MSFE_mean_90_no_sentiment', 'MSFE_mean_90_Paragraphs_MNB_0_4', 'MSFE_mean_90_Paragraphs_OLR_0_4', 'MSFE_mean_90_Paragraphs_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_90_Paragraphs_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')
temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4 <- plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4)
plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4
dev.off()

plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.01225,0.0225)
plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4_combined <- plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MSFE_mean_90 0_4 Combined Reduced Scale.png', width = 1000, height = 400)
combined_MSFE_mean_90_reduced_scale_0_4 <- ggarrange(plot_MSFE_mean_90_reduced_scale_Sentences_0_4_combined,
                                                     plot_MSFE_mean_90_reduced_scale_Paragraphs_0_4_combined, 
                                                     ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MSFE_mean_90_reduced_scale_0_4 <- combined_MSFE_mean_90_reduced_scale_0_4 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MSFE_mean_90_reduced_scale_0_4)
dev.off()

#### MSFE Mean 80 ####
#sentences 0_1
png(file='MSFE_mean_80 Sentences 0_1.png', width = 500, height = 400)

MSFE_mean_80_sentences_0_1 <- c('MSFE_mean_80_no_sentiment', 'MSFE_mean_80_Sentences_MNB_0_1', 'MSFE_mean_80_Sentences_LR_0_1', 'MSFE_mean_80_Sentences_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_80_sentences_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Forecasting Error 80 Sentences -1_1', file = 'Mean Squared Forecasting Error 80 Sentences 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

test <- temp_dataset

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_80 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable


#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MSFE_mean_80_Sentences_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_80_Sentences_0_1 <- plot_MSFE_mean_80_Sentences_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_80_Sentences_0_1)
plot_MSFE_mean_80_Sentences_0_1
dev.off()

plot_MSFE_mean_80_Sentences_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.010,0.035)
plot_MSFE_mean_80_Sentences_0_1_combined <- plot_MSFE_mean_80_Sentences_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))


#Paragraphs 0_1
png(file='MSFE_mean_80 Paragraphs 0_1.png')

MSFE_mean_80_Paragraphs_0_1 <- c('MSFE_mean_80_no_sentiment', 'MSFE_mean_80_Paragraphs_MNB_0_1', 'MSFE_mean_80_Paragraphs_LR_0_1', 'MSFE_mean_80_Paragraphs_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_80_Paragraphs_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Forecasting Error 80 Paragraphs -1_1', file = 'Mean Squared Forecasting Error 80 Paragraphs 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_80 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MSFE_mean_80_Paragraphs_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_80_Paragraphs_0_1 <- plot_MSFE_mean_80_Paragraphs_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_80_Paragraphs_0_1)
plot_MSFE_mean_80_Paragraphs_0_1
dev.off()

plot_MSFE_mean_80_Paragraphs_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.010,0.035)
plot_MSFE_mean_80_Paragraphs_0_1_combined <- plot_MSFE_mean_80_Paragraphs_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MSFE_mean_80 0_1 Combined.png', width = 1000, height = 400)
combined_MSFE_mean_80_0_1 <- ggarrange(plot_MSFE_mean_80_Sentences_0_1_combined,
                                       plot_MSFE_mean_80_Paragraphs_0_1_combined, 
                                       ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MSFE_mean_80_0_1 <- combined_MSFE_mean_80_0_1 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MSFE_mean_80_0_1)
dev.off()

#sentences 0_4
png(file='MSFE_mean_80 Sentences 0_4.png', width = 500, height = 400)

MSFE_mean_80_sentences_0_4 <- c('MSFE_mean_80_no_sentiment', 'MSFE_mean_80_Sentences_MNB_0_4', 'MSFE_mean_80_Sentences_OLR_0_4', 'MSFE_mean_80_Sentences_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_80_sentences_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Forecasting Error 80 Sentences -2_2', file = 'Mean Squared Forecasting Error 80 Sentences 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_80 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

nlevels(temp_dataset$sentiment_approach)

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MSFE_mean_80_Sentences_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) 
plot_MSFE_mean_80_Sentences_0_4 <- plot_MSFE_mean_80_Sentences_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_80_Sentences_0_4)
plot_MSFE_mean_80_Sentences_0_4
dev.off()

plot_MSFE_mean_80_Sentences_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.010,0.035)
plot_MSFE_mean_80_Sentences_0_4_combined <- plot_MSFE_mean_80_Sentences_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

#Paragraphs 0_4
png(file='MSFE_mean_80 Paragraphs 0_4.png')

MSFE_mean_80_Paragraphs_0_4 <- c('MSFE_mean_80_no_sentiment', 'MSFE_mean_80_Paragraphs_MNB_0_4', 'MSFE_mean_80_Paragraphs_OLR_0_4', 'MSFE_mean_80_Paragraphs_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MSFE_mean_80_Paragraphs_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Squared Forecasting Error 80 Paragraphs -2_2', file = 'Mean Squared Forecasting Error 80 Paragraphs 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MSFE_mean_80 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MSFE_mean_80_Paragraphs_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MSFE_mean_80_Paragraphs_0_4 <- plot_MSFE_mean_80_Paragraphs_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MSFE_mean_80_Paragraphs_0_4)
plot_MSFE_mean_80_Paragraphs_0_4
dev.off()

plot_MSFE_mean_80_Paragraphs_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MSFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Squared Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Squared Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.010,0.035)
plot_MSFE_mean_80_Paragraphs_0_4_combined <- plot_MSFE_mean_80_Paragraphs_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MSFE_mean_80 0_4 Combined.png', width = 1000, height = 400)
combined_MSFE_mean_80_0_4 <- ggarrange(plot_MSFE_mean_80_Sentences_0_4_combined,
                                       plot_MSFE_mean_80_Paragraphs_0_4_combined, 
                                       ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MSFE_mean_80_0_4 <- combined_MSFE_mean_80_0_4 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MSFE_mean_80_0_4)
dev.off()

### MAFE Mean 90 ####

#sentences 0_1
png(file='MAFE_mean_90 Sentences 0_1.png', width = 500, height = 400)

MAFE_mean_90_sentences_0_1 <- c('MAFE_mean_90_no_sentiment', 'MAFE_mean_90_Sentences_MNB_0_1', 'MAFE_mean_90_Sentences_LR_0_1', 'MAFE_mean_90_Sentences_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MAFE_mean_90_sentences_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Absolute Forecasting Error 90 Sentences -1_1', file = 'Mean Absolute Forecasting Error 90 Sentences 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

test <- temp_dataset

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MAFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

nlevels(temp_dataset$sentiment_approach)

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MAFE_mean_90_Sentences_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MAFE_mean_90_Sentences_0_1 <- plot_MAFE_mean_90_Sentences_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MAFE_mean_90_Sentences_0_1)
plot_MAFE_mean_90_Sentences_0_1
dev.off()

plot_MAFE_mean_90_Sentences_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.07,0.12)
plot_MAFE_mean_90_Sentences_0_1_combined <- plot_MAFE_mean_90_Sentences_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))


#Paragraphs 0_1
png(file='MAFE_mean_90 Paragraphs 0_1.png')

MAFE_mean_90_Paragraphs_0_1 <- c('MAFE_mean_90_no_sentiment', 'MAFE_mean_90_Paragraphs_MNB_0_1', 'MAFE_mean_90_Paragraphs_LR_0_1', 'MAFE_mean_90_Paragraphs_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MAFE_mean_90_Paragraphs_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Absolute Forecasting Error 90 Paragraphs -1_1', file = 'Mean Absolute Forecasting Error 90 Paragraphs 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MAFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MAFE_mean_90_Paragraphs_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MAFE_mean_90_Paragraphs_0_1 <- plot_MAFE_mean_90_Paragraphs_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MAFE_mean_90_Paragraphs_0_1)
plot_MAFE_mean_90_Paragraphs_0_1
dev.off()

plot_MAFE_mean_90_Paragraphs_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.07,0.12)
plot_MAFE_mean_90_Paragraphs_0_1_combined <- plot_MAFE_mean_90_Paragraphs_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MAFE_mean_90 0_1 Combined.png', width = 1000, height = 400)
combined_MAFE_mean_90_0_1 <- ggarrange(plot_MAFE_mean_90_Sentences_0_1_combined,
                                       plot_MAFE_mean_90_Paragraphs_0_1_combined, 
                                       ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MAFE_mean_90_0_1 <- combined_MAFE_mean_90_0_1 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MAFE_mean_90_0_1)
dev.off()

#sentences 0_4
png(file='MAFE_mean_90 Sentences 0_4.png', width = 500, height = 400)

MAFE_mean_90_sentences_0_4 <- c('MAFE_mean_90_no_sentiment', 'MAFE_mean_90_Sentences_MNB_0_4', 'MAFE_mean_90_Sentences_OLR_0_4', 'MAFE_mean_90_Sentences_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MAFE_mean_90_sentences_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Absolute Forecasting Error 90 Sentences -2_2', file = 'Mean Absolute Forecasting Error 90 Sentences 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MAFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

nlevels(temp_dataset$sentiment_approach)

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MAFE_mean_90_Sentences_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MAFE_mean_90_Sentences_0_4 <- plot_MAFE_mean_90_Sentences_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MAFE_mean_90_Sentences_0_4)
plot_MAFE_mean_90_Sentences_0_4
dev.off()

plot_MAFE_mean_90_Sentences_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.07,0.12)
plot_MAFE_mean_90_Sentences_0_4_combined <- plot_MAFE_mean_90_Sentences_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

#Paragraphs 0_4
png(file='MAFE_mean_90 Paragraphs 0_4.png')

MAFE_mean_90_Paragraphs_0_4 <- c('MAFE_mean_90_no_sentiment', 'MAFE_mean_90_Paragraphs_MNB_0_4', 'MAFE_mean_90_Paragraphs_OLR_0_4', 'MAFE_mean_90_Paragraphs_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MAFE_mean_90_Paragraphs_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Absolute Forecasting Error 90 Paragraphs -2_2', file = 'Mean Absolute Forecasting Error 90 Paragraphs 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MAFE_mean_90 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MAFE_mean_90_Paragraphs_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MAFE_mean_90_Paragraphs_0_4 <- plot_MAFE_mean_90_Paragraphs_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MAFE_mean_90_Paragraphs_0_4)
plot_MAFE_mean_90_Paragraphs_0_4
dev.off()

plot_MAFE_mean_90_Paragraphs_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_90, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (90% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.07,0.12)
plot_MAFE_mean_90_Paragraphs_0_4_combined <- plot_MAFE_mean_90_Paragraphs_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MAFE_mean_90 0_4 Combined.png', width = 1000, height = 400)
combined_MAFE_mean_90_0_4 <- ggarrange(plot_MAFE_mean_90_Sentences_0_4_combined,
                                       plot_MAFE_mean_90_Paragraphs_0_4_combined, 
                                       ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MAFE_mean_90_0_4 <- combined_MAFE_mean_90_0_4 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MAFE_mean_90_0_4)
dev.off()

### MAFE Mean 80 ####

#sentences 0_1
png(file='MAFE_mean_80 Sentences 0_1.png', width = 500, height = 400)

MAFE_mean_80_sentences_0_1 <- c('MAFE_mean_80_no_sentiment', 'MAFE_mean_80_Sentences_MNB_0_1', 'MAFE_mean_80_Sentences_LR_0_1', 'MAFE_mean_80_Sentences_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MAFE_mean_80_sentences_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Absolute Forecasting Error 80 Sentences -1_1', file = 'Mean Absolute Forecasting Error 80 Sentences 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

test <- temp_dataset

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MAFE_mean_80 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

nlevels(temp_dataset$sentiment_approach)

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MAFE_mean_80_Sentences_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MAFE_mean_80_Sentences_0_1 <- plot_MAFE_mean_80_Sentences_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MAFE_mean_80_Sentences_0_1)
plot_MAFE_mean_80_Sentences_0_1
dev.off()

plot_MAFE_mean_80_Sentences_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Sentence Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.07,0.12) 
plot_MAFE_mean_80_Sentences_0_1_combined <- plot_MAFE_mean_80_Sentences_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))


#Paragraphs 0_1
png(file='MAFE_mean_80 Paragraphs 0_1.png')

MAFE_mean_80_Paragraphs_0_1 <- c('MAFE_mean_80_no_sentiment', 'MAFE_mean_80_Paragraphs_MNB_0_1', 'MAFE_mean_80_Paragraphs_LR_0_1', 'MAFE_mean_80_Paragraphs_BERT_0_1')
temp_dataset <- subset(results_aggregated, select = MAFE_mean_80_Paragraphs_0_1)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Absolute Forecasting Error 80 Paragraphs -1_1', file = 'Mean Absolute Forecasting Error 80 Paragraphs 1_1.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MAFE_mean_80 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -1_1','LR -1_1','BERT -1_1')))
plot_MAFE_mean_80_Paragraphs_0_1 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MAFE_mean_80_Paragraphs_0_1 <- plot_MAFE_mean_80_Paragraphs_0_1 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MAFE_mean_80_Paragraphs_0_1)
plot_MAFE_mean_80_Paragraphs_0_1
dev.off()

plot_MAFE_mean_80_Paragraphs_0_1_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Paragraph Aggregation -1_1') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.07,0.12)
plot_MAFE_mean_80_Paragraphs_0_1_combined <- plot_MAFE_mean_80_Paragraphs_0_1_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MAFE_mean_80 0_1 Combined.png', width = 1000, height = 400)
combined_MAFE_mean_80_0_1 <- ggarrange(plot_MAFE_mean_80_Sentences_0_1_combined,
                                       plot_MAFE_mean_80_Paragraphs_0_1_combined, 
                                       ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MAFE_mean_80_0_1 <- combined_MAFE_mean_80_0_1 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MAFE_mean_80_0_1)
dev.off()

#sentences 0_4
png(file='MAFE_mean_80 Sentences 0_4.png', width = 500, height = 400)

MAFE_mean_80_sentences_0_4 <- c('MAFE_mean_80_no_sentiment', 'MAFE_mean_80_Sentences_MNB_0_4', 'MAFE_mean_80_Sentences_OLR_0_4', 'MAFE_mean_80_Sentences_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MAFE_mean_80_sentences_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Absolute Forecasting Error 80 Sentences -2_2', file = 'Mean Absolute Forecasting Error 80 Sentences 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MAFE_mean_80 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

nlevels(temp_dataset$sentiment_approach)

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MAFE_mean_80_Sentences_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MAFE_mean_80_Sentences_0_4 <- plot_MAFE_mean_80_Sentences_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MAFE_mean_80_Sentences_0_4)
plot_MAFE_mean_80_Sentences_0_4
dev.off()

plot_MAFE_mean_80_Sentences_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach), show.legend = FALSE) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Sentence Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.07,0.12)
plot_MAFE_mean_80_Sentences_0_4_combined <- plot_MAFE_mean_80_Sentences_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

#Paragraphs 0_4
png(file='MAFE_mean_80 Paragraphs 0_4.png')

MAFE_mean_80_Paragraphs_0_4 <- c('MAFE_mean_80_no_sentiment', 'MAFE_mean_80_Paragraphs_MNB_0_4', 'MAFE_mean_80_Paragraphs_OLR_0_4', 'MAFE_mean_80_Paragraphs_BERT_0_4')
temp_dataset <- subset(results_aggregated, select = MAFE_mean_80_Paragraphs_0_4)

temp_dataset <- as.data.frame(t(temp_dataset))

sentiment_models <- c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')

table_dataset <- temp_dataset
table_dataset <- as.data.frame(t(na.omit(t(table_dataset))))
table_dataset$SQA <- sentiment_models
dim(table_dataset)[2]
table_dataset <- table_dataset[c(dim(table_dataset)[2],1:(dim(table_dataset)[2]-1))]
names(table_dataset)[names(table_dataset) == 'SQA'] <- 'Sentiment Quantification Approach'
tab_df(table_dataset, title = 'Mean Absolute Forecasting Error 80 Paragraphs -2_2', file = 'Mean Absolute Forecasting Error 80 Paragraphs 2_2.doc')

temp_dataset$sentiment_models <- sentiment_models

temp_dataset <- melt(temp_dataset,id.vars = 'sentiment_models')
temp_dataset <- na.omit(temp_dataset)
temp_dataset <- transform(temp_dataset, value = as.numeric(value))

temp_dataset$MAFE_mean_80 <- temp_dataset$value
temp_dataset$sentiment_approach <- temp_dataset$variable

#manually declaring the order
temp_dataset <- temp_dataset %>% mutate(sentiment_models = factor(sentiment_models, levels = c('No sentiment','MNB -2_2','OLR -2_2','BERT -2_2')))
plot_MAFE_mean_80_Paragraphs_0_4 <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
plot_MAFE_mean_80_Paragraphs_0_4 <- plot_MAFE_mean_80_Paragraphs_0_4 + guides(shape = guide_legend(override.aes = list(size = 5)))
print(plot_MAFE_mean_80_Paragraphs_0_4)
plot_MAFE_mean_80_Paragraphs_0_4
dev.off()

plot_MAFE_mean_80_Paragraphs_0_4_combined <- ggplot() + 
  geom_point(data = temp_dataset, aes(x = sentiment_models, y = MAFE_mean_80, shape = sentiment_approach, size = 1.5, group = sentiment_approach)) + guides(size = FALSE) +
  ggtitle('Mean Absolute Forecasting Error Paragraph Aggregation -2_2') + theme(plot.title = element_text(hjust = 0.5), legend.key.size = unit(2,'line')) +
  labs(y = 'Mean Absolute Forecasting Error (80% sampled)', x = 'Sentiment Quantification Approach', shape = 'Method Used') + 
  scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13)) + ylim(0.07,0.12)
plot_MAFE_mean_80_Paragraphs_0_4_combined <- plot_MAFE_mean_80_Paragraphs_0_4_combined + guides(shape = guide_legend(override.aes = list(size = 5)))

png(file='MAFE_mean_80 0_4 Combined.png', width = 1000, height = 400)
combined_MAFE_mean_80_0_4 <- ggarrange(plot_MAFE_mean_80_Sentences_0_4_combined,
                                       plot_MAFE_mean_80_Paragraphs_0_4_combined, 
                                       ncol = 2, nrow = 1, widths = c(1,1.5))
combined_MAFE_mean_80_0_4 <- combined_MAFE_mean_80_0_4 + scale_shape_manual(values = c(1,2,3,4,5,6,7,9,10,11,12,13))
print(combined_MAFE_mean_80_0_4)
dev.off()

