############################################
# DSO 530 Final Project
# Group 1: Therese Anders, Ming-Min Yang, Jihyun Shin
# Data: ties_prepped.dta and mid_prepped.dta
# Task: Analysis
# Updated: 11/24/2015
############################################


# Loading the data and some additional data manipulation
rm(list = ls())
library(ggplot2)
library(dplyr)
library(gridExtra)
library(coefplot)

#setwd("/Users/thereseanders/Documents/UNI/USC/Fall\ 2015/DSO\ 530/Final\ Project")
setwd("/Users/jihyunshin/Dropbox/USC Coursework/DSO 530 Sanctions Project")
data <- read.csv("merged_new_final_cluster.csv", na.strings=c(""))
data <- select(data, -Column.1)

#Changing some variables to factor
names(data)
data$targetcosts <- as.factor(data$targetcosts)
data$ongoing_dum <- as.factor(data$ongoing_dum)
data$duration_sanction_clust5 <- as.factor(data$duration_sanction_clust5)
data$issue_mil_relevant_broad <- as.factor(data$issue_mil_relevant_broad)
data$issue_mil_relevant_narrow <- as.factor(data$issue_mil_relevant_narrow)
data$finaloutcome <- as.factor(data$finaloutcome)
data$carrots_dummy <- as.factor(data$carrots_dummy)
data$hihost_dummy <- as.factor(data$hihost_dummy)

#Recode finaloutcome variable
levels(data$finaloutcome) <- list(target_cap = c(6,7), 
                                  sender_cap = c(3,8),
                                  stalemate = 9,
                                  negotiation = 10)
table(data$finaloutcome)

# Model Selection (linear model)
## Determining the optimal number of variables
library(leaps)
mod_all <- regsubsets(hihost_dummy ~ duration_months_ties  + 
                        duration_months_ties_sq + 
                        ongoing_dum + 
                        issue_mil_relevant_narrow  + 
                        issue_mil_relevant_broad + 
                        sendercosts +
                        targetcosts +
                        sanction_start_year +
                        carrots_dummy +
                        finaloutcome, 
                      data = data,
                      nvmax = 10)
mod_all_summary <- summary(mod_all)
IVNum <- seq(from = 1, to = length(mod_all_summary$rsq), by = 1)
R2 <- mod_all_summary$rsq
AdjR2 <- mod_all_summary$adjr2
CP <- mod_all_summary$cp
BIC <- mod_all_summary$bic

#Building Data Frame
mod_select <- data.frame(IVNum, R2, AdjR2, CP, BIC)

#Plotting it
plot_R2 <- ggplot(mod_select, aes(x = IVNum, y = R2)) +
  geom_line(alpha = .6) +
  scale_x_discrete(breaks = seq(from = 1, to = nrow(mod_select), by = 1)) +
  labs(title = "Model Selection based on R2",
       x = "Number of Predictors",
       y = "R2") +
  geom_vline(xintercept = which.max(R2),
             color = "indianred3",
             alpha = .7) +
  geom_point(data = mod_select,
             aes(x = which.max(R2), y = max(R2)),
             color = "indianred3",
             size = 2) +
  theme_minimal()

plot_AdjR2 <- ggplot(mod_select, aes(x = IVNum, y = AdjR2)) +
  geom_line(alpha = .6) +
  scale_x_discrete(breaks = seq(from = 1, to = nrow(mod_select), by = 1)) +
  labs(title = "Model Selection based on Adjusted R2",
       x = "Number of Predictors",
       y = "Adjusted R2") +
  geom_vline(xintercept = which.max(AdjR2),
             color = "green4",
             alpha = .7) +
  geom_point(data = mod_select,
             aes(x = which.max(AdjR2), y = max(AdjR2)),
             color = "green4",
             size = 2) +
  theme_minimal()

plot_CP <- ggplot(mod_select, aes(x = IVNum, y = CP)) +
  geom_line(alpha = .6) +
  scale_x_discrete(breaks = seq(from = 1, to = nrow(mod_select), by = 1)) +
  labs(title = "Model Selection based on Cp",
       x = "Number of Predictors",
       y = "Cp") +
  geom_vline(xintercept = which.min(CP),
             color = "cornflowerblue",
             alpha = .7) +
  geom_point(data = mod_select,
             aes(x = which.min(CP), y = min(CP)),
             color = "cornflowerblue",
             size = 2) +
  theme_minimal()

plot_BIC <- ggplot(mod_select, aes(x = IVNum, y = BIC)) +
  geom_line(alpha = .6) +
  scale_x_discrete(breaks = seq(from = 1, to = nrow(mod_select), by = 1)) +
  labs(title = "Model Selection based on BIC",
       x = "Number of Predictors",
       y = "BIC") +
  geom_vline(xintercept = which.min(BIC),
             color = "darkgoldenrod2",
             alpha = .7) +
  geom_point(data = mod_select,
             aes(x = which.min(BIC), y = min(BIC)),
             color = "darkgoldenrod2",
             size = 2) +
  theme_minimal()


grid.arrange(plot_R2,
             plot_AdjR2,
             plot_CP,
             plot_BIC, nrow = 2)


## Variable Selection using the `leaps` package
mod_all_summary


# Looking at the correlation between variables
library(dplyr)
data_select <- data %>%
  select(hihost_dummy,
         duration_months_ties,
         duration_months_ties_sq,
         duration_sanction_clust5,
         ongoing_dum,
         issue_mil_relevant_broad,
         targetcosts,
         sanction_start_year,
         carrots_dummy)
data_select <- na.omit(data_select)

names(data_select)[names(data_select) == "ongoing_dum"] <- "OngoingDum"
names(data_select)[names(data_select) == "duration_months_ties"] <- "SanctionDuration"
names(data_select)[names(data_select) == "duration_months_ties_sq"] <- "SanctionDurationSq"
names(data_select)[names(data_select) == "duration_sanction_clust5"] <- "SanctionDurationClust5"
names(data_select)[names(data_select) == "issue_mil_relevant_broad"] <- "MilRelevantDum"
names(data_select)[names(data_select) == "targetcosts"] <- "TargetCost"
names(data_select)[names(data_select) == "sanction_start_year"] <- "StartYear"
names(data_select)[names(data_select) == "carrots_dummy"] <- "CarrotsDum"
names(data_select)[names(data_select) == "hihost_dummy"] <- "MilDispute"

library(corrplot)
data_cor <- data_select
str(data_cor)
data_cor$SanctionDurationClust5 <- as.numeric(data_cor$SanctionDurationClust5)
data_cor$MilDispute <- as.numeric(data_cor$MilDispute)
data_cor$TargetCost <- as.numeric(data_cor$TargetCost)
data_cor$CarrotsDum <- as.numeric(data_cor$CarrotsDum)
data_cor$MilRelevantDum <- as.numeric(data_cor$MilRelevantDum)
vars_cor <- cor(data_cor, use="pairwise.complete.obs")
par(mfrow = c(1,1))
corrplot(vars_cor, type = "upper", main = "Correlation Matrix")

# Splitting in testing and training
set.seed(123)
data_select_num <- data_select
data_select_num$MilDispute <- as.numeric(data_select_num$MilDispute)

train <- sample(1:nrow(data_select), 500)
test <- -train
training_data <- data_select[train,]
training_data_num <- data_select_num[train,]
testing_data <- data_select[test,]
testing_data_num <- data_select_num[test,]

testing_y <- testing_data$MilDispute
testing_y_num <- testing_data_num$MilDispute

formula_base <- MilDispute ~ SanctionDuration  + 
  SanctionDurationSq + 
  OngoingDum + 
  MilRelevantDum + 
  TargetCost +
  StartYear +
  CarrotsDum


mod_lm <- lm(formula_base, data = training_data_num)
summary(mod_lm)
turning_point <- -(mod_lm$coefficients[2])/(2*mod_lm$coefficients[3])
turning_point #599.4017 
predicted_y_lm <- predict(mod_lm, newdata = testing_data_num)
mse_lm <- mean((predicted_y_lm - testing_y_num)^2)
mse_lm


# Assessing the predictive power of different models
## Simple logit model for Hostility Dummy
mod_logit <- glm(formula_base, 
                 data = training_data,
                 family = "binomial")
summary(mod_logit)
predicted_y_logit <- predict(mod_logit,
                             newdata = testing_data,
                             type = "response")
logistic_predicted_success <- ifelse(predicted_y_logit > 0.5, 1, 0)
mse_logit <- mean(testing_y != logistic_predicted_success)
mse_logit


### Showing the results of the linear and logistic regression models
library(memisc)
tab_lm_logit <- mtable('Linear Model' = mod_lm,
                       'Logit Model' = mod_logit,
                       summary.stats = c('R-squared','F','N'))
tab_lm_logit
detach("package:memisc", unload=TRUE)

## LDA
detach("package:dplyr", unload=TRUE)
library(MASS)
lda_mod <- lda(formula_base, data = training_data)
predict_lda <- predict(lda_mod, testing_data)
predicted_y_lda <- predict_lda$class
mse_lda <- mean(testing_y != predicted_y_lda)
mse_lda

## QDA
qda_mod <- qda(formula_base, data = training_data)
predict_qda <- predict(qda_mod, testing_data)
predicted_y_qda <- predict_qda$class
mse_qda <- mean(testing_y != predicted_y_qda)
mse_qda

# Tree Model
library(tree)
tree_model <- tree(formula_base,
                   training_data)
summary(tree_model)
plot(tree_model) #Plotting the model
text(tree_model, cex = .7)
Tree_y_predict <- predict(tree_model, 
                          testing_data, 
                          type = "class")
Tree_MSE <- mean(testing_y!=Tree_y_predict)
Tree_MSE

# Bagging
set.seed(123)
library(randomForest)
bagging_model <- randomForest(formula_base,
                              training_data,
                              mtry=7,
                              importance=TRUE)
bagging_model
y_pred_bagging <- predict(bagging_model, testing_data, type="class") 
mse_bagging <- mean(testing_y != y_pred_bagging)
mse_bagging

## Random Forests
rf_model <- randomForest(formula_base,
                         training_data,
                         mtry=3,
                         importance=TRUE,
                         ntree = 1000)
rf_model
y_pred_rf <- predict(rf_model, testing_data, type="class") 
rf_MSE <- mean(testing_y != y_pred_rf)
rf_MSE

### Comparison Table
Models <- c("Linear", "Logit", "LDA", "QDA", "Tree", "Bagging", "Random Forest")
Misclassification_Error <- round(c(mse_lm, 
                                   mse_logit,
                                   mse_lda, 
                                   mse_qda,
                                   Tree_MSE,
                                   mse_bagging,
                                   rf_MSE),4)
tab_comp <- cbind(Models, Misclassification_Error)
tab_comp


mod_lm_bivar <- lm(MilDispute ~ SanctionDuration,
                   data = data_select)
mod_logit_bivar <- glm(MilDispute ~ SanctionDuration,
                       data = data_select,
                       family = "binomial")
par(mfrow = c(1,1), mar=c(5.1,4.1,4.1,2.1))
plot(y = data_select$MilDispute,
     x = data_select$SanctionDuration,
     main = "Comparing Linear and Logistic Regression",
     xlab = "Sanction Duration (months)",
     ylab = "Militarized Interstate Dispute")
abline(mod_lm_bivar, col="red")
curve(predict(mod_logit_bivar,
              data.frame(SanctionDuration = x),
              type="resp"),
      add=TRUE,
      col = "blue",
      lty = "dashed")
legend(20, 0.8, 
       c("Linear","Logit"),
       col = c("red", "blue"),
       lty = c("solid", "dashed"),
       cex = .8)


# Looking at clustered IV
## Graphically presenting the results of the clustering
ggplot() +
  geom_density(data = data_select, 
               aes(x = SanctionDuration, fill = SanctionDurationClust5),
               color = NA,
               alpha = .6) +
  scale_fill_discrete(name = "Hierarchical \nCluster",
                      labels = c("Cluster 1",
                                 "Cluster 2",
                                 "Cluster 3",
                                 "Cluster 4",
                                 "Cluster 5")) +
  geom_line(data = data_select, 
            aes(x = SanctionDuration), 
            stat = "density",
            size = 2,
            alpha = .6) +
  theme_minimal() +
  ggtitle(expression(atop("Duration of Sanctions episodes",
                          atop(italic("Continuous variable and 5-cluster version"), 
                               "")))) +
  xlab("Sanction Duration in Months") +
  ylab("Density")

## Model comparison using clustered variable
formula_clust <- MilDispute ~ SanctionDurationClust5  + 
  OngoingDum + 
  MilRelevantDum + 
  TargetCost +
  StartYear +
  CarrotsDum

mod_logit_clust <- glm(formula_clust, 
                       data = training_data,
                       family = "binomial")
summary(mod_logit_clust)







