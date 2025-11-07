library(dplyr)
library(ggplot2)
library(ISLR2)
library(boot)




# Part 1 EDA 

#Removing Minutes from Time Variable 
VicRoadFatalData$ACCIDENTTIME <- format(strptime(VicRoadFatalData$ACCIDENTTIME, format = "%H:%M"), format = "%H")

Fatal.Accidents <- filter(VicRoadFatalData, fatal == "TRUE")
Non.Fatal.Accidents <- filter(VicRoadFatalData, fatal == "FALSE")

# General accident spread by gender
M.Accident.Percent <- sum(VicRoadFatalData$SEX == "M")/200000*100
F.Accident.Percent <- sum(VicRoadFatalData$SEX == "F")/200000*100

# Fatal Accident Percentages
M.Fatal.Percent <- sum(Fatal.Accidents$SEX == "M")/3387*100
F.Fatal.Percent <- sum(Fatal.Accidents$SEX == "F")/3387*100

#Tables and visualizations of distributions
hist(VicRoadFatalData$AGE, xlab = "Age", main = "Histogram Showing Age Distribution")
hist(Fatal.Accidents$AGE, xlab = "Age", main = "Histogram Showing Age Distribution For Fatal Accidents")

table(VicRoadFatalData$fatal, VicRoadFatalData$VEHICLE_TYPE)
table(VicRoadFatalData$HELMET_BELT_WORN, VicRoadFatalData$SEX)
table(Fatal.Accidents$HELMET_BELT_WORN, Fatal.Accidents$SEX)

#EDA for accident conditions
ggplot(VicRoadFatalData, aes(x=DAY_OF_WEEK)) +
  geom_bar(fill='steelblue') +
  labs(x='Day of Week', y='Accident Frequency')

ggplot(VicRoadFatalData, aes(x=ACCIDENTTIME)) +
  geom_bar(fill='steelblue') +
  labs(x='24 Hour Time', y='Accident Frequency')

ggplot(Fatal.Accidents, aes(x=ACCIDENTTIME)) +
  geom_bar(fill='steelblue') +
  labs(x='24 Hour Time', y='Fatal Accident Frequency')

hist(VicRoadFatalData$SPEED_ZONE, xlab = "Speed", main = "Speed Distribution")
hist(Fatal.Accidents$SPEED_ZONE, xlab = "Speed", main = "Fatal Speed Distribution")

VicRoadFatalData %>% ggplot() + 
  geom_bar(mapping=aes(x=fatal, colour=SURFACE_COND)) +
  labs(x='Fatality')

Fatal.Accidents %>% ggplot() + 
  geom_bar(mapping=aes(x=fatal, colour=SURFACE_COND)) +
  labs(x='Fatality')

VicRoadFatalData %>% ggplot() + 
  geom_bar(mapping=aes(x=fatal, colour=ACCIDENT_TYPE)) +
  labs(x='Fatality')
  
Fatal.Accidents %>% ggplot() + 
  geom_bar(mapping=aes(x=fatal, colour=ACCIDENT_TYPE)) +
  labs(x='Fatality')

Accident.Mosaic <- table(VicRoadFatalData$SURFACE_COND, VicRoadFatalData$ROAD_GEOMETRY)
mosaicplot(Accident.Mosaic)
Fatal.Mosaic <- table(Fatal.Accidents$SURFACE_COND, Fatal.Accidents$ROAD_GEOMETRY)
mosaicplot(Fatal.Mosaic)

table(VicRoadFatalData$fatal, VicRoadFatalData$LIGHT_CONDITION, VicRoadFatalData$ROAD_SURFACE_TYPE)






#Part 2 Modelling Relationship

set.seed(123) 
VicRoadFatalData$fatal <- as.numeric(VicRoadFatalData$fatal == "TRUE")
trainIndex <- createDataPartition(VicRoadFatalData$fatal, p = 0.7, list = FALSE)
train <- VicRoadFatalData[trainIndex, ]
test <- VicRoadFatalData[-trainIndex, ]

train_response <- train$fatal
train_predictors <- as.matrix(train[, -c(which(names(train) == "fatal"))])

#Creating Initial Model to see relevant predictors
model0 <- glm(fatal ~ AGE + SEX + VEHICLE_TYPE + HELMET_BELT_WORN + TOTAL_NO_OCCUPANTS + ACCIDENTTIME + ACCIDENT_TYPE + ROAD_GEOMETRY + ATMOSPH_COND + SPEED_ZONE + SURFACE_COND + ROAD_SURFACE_TYPE + LIGHT_CONDITION, data = train, family = binomial)

#Removed irrelevant predictors
model1 <- glm(fatal ~ AGE + SEX + HELMET_BELT_WORN + ACCIDENTTIME + ACCIDENT_TYPE + SPEED_ZONE + ROAD_SURFACE_TYPE + SURFACE_COND + ROAD_GEOMETRY + LIGHT_CONDITION + VEHICLE_TYPE + TOTAL_NO_OCCUPANTS, data = train, family = binomial)
AIC1 <- AIC(model1)
BIC1 <- BIC(model1)



# Specify class weights
class_weights <- ifelse(train$fatal == "1", 0.98, 0.02)

# Train your GLM model with class weights
trained_glm_model <- glm(fatal ~ AGE + SEX + HELMET_BELT_WORN + ACCIDENTTIME + ACCIDENT_TYPE + SPEED_ZONE + SURFACE_COND + LIGHT_CONDITION + VEHICLE_TYPE + TOTAL_NO_OCCUPANTS, data = train, family = binomial, weights = class_weights)
summary(trained_glm_model)
AICT <- AIC(trained_glm_model)
BICT <- BIC(trained_glm_model)

predictions <- predict(trained_glm_model, newdata = test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
confusion_matrix <- table(predicted_classes, test$fatal)

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * precision * recall / (precision + recall)





#Part 3

#Lasso and Ridge regression

x_var <- model.matrix(fatal ~ AGE + SEX + HELMET_BELT_WORN +  + VEHICLE_TYPE + TOTAL_NO_OCCUPANTS, data = VicRoadFatalData)[, -1]
y_var <- VicRoadFatalData$fatal[!is.na(VicRoadFatalData$fatal)]

# Fit a lasso and ridge regression

library(glmnet)

grid <- 10^seq(10, -2, length = 100)

ridge.mod <- glmnet(x_var, y_var, alpha = 0, lambda = grid)
lasso.mod <- glmnet(x_var, y_var, alpha = 1, lambda = grid)

# Finding best lambda

set.seed(1)
cv.ridge <- cv.glmnet(x_var, y_var, alpha = 0)
cv.lasso <- cv.glmnet(x_var, y_var, alpha = 1)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.lasso <- cv.lasso$lambda.min

# making predictions and finding coefficients

ridge.pred <- predict(ridge.mod , s = bestlam.ridge , newx = x_var)
predict(ridge.mod , type = "coefficients", s = bestlam.ridge)

lasso.pred <- predict(lasso.mod , s = bestlam.lasso , newx = x_var)
predict(lasso.mod , type = "response", s = bestlam.lasso)





#Predict 2500 possible accidents
class_weights2 <- ifelse(VicRoadFatalData$fatal == "1", 0.98, 0.02)
predictive_glm_model <- glm(fatal ~ AGE + SEX + HELMET_BELT_WORN + VEHICLE_TYPE + TOTAL_NO_OCCUPANTS, data = VicRoadFatalData, family = binomial, weights = class_weights2)
predicted_probs <- predict(predictive_glm_model, Drivers_Eval, type = "response")

Drivers_Eval$predicted_prob <- predicted_probs
sorted_data_df <- Drivers_Eval[order(-Drivers_Eval$predicted_prob), ]

top_2500_observations <- head(sorted_data_df, 2500)
