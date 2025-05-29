## Ben Taylor
### ECON 453
#### PS2
##### 02/26/2025

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(data.table)
library("readxl")
library(gt)
library(dplyr)


# Set seed
set.seed(418518)

# Set working directory 
setwd("C:/Users/ual-laptop/Desktop/Data")

#Extract Data for both questions
data <- read_xlsx("pset34_data.xlsx", sheet =2)
data_2 <- read_xlsx("pset34_data.xlsx", sheet =1)

#1.1
data_2 <- data_2 %>%
  mutate(
    age2 = age^2,
    educ2 = education^2,
    logwage = log(earnings),
    region_midwest = ifelse(region == "Midwest", 1, 0),
    region_south = ifelse(region == "South", 1, 0),
    region_west = ifelse(region == "West", 1, 0)
  )

 # 1.2
model_log <- lm(logwage ~ age + education + region_midwest + region_south + region_west, data = data_2)
summary(model_log)
anova(model_log)

 #1.3
model_wage <- lm(earnings ~ age + education + region_midwest + region_south + region_west, data = data_2)
summary(model_log)
summary(model_wage)

 #1.4
model_log_age2 <- lm(logwage ~ age + age2 + education + region_midwest + region_south + region_west, data = data_2)
summary(model_log_age2)
anova(model_log, model_log_age2)
 
 #1.5
model_log_educ2 <- lm(logwage ~ age + education + educ2 + region_midwest + region_south + region_west, data = data_2)
summary(model_log_educ2)
anova(model_log, model_log_educ2)


##### QUESTION 2

 #2.1
library(ggplot2)
ggplot(data, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Wage vs. Age", x = "Age", y = "Wage")

 #2.2
library(modelsummary)

train <- data[1:120, ]
val <- data[121:160, ]

train <- train %>% mutate(Age2 = Age^2)
val <- val %>% mutate(Age2 = Age^2)

mod1 <- lm(Wage ~ Graduate + Age, data = train)
mod2 <- lm(Wage ~ Graduate + Age + Age2, data = train)

modelsummary(
  list("Model 1" = mod1, "Model 2" = mod2),
  statistic = c("std.error", "p.value"),
  gof_omit = "AIC|BIC|Log.Lik"
)

#2.3
new_obs <- data.frame(Graduate = 1, Age = 30, Age2 = 900)
predict(mod1, newdata = new_obs[, c("Graduate", "Age")])  
predict(mod2, newdata = new_obs)


 #2.4 
b1 <- coef(mod2)["Age"]
b2 <- coef(mod2)["Age2"]

max_age <- -b1 / (2 * b2)
max_age

 #2.5
mae  <- function(obs, pred) mean(abs(obs - pred))
mse  <- function(obs, pred) mean((obs - pred)^2)

train_mae1 <- mean(abs(residuals(mod1)))
train_mse1 <- mean(residuals(mod1)^2)
train_mae2 <- mean(abs(residuals(mod2)))
train_mse2 <- mean(residuals(mod2)^2)

pred1_val <- predict(mod1, newdata = val)
pred2_val <- predict(mod2, newdata = val)
val_mae1  <- mae(val$Wage, pred1_val)
val_mse1  <- mse(val$Wage, pred1_val)
val_mae2  <- mae(val$Wage, pred2_val)
val_mse2  <- mse(val$Wage, pred2_val)

error_summary <- data.frame(
  Dataset = rep(c("Train", "Validation"), each = 2),
  Model   = rep(c("Model 1", "Model 2"), times = 2),
  MAE     = c(train_mae1, train_mae2, val_mae1, val_mae2),
  MSE     = c(train_mse1, train_mse2, val_mse1, val_mse2)
)
print(error_summary)
