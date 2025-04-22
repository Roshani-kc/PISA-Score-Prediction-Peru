#OLS

library(haven) # to read .sav format
library(tibble)
library(tidyverse)
library(caret)
library(ggplot2)

df <- read_sav("pisa_data/2022/filtered_PER.sav")

#mutating NA with the mean of columns
df2 <- df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))


#creating training and testing datasets
index_1 <-  initial_split(df2, prop=0.7)
train_1 <- training(index_1)
test_1 <- testing(index_1)

lm_model <- lm(PVMATH ~ ., data = train_1)

# Predicting PVMATH values on the test set
predictions <- predict(lm_model, newdata = test_1)

# Printing the first few predicted values
head(predictions)

# Creating a data frame for the true and predicted values
plot_data <- data.frame(Actual = test_1$PVMATH, Predicted = predictions)

# Creating the scatter plot
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "Test Data",
       x = "True Values (PVMATH)",
       y = "Predicted Values (PVMATH)")

mse_test <- mean((test_1$PVMATH - predictions)^2)

# checking the plot and mse of the training data
predictions <- predict(lm_model, newdata = train_1)
mse_train <- mean((train_1$PVMATH - predictions)^2)

plot_data_train <- data.frame(Actual = train_1$PVMATH, Predicted = predictions)

# Creating the scatter plot
ggplot(plot_data_train, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "Train Data",
       x = "True Values (PVMATH)",
       y = "Predicted Values (PVMATH)")

#creating a table to display the MSE of training and test data

# Creating a table
mse_table <- tibble(
  Dataset = c("Train", "Test"),
  MSE = c(mse_train, mse_test)
)

# Display the table
print(mse_table)

