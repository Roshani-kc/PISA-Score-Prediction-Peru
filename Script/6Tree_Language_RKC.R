library(haven)
# Helper packages 
library(tidyverse)  
library(rsample)
# Modeling packages
library(rpart)       # direct engine for decision tree
library(caret)       # meta engine for decision tree
# Model interpretability packages
library(rpart.plot)  # plotting decision trees
library(vip)         # variable importance
library(pdp)         # variable effects
library(ggplot2)     # for plots
library(grid)

#Used Random selection database for backup 
####1. Loading Database ####
df <- read_sav("pisa_data/2022/FinalRandomSelectionLanguage.sav")

####2. Splitting ####
set.seed(123)

split  <- initial_split(df, prop = 0.7)
train  <- training(split)
test   <- testing(split)

####3.Basic Regression####

decision_tree1 <- rpart(formula = PVREAD ~ .,
                  data    = train,
                  method  = "anova"
)

decision_tree1

rpart.plot(decision_tree1) # plot the decision tree

plotcp(decision_tree1) # checking the best tree size
# using Breiman et al. (1984) suggestion to set the optimum tree size to 7

decision_tree2 <- rpart(
  formula = PVREAD ~ .,
  data    = train,
  method  = "anova", 
  control = list(cp = 0, xval = 6)
)

plotcp(decision_tree2)
abline(v = 7, lty = "dashed")

#
decision_tree1$cptable

#
decision_tree3 <- rpart(
  formula = PVREAD ~ .,
  data    = train,
  method  = "anova", 
  control = list(minsplit = 10,
                 maxdepth = 12,
                 xval     = 6)
)


#Create the tuning grid
hyper_grid <- expand.grid(
                 minsplit = seq(5, 15, 1),
                 maxdepth = seq(8, 15, 1)
              )

#Loop over all possible value combinations of the grid with rpart
models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]

  # train a model and store in the list
  models[[i]] <- rpart(
    formula = PVREAD ~ .,
    data    = train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
    )
}

#Create a function to extract min error associated
#with the optimal CP (alpha) for each model

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp     <- x$cptable[min, "CP"] 
  return(cp)
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
  return(xerror)
}

hyper_grid %>%
      mutate(
        cp    = map_dbl(models, get_cp),
        error = map_dbl(models, get_min_error)
        ) %>%
      arrange(desc(error)) %>%
      slice_max(n = 5, order_by = desc(error))



##use this last tree to predict the testing dataset

optimal_tree <- rpart(
  formula = PVREAD ~ .,
  data    = train,
  method  = "anova",
  control = list(minsplit = 19, maxdepth = 10, cp = 0.01)
)

####Decision####
decision_tree_pred_train <- predict(optimal_tree, newdata = train)
decision_tree_pred_test  <- predict(optimal_tree, newdata = test)

decision_tree_rmse_train <- RMSE(pred = decision_tree_pred_train, obs = train$PVREAD)
decision_tree_rmse_test  <- RMSE(pred = decision_tree_pred_test,  obs = test$PVREAD)

####Plots####
#plot variable importance (feature interpretation)
vip(optimal_tree, num_features = 30, bar = FALSE)


#plotting the true vs predicted values
# Create a data frame for true and predicted values
plot_data_train <- data.frame(
  True = train$PVREAD,
  Predicted = decision_tree_pred_train
)

plot_data_test <- data.frame(
  True = test$PVREAD,
  Predicted = decision_tree_pred_test
)

# Plot true vs predicted values for training set
ggplot(plot_data_train, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Training Set)",
       x = "True Values",
       y = "Predicted Values")

# Plot true vs predicted values for testing set
ggplot(plot_data_test, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Testing Set)",
       x = "True Values",
       y = "Predicted Values")

####4. Bagging####

#Bagging with caret

#Setup 10-fold CV
setupCV <- trainControl(method = "cv",  number = 10) 

#Train a CV bagged tree
bag1 <- train(
  PVREAD ~ .,
  data = train,
  method = "treebag",
  trControl = setupCV,
  importance = TRUE
)

bag1
#Plot 20-most important variables
plot(varImp(bag1), 20)

# checking if it has improved with respect to the single decision tree
  
bag_pred_train <- predict(bag1, newdata = train)
bag_pred_test  <- predict(bag1, newdata = test)

bag_rmse_train <- RMSE(pred = bag_pred_train, obs = train$PVREAD)
bag_rmse_test  <- RMSE(pred = bag_pred_test,  obs = test$PVREAD)



#plotting the true vs predicted values
# Create a data frame for true and predicted values
plot_data_train_bag <- data.frame(
  True = train$PVREAD,
  Predicted = bag_pred_train
)

plot_data_test_bag <- data.frame(
  True = test$PVREAD,
  Predicted = bag_pred_test
)

# Plot true vs predicted values for training set
ggplot(plot_data_train_bag, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Training Set)",
       x = "True Values",
       y = "Predicted Values")

# Plot true vs predicted values for testing set
ggplot(plot_data_test_bag, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Testing Set)",
       x = "True Values",
       y = "Predicted Values")


####5.  Random Forests ####
library(ranger)

# Number of vars
p <- length(setdiff(names(train), "PVREAD"))

# Train a default RF
rf1 <- ranger(
  PVREAD ~ ., 
  data = train,
  mtry = floor(p / 3),
  respect.unordered.factors = "order",
  seed = 123
)

default_rmse <- sqrt(rf1$prediction.error)


#tuning some of the typical hyperparameters
# Hyperparam grid
hyper_grid_1 <- expand.grid(mtry            = floor(c(seq(2,p, length.out = 5), p/3)),
                            replace         = c(TRUE, FALSE),
                            sample.fraction = c(.5, .65, .8),
                            min.node.size   = c(1, 3, 5, 10), 
                            rmse            = NA                                               
)

# Execute full grid search
for(i in seq_len(nrow(hyper_grid_1))) {
  # Fit model for the i-th hyperparameter combination 
  fit <- ranger(
    formula         = PVREAD ~ ., 
    data            = train, 
    num.trees       = 10*p,
    mtry            = hyper_grid_1$mtry[i],
    min.node.size   = hyper_grid_1$min.node.size[i],
    replace         = hyper_grid_1$replace[i],
    sample.fraction = hyper_grid_1$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )
  # export OOB error 
  hyper_grid_1$rmse[i] <- sqrt(fit$prediction.error)
}

# assess top 10 models
hyper_grid_1 %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)


# we've identified the optimal rf through ranger
# keeping the results from ranger with the hyperparameters
# num.trees = 10p, mtry = 7, min.node.size = 1, sample.fraction = .5, replace = F

# Re-run model with impurity-based variable importance
rf_optim <- ranger(
  formula         = PVREAD ~ ., 
  data            = train, 
  num.trees       = 10*p,
  mtry            = 7,
  min.node.size   = 1,
  sample.fraction = .50,
  replace         = FALSE,
  importance      = "impurity",
  respect.unordered.factors = "order",
  verbose         = FALSE,
  seed            = 123
)

vip::vip(rf_optim,    num_features = 25, bar = FALSE)

rf_pred_train <- predict(rf_optim, data = train)
rf_pred_test  <- predict(rf_optim, data = test)

rf_rmse_train <- RMSE(pred = rf_pred_train$predictions, obs = train$PVREAD)
rf_rmse_test  <- RMSE(pred = rf_pred_test$predictions,  obs = test$PVREAD)



####6. Gradient Boosting Machines####


library(gbm) # Load gbm package

set.seed(123)

gbm_basic <- gbm(
  formula = PVREAD ~ .,
  data = train,
  distribution = "gaussian",  # SSE loss function
  n.trees = 2000,
  shrinkage = 0.1,
  interaction.depth = 3,
  n.minobsinnode = 10,
  cv.folds = 10
)

# Our GBM has fitted 2000 trees, the one that gives the min CV error is:
best <- which.min(gbm_basic$cv.error)
best

# What RMSE corresponds to that 'best' one?
sqrt(gbm_basic$cv.error[best])
# Plot of performance
gbm.perf(gbm_basic, method = "cv")


# Create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage         = c(.05, .1, .2), # Values between .05 and .2 work well
  interaction.depth = c(1, 3, 5, 7),
  n.minobsinnode    = c(5, 10, 15),
  optimal_trees     = NA,            # Result storage
  min_RMSE          = NA             # Result storage
)

# total number of combinations
nrow(hyper_grid)


# Grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # Reproducibility
  set.seed(123)
  
  # Train model
  gbm_tuned <- gbm(
    formula           = PVREAD ~ .,
    distribution      = "gaussian",
    data              = train,
    n.trees           = 2000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage         = hyper_grid$shrinkage[i],
    n.minobsinnode    = hyper_grid$n.minobsinnode[i],
    cv.folds          = 5,
    n.cores           = NULL, # Uses all cores by default
    verbose           = FALSE
  )
  
  # Save min training error and number of optimal trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm_tuned$cv.error)
  hyper_grid$min_RMSE[i]      <- sqrt(min(gbm_tuned$cv.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


# Create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage         = seq(0.05, 0.1, length.out = 4), 
  interaction.depth = c(1, 3, 5, 7),    # from the hyper grid output 
  n.minobsinnode    = c(5,10,15),    # taking the best values from the hyper grid output
  optimal_trees     = NA,            # Result storage
  min_RMSE          = NA             # Result storage
)

# total number of combinations
nrow(hyper_grid)

# Grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # Reproducibility
  set.seed(123)
  
  # Train model
  gbm_tuned <- gbm(
    formula           = PVREAD ~ .,
    distribution      = "gaussian",
    data              = train,
    n.trees           = 920,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage         = hyper_grid$shrinkage[i],
    n.minobsinnode    = hyper_grid$n.minobsinnode[i],
    cv.folds          = 5,
    n.cores           = NULL, # Uses all cores by default
    verbose           = FALSE
  )
  
  # Save min training error and number of optimal trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm_tuned$cv.error)
  hyper_grid$min_RMSE[i]      <- sqrt(min(gbm_tuned$cv.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

gbm_rmse_test <- sqrt(mean((predict(gbm_tuned, newdata = test, n.trees = gbm_tuned$gbm.call$best.trees) - test$PVREAD)^2))
gbm_rmse_train <- min(hyper_grid$min_RMSE)


####Store MSE values for each method####
mse_values <- data.frame(
  Method = c("Decision Tree", "Bagging", "Random Forest", "Gradient Boosting"),
  Train_RMSE = c(decision_tree_rmse_train, bag_rmse_train, rf_rmse_train, gbm_rmse_train),
  Test_RMSE = c(decision_tree_rmse_test, bag_rmse_test, rf_rmse_test, gbm_rmse_test),
  Train_MSE = round(c(decision_tree_rmse_train, bag_rmse_train, rf_rmse_train, gbm_rmse_train)**2,3),
  Test_MSE = round(c(decision_tree_rmse_test, bag_rmse_test, rf_rmse_test, gbm_rmse_test)**2,3)
)

# Print and save MSE values
print(mse_values)
write.csv(mse_values, "mse_values_tree_language.csv", row.names = FALSE)

# Plotting the predicted vs true values from different random tree models.

# Define multiplot function to arrange plots
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols),
                     byrow = TRUE)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Define theme with smaller text size
small_theme <- theme_minimal() +
  theme(
    text = element_text(size = 6)
  )

# Plot true vs predicted values for each method
pdf("true_vs_predicted_plots_Language.pdf")

# Plot true vs predicted values for Decision Tree
plot_data_train_decision_tree <- data.frame(
  True = train$PVREAD,
  Predicted = decision_tree_pred_train
)

plot_data_test_decision_tree <- data.frame(
  True = test$PVREAD,
  Predicted = decision_tree_pred_test
)

plot_decision_tree_train <- ggplot(plot_data_train_decision_tree, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Decision Tree - Training Set)",
       x = "True Values (PVREAD)",
       y = "Predicted Values") +
  small_theme

plot_decision_tree_test <- ggplot(plot_data_test_decision_tree, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Decision Tree - Testing Set)",
       x = "True Values (PVREAD)",
       y = "Predicted Values") +
  small_theme

# Plot true vs predicted values for Bagging
plot_data_train_bag <- data.frame(
  True = train$PVREAD,
  Predicted = bag_pred_train
)

plot_data_test_bag <- data.frame(
  True = test$PVREAD,
  Predicted = bag_pred_test
)

plot_bag_train <- ggplot(plot_data_train_bag, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Bagging - Training Set)",
       x = "True Values (PVREAD)",
       y = "Predicted Values") +
  small_theme

plot_bag_test <- ggplot(plot_data_test_bag, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Bagging - Testing Set)",
       x = "True Values (PVREAD)",
       y = "Predicted Values") +
  small_theme

# Plot true vs predicted values for Random Forest
plot_data_train_rf <- data.frame(
  True = train$PVREAD,
  Predicted = rf_pred_train$predictions
)

plot_data_test_rf <- data.frame(
  True = test$PVREAD,
  Predicted = rf_pred_test$predictions
)

plot_rf_train <- ggplot(plot_data_train_rf, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Random Forest - Training Set)",
       x = "True Values (PVREAD)",
       y = "Predicted Values") +
  small_theme

plot_rf_test <- ggplot(plot_data_test_rf, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Random Forest - Testing Set)",
       x = "True Values (PVREAD)",
       y = "Predicted Values") +
  small_theme

# Plot true vs predicted values for Gradient Boosting
plot_data_train_gbm <- data.frame(
  True = train$PVREAD,
  Predicted = predict(gbm_basic, newdata = train, n.trees = best)
)

plot_data_test_gbm <- data.frame(
  True = test$PVREAD,
  Predicted = predict(gbm_basic, newdata = test, n.trees = best)
)

plot_gbm_train <- ggplot(plot_data_train_gbm, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Gradient Boosting - Training Set)",
       x = "True Values (PVREAD)",
       y = "Predicted Values") +
  small_theme

plot_gbm_test <- ggplot(plot_data_test_gbm, aes(x = True, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  labs(title = "True vs Predicted Values (Gradient Boosting - Testing Set)",
       x = "True Values (PVREAD)",
       y = "Predicted Values") +
  small_theme

# Arrange plots and save
multiplot(plot_decision_tree_train, plot_decision_tree_test, plot_bag_train, plot_bag_test,
          plot_rf_train, plot_rf_test, plot_gbm_train, plot_gbm_test,
          cols = 2)

dev.off()
multiplot(plot_decision_tree_train, plot_decision_tree_test, plot_bag_train, plot_bag_test,
          plot_rf_train, plot_rf_test, plot_gbm_train, plot_gbm_test,
          cols = 2)

