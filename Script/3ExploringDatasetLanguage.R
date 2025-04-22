############### Assignment1 ###################
#####      Predicting Language Score      #####
#####          Exploring Data             #####
################OutlineSummary#################

#fitting the data
library(haven) # to read .sav format
library(tibble)
library(tidyverse)

#data loading 
df <- read_sav("pisa_data/2022/filtered_PER_Language.sav")

# Setting PVREAD is the response variable
response_variable <- "PVREAD"

# Convert labelled factors to numeric
df <- as.data.frame(lapply(df, function(x) {
  if (inherits(x, "labelled")) {
    as.numeric(x)
  } else {
    x
  }
}))

numerical_columns <- sapply(df, is.numeric)
numeric_cols <- names(df)[numerical_columns]

# Impute missing values with mean

df2 <- df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

#for random selection of columns
all_columns <- names(df2)
# Remove "PVREAD" from the list of all columns
# we will remove pvread from the database before random selection, and add them later before saving
all_columns <- setdiff(all_columns, c("PVREAD"))

# Set the seed for reproducibility
set.seed(123)

# Randomly sample 24 column names
selected_columns <- sample(all_columns, 25)

# Add "PVREAD" back to the selected column names
selected_columns <- c("PVREAD", selected_columns)

# Subset the dataframe with the selected column names
df_selected <- df2[selected_columns]

# saving the dataframe created from randomly selected columns
write_sav(df_selected, "pisa_data/2022/FinalRandomSelectionLanguage.sav")
#Using this for bestsubset in ols 
#and will be working with this database on different models to test
#also as a substitute if database below is not working 


#for PVREAD
# Fit the OLS regression model
lm_model <- lm(PVREAD ~ ., data = df2)

# Extract coefficients and p-values from the summary of the model
coef_summary <- summary(lm_model)$coefficients

# Select variables with p-values less than 0.05 (excluding the intercept term)
significant_variables_language <- rownames(coef_summary)[-1][coef_summary[-1, "Pr(>|t|)"] < 0.05]


# Select only the significant variables from df
df_selected <- df2[, c("CNTSCHID", "PVREAD", significant_variables_language)]
#160 variabels
summary(df_selected)

# saving the selected dataset from OLS regression with the most significant variables

# we will use this for linearized and Tree based models  
write_sav(df_selected, "pisa_data/2022/InitialDatabasePerLanguage.sav")



