############### Assignment1 ###################
#####      Predicting Math Score          #####
#####      Predicting Language Score      #####
#####         Descriptive Statistics      #####
###############Outline Summary#################
#Using Final Saved Database from 1.Merging and 
#2 Reading and Filtering Files of both for Math and Language
#setting Library
library(haven) #read.sav
library(sparklyr)
library(dplyr)
library(tidyverse)
library(purrr)

#Loading Original Database after merging and checking it 
df = read_sav("pisa_data/2022/df_per.sav")

#checking how big the dataset is 
dim(df)

print(df, n=10, ncol=10, width=Inf)
glimpse(df)

#library(tibble)
#df %>%  
#  map_dfc(attr, "label") %>% 
#  rbind(df)
#df %>%  
#  map_dfc(attr, "label") %>% 
#  bind_rows(mutate_all(df, as.character))

# Function to extract column name and label
extract_label <- function(column_name, data) {
  label <- attr(data[[column_name]], "label")
  tibble(column_name = column_name, label = label)
}

# function to each column of the dataframe and bind the results
column_labels <- names(df) %>% 
  map_df(~ extract_label(.x, df))

# Print the resulting dataframe
print(column_labels)

library(dplyr)

# Filter out non-numeric variables
numeric_df <- df %>%
  select_if(is.numeric)

# Calculate mean, median, standard deviation, and other descriptive statistics for each numeric variable
summary_stats <- data.frame(
  Variable = names(numeric_df),
  Mean = sapply(numeric_df, mean, na.rm = TRUE),
  Median = sapply(numeric_df, median, na.rm = TRUE),
  SD = sapply(numeric_df, sd, na.rm = TRUE),
  Min = sapply(numeric_df, min, na.rm = TRUE),
  Max = sapply(numeric_df, max, na.rm = TRUE)
)

# Print the table of descriptive statistics
print(summary_stats)

library(dplyr)
library(purrr)

# Function to extract column name and label
extract_label <- function(column_name, data) {
  label <- attr(data[[column_name]], "label")
  tibble(column_name = column_name, label = label)
}

# Apply the function to each column of the dataframe and bind the results
column_labels <- names(df) %>% 
  map_df(~ extract_label(.x, df))

# Print the resulting dataframe
print(column_labels)


# Check if variables are numeric
numeric_columns <- sum(sapply(df, is.numeric))
print(numeric_columns)

# Check if variables are character
character_columns <- sum(sapply(df, is.character))
print(character_columns)


# Filter out non-numeric variables
numeric_df <- df %>%
  select_if(is.numeric)
#only three columns with character

# Calculate mean, median, standard deviation, and other descriptive statistics for each numeric variable
summary_stats <- data.frame(
  Variable = names(numeric_df),
  Mean = sapply(numeric_df, mean, na.rm = TRUE),
  Median = sapply(numeric_df, median, na.rm = TRUE),
  SD = sapply(numeric_df, sd, na.rm = TRUE),
  Min = sapply(numeric_df, min, na.rm = TRUE),
  Max = sapply(numeric_df, max, na.rm = TRUE)
)

# Print the table of descriptive statistics
print(summary_stats)

#merging both 
final_summary_stats<- merge.data.frame(column_labels, summary_stats, by = "Variable", "column_name", all.x=TRUE)
final_summary_stats
