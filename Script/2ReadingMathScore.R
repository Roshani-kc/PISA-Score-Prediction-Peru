
####1.b. Reading and Saving Dataset####
library(haven) # to read .sav format
library(dplyr)
library(tibble)
library(tidyverse)


df = read_sav("pisa_data/2022/df_per.sav")

####2 Selecting only columns with unique values >2 in rows

df_filteredV2 = df[apply(df, 2, FUN=function(r){length(unique(r))} ) > 2]
df_filteredV2$PVMATH <- (df_filteredV2$PV1MATH + df_filteredV2$PV2MATH + df_filteredV2$PV3MATH + df_filteredV2$PV4MATH + df_filteredV2$PV5MATH + df_filteredV2$PV6MATH + df_filteredV2$PV7MATH + df_filteredV2$PV8MATH + df_filteredV2$PV9MATH + df_filteredV2$PV10MATH)/10
#df_filteredV2$PVREAD <- (df_filteredV2$PV1READ + df_filteredV2$PV2READ + df_filteredV2$PV3READ + df_filteredV2$PV4READ + df_filteredV2$PV5READ + df_filteredV2$PV6READ + df_filteredV2$PV7READ + df_filteredV2$PV8READ + df_filteredV2$PV9READ + df_filteredV2$PV10READ)/10

# Print the filtered dataframe
print(df_filteredV2)

#filtering non-numeric columns out of df_filteredV2
df_filteredV3 <- df_filteredV2 %>% select_if(is.numeric)

#tried with correlation of PV##MATH with the rest of the variables in the dataframe
#correlation1 <- cor(df_filteredV3[-1],df_filteredV3$PV1MATH, use='pairwise.complete.obs')

# Select columns where absolute correlation with PV1MATH is more than 0.7
#selected_columns <- (which(abs(correlation1) > 0.7))

# PV1MATH does not have strong negative correlation with most of the variables.
#(which((correlation1) < -0.3))


## remove columns where more than 50% of the answers are N/A
df_filteredV4 <- df_filteredV3[, which(colMeans(!is.na(df_filteredV3)) > 0.5)]
print(df_filteredV4)


##to select only the dataframe with low correlation among the columns
##set the upper triangle to be zero and 
##then remove any rows that have values over 0.99

#creating temp df to store the correlation matrix of df
tmp <- cor(df_filteredV4)
#setting the upper half and diagonal of the matrix to 0
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0

#final df where correlation among the variables is less than 0.7
df_filteredV5 <- 
  df_filteredV4[, !apply(tmp, 2, function(x) any(abs(x) > 0.7, na.rm = TRUE))]
head(df_filteredV5)


#removing some of the teachers' questionnaire columns
# Pattern to match in column names
pattern <- "^TC"  
# Filter columns with names matching the pattern
matching_columns <- grep(pattern, names(df_filteredV5))  
#select the columns where the columns are not matched
df_filteredV6 <- df_filteredV5[, -matching_columns]
#df_filteredV6$PVMATH <- df_filteredV2$PVMATH # adding PVMATH column back to the dataframe

write_sav(df_filteredV6, "pisa_data/2022/filtered_PER.sav")
#will be using this database for exploring further for predicting maths score

