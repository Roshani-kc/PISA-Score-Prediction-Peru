############### Assignment1 ###################
#####      Predicting Math Score          #####
#####      Predicting Language Score      #####
#####          Merging Data               #####
################OutlineSummary#################

####1.b. Merging and Saving Dataset####
####Country PERU####
#Loading Library
library(haven) # to read .sav format
library(dplyr)
library(tibble)
library(tidyverse)

####a.READING####
#Using Dataset of Country Peru and selecting the column "CNT" by Country Code

####School file####
sch <- filter(as_tibble(read_sav("pisa_data/2022/CY08MSP_SCH_QQQ.sav")), 
              CNT== "PER")

####Student questionnaire file####
stu <- filter(as_tibble(read_sav("pisa_data/2022/CY08MSP_STU_QQQ.sav" )), 
              CNT=="PER")

####Student timing file####
tim <- filter(as_tibble(read_sav("pisa_data/2022/CY08MSP_STU_TIM.sav")), 
              CNT=="PER")

####Teacher questionnaire file####
tch <- filter(as_tibble(read_sav("pisa_data/2022/CY08MSP_TCH_QQQ.sav")), 
              CNT=="PER")

####Student cognitive file####
#cog <- read_sav("pisa_data/2022/CY08MSP_STU_COG.sav", col_select = "CNT")

# Using different approach to obtain first and last row index of the selected country
# Obtain first and last row index of the selected country
first_cnt_index = first(which(cog$CNT == 'PER'))
last_cnt_index = last(which(cog$CNT == 'PER'))

#filtering the database by the following line using skip
cog <- read_sav("pisa_data/2022/CY08MSP_STU_COG.sav", skip = first_cnt_index-1, n_max = last_cnt_index - first_cnt_index+1)

#saving 
save(cog, sch, stu, tch, tim, file = "pisa2022.RData")

####b.MERGING####
library(tidyverse)

df  <- stu %>%
  left_join(cog) %>%
  left_join(sch,
            by = intersect(colnames(stu), colnames(sch))[1:10],
            suffix   = c("stu", "sch"),
            multiple = "any") %>%
  left_join(tch,
            by = intersect(colnames(stu), colnames(tch))[1:9],
            suffix   = c("stu", "tch"),
            multiple = "any") %>%
  left_join(tim) %>%
  filter(CNT == "PER")

####c.SAVING####
#save(df, file = "pisa_data/2022/PISA_PER.RData")
#using write_sav as the above line didn't save data properly

write_sav(df, "pisa_data/2022/df_per.sav" )
#Using this data set for 2a.Reading, Filtering 2b.Descriptive Statistics 
####Next-2a.Reading, Filtering 2b.Descriptive Statistics
