############### Assignment1 ###################
#####      Predicting Math Score          #####
#####      Predicting Language Score      #####
#####          Downloading Data           #####
##    Advanced Topic In Economics            ##
## Emperical Application with Big Data Tools ##
##            Assignment 1                   ##
## Submitted By: Roshani KC                  ##
################OutlineSummary#################

####1.Downloaded 2022 Pisa Data####
##Downloaded the data from the PISA database directly from R 
##from the following codes 12-26
getwd() #just insuring 
options(timeout=3000)

path  <- "pisa_data/2022"
url   <- c("https://webfs.oecd.org/pisa2022/SCH_QQQ_SPSS.zip",
           "https://webfs.oecd.org/pisa2022/STU_QQQ_SPSS.zip",
           "https://webfs.oecd.org/pisa2022/STU_COG_SPSS.zip",
           "https://webfs.oecd.org/pisa2022/STU_TIM_SPSS.zip",
           "https://webfs.oecd.org/pisa2022/TCH_QQQ_SPSS.zip"
)
destfile <- c("/SCH_QQQ_SPSS.zip",
              "/STU_QQQ_SPSS.zip",
              "/STU_COG_SPSS.zip",
              "/STU_TIM_SPSS.zip",
              "/TCH_QQQ_SPSS.zip"
)


####1.a. Unzip, Remove .zip####
##This code below for the Downloaded file, to unzip and remove the .zip 
for (i in 2:length(url)){
  download.file(url[i], file.path(path,destfile[i]))
  unzip(paste0(path,destfile[i]), exdir = path)
  file.remove(paste0(path,destfile[i]))
}
#Next- Reading, Merging and Saving Script#