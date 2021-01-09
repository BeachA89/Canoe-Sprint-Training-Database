library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(quantmod)
library(zoo)
library(R.utils)
library(DT)
library(ggplot2)
library(data.table)
library(stringr)
library(plyr)
library(lubridate)
library(reshape2)
library(formattable)
library(matrixStats)
library(broom)
library(tibble)
library(Rmisc)
library(tableHTML)
library(tidyverse)
library(rdrop2)
library(stringr)
library(plotly)




sec_to_ms <- function(secs){
  
  as.character(      
    paste(
      formatC(secs %/% 60 %% 60, width = 1, format = "d", flag = "0"),
      formatC(secs %% 60, width = 5, format = "f", digits = 2, flag = "0"),
      sep = ":"
    )
  )
  
}



# token <- readRDS("droptoken.rds")
# # Then pass the token to each drop_ function
# drop_acc(dtoken = token)
# db_folder <- "CanoeRaceProfileData"
# 
# save_db <- function(dat) {
#   if(exists("mydata")) dat <- rbind(mydata$x, dat)
#   file_path <- file.path(tempdir(), "data.csv") # create temporary file
#   write.csv(dat, file_path, row.names = FALSE)
#   drop_upload(file_path, dest = db_folder)
# }
# 
# load_db <- function(x) {
#   dd <- drop_read_csv(file.path(db_folder,x), header=TRUE, stringsAsFactors=FALSE,dtoken=token)
#   return(dd)
# }
# 
# outputDir <- "CanoeRaceProfileData"
# loadData <- function() {
#   # Read all the files into a list
#   filesInfo <- drop_dir(outputDir)
#   filePaths <- filesInfo$path_display
#   table1 <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE,skip = 1, header=TRUE)
#   
#   # Concatenate all data together into one data.frame
#   
# }
Prognostic.Velocities <- data.frame(fread("Prognostic Velocities Profile.csv", header = TRUE, stringsAsFactors=FALSE))

# #### Directory data load ####
filePaths <- list.files("data", full.names = TRUE, pattern = "\\.csv$")
# table1 <-  lapply(filePaths, fread, skip = 1, header=TRUE, stringsAsFactors=FALSE)
table1 <-  isolate(lapply(filePaths, fread, skip = 1, header=TRUE, stringsAsFactors=FALSE))
#
#table1 <-  isolate(lapply(input$file1$datapath, fread, skip = 1, header=TRUE, stringsAsFactors=FALSE))


#### identify data labels ####

#dataname  <- input$file1[['name']]
dataname <-  str_remove_all(filePaths, ".csv")
dataname <-  str_remove_all(dataname, "data/")


newdataframe = NULL
for (i in 1:length(dataname)){
  a <- data.frame(strsplit(dataname[i], "_"))
  if (nrow(a) == 8){
    newdataframe[[i]] <- data.frame(a[1,], NA, NA, NA, a[2,], a[3,], a[4,], a[5,], a[6,], a[7,], a[8,])
  }else if (nrow(a) == 9){
    newdataframe[[i]] <- data.frame(a[1,], a[2,], NA, NA,  a[3,], a[4,], a[5,], a[6,], a[7,], a[8,], a[9,])
    
  } else if (nrow(a) == 11){
    newdataframe[[i]] <- data.frame(a[1,], a[2,],a[3,], a[4,], a[5,], a[6,], a[7,], a[8,], a[9,], a[10,], a[11,])
    
  }
}
newdataframe_combine <-  rbindlist(newdataframe, use.names=FALSE)




#labels <-  as.data.frame(t(data.frame(strsplit(dataname, "_"))))
#labels$V1b <- paste0(labels$V1," ", labels$V2)


#fullnames <-  data.frame(dataname, labels)
fullnames <-  data.frame(dataname, newdataframe_combine)

fullnames1 <-  fullnames[,c(1,2,3,4,5,6,9,10,11,12)]
fullnames_200 <-  fullnames1[fullnames1[,8] == 200,]
fullnames_500 <-  fullnames1[fullnames1[,8] == 500,]
fullnames_1000 <-  fullnames1[fullnames1[,8] == 1000,]


##### Convert mm:ss.0 time to secs.0 ####

for (i in 1:length(table1)){
  for (j in 1:length(table1[[i]][["Time"]])){
    if (str_detect(replace_na(table1[[i]][["Time"]][j],''), ":") == TRUE){
      table1[[i]][["Time"]][j] <- as.numeric(ms(table1[[i]][["Time"]][j]))
    }else{
      table1[[i]][["Time"]][j] <-  as.numeric(table1[[i]][["Time"]][j])
    }
  }
}


#### Convert whole Time variable to numeric ####

for (i in 1:length(table1)){
  table1[[i]][["Time"]] <-  as.numeric(table1[[i]][["Time"]])
}


#### Create col names ####
col_names_200 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  "Time_25m", "Time_50m", "Time_75m", "Time_100m",
  "Time_125m", "Time_150m", "Time_175m", "Time_200m",
  "Split_25m", "Split_50m", "Split_75m", "Split_100m",
  "Split_125m", "Split_150m", "Split_175m", "Split_200m", "Split_Avg",
  "Vel_25m", "Vel_50m", "Vel_75m", "Vel_100m",
  "Vel_125m", "Vel_150m", "Vel_175m", "Vel_200m", "Vel_Avg",
  "SR_25m", "SR_50m", "SR_75m", "SR_100m",
  "SR_125m", "SR_150m", "SR_175m", "SR_200m", "SR_Avg")


col_names_500 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  "Time_25m", "Time_50m", "Time_75m", "Time_100m",
  "Time_125m", "Time_150m", "Time_175m", "Time_200m",
  "Time_225m", "Time_250m", "Time_275m", "Time_300m",
  "Time_325m", "Time_350m", "Time_375m", "Time_400m",
  "Time_425m", "Time_450m", "Time_475m", "Time_500m",
  "Split_25m", "Split_50m", "Split_75m", "Split_100m",
  "Split_125m", "Split_150m", "Split_175m", "Split_200m",
  "Split_225m", "Split_250m", "Split_275m", "Split_300m",
  "Split_325m", "Split_350m", "Split_375m", "Split_400m",
  "Split_425m", "Split_450m", "Split_475m", "Split_500m", "Split_Avg",
  "Vel_25m", "Vel_50m", "Vel_75m", "Vel_100m",
  "Vel_125m", "Vel_150m", "Vel_175m", "Vel_200m",
  "Vel_225m", "Vel_250m", "Vel_275m", "Vel_300m",
  "Vel_325m", "Vel_350m", "Vel_375m", "Vel_400m",
  "Vel_425m", "Vel_450m", "Vel_475m", "Vel_500m", "Vel_Avg",
  "SR_25m", "SR_50m", "SR_75m", "SR_100m",
  "SR_125m", "SR_150m", "SR_175m", "SR_200m",
  "SR_225m", "SR_250m", "SR_275m", "SR_300m",
  "SR_325m", "SR_350m", "SR_375m", "SR_400m",
  "SR_425m", "SR_450m", "SR_475m", "SR_500m", "SR_Avg")

col_names_1000 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  "Time_25m", "Time_50m", "Time_75m", "Time_100m",
  "Time_125m", "Time_150m", "Time_175m", "Time_200m",
  "Time_225m", "Time_250m", "Time_275m", "Time_300m",
  "Time_325m", "Time_350m", "Time_375m", "Time_400m",
  "Time_425m", "Time_450m", "Time_475m", "Time_500m",
  "Time_525m", "Time_550m", "Time_575m", "Time_600m",
  "Time_625m", "Time_650m", "Time_675m", "Time_700m",
  "Time_725m", "Time_750m", "Time_775m", "Time_800m",
  "Time_825m", "Time_850m", "Time_875m", "Time_900m",
  "Time_925m", "Time_950m", "Time_975m", "Time_1000m",
  
  
  "Split_25m", "Split_50m", "Split_75m", "Split_100m",
  "Split_125m", "Split_150m", "Split_175m", "Split_200m",
  "Split_225m", "Split_250m", "Split_275m", "Split_300m",
  "Split_325m", "Split_350m", "Split_375m", "Split_400m",
  "Split_425m", "Split_450m", "Split_475m", "Split_500m",
  "Split_525m", "Split_550m", "Split_575m", "Split_600m",
  "Split_625m", "Split_650m", "Split_675m", "Split_700m",
  "Split_725m", "Split_750m", "Split_775m", "Split_800m",
  "Split_825m", "Split_850m", "Split_875m", "Split_900m",
  "Split_925m", "Split_950m", "Split_975m", "Split_1000m",
  "Split_Avg",
  
  "Vel_25m", "Vel_50m", "Vel_75m", "Vel_100m",
  "Vel_125m", "Vel_150m", "Vel_175m", "Vel_200m",
  "Vel_225m", "Vel_250m", "Vel_275m", "Vel_300m",
  "Vel_325m", "Vel_350m", "Vel_375m", "Vel_400m",
  "Vel_425m", "Vel_450m", "Vel_475m", "Vel_500m",
  "Vel_525m", "Vel_550m", "Vel_575m", "Vel_600m",
  "Vel_625m", "Vel_650m", "Vel_675m", "Vel_700m",
  "Vel_725m", "Vel_750m", "Vel_775m", "Vel_800m",
  "Vel_825m", "Vel_850m", "Vel_875m", "Vel_900m",
  "Vel_925m", "Vel_950m", "Vel_975m", "Vel_1000m",
  "Vel_Avg",
  
  "SR_25m", "SR_50m", "SR_75m", "SR_100m",
  "SR_125m", "SR_150m", "SR_175m", "SR_200m",
  "SR_225m", "SR_250m", "SR_275m", "SR_300m",
  "SR_325m", "SR_350m", "SR_375m", "SR_400m",
  "SR_425m", "SR_450m", "SR_475m", "SR_500m",
  "SR_525m", "SR_550m", "SR_575m", "SR_600m",
  "SR_625m", "SR_650m", "SR_675m", "SR_700m",
  "SR_725m", "SR_750m", "SR_775m", "SR_800m",
  "SR_825m", "SR_850m", "SR_875m", "SR_900m",
  "SR_925m", "SR_950m", "SR_975m", "SR_1000m",
  "SR_Avg")

col_namesdist_200 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  25, 50, 75, 100,
  125, 150, 175, 200)

col_namesdist_500 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  25, 50, 75, 100,
  125, 150, 175, 200,
  225, 250, 275, 300,
  325, 350, 375, 400,
  425, 450, 475, 500)

col_namesdist_1000 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  25, 50, 75, 100,
  125, 150, 175, 200,
  225, 250, 275, 300,
  325, 350, 375, 400,
  425, 450, 475, 500,
  525, 550, 575, 600,
  625, 650, 675, 700,
  725, 750, 775, 800,
  825, 850, 875, 900,
  925, 950, 975, 1000)

col_namesdistavg_200 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  25, 50, 75, 100,
  125, 150, 175, 200, "avg")

col_namesdistavg_500 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  25, 50, 75, 100,
  125, 150, 175, 200,
  225, 250, 275, 300,
  325, 350, 375, 400,
  425, 450, 475, 500, "avg")


col_namesdistavg_1000 <- c(
  "ID", "Name1",  "Name2",  "Name3",  "Name4",
  "Competition",  "Class",  "Distance",  "Age","Phase",
  25, 50, 75, 100,
  125, 150, 175, 200,
  225, 250, 275, 300,
  325, 350, 375, 400,
  425, 450, 475, 500,
  525, 550, 575, 600,
  625, 650, 675, 700,
  725, 750, 775, 800,
  825, 850, 875, 900,
  925, 950, 975, 1000, "avg")




#### Transpose Data ####
#Transpose data to along columns of one row



data_transposed_200 = list()

data_transposed_500 = list()

data_transposed_1000 = list()

for (i in 1:length(table1)){
  data2 = table1[[i]]
  if (data2[9,1]=="Av"){
    data_transposed_200[[i]] <-  data.frame(t(data.frame(data2[1:8,2])), t(data.frame(data2[1:9,3])), t(data.frame(data2[1:9,4])), t(data.frame(data2[1:9,5])))
  } else if (data2[21,1]=="Av"){
    data_transposed_500[[i]] <-  data.frame(t(data.frame(data2[1:20,2])), t(data.frame(data2[1:21,3])), t(data.frame(data2[1:21,4])), t(data.frame(data2[1:21,5])))
  } else if (data2[21,1]==525){
    data_transposed_1000[[i]] <-  data.frame(t(data.frame(data2[1:40,2])), t(data.frame(data2[1:41,3])), t(data.frame(data2[1:41,4])), t(data.frame(data2[1:41,5])))
  }
  
}





data_transposed_200 = data_transposed_200 %>% discard(is.null)
data_transposed_500 = data_transposed_500 %>% discard(is.null)
data_transposed_1000 = data_transposed_1000 %>% discard(is.null)

#combine all dataframes in list to one dataframe

data_200 <-  rbindlist(data_transposed_200, fill=TRUE)
data_500 <-  rbindlist(data_transposed_500, fill=TRUE)
data_1000 <-  rbindlist(data_transposed_1000, fill=TRUE)




#### combine  labels and data into one dataframe ####


Labelled_data_200 <-  data.frame(fullnames_200, data_200)
Labelled_data_500 <-  data.frame(fullnames_500, data_500)
Labelled_data_1000 <-  data.frame(fullnames_1000, data_1000)

#define column and row names (data500 labels)

row_numbers_200 <-  1:nrow(fullnames_200)
row_numbers_500 <-  1:nrow(fullnames_500)
row_numbers_1000 <-  1:nrow(fullnames_1000)

colnames(Labelled_data_200) <-  col_names_200
colnames(Labelled_data_500) <-  col_names_500
colnames(Labelled_data_1000) <-  col_names_1000


rownames(Labelled_data_200) <-  row_numbers_200
rownames(Labelled_data_500) <-  row_numbers_500
rownames(Labelled_data_1000) <-  row_numbers_1000




#### sort data500combined by final time ####
Labelled_data_200 <-Labelled_data_200[,1:45]
Labelled_data_200 <-  dplyr::arrange(Labelled_data_200, Time_200m)
Labelled_data_500 <-Labelled_data_500[,1:93]
Labelled_data_500 <-  dplyr::arrange(Labelled_data_500, Time_500m)
Labelled_data_1000 <-Labelled_data_1000[,1:173]
Labelled_data_1000 <-  dplyr::arrange(Labelled_data_1000, Time_1000m)


#### extract just time variable ####

Labelled_data_200_time <-  Labelled_data_200[,1:18]

colnames(Labelled_data_200_time) <-  col_namesdist_200


Labelled_data_500_time <-  Labelled_data_500[,1:30]

colnames(Labelled_data_500_time) <-  col_namesdist_500

#1000
Labelled_data_1000_time <-  Labelled_data_1000[,1:50]

colnames(Labelled_data_1000_time) <-  col_namesdist_1000

#change type to character
Labelled_data_200_time$Class = as.character(Labelled_data_200_time$Class)
Labelled_data_200_time$Name1 = as.character(Labelled_data_200_time$Name1)
Labelled_data_200_time$Name2 = as.character(Labelled_data_200_time$Name2)
Labelled_data_200_time$Name3 = as.character(Labelled_data_200_time$Name3)
Labelled_data_200_time$Name4 = as.character(Labelled_data_200_time$Name4)
Labelled_data_200_time$Competition = as.character(Labelled_data_200_time$Competition)
Labelled_data_200_time$Phase = as.character(Labelled_data_200_time$Phase)


Labelled_data_500_time$Class = as.character(Labelled_data_500_time$Class)
Labelled_data_500_time$Name1 = as.character(Labelled_data_500_time$Name1)
Labelled_data_500_time$Name2 = as.character(Labelled_data_500_time$Name2)
Labelled_data_500_time$Name3 = as.character(Labelled_data_500_time$Name3)
Labelled_data_500_time$Name4 = as.character(Labelled_data_500_time$Name4)
Labelled_data_500_time$Competition = as.character(Labelled_data_500_time$Competition)
Labelled_data_500_time$Phase = as.character(Labelled_data_500_time$Phase)


#Filter data for plot
Labelled_data_1000_time$Class = as.character(Labelled_data_1000_time$Class)
Labelled_data_1000_time$Name1 = as.character(Labelled_data_1000_time$Name1)
Labelled_data_1000_time$Name2 = as.character(Labelled_data_1000_time$Name2)
Labelled_data_1000_time$Name3 = as.character(Labelled_data_1000_time$Name3)
Labelled_data_1000_time$Name4 = as.character(Labelled_data_1000_time$Name4)
Labelled_data_1000_time$Competition = as.character(Labelled_data_1000_time$Competition)
Labelled_data_1000_time$Phase = as.character(Labelled_data_1000_time$Phase)


#### extract just Split variable ####
cols <-c(1:10, 19:26)
Labelled_data_200_split <-  Labelled_data_200[,cols]

colnames(Labelled_data_200_split) <-  col_namesdist_200


cols <-c(1:10, 31:50)
Labelled_data_500_split <-  Labelled_data_500[,cols]

colnames(Labelled_data_500_split) <-  col_namesdist_500

#1000
cols <-c(1:10, 51:90)
Labelled_data_1000_split <-  Labelled_data_1000[,cols]

colnames(Labelled_data_1000_split) <-  col_namesdist_1000



#Filter data for plot
Labelled_data_200_split$Class = as.character(Labelled_data_200_split$Class)
Labelled_data_200_split$Name1 = as.character(Labelled_data_200_split$Name1)
Labelled_data_200_split$Name2 = as.character(Labelled_data_200_split$Name2)
Labelled_data_200_split$Name3 = as.character(Labelled_data_200_split$Name3)
Labelled_data_200_split$Name4 = as.character(Labelled_data_200_split$Name4)
Labelled_data_200_split$Competition = as.character(Labelled_data_200_split$Competition)
Labelled_data_200_split$Phase = as.character(Labelled_data_200_split$Phase)


Labelled_data_500_split$Class = as.character(Labelled_data_500_split$Class)
Labelled_data_500_split$Name1 = as.character(Labelled_data_500_split$Name1)
Labelled_data_500_split$Name2 = as.character(Labelled_data_500_split$Name2)
Labelled_data_500_split$Name3 = as.character(Labelled_data_500_split$Name3)
Labelled_data_500_split$Name4 = as.character(Labelled_data_500_split$Name4)
Labelled_data_500_split$Competition = as.character(Labelled_data_500_split$Competition)
Labelled_data_500_split$Phase = as.character(Labelled_data_500_split$Phase)


#Filter data for plot
Labelled_data_1000_split$Class = as.character(Labelled_data_1000_split$Class)
Labelled_data_1000_split$Name1 = as.character(Labelled_data_1000_split$Name1)
Labelled_data_1000_split$Name2 = as.character(Labelled_data_1000_split$Name2)
Labelled_data_1000_split$Name3 = as.character(Labelled_data_1000_split$Name3)
Labelled_data_1000_split$Name4 = as.character(Labelled_data_1000_split$Name4)
Labelled_data_1000_split$Competition = as.character(Labelled_data_1000_split$Competition)
Labelled_data_1000_split$Phase = as.character(Labelled_data_1000_split$Phase)




#### extract just Vel variable ####

cols <-c(1:10, 28:35)
Labelled_data_200_vel <-  Labelled_data_200[,cols]

colnames(Labelled_data_200_vel) <-  col_namesdist_200


cols <-c(1:10, 52:71)
Labelled_data_500_vel <-  Labelled_data_500[,cols]

colnames(Labelled_data_500_vel) <-  col_namesdist_500

#1000
cols <-c(1:10, 92:131)
Labelled_data_1000_vel <-  Labelled_data_1000[,cols]

colnames(Labelled_data_1000_vel) <-  col_namesdist_1000



#Filter data for plot
Labelled_data_200_vel$Class = as.character(Labelled_data_200_vel$Class)
Labelled_data_200_vel$Name1 = as.character(Labelled_data_200_vel$Name1)
Labelled_data_200_vel$Name2 = as.character(Labelled_data_200_vel$Name2)
Labelled_data_200_vel$Name3 = as.character(Labelled_data_200_vel$Name3)
Labelled_data_200_vel$Name4 = as.character(Labelled_data_200_vel$Name4)
Labelled_data_200_vel$Competition = as.character(Labelled_data_200_vel$Competition)
Labelled_data_200_vel$Phase = as.character(Labelled_data_200_vel$Phase)

Labelled_data_500_vel$Class = as.character(Labelled_data_500_vel$Class)
Labelled_data_500_vel$Name1 = as.character(Labelled_data_500_vel$Name1)
Labelled_data_500_vel$Name2 = as.character(Labelled_data_500_vel$Name2)
Labelled_data_500_vel$Name3 = as.character(Labelled_data_500_vel$Name3)
Labelled_data_500_vel$Name4 = as.character(Labelled_data_500_vel$Name4)
Labelled_data_500_vel$Competition = as.character(Labelled_data_500_vel$Competition)
Labelled_data_500_vel$Phase = as.character(Labelled_data_500_vel$Phase)


#Filter data for plot
Labelled_data_1000_vel$Class = as.character(Labelled_data_1000_vel$Class)
Labelled_data_1000_vel$Name1 = as.character(Labelled_data_1000_vel$Name1)
Labelled_data_1000_vel$Name2 = as.character(Labelled_data_1000_vel$Name2)
Labelled_data_1000_vel$Name3 = as.character(Labelled_data_1000_vel$Name3)
Labelled_data_1000_vel$Name4 = as.character(Labelled_data_1000_vel$Name4)
Labelled_data_1000_vel$Competition = as.character(Labelled_data_1000_vel$Competition)
Labelled_data_1000_vel$Phase = as.character(Labelled_data_1000_vel$Phase)




#### extract just Vel variable ####
cols <-c(1:10, 37:44)
Labelled_data_200_SR <-  Labelled_data_200[,cols]

colnames(Labelled_data_200_SR) <-  col_namesdist_200



cols <-c(1:10, 73:92)
Labelled_data_500_SR <-  Labelled_data_500[,cols]

colnames(Labelled_data_500_SR) <-  col_namesdist_500

#1000
cols <-c(1:10, 133:172)
Labelled_data_1000_SR <-  Labelled_data_1000[,cols]

colnames(Labelled_data_1000_SR) <-  col_namesdist_1000



#Filter data for plot
Labelled_data_200_SR$Class = as.character(Labelled_data_200_SR$Class)
Labelled_data_200_SR$Name1 = as.character(Labelled_data_200_SR$Name1)
Labelled_data_200_SR$Name2 = as.character(Labelled_data_200_SR$Name2)
Labelled_data_200_SR$Name3 = as.character(Labelled_data_200_SR$Name3)
Labelled_data_200_SR$Name4 = as.character(Labelled_data_200_SR$Name4)
Labelled_data_200_SR$Competition = as.character(Labelled_data_200_SR$Competition)
Labelled_data_200_SR$Phase = as.character(Labelled_data_200_SR$Phase)


Labelled_data_500_SR$Class = as.character(Labelled_data_500_SR$Class)
Labelled_data_500_SR$Name1 = as.character(Labelled_data_500_SR$Name1)
Labelled_data_500_SR$Name2 = as.character(Labelled_data_500_SR$Name2)
Labelled_data_500_SR$Name3 = as.character(Labelled_data_500_SR$Name3)
Labelled_data_500_SR$Name4 = as.character(Labelled_data_500_SR$Name4)
Labelled_data_500_SR$Competition = as.character(Labelled_data_500_SR$Competition)
Labelled_data_500_SR$Phase = as.character(Labelled_data_500_SR$Phase)


#Filter data for plot
Labelled_data_1000_SR$Class = as.character(Labelled_data_1000_SR$Class)
Labelled_data_1000_SR$Name1 = as.character(Labelled_data_1000_SR$Name1)
Labelled_data_1000_SR$Name2 = as.character(Labelled_data_1000_SR$Name2)
Labelled_data_1000_SR$Name3 = as.character(Labelled_data_1000_SR$Name3)
Labelled_data_1000_SR$Name4 = as.character(Labelled_data_1000_SR$Name4)
Labelled_data_1000_SR$Competition = as.character(Labelled_data_1000_SR$Competition)
Labelled_data_1000_SR$Phase = as.character(Labelled_data_1000_SR$Phase)

