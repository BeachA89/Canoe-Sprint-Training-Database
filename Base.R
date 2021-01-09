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





labels <-  as.data.frame(t(data.frame(strsplit(dataname, "_"))))
#labels$V1b <- paste0(labels$V1," ", labels$V2)


#fullnames <-  data.frame(dataname, labels)
fullnames <-  data.frame(dataname, labels)

fullnames_3x3x1km <-  fullnames[fullnames[,4] == "3x3x1km",]

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

for (i in 1:length(table1)){
  table1[[i]][["EWS"]] <-  round((table1[[i]][["Vel"]]^3)/(table1[[i]][["SR"]]/60),2)
}


#### Create col names ####

col_names_3x3x1km <- c(
  "ID", "Name",  "Date", "Session", "Set", "Effort",
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
  "SR_Avg",
  
  "EWS_25m", "EWS_50m", "EWS_75m", "EWS_100m",
  "EWS_125m", "EWS_150m", "EWS_175m", "EWS_200m",
  "EWS_225m", "EWS_250m", "EWS_275m", "EWS_300m",
  "EWS_325m", "EWS_350m", "EWS_375m", "EWS_400m",
  "EWS_425m", "EWS_450m", "EWS_475m", "EWS_500m",
  "EWS_525m", "EWS_550m", "EWS_575m", "EWS_600m",
  "EWS_625m", "EWS_650m", "EWS_675m", "EWS_700m",
  "EWS_725m", "EWS_750m", "EWS_775m", "EWS_800m",
  "EWS_825m", "EWS_850m", "EWS_875m", "EWS_900m",
  "EWS_925m", "EWS_950m", "EWS_975m", "EWS_1000m",
  "EWS_Avg")


col_namesdist_3x3x1km <- c(
  "ID", "Name",  "Date", "Session", "Set", "Effort",
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



col_namesdistavg_3x3x1km <- c(
  "ID", "Name",  "Date", "Session", "Set", "Effort",
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



data_transposed_3x3x1km = list()

for (i in 1:length(table1)){
  data2 = table1[[i]]
  if (data2[21,1]==525){
    data_transposed_3x3x1km[[i]] <-  data.frame(t(data.frame(data2[1:40,2])), t(data.frame(data2[1:41,3])), t(data.frame(data2[1:41,4])), t(data.frame(data2[1:41,5])), t(data.frame(data2[1:41,7])))
  }
  
}





data_transposed_3x3x1km = data_transposed_3x3x1km %>% discard(is.null)

#combine all dataframes in list to one dataframe


data_3x3x1km <-  rbindlist(data_transposed_3x3x1km, fill=TRUE)




#### combine  labels and data into one dataframe ####


Labelled_data_3x3x1km <-  data.frame(fullnames_3x3x1km, data_3x3x1km)

#define column and row names (data500 labels)


row_numbers_3x3x1km <-  1:nrow(fullnames_3x3x1km)


colnames(Labelled_data_3x3x1km) <-  col_names_3x3x1km

rownames(Labelled_data_3x3x1km) <-  row_numbers_3x3x1km




#### sort data500combined by final time ####
# 
# Labelled_data_3x3x1km <-Labelled_data_3x3x1km[,1:173]
# Labelled_data_3x3x1km <-  dplyr::arrange(Labelled_data_3x3x1km, Time_3x3x1km)


#### extract just time variable ####


#1000
Labelled_data_3x3x1km_time <-  Labelled_data_3x3x1km[,1:46]

colnames(Labelled_data_3x3x1km_time) <-  col_namesdist_3x3x1km


#Filter data for plot
Labelled_data_3x3x1km_time$Name = as.character(Labelled_data_3x3x1km_time$Name)
Labelled_data_3x3x1km_time$Date = as.character(Labelled_data_3x3x1km_time$Date)
Labelled_data_3x3x1km_time$Session = as.character(Labelled_data_3x3x1km_time$Session)
Labelled_data_3x3x1km_time$Set = as.character(Labelled_data_3x3x1km_time$Set)
Labelled_data_3x3x1km_time$Effort = as.character(Labelled_data_3x3x1km_time$Effort)

#### extract just Split variable ####

#1000
cols <-c(1:6, 47:86)
Labelled_data_3x3x1km_split <-  Labelled_data_3x3x1km[,cols]

colnames(Labelled_data_3x3x1km_split) <-  col_namesdist_3x3x1km


#Filter data for plot
Labelled_data_3x3x1km_split$Name = as.character(Labelled_data_3x3x1km_split$Name)
Labelled_data_3x3x1km_split$Date = as.character(Labelled_data_3x3x1km_split$Date)
Labelled_data_3x3x1km_split$Session = as.character(Labelled_data_3x3x1km_split$Session)
Labelled_data_3x3x1km_split$Set = as.character(Labelled_data_3x3x1km_split$Set)
Labelled_data_3x3x1km_split$Effort = as.character(Labelled_data_3x3x1km_split$Effort)




#### extract just Vel variable ####

#1000
cols <-c(1:6, 88:127)
Labelled_data_3x3x1km_vel <-  Labelled_data_3x3x1km[,cols]

colnames(Labelled_data_3x3x1km_vel) <-  col_namesdist_3x3x1km



#Filter data for plot

#Filter data for plot
Labelled_data_3x3x1km_vel$Name = as.character(Labelled_data_3x3x1km_vel$Name)
Labelled_data_3x3x1km_vel$Date = as.character(Labelled_data_3x3x1km_vel$Date)
Labelled_data_3x3x1km_vel$Session = as.character(Labelled_data_3x3x1km_vel$Session)
Labelled_data_3x3x1km_vel$Set = as.character(Labelled_data_3x3x1km_vel$Set)
Labelled_data_3x3x1km_vel$Effort = as.character(Labelled_data_3x3x1km_vel$Effort)




#### extract just Vel variable ####

#1000
cols <-c(1:6, 129:168)
Labelled_data_3x3x1km_SR <-  Labelled_data_3x3x1km[,cols]

colnames(Labelled_data_3x3x1km_SR) <-  col_namesdist_3x3x1km



#Filter data for plot
Labelled_data_3x3x1km_SR$Name = as.character(Labelled_data_3x3x1km_SR$Name)
Labelled_data_3x3x1km_SR$Date = as.character(Labelled_data_3x3x1km_SR$Date)
Labelled_data_3x3x1km_SR$Session = as.character(Labelled_data_3x3x1km_SR$Session)
Labelled_data_3x3x1km_SR$Set = as.character(Labelled_data_3x3x1km_SR$Set)
Labelled_data_3x3x1km_SR$Effort = as.character(Labelled_data_3x3x1km_SR$Effort)

#### extract just Vel variable ####

#1000
cols <-c(1:6, 170:209)
Labelled_data_3x3x1km_EWS <-  Labelled_data_3x3x1km[,cols]

colnames(Labelled_data_3x3x1km_EWS) <-  col_namesdist_3x3x1km



#Filter data for plot
Labelled_data_3x3x1km_EWS$Name = as.character(Labelled_data_3x3x1km_EWS$Name)
Labelled_data_3x3x1km_EWS$Date = as.character(Labelled_data_3x3x1km_EWS$Date)
Labelled_data_3x3x1km_EWS$Session = as.character(Labelled_data_3x3x1km_EWS$Session)
Labelled_data_3x3x1km_EWS$Set = as.character(Labelled_data_3x3x1km_EWS$Set)
Labelled_data_3x3x1km_EWS$Effort = as.character(Labelled_data_3x3x1km_EWS$Effort)



#### Read data into a list ####
# filesInfo <- drop_dir("CanoeRaceProfileData")
# filePaths <- filesInfo$path_display
# table1 <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE,skip = 1, header=TRUE)

#table1 <-  isolate(lapply(input$file1$datapath, fread, skip = 1, header=TRUE, stringsAsFactors=FALSE))







#### Filtering based on dropdowns - Time ####


# 
tab_Time_R1 <-  reactive({
  get(paste0(input$Session,"_time"))%>%
    melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
    filter(Name == input$Name) %>%
    filter(Date == input$Date) %>%
    mutate(variable = as.numeric(as.character(variable)))
})


Time <- Labelled_data_3x3x1km_time %>%
  melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
  filter(Name == "Jo Brigden-Jones") %>%
  filter(Date == "200911") %>%
  mutate(variable = as.numeric(as.character(variable)))


TimeS1E1 <- Time %>%
  filter(Set == "S1") %>%
  filter(Effort == "E1")

TimeS1E2 <- Time %>%
  filter(Set == "S1") %>%
  filter(Effort == "E2")

TimeS1E3 <- Time %>%
  filter(Set == "S1") %>%
  filter(Effort == "E3")

TimeS2E1 <- Time %>%
  filter(Set == "S2") %>%
  filter(Effort == "E1")

TimeS2E2 <- Time %>%
  filter(Set == "S2") %>%
  filter(Effort == "E2")

TimeS2E3 <- Time %>%
  filter(Set == "S2") %>%
  filter(Effort == "E3")

TimeS3E1 <- Time %>%
  filter(Set == "S3") %>%
  filter(Effort == "E1")

TimeS3E2 <- Time %>%
  filter(Set == "S3") %>%
  filter(Effort == "E2")

TimeS3E3 <- Time %>%
  filter(Set == "S3") %>%
  filter(Effort == "E3")



output$select_Name <-  renderUI({
  
  selectizeInput('Name', 'Select Name', choices = c("select" = "", unique(get(paste0(input$Session,"_time"))$Name)))  
}) 

output$select_Date <-  renderUI({
  inputName = as.character(input$Name)
  choice_Date <- reactive({
    get(paste0(input$Session,"_time")) %>% 
      filter(Name == inputName) %>% 
      pull(Date) %>% 
      as.character()
  })
  
  
  
  
  selectizeInput('Date', 'Select Date', choices = c("select" = "", choice_Date()))  
})         




# output$table2 <- renderDataTable({ 
#   
#   tab_Time_R1()
# })



#### Filtering based on dropdowns - Split Race 1 ####



tab_Split_R1 <-  reactive({

    get(paste0(input$Session,"_split"))%>%
      melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
      filter(Name == input$Name) %>%
      filter(Date == input$Date) %>%
      mutate(variable = as.numeric(as.character(variable)))
  })
# 
Split <- Labelled_data_3x3x1km_split %>%
  melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
  filter(Name == "Jo Brigden-Jones") %>%
  filter(Date == "200911") %>%
  mutate(variable = as.numeric(as.character(variable)))

SplitS1E1 <- Split %>%
  filter(Set == "S1") %>%
  filter(Effort == "E1")

SplitS1E2 <- Split %>%
  filter(Set == "S1") %>%
  filter(Effort == "E2")

SplitS1E3 <- Split %>%
  filter(Set == "S1") %>%
  filter(Effort == "E3")

SplitS2E1 <- Split %>%
  filter(Set == "S2") %>%
  filter(Effort == "E1")

SplitS2E2 <- Split %>%
  filter(Set == "S2") %>%
  filter(Effort == "E2")

SplitS2E3 <- Split %>%
  filter(Set == "S2") %>%
  filter(Effort == "E3")

SplitS3E1 <- Split %>%
  filter(Set == "S3") %>%
  filter(Effort == "E1")

SplitS3E2 <- Split %>%
  filter(Set == "S3") %>%
  filter(Effort == "E2")

SplitS3E3 <- Split %>%
  filter(Set == "S3") %>%
  filter(Effort == "E3")
# tab2_Split <-  reactive({
#   get(paste0(input$distance,"_split")) %>% 
#     filter(Class == input$Class) %>%
#     head(10) %>%
#     melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
#     group_by(variable) %>%
#     dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
#                      Average = mean(value), 
#                      LowerLimit = CI(value, ci=0.95)[3]) %>%
#     mutate(variable = as.numeric(as.character(variable))) %>%
#     mutate(Average = round(Average, digits = 2)) %>%
#     mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
#     mutate(LowerLimit = round(LowerLimit, digits = 2))
#   
# })


###################
tab_Vel_R1 <-  reactive({
  get(paste0(input$Session,"_vel"))%>%
    melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
    filter(Name == input$Name) %>%
    filter(Date == input$Date) %>%
    mutate(variable = as.numeric(as.character(variable)))
})
Vel <- Labelled_data_3x3x1km_vel %>%
  melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
  filter(Name == "Jo Brigden-Jones") %>%
  filter(Date == "200911") %>%
  mutate(variable = as.numeric(as.character(variable)))

VelS1E1 <- Vel %>%
  filter(Set == "S1") %>%
  filter(Effort == "E1")

VelS1E2 <- Vel %>%
  filter(Set == "S1") %>%
  filter(Effort == "E2")

VelS1E3 <- Vel %>%
  filter(Set == "S1") %>%
  filter(Effort == "E3")

VelS2E1 <- Vel %>%
  filter(Set == "S2") %>%
  filter(Effort == "E1")

VelS2E2 <- Vel %>%
  filter(Set == "S2") %>%
  filter(Effort == "E2")

VelS2E3 <- Vel %>%
  filter(Set == "S2") %>%
  filter(Effort == "E3")

VelS3E1 <- Vel %>%
  filter(Set == "S3") %>%
  filter(Effort == "E1")

VelS3E2 <- Vel %>%
  filter(Set == "S3") %>%
  filter(Effort == "E2")

VelS3E3 <- Vel %>%
  filter(Set == "S3") %>%
  filter(Effort == "E3")

# tab_Vel_R1 <-  reactive({
#   if (input$distance == "Labelled_data_200"){
#     distancea = 200
#   }  else if (input$distance == "Labelled_data_500"){
#     distancea = 500
#   }else if (input$distance == "Labelled_data_3x3x1km"){
#     distancea = 1000
#   }
#   
#   ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
#   ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
#   
#   get(paste0(input$distance,"_vel"))%>%
#     melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
#     filter(Class == input$Class) %>% 
#     filter_all(any_vars(str_detect(., pattern = input$Name)))%>% 
#     filter(Competition == input$Competition) %>% 
#     filter(Phase == input$Phase) %>%
#     mutate(variable = as.numeric(as.character(variable))) %>%
#     mutate(ProgSpeed = (ClassProgSpeed/value)*100)
# })  






# tab2_Vel <-  reactive({
#   if (input$distance == "Labelled_data_200"){
#     distancea = 200
#   }  else if (input$distance == "Labelled_data_500"){
#     distancea = 500
#   }else if (input$distance == "Labelled_data_3x3x1km"){
#     distancea = 1000
#   }
#   
#   ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
#   ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
#   
#   get(paste0(input$distance,"_vel")) %>% 
#     filter(Class == input$Class) %>%
#     head(10) %>%
#     melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
#     group_by(variable) %>%
#     dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
#                      Average = mean(value), 
#                      LowerLimit = CI(value, ci=0.95)[3]) %>%
#     mutate(variable = as.numeric(as.character(variable))) %>%
#     mutate(Average = round(Average, digits = 2)) %>%
#     mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
#     mutate(LowerLimit = round(LowerLimit, digits = 2))%>%
#     mutate(ProgSpeedUpperLimit = (ClassProgSpeed/UpperLimit)*100) %>%
#     mutate(ProgSpeedAverage = (ClassProgSpeed/Average)*100)%>%
#     mutate(ProgSpeedLowerLimit = (ClassProgSpeed/LowerLimit)*100)
#   
# })

tab_SR_R1 <-  reactive({
  get(paste0(input$Session,"_SR"))%>%
    melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
    filter(Name == input$Name) %>%
    filter(Date == input$Date) %>%
    mutate(variable = as.numeric(as.character(variable)))
})

SR <- Labelled_data_3x3x1km_SR %>%
  melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
  filter(Name == "Jo Brigden-Jones") %>%
  filter(Date == "200911") %>%
  mutate(variable = as.numeric(as.character(variable)))

SRS1E1 <- SR %>%
  filter(Set == "S1") %>%
  filter(Effort == "E1")

SRS1E2 <- SR %>%
  filter(Set == "S1") %>%
  filter(Effort == "E2")

SRS1E3 <- SR %>%
  filter(Set == "S1") %>%
  filter(Effort == "E3")

SRS2E1 <- SR %>%
  filter(Set == "S2") %>%
  filter(Effort == "E1")

SRS2E2 <- SR %>%
  filter(Set == "S2") %>%
  filter(Effort == "E2")

SRS2E3 <- SR %>%
  filter(Set == "S2") %>%
  filter(Effort == "E3")

SRS3E1 <- SR %>%
  filter(Set == "S3") %>%
  filter(Effort == "E1")

SRS3E2 <- SR %>%
  filter(Set == "S3") %>%
  filter(Effort == "E2")

SRS3E3 <- SR %>%
  filter(Set == "S3") %>%
  filter(Effort == "E3")




tab_EWS_R1 <-  reactive({
  get(paste0(input$Session,"_EWS"))%>%
    melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
    filter(Name == input$Name) %>%
    filter(Date == input$Date) %>%
    mutate(variable = as.numeric(as.character(variable)))
})

EWS <- Labelled_data_3x3x1km_EWS %>%
  melt(id = c("ID","Name", "Date","Session", "Set", "Effort"))%>%
  filter(Name == "Jo Brigden-Jones") %>%
  filter(Date == "200911") %>%
  mutate(variable = as.numeric(as.character(variable)))

EWSS1E1 <- EWS %>%
  filter(Set == "S1") %>%
  filter(Effort == "E1")

EWSS1E2 <- EWS %>%
  filter(Set == "S1") %>%
  filter(Effort == "E2")

EWSS1E3 <- EWS %>%
  filter(Set == "S1") %>%
  filter(Effort == "E3")

EWSS2E1 <- EWS %>%
  filter(Set == "S2") %>%
  filter(Effort == "E1")

EWSS2E2 <- EWS %>%
  filter(Set == "S2") %>%
  filter(Effort == "E2")

EWSS2E3 <- EWS %>%
  filter(Set == "S2") %>%
  filter(Effort == "E3")

EWSS3E1 <- EWS %>%
  filter(Set == "S3") %>%
  filter(Effort == "E1")

EWSS3E2 <- EWS %>%
  filter(Set == "S3") %>%
  filter(Effort == "E2")

EWSS3E3 <- EWS %>%
  filter(Set == "S3") %>%
  filter(Effort == "E3")
# 
# tab_SR_R1 <-  reactive({
#   if (input$distance == "Labelled_data_200"){
#     distancea = 200
#   }  else if (input$distance == "Labelled_data_500"){
#     distancea = 500
#   }else if (input$distance == "Labelled_data_3x3x1km"){
#     distancea = 1000
#   }
#   
#   ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
#   ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
#   
#   get(paste0(input$distance,"_SR"))%>%
#     melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
#     filter(Class == input$Class) %>% 
#     filter_all(any_vars(str_detect(., pattern = input$Name)))%>% 
#     filter(Competition == input$Competition) %>% 
#     filter(Phase == input$Phase) %>%
#     mutate(variable = as.numeric(as.character(variable)))
# })  
# 
# 
# 
# 
# 
# 
# tab2_SR <-  reactive({
#   if (input$distance == "Labelled_data_200"){
#     distancea = 200
#   }  else if (input$distance == "Labelled_data_500"){
#     distancea = 500
#   }else if (input$distance == "Labelled_data_3x3x1km"){
#     distancea = 1000
#   }
#   
#   ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
#   ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
#   
#   get(paste0(input$distance,"_SR")) %>% 
#     filter(Class == input$Class) %>%
#     head(10) %>%
#     melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
#     group_by(variable) %>%
#     dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
#                      Average = mean(value), 
#                      LowerLimit = CI(value, ci=0.95)[3]) %>%
#     mutate(variable = as.numeric(as.character(variable))) %>%
#     mutate(Average = round(Average, digits = 2)) %>%
#     mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
#     mutate(LowerLimit = round(LowerLimit, digits = 2))
#   
#   
# })


#### Filtering based on dropdowns - Time Race 2 ####

tab_Time_R2 <-  reactive({
  
  get(paste0(input$distance,"_time"))%>%
    melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
    filter(Class == input$Class) %>% 
    filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
    filter(Competition == input$Competition2) %>% 
    filter(Phase == input$Phase2) %>%
    mutate(variable = as.numeric(as.character(variable)))
})  


output$select_Class <-  renderUI({
  
  
  selectizeInput('Class', 'Select Class', choices = c("select" = "", unique(get(paste0(input$distance,"_time"))$Class)))  
  
}) 

output$select_Name2 <-  renderUI({
  inputClass = as.character(input$Class)
  choice_Name2 <- reactive({
    get(paste0(input$distance,"_time")) %>% 
      filter(Class == inputClass) %>% 
      select(contains("Name")) %>% as.list() %>%
      unlist() %>% as.character() %>% na.omit() 
    
  })
  
  
  
  if (input$Report_Type == "Two Races"){
    selectizeInput('Name2', 'Select Name 2', choices = c("select" = "", choice_Name2()))  
  }
})         



output$select_Competition2 <-  renderUI({
  inputClass = as.character(input$Class)
  inputName2 = as.character(input$Name2)
  choice_Competition2 <- reactive({
    get(paste0(input$distance,"_time")) %>% 
      filter(Class == inputClass) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
      pull(Competition) %>% 
      as.character()
    
    
  })
  
  if (input$Report_Type == "Two Races"){
    selectizeInput('Competition2', 'Select Competition 2', choices = c("select" = "", choice_Competition2()))  
  }
})

output$select_Phase2 <-  renderUI({
  inputClass = as.character(input$Class)
  inputName2 = as.character(input$Name2)
  inputcompetition2 = as.character(input$Competition2)
  choice_Phase2 <- reactive({
    get(paste0(input$distance,"_time")) %>% 
      filter(Class == inputClass) %>% 
      filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
      filter(Competition == inputcompetition2) %>% 
      pull(Phase) %>% 
      as.character()
    
    
  })
  
  if (input$Report_Type == "Two Races"){
    selectizeInput('Phase2', 'Select Phase 2', choices = c("select" = "", choice_Phase2()))  
  }
})     

# output$table2 <- renderDataTable({ 
#   
#   tab_Time_R1()
# })



#### Filtering based on dropdowns - Split Race 1 ####



tab_Split_R2 <-  reactive({
  
  get(paste0(input$distance,"_split"))%>%
    melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
    filter(Class == input$Class) %>% 
    filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
    filter(Competition == input$Competition2) %>% 
    filter(Phase == input$Phase) %>%
    mutate(variable = as.numeric(as.character(variable)))
})  




###################
tab_Vel_R2 <-  reactive({
  if (input$distance == "Labelled_data_200"){
    distancea = 200
  }  else if (input$distance == "Labelled_data_500"){
    distancea = 500
  }else if (input$distance == "Labelled_data_3x3x1km"){
    distancea = 1000
  }
  
  ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
  ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
  
  get(paste0(input$distance,"_vel"))%>%
    melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
    filter(Class == input$Class) %>% 
    filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
    filter(Competition == input$Competition2) %>% 
    filter(Phase == input$Phase2) %>%
    mutate(variable = as.numeric(as.character(variable))) %>%
    mutate(ProgSpeed = (ClassProgSpeed/value)*100)
})  



tab_SR_R2 <-  reactive({
  if (input$distance == "Labelled_data_200"){
    distancea = 200
  }  else if (input$distance == "Labelled_data_500"){
    distancea = 500
  }else if (input$distance == "Labelled_data_3x3x1km"){
    distancea = 1000
  }
  
  ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
  ClassProgSpeed <- ClassProgSpeed[1,'ProgSpeed100']
  
  get(paste0(input$distance,"_SR"))%>%
    melt(id = c("ID","Name1", "Name2","Name3", "Name4", "Competition","Class", "Distance","Age", "Phase"))%>%
    filter(Class == input$Class) %>% 
    filter_all(any_vars(str_detect(., pattern = input$Name2)))%>% 
    filter(Competition == input$Competition2) %>% 
    filter(Phase == input$Phase2) %>%
    mutate(variable = as.numeric(as.character(variable)))
})  


#### Race 1 ####
Race1 <- reactive({
  ### Splits Time calculations ###
  Filtered_Time_data <-  tab_Time_R1() %>% select(variable, value)
  Filtered_Time_data <-  TimeS1E1 %>% select(variable, value)
  
  Filtered_Time_data <- column_to_rownames(Filtered_Time_data,'variable')
  Filtered_Time_data$value <- as.numeric(Filtered_Time_data$value)
  
  Filtered_SR_data <-  SRS1E1 %>% select(variable, value)
  Filtered_SR_data <- column_to_rownames(Filtered_SR_data,'variable')
  Filtered_SR_data$value <- as.numeric(Filtered_SR_data$value)
  
  Filtered_EWS_data <-  EWSS1E1 %>% select(variable, value)
  Filtered_EWS_data <- column_to_rownames(Filtered_EWS_data,'variable')
  Filtered_EWS_data$value <- as.numeric(Filtered_EWS_data$value)
  
  
  #250 splits
  
  Split250_Time_250 <- Filtered_Time_data["250",]
  Split250_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["250",]
  Split250_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["500",]
  Split250_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["750",]
  
    Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500,
                                      Split250_Time_750,  Split250_Time_1000)
    Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))


  
  # Split250_Time_Max1 <- max(t(Split250_Time_Data))
  # Split250_Time_Min1 <- min(t(Split250_Time_Data))
  # Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
  # Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
  # Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
  
  
  
  # AvVel 250 splits
  Split250_AvVel_250 <-  round(((250/Split250_Time_250)*3.6),2)
  Split250_AvVel_500 <-  round(((250/Split250_Time_500)*3.6),2)
  Split250_AvVel_750 <-  round(((250/Split250_Time_750)*3.6),2)
  Split250_AvVel_1000 <-  round(((250/Split250_Time_1000)*3.6),2)
  

    
    Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500,
                                       Split250_AvVel_750,  Split250_AvVel_1000)
    Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
    Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)

  
  
  
  # Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
  # Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
  # Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
  
  
  #250 splits
  
  Split250_SR_250 <- round((sum(Filtered_SR_data[1:10,])/10),2)
  Split250_SR_500 <- round((sum(Filtered_SR_data[11:20,])/10),2)
  Split250_SR_750 <- round((sum(Filtered_SR_data[21:30,])/10),2)
  Split250_SR_1000 <- round((sum(Filtered_SR_data[31:40,])/10),2)
  

    
    Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500,
                                    Split250_SR_750,  Split250_SR_1000)
    Split250_SR_Avg <- round(mean(t(Split250_SR_Data)),2)
    
    
    #250 splits
    
    Split250_EWS_250 <- round((sum(Filtered_EWS_data[1:10,])/10),2)
    Split250_EWS_500 <- round((sum(Filtered_EWS_data[11:20,])/10),2)
    Split250_EWS_750 <- round((sum(Filtered_EWS_data[21:30,])/10),2)
    Split250_EWS_1000 <- round((sum(Filtered_EWS_data[31:40,])/10),2)
    
    
    
    Split250_EWS_Data <-  data.table(Split250_EWS_250, Split250_EWS_500,
                                    Split250_EWS_750,  Split250_EWS_1000)
    Split250_EWS_Avg <- round(mean(t(Split250_EWS_Data)),2)
    

  # Split250_SR_Max1 <- max(t(Split250_SR_Data))
  # Split250_SR_Min1 <- min(t(Split250_SR_Data))
  # Split250_SR_DO <-  round((Split250_SR_Max1-Split250_SR_Min1)/Split250_SR_Max1*100,2)
  # Split250_SR_Max <-  sec_to_ms(Split250_SR_Max1)
  # Split250_SR_Min <-  sec_to_ms(Split250_SR_Min1)
  
  
  
  # Pace 250 splits
  Split250_Pace500_250 <-  Split250_Time_250*2
  Split250_Pace500_500 <-  Split250_Time_500*2
  Split250_Pace500_750 <-  Split250_Time_750*2
  Split250_Pace500_1000 <-  Split250_Time_1000*2
  
    Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500,
                                         Split250_Pace500_750,  Split250_Pace500_1000)
    Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
    Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))

  
  
  # Split250_Pace500_Max <- round(max(t(Split250_Pace500_Data)),2)
  # Split250_Pace500_Min <- round(min(t(Split250_Pace500_Data)),2)
  # Split250_Pace500_DO <-  round((Split250_Pace500_Max-Split250_Pace500_Min)/Split250_Pace500_Max*100,2)
  
  
  
  # Pace 250 splits
  Split250_Pace1000_250 <-  Split250_Time_250*4
  Split250_Pace1000_500 <-  Split250_Time_500*4
  Split250_Pace1000_750 <-  Split250_Time_750*4
  Split250_Pace1000_1000 <-  Split250_Time_1000*4
  
    Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500,
                                          Split250_Pace1000_750,  Split250_Pace1000_1000)
    Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
    Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))

  
  
  
  # Split250_Pace1000_Max <- round(max(t(Split250_Pace1000_Data)),2)
  # Split250_Pace1000_Min <- round(min(t(Split250_Pace1000_Data)),2)
  # Split250_Pace1000_DO <-  round((Split250_Pace1000_Max-Split250_Pace1000_Min)/Split250_Pace1000_Max*100,2)
  
  # Pace /500 splits
  
  
  #250 splits
  
  Split250_Time_250 <- sec_to_ms(Split250_Time_250)
  Split250_Time_500 <- sec_to_ms(Split250_Time_500)
  Split250_Time_750 <- sec_to_ms(Split250_Time_750)
  Split250_Time_1000 <- sec_to_ms(Split250_Time_1000)
  
  
  
  
  #250 splits
  
  Split250_Pace500_250 <- sec_to_ms(Split250_Pace500_250)
  Split250_Pace500_500 <- sec_to_ms(Split250_Pace500_500)
  Split250_Pace500_750 <- sec_to_ms(Split250_Pace500_750)
  Split250_Pace500_1000 <- sec_to_ms(Split250_Pace500_1000)
  
  
  
  
  
  #250 splits
  
  Split250_Pace1000_250 <- sec_to_ms(Split250_Pace1000_250)
  Split250_Pace1000_500 <- sec_to_ms(Split250_Pace1000_500)
  Split250_Pace1000_750 <- sec_to_ms(Split250_Pace1000_750)
  Split250_Pace1000_1000 <- sec_to_ms(Split250_Pace1000_1000)
  
  
  ################### 
  
  # if (input$distance== "Labelled_data_3x3x1km"){
    
    
    Race1 <-  data.table("Distance (m)" = c( "250", "500", "750", "1000", "", "Average"),

                         "splits (secs)" = c(   Split250_Time_250,     Split250_Time_500,     Split250_Time_750,     Split250_Time_1000,"",
                                                   Split250_Time_Avg),

                         "Pace /500" = c(   Split250_Pace500_250,     Split250_Pace500_500,     Split250_Pace500_750,     Split250_Pace500_1000,"",
                                               Split250_Pace500_Avg),

                         
                         "Pace /1000" = c(   Split250_Pace1000_250,     Split250_Pace1000_500,     Split250_Pace1000_750,     Split250_Pace1000_1000,"",
                                                Split250_Pace1000_Avg),

                         "vel (km/h)" = c(   Split250_AvVel_250,     Split250_AvVel_500,     Split250_AvVel_750,     Split250_AvVel_1000,"",
                                                Split250_AvVel_Avg),

                         "SR (spm)" = c(   Split250_SR_250,     Split250_SR_500,     Split250_SR_750,     Split250_SR_1000,"",
                                              Split250_SR_Avg),
                         "EWS" = c(   Split250_EWS_250,     Split250_EWS_500,     Split250_EWS_750,     Split250_EWS_1000,"",
                                                Split250_EWS_Avg)

    )
    
  
  return(Race1)
  
  
  
})

#select

#### Race 2 ####
Race2 <- reactive({
  ### Splits Time calculations ###
  Filtered_Time_data <-  tab_Time_R2() %>% select(variable, value)
  Filtered_Time_data <- column_to_rownames(Filtered_Time_data,'variable')
  Filtered_Time_data$value <- as.numeric(Filtered_Time_data$value)
  #50 splits
  Split50_Time_50 <- Filtered_Time_data["50",]
  Split50_Time_100 <- Filtered_Time_data["100",]-Filtered_Time_data["50",]
  Split50_Time_150 <- Filtered_Time_data["150",]-Filtered_Time_data["100",]
  Split50_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["150",]
  Split50_Time_250 <- Filtered_Time_data["250",]-Filtered_Time_data["200",]
  Split50_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["250",]
  Split50_Time_350 <- Filtered_Time_data["350",]-Filtered_Time_data["300",]
  Split50_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["350",]
  Split50_Time_450 <- Filtered_Time_data["450",]-Filtered_Time_data["400",]
  Split50_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["450",]
  Split50_Time_550 <- Filtered_Time_data["550",]-Filtered_Time_data["500",]
  Split50_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["550",]
  Split50_Time_650 <- Filtered_Time_data["650",]-Filtered_Time_data["600",]
  Split50_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["650",]
  Split50_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["700",]
  Split50_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["750",]
  Split50_Time_850 <- Filtered_Time_data["850",]-Filtered_Time_data["800",]
  Split50_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["850",]
  Split50_Time_950 <- Filtered_Time_data["950",]-Filtered_Time_data["900",]
  Split50_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["950",]
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                     Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                     Split50_Time_450, Split50_Time_500, Split50_Time_550, Split50_Time_600,
                                     Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800,
                                     Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000)
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                     Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                     Split50_Time_450, Split50_Time_500)
    
  }else if(input$distance== "Labelled_data_200"){
    
    Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200)
    
    
  }
  Split50_Time_Avg <- sec_to_ms(mean(t(Split50_Time_Data)))
  # Split50_Time_Max1 <- max(t(Split50_Time_Data))
  # Split50_Time_Min1 <- min(t(Split50_Time_Data))
  # Split50_Time_DO <-  round((Split50_Time_Max1-Split50_Time_Min1)/Split50_Time_Max1*100,2)
  # Split50_Time_Max <-  sec_to_ms(Split50_Time_Max1)
  # Split50_Time_Min <-  sec_to_ms(Split50_Time_Min1)
  
  #100 splits
  Split100_Time_100 <- Filtered_Time_data["100",]
  Split100_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["100",]
  Split100_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["200",]
  Split100_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["300",]
  Split100_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["400",]
  Split100_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["500",]
  Split100_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["600",]
  Split100_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["700",]
  Split100_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["800",]
  Split100_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["900",]
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                      Split100_Time_300,  Split100_Time_400,
                                      Split100_Time_500, Split100_Time_600,
                                      Split100_Time_700, Split100_Time_800,
                                      Split100_Time_900, Split100_Time_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                      Split100_Time_300,  Split100_Time_400,
                                      Split100_Time_500)
  }else if(input$distance== "Labelled_data_200"){
    Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200)
    
  }
  
  
  Split100_Time_Avg <- sec_to_ms(mean(t(Split100_Time_Data)))
  # Split100_Time_Max1 <- max(t(Split100_Time_Data))
  # Split100_Time_Min1 <- min(t(Split100_Time_Data))
  # Split100_Time_DO <-  round((Split100_Time_Max1-Split100_Time_Min1)/Split100_Time_Max1*100,2)
  # Split100_Time_Max <-  sec_to_ms(Split100_Time_Max1)
  # Split100_Time_Min <-  sec_to_ms(Split100_Time_Min1)
  
  #250 splits
  
  Split250_Time_250 <- Filtered_Time_data["250",]
  Split250_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["250",]
  Split250_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["500",]
  Split250_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["750",]
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500,
                                      Split250_Time_750,  Split250_Time_1000)
    Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
  }else if(input$distance== "Labelled_data_500"){
    
    Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500)
    Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
  }
  
  # Split250_Time_Max1 <- max(t(Split250_Time_Data))
  # Split250_Time_Min1 <- min(t(Split250_Time_Data))
  # Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
  # Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
  # Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
  
  #500 Splits
  Split500_Time_500 <- Filtered_Time_data["500",]
  Split500_Time_1000 <- Filtered_Time_data["1000",] - Filtered_Time_data["500",]
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_Time_Data <-  data.table(Split500_Time_500,
                                      Split500_Time_1000)
    Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
    
  }else if(input$distance== "Labelled_data_500"){
    Split500_Time_Data <-  data.table(Split500_Time_500)
    Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
    
  }
  
  # Split500_Time_Max1 <- max(t(Split500_Time_Data))
  # Split500_Time_Min1 <- min(t(Split500_Time_Data))
  # Split500_Time_DO <-  round((Split500_Time_Max1-Split500_Time_Min1)/Split500_Time_Max1*100,2)
  # Split500_Time_Max <-  sec_to_ms(Split500_Time_Max1)
  # Split500_Time_Min <-  sec_to_ms(Split500_Time_Min1)
  
  
  ########################
  #AvVel 50 splits
  Split50_AvVel_50 <-  round(((50/Split50_Time_50)*3.6),2)
  Split50_AvVel_100 <-  round(((50/Split50_Time_100)*3.6),2)
  Split50_AvVel_150 <-  round(((50/Split50_Time_150)*3.6),2)
  Split50_AvVel_200 <-  round(((50/Split50_Time_200)*3.6),2)
  Split50_AvVel_250 <-  round(((50/Split50_Time_250)*3.6),2)
  Split50_AvVel_300 <-  round(((50/Split50_Time_300)*3.6),2)
  Split50_AvVel_350 <-  round(((50/Split50_Time_350)*3.6),2)
  Split50_AvVel_400 <-  round(((50/Split50_Time_400)*3.6),2)
  Split50_AvVel_450 <-  round(((50/Split50_Time_450)*3.6),2)
  Split50_AvVel_500 <-  round(((50/Split50_Time_500)*3.6),2)
  Split50_AvVel_550 <-  round(((50/Split50_Time_550)*3.6),2)
  Split50_AvVel_600 <-  round(((50/Split50_Time_600)*3.6),2)
  Split50_AvVel_650 <-  round(((50/Split50_Time_650)*3.6),2)
  Split50_AvVel_700 <-  round(((50/Split50_Time_700)*3.6),2)
  Split50_AvVel_750 <-  round(((50/Split50_Time_750)*3.6),2)
  Split50_AvVel_800 <-  round(((50/Split50_Time_800)*3.6),2)
  Split50_AvVel_850 <-  round(((50/Split50_Time_850)*3.6),2)
  Split50_AvVel_900 <-  round(((50/Split50_Time_900)*3.6),2)
  Split50_AvVel_950 <-  round(((50/Split50_Time_950)*3.6),2)
  Split50_AvVel_1000 <-  round(((50/Split50_Time_1000)*3.6),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                      Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                      Split50_AvVel_450, Split50_AvVel_500, Split50_AvVel_550, Split50_AvVel_600,
                                      Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800,
                                      Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000)
    
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                      Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                      Split50_AvVel_450, Split50_AvVel_500)
    
  }else if(input$distance== "Labelled_data_200"){
    Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200)
    
  }
  Split50_AvVel_Data <-  as.numeric(Split50_AvVel_Data)
  Split50_AvVel_Avg <- round(mean(t(Split50_AvVel_Data)),2)
  # Split50_AvVel_Max <- round(max(t(Split50_AvVel_Data)),2)
  # Split50_AvVel_Min <- round(min(t(Split50_AvVel_Data)),2)
  # Split50_AvVel_DO <-  round((Split50_AvVel_Max-Split50_AvVel_Min)/Split50_AvVel_Max*100,2)
  # AvVel 100 splits
  Split100_AvVel_100 <-  round(((100/Split100_Time_100)*3.6),2)
  Split100_AvVel_200 <-  round(((100/Split100_Time_200)*3.6),2)
  Split100_AvVel_300 <-  round(((100/Split100_Time_300)*3.6),2)
  Split100_AvVel_400 <-  round(((100/Split100_Time_400)*3.6),2)
  Split100_AvVel_500 <-  round(((100/Split100_Time_500)*3.6),2)
  Split100_AvVel_600 <-  round(((100/Split100_Time_600)*3.6),2)
  Split100_AvVel_700 <-  round(((100/Split100_Time_700)*3.6),2)
  Split100_AvVel_800 <-  round(((100/Split100_Time_800)*3.6),2)
  Split100_AvVel_900 <-  round(((100/Split100_Time_900)*3.6),2)
  Split100_AvVel_1000 <-  round(((100/Split100_Time_100)*3.6),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                       Split100_AvVel_300,  Split100_AvVel_400,
                                       Split100_AvVel_500, Split100_AvVel_600,
                                       Split100_AvVel_700, Split100_AvVel_800,
                                       Split100_AvVel_900, Split100_AvVel_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                       Split100_AvVel_300,  Split100_AvVel_400,
                                       Split100_AvVel_500)
  }else if(input$distance== "Labelled_data_200"){
    Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200)
    
  }
  Split100_AvVel_Data <-  as.numeric(Split100_AvVel_Data)
  Split100_AvVel_Avg <- round(mean(t(Split100_AvVel_Data)),2)
  # Split100_AvVel_Max <- round(max(t(Split100_AvVel_Data)),2)
  # Split100_AvVel_Min <- round(min(t(Split100_AvVel_Data)),2)
  # Split100_AvVel_DO <-  round((Split100_AvVel_Max-Split100_AvVel_Min)/Split100_AvVel_Max*100,2)
  
  
  # AvVel 250 splits
  Split250_AvVel_250 <-  round(((250/Split250_Time_250)*3.6),2)
  Split250_AvVel_500 <-  round(((250/Split250_Time_500)*3.6),2)
  Split250_AvVel_750 <-  round(((250/Split250_Time_750)*3.6),2)
  Split250_AvVel_1000 <-  round(((250/Split250_Time_1000)*3.6),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500,
                                       Split250_AvVel_750,  Split250_AvVel_1000)
    Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
    Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)
  }else if(input$distance== "Labelled_data_500"){
    Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500)
    Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
    Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2) 
    
  }
  
  
  
  # Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
  # Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
  # Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
  
  # AvVel 500 splits
  Split500_AvVel_500 <-  round(((500/Split500_Time_500)*3.6),2)
  Split500_AvVel_1000 <-  round(((500/Split500_Time_1000)*3.6),2)
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_AvVel_Data <-  data.table(Split500_AvVel_500,
                                       Split500_AvVel_1000)
    Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
    Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
  }else if(input$distance== "Labelled_data_500"){
    Split500_AvVel_Data <-  data.table(Split500_AvVel_500)
    
    Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
    Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
  }
  
  # Split500_AvVel_Max <- round(max(t(Split500_AvVel_Data)),2)
  # Split500_AvVel_Min <- round(min(t(Split500_AvVel_Data)),2)
  # Split500_AvVel_DO <-  round((Split500_AvVel_Max-Split500_AvVel_Min)/Split500_AvVel_Max*100,2)
  
  ##############
  
  Filtered_SR_data <-  tab_SR_R1() %>% select(variable, value)
  Filtered_SR_data <- column_to_rownames(Filtered_SR_data,'variable')
  Filtered_SR_data$value <- as.numeric(Filtered_SR_data$value)
  #50 splits
  Split50_SR_50 <- round(((Filtered_SR_data["25",]+Filtered_SR_data["50",])/2),2)
  Split50_SR_100 <- round(((Filtered_SR_data["75",]+Filtered_SR_data["100",])/2),2)
  Split50_SR_150 <- round(((Filtered_SR_data["125",]+Filtered_SR_data["150",])/2),2)
  Split50_SR_200 <- round(((Filtered_SR_data["175",]+Filtered_SR_data["200",])/2),2)
  Split50_SR_250 <- round(((Filtered_SR_data["225",]+Filtered_SR_data["250",])/2),2)
  Split50_SR_300 <- round(((Filtered_SR_data["275",]+Filtered_SR_data["300",])/2),2)
  Split50_SR_350 <- round(((Filtered_SR_data["325",]+Filtered_SR_data["350",])/2),2)
  Split50_SR_400 <- round(((Filtered_SR_data["375",]+Filtered_SR_data["400",])/2),2)
  Split50_SR_450 <- round(((Filtered_SR_data["425",]+Filtered_SR_data["450",])/2),2)
  Split50_SR_500 <- round(((Filtered_SR_data["475",]+Filtered_SR_data["500",])/2),2)
  Split50_SR_550 <- round(((Filtered_SR_data["525",]+Filtered_SR_data["550",])/2),2)
  Split50_SR_600 <- round(((Filtered_SR_data["575",]+Filtered_SR_data["600",])/2),2)
  Split50_SR_650 <- round(((Filtered_SR_data["625",]+Filtered_SR_data["650",])/2),2)
  Split50_SR_700 <- round(((Filtered_SR_data["675",]+Filtered_SR_data["700",])/2),2)
  Split50_SR_750 <- round(((Filtered_SR_data["725",]+Filtered_SR_data["750",])/2),2)
  Split50_SR_800 <- round(((Filtered_SR_data["775",]+Filtered_SR_data["800",])/2),2)
  Split50_SR_850 <- round(((Filtered_SR_data["825",]+Filtered_SR_data["850",])/2),2)
  Split50_SR_900 <- round(((Filtered_SR_data["875",]+Filtered_SR_data["900",])/2),2)
  Split50_SR_950 <- round(((Filtered_SR_data["925",]+Filtered_SR_data["950",])/2),2)
  Split50_SR_1000 <- round(((Filtered_SR_data["975",]+Filtered_SR_data["1000",])/2),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                   Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                   Split50_SR_450, Split50_SR_500, Split50_SR_550, Split50_SR_600,
                                   Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800,
                                   Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000)
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                   Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                   Split50_SR_450, Split50_SR_500)
    
  }else if(input$distance== "Labelled_data_200"){
    Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200)
    
    
  }
  Split50_SR_Avg <- mean(t(Split50_SR_Data))
  # Split50_SR_Max1 <- max(t(Split50_SR_Data))
  # Split50_SR_Min1 <- min(t(Split50_SR_Data))
  # Split50_SR_DO <-  round((Split50_SR_Max1-Split50_SR_Min1)/Split50_SR_Max1*100,2)
  # Split50_SR_Max <-  sec_to_ms(Split50_SR_Max1)
  # Split50_SR_Min <-  sec_to_ms(Split50_SR_Min1)
  
  #100 splits
  Split100_SR_100 <- round(((Split50_SR_50+Split50_SR_100)/2),2)
  Split100_SR_200 <- round(((Split50_SR_150+Split50_SR_200)/2),2)
  Split100_SR_300 <- round(((Split50_SR_250+Split50_SR_300)/2),2)
  Split100_SR_400 <- round(((Split50_SR_350+Split50_SR_400)/2),2)
  Split100_SR_500 <- round(((Split50_SR_450+Split50_SR_500)/2),2)
  Split100_SR_600 <- round(((Split50_SR_550+Split50_SR_600)/2),2)
  Split100_SR_700 <- round(((Split50_SR_650+Split50_SR_700)/2),2)
  Split100_SR_800 <- round(((Split50_SR_750+Split50_SR_800)/2),2)
  Split100_SR_900 <- round(((Split50_SR_850+Split50_SR_900)/2),2)
  Split100_SR_1000 <- round(((Split50_SR_50+Split50_SR_1000)/2),2)
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                    Split100_SR_300,  Split100_SR_400,
                                    Split100_SR_500, Split100_SR_600,
                                    Split100_SR_700, Split100_SR_800,
                                    Split100_SR_900, Split100_SR_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                    Split100_SR_300,  Split100_SR_400,
                                    Split100_SR_500)
  }else if(input$distance== "Labelled_data_200"){
    Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200)
    
  }
  
  
  Split100_SR_Avg <- mean(t(Split100_SR_Data))
  # Split100_SR_Max1 <- max(t(Split100_SR_Data))
  # Split100_SR_Min1 <- min(t(Split100_SR_Data))
  # Split100_SR_DO <-  round((Split100_SR_Max1-Split100_SR_Min1)/Split100_SR_Max1*100,2)
  # Split100_SR_Max <-  sec_to_ms(Split100_SR_Max1)
  # Split100_SR_Min <-  sec_to_ms(Split100_SR_Min1)
  
  #250 splits
  
  Split250_SR_250 <- round(((Split50_SR_50+Split50_SR_100+Split50_SR_150+Split50_SR_200+Split50_SR_250)/5),2)
  Split250_SR_500 <- round(((Split50_SR_300+Split50_SR_350+Split50_SR_400+Split50_SR_450+Split50_SR_500)/5),2)
  Split250_SR_750 <- round(((Split50_SR_550+Split50_SR_600+Split50_SR_650+Split50_SR_700+Split50_SR_750)/5),2)
  Split250_SR_1000 <- round(((Split50_SR_800+Split50_SR_850+Split50_SR_900+Split50_SR_950+Split50_SR_1000)/5),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500,
                                    Split250_SR_750,  Split250_SR_1000)
    Split250_SR_Avg <- mean(t(Split250_SR_Data))
  }else if(input$distance== "Labelled_data_500"){
    
    Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500)
    Split250_SR_Avg <- mean(t(Split250_SR_Data)) 
  }
  
  # Split250_SR_Max1 <- max(t(Split250_SR_Data))
  # Split250_SR_Min1 <- min(t(Split250_SR_Data))
  # Split250_SR_DO <-  round((Split250_SR_Max1-Split250_SR_Min1)/Split250_SR_Max1*100,2)
  # Split250_SR_Max <-  sec_to_ms(Split250_SR_Max1)
  # Split250_SR_Min <-  sec_to_ms(Split250_SR_Min1)
  
  #500 Splits
  Split500_SR_500 <- round(((Split250_SR_250+Split250_SR_500)/2),2)
  Split500_SR_1000 <- round(((Split250_SR_750+Split250_SR_1000)/2),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_SR_Data <-  data.table(Split500_SR_500,
                                    Split500_SR_1000)
    Split500_SR_Avg <- mean(t(Split500_SR_Data))
  }else if(input$distance== "Labelled_data_500"){
    Split500_SR_Data <-  data.table(Split500_SR_500)
    Split500_SR_Avg <- mean(t(Split500_SR_Data))
    
    
  }
  
  
  
  ########################
  
  #AvVel 50 splits
  
  Split50_Pace500_50 <-  Split50_Time_50*10
  Split50_Pace500_100 <-  Split50_Time_100*10
  Split50_Pace500_150 <-  Split50_Time_150*10
  Split50_Pace500_200 <-  Split50_Time_200*10
  Split50_Pace500_250 <-  Split50_Time_250*10
  Split50_Pace500_300 <-  Split50_Time_300*10
  Split50_Pace500_350 <-  Split50_Time_350*10
  Split50_Pace500_400 <-  Split50_Time_400*10
  Split50_Pace500_450 <-  Split50_Time_450*10
  Split50_Pace500_500 <-  Split50_Time_500*10
  Split50_Pace500_550 <-  Split50_Time_550*10
  Split50_Pace500_600 <-  Split50_Time_600*10
  Split50_Pace500_650 <-  Split50_Time_650*10
  Split50_Pace500_700 <-  Split50_Time_700*10
  Split50_Pace500_750 <-  Split50_Time_750*10
  Split50_Pace500_800 <-  Split50_Time_800*10
  Split50_Pace500_850 <-  Split50_Time_850*10
  Split50_Pace500_900 <-  Split50_Time_900*10
  Split50_Pace500_950 <-  Split50_Time_950*10
  Split50_Pace500_1000 <-  Split50_Time_1000*10
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                        Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                        Split50_Pace500_450, Split50_Pace500_500, Split50_Pace500_550, Split50_Pace500_600,
                                        Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800,
                                        Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000)
    
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                        Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                        Split50_Pace500_450, Split50_Pace500_500)
    
  }else if(input$distance== "Labelled_data_200"){
    Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200)
    
  }
  Split50_Pace500_Data <-  as.numeric(Split50_Pace500_Data)
  Split50_Pace500_Avg <- sec_to_ms(round(mean(t(Split50_Pace500_Data)),2))
  # Split50_Pace500_Max <- round(max(t(Split50_Pace500_Data)),2)
  # Split50_Pace500_Min <- round(min(t(Split50_Pace500_Data)),2)
  # Split50_Pace500_DO <-  round((Split50_Pace500_Max-Split50_Pace500_Min)/Split50_Pace500_Max*100,2)
  # Pace 100 splits
  Split100_Pace500_100 <-  Split100_Time_100*5
  Split100_Pace500_200 <-  Split100_Time_200*5
  Split100_Pace500_300 <-  Split100_Time_300*5
  Split100_Pace500_400 <-  Split100_Time_400*5
  Split100_Pace500_500 <-  Split100_Time_500*5
  Split100_Pace500_600 <-  Split100_Time_600*5
  Split100_Pace500_700 <-  Split100_Time_700*5
  Split100_Pace500_800 <-  Split100_Time_800*5
  Split100_Pace500_900 <-  Split100_Time_900*5
  Split100_Pace500_1000 <-  Split100_Time_1000*5
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                         Split100_Pace500_300,  Split100_Pace500_400,
                                         Split100_Pace500_500, Split100_Pace500_600,
                                         Split100_Pace500_700, Split100_Pace500_800,
                                         Split100_Pace500_900, Split100_Pace500_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                         Split100_Pace500_300,  Split100_Pace500_400,
                                         Split100_Pace500_500)
  }else if(input$distance== "Labelled_data_200"){
    Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200)
  }
  Split100_Pace500_Data <-  as.numeric(Split100_Pace500_Data)
  Split100_Pace500_Avg <- sec_to_ms(round(mean(t(Split100_Pace500_Data)),2))
  # Split100_Pace500_Max <- round(max(t(Split100_Pace500_Data)),2)
  # Split100_Pace500_Min <- round(min(t(Split100_Pace500_Data)),2)
  # Split100_Pace500_DO <-  round((Split100_Pace500_Max-Split100_Pace500_Min)/Split100_Pace500_Max*100,2)
  
  
  # Pace 250 splits
  Split250_Pace500_250 <-  Split250_Time_250*2
  Split250_Pace500_500 <-  Split250_Time_500*2
  Split250_Pace500_750 <-  Split250_Time_750*2
  Split250_Pace500_1000 <-  Split250_Time_1000*2
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500,
                                         Split250_Pace500_750,  Split250_Pace500_1000)
    Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
    Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
  }else if(input$distance== "Labelled_data_500"){
    Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500)
    
    Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
    Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
  }
  
  
  
  # Split250_Pace500_Max <- round(max(t(Split250_Pace500_Data)),2)
  # Split250_Pace500_Min <- round(min(t(Split250_Pace500_Data)),2)
  # Split250_Pace500_DO <-  round((Split250_Pace500_Max-Split250_Pace500_Min)/Split250_Pace500_Max*100,2)
  
  # Pace /500 splits
  Split500_Pace500_500 <-  Split500_Time_500*1
  Split500_Pace500_1000 <-  Split500_Time_1000*1
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_Pace500_Data <-  data.table(Split500_Pace500_500,
                                         Split500_Pace500_1000)
    Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
    Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
  }else if(input$distance== "Labelled_data_500"){
    Split500_Pace500_Data <-  data.table(Split500_Pace500_500)
    
    Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
    Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
  }
  
  # Split500_Pace500_Max <- round(max(t(Split500_Pace500_Data)),2)
  # Split500_Pace500_Min <- round(min(t(Split500_Pace500_Data)),2)
  # Split500_Pace500_DO <-  round((Split500_Pace500_Max-Split500_Pace500_Min)/Split500_Pace500_Max*100,2)
  
  
  #######################
  
  
  #AvVel 50 splits
  
  Split50_Pace1000_50 <-  Split50_Time_50*20
  Split50_Pace1000_100 <-  Split50_Time_100*20
  Split50_Pace1000_150 <-  Split50_Time_150*20
  Split50_Pace1000_200 <-  Split50_Time_200*20
  Split50_Pace1000_250 <-  Split50_Time_250*20
  Split50_Pace1000_300 <-  Split50_Time_300*20
  Split50_Pace1000_350 <-  Split50_Time_350*20
  Split50_Pace1000_400 <-  Split50_Time_400*20
  Split50_Pace1000_450 <-  Split50_Time_450*20
  Split50_Pace1000_500 <-  Split50_Time_500*20
  Split50_Pace1000_550 <-  Split50_Time_550*20
  Split50_Pace1000_600 <-  Split50_Time_600*20
  Split50_Pace1000_650 <-  Split50_Time_650*20
  Split50_Pace1000_700 <-  Split50_Time_700*20
  Split50_Pace1000_750 <-  Split50_Time_750*20
  Split50_Pace1000_800 <-  Split50_Time_800*20
  Split50_Pace1000_850 <-  Split50_Time_850*20
  Split50_Pace1000_900 <-  Split50_Time_900*20
  Split50_Pace1000_950 <-  Split50_Time_950*20
  Split50_Pace1000_1000 <-  Split50_Time_1000*20
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                         Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                         Split50_Pace1000_450, Split50_Pace1000_500, Split50_Pace1000_550, Split50_Pace1000_600,
                                         Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800,
                                         Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000)
    
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                         Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                         Split50_Pace1000_450, Split50_Pace1000_500)
    
  }else if(input$distance== "Labelled_data_200"){
    Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200)
    
  }
  Split50_Pace1000_Data <-  as.numeric(Split50_Pace1000_Data)
  Split50_Pace1000_Avg <- sec_to_ms(round(mean(t(Split50_Pace1000_Data)),2))
  # Split50_Pace1000_Max <- round(max(t(Split50_Pace1000_Data)),2)
  # Split50_Pace1000_Min <- round(min(t(Split50_Pace1000_Data)),2)
  # Split50_Pace1000_DO <-  round((Split50_Pace1000_Max-Split50_Pace1000_Min)/Split50_Pace1000_Max*100,2)
  # Pace 100 splits
  Split100_Pace1000_100 <-  Split100_Time_100*10
  Split100_Pace1000_200 <-  Split100_Time_200*10
  Split100_Pace1000_300 <-  Split100_Time_300*10
  Split100_Pace1000_400 <-  Split100_Time_400*10
  Split100_Pace1000_500 <-  Split100_Time_500*10
  Split100_Pace1000_600 <-  Split100_Time_600*10
  Split100_Pace1000_700 <-  Split100_Time_700*10
  Split100_Pace1000_800 <-  Split100_Time_800*10
  Split100_Pace1000_900 <- Split100_Time_900*10
  Split100_Pace1000_1000 <-  Split100_Time_1000*10
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                          Split100_Pace1000_300,  Split100_Pace1000_400,
                                          Split100_Pace1000_500, Split100_Pace1000_600,
                                          Split100_Pace1000_700, Split100_Pace1000_800,
                                          Split100_Pace1000_900, Split100_Pace1000_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                          Split100_Pace1000_300,  Split100_Pace1000_400,
                                          Split100_Pace1000_500)
    
    
  }else if(input$distance== "Labelled_data_200"){
    Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200)
    
  }
  Split100_Pace1000_Data <-  as.numeric(Split100_Pace1000_Data)
  Split100_Pace1000_Avg <- sec_to_ms(round(mean(t(Split100_Pace1000_Data)),2))
  # Split100_Pace1000_Max <- round(max(t(Split100_Pace1000_Data)),2)
  # Split100_Pace1000_Min <- round(min(t(Split100_Pace1000_Data)),2)
  # Split100_Pace1000_DO <-  round((Split100_Pace1000_Max-Split100_Pace1000_Min)/Split100_Pace1000_Max*100,2)
  
  
  # Pace 250 splits
  Split250_Pace1000_250 <-  Split250_Time_250*8
  Split250_Pace1000_500 <-  Split250_Time_500*8
  Split250_Pace1000_750 <-  Split250_Time_750*8
  Split250_Pace1000_1000 <-  Split250_Time_1000*8
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500,
                                          Split250_Pace1000_750,  Split250_Pace1000_1000)
    Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
    Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
  }else if(input$distance== "Labelled_data_500"){
    Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500)
    
    Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
    Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
  }
  
  
  
  # Split250_Pace1000_Max <- round(max(t(Split250_Pace1000_Data)),2)
  # Split250_Pace1000_Min <- round(min(t(Split250_Pace1000_Data)),2)
  # Split250_Pace1000_DO <-  round((Split250_Pace1000_Max-Split250_Pace1000_Min)/Split250_Pace1000_Max*100,2)
  
  # Pace /500 splits
  Split500_Pace1000_500 <-  Split500_Time_500*2
  Split500_Pace1000_1000 <-  Split500_Time_1000*2
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500,
                                          Split500_Pace1000_1000)
    Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
    Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
  }else if(input$distance== "Labelled_data_500"){
    Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500)
    
    Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
    Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
    
  }
  
  # Split500_Pace1000_Max <- round(max(t(Split500_Pace1000_Data)),2)
  # Split500_Pace1000_Min <- round(min(t(Split500_Pace1000_Data)),2)
  # Split500_Pace1000_DO <-  round((Split500_Pace1000_Max-Split500_Pace1000_Min)/Split500_Pace1000_Max*100,2)
  
  
  #### Convert to ms in table
  Split50_Time_50 <- sec_to_ms(Split50_Time_50)
  Split50_Time_100 <- sec_to_ms(Split50_Time_100)
  Split50_Time_150 <- sec_to_ms(Split50_Time_150)
  Split50_Time_200 <- sec_to_ms(Split50_Time_200)
  Split50_Time_250 <- sec_to_ms(Split50_Time_250)
  Split50_Time_300 <- sec_to_ms(Split50_Time_300)
  Split50_Time_350 <- sec_to_ms(Split50_Time_350)
  Split50_Time_400 <- sec_to_ms(Split50_Time_400)
  Split50_Time_450 <- sec_to_ms(Split50_Time_450)
  Split50_Time_500 <- sec_to_ms(Split50_Time_500)
  Split50_Time_550 <- sec_to_ms(Split50_Time_550)
  Split50_Time_600 <- sec_to_ms(Split50_Time_600)
  Split50_Time_650 <- sec_to_ms(Split50_Time_650)
  Split50_Time_700 <- sec_to_ms(Split50_Time_700)
  Split50_Time_750 <- sec_to_ms(Split50_Time_750)
  Split50_Time_800 <- sec_to_ms(Split50_Time_800)
  Split50_Time_850 <- sec_to_ms(Split50_Time_850)
  Split50_Time_900 <- sec_to_ms(Split50_Time_900)
  Split50_Time_950 <- sec_to_ms(Split50_Time_950)
  Split50_Time_1000 <- sec_to_ms(Split50_Time_1000)
  
  
  #100 splits
  Split100_Time_100 <- sec_to_ms(Split100_Time_100)
  Split100_Time_200 <- sec_to_ms(Split100_Time_200)
  Split100_Time_300 <- sec_to_ms(Split100_Time_300)
  Split100_Time_400 <- sec_to_ms(Split100_Time_400)
  Split100_Time_500 <- sec_to_ms(Split100_Time_500)
  Split100_Time_600 <- sec_to_ms(Split100_Time_600)
  Split100_Time_700 <- sec_to_ms(Split100_Time_700)
  Split100_Time_800 <- sec_to_ms(Split100_Time_800)
  Split100_Time_900 <- sec_to_ms(Split100_Time_900)
  Split100_Time_1000 <- sec_to_ms(Split100_Time_1000)
  
  
  #250 splits
  
  Split250_Time_250 <- sec_to_ms(Split250_Time_250)
  Split250_Time_500 <- sec_to_ms(Split250_Time_500)
  Split250_Time_750 <- sec_to_ms(Split250_Time_750)
  Split250_Time_1000 <- sec_to_ms(Split250_Time_1000)
  
  
  
  #500 Splits
  Split500_Time_500 <- sec_to_ms(Split500_Time_500)
  Split500_Time_1000 <- sec_to_ms(Split500_Time_1000)
  
  ###################
  #### Convert to ms in table
  Split50_Pace500_50 <- sec_to_ms(Split50_Pace500_50)
  Split50_Pace500_100 <- sec_to_ms(Split50_Pace500_100)
  Split50_Pace500_150 <- sec_to_ms(Split50_Pace500_150)
  Split50_Pace500_200 <- sec_to_ms(Split50_Pace500_200)
  Split50_Pace500_250 <- sec_to_ms(Split50_Pace500_250)
  Split50_Pace500_300 <- sec_to_ms(Split50_Pace500_300)
  Split50_Pace500_350 <- sec_to_ms(Split50_Pace500_350)
  Split50_Pace500_400 <- sec_to_ms(Split50_Pace500_400)
  Split50_Pace500_450 <- sec_to_ms(Split50_Pace500_450)
  Split50_Pace500_500 <- sec_to_ms(Split50_Pace500_500)
  Split50_Pace500_550 <- sec_to_ms(Split50_Pace500_550)
  Split50_Pace500_600 <- sec_to_ms(Split50_Pace500_600)
  Split50_Pace500_650 <- sec_to_ms(Split50_Pace500_650)
  Split50_Pace500_700 <- sec_to_ms(Split50_Pace500_700)
  Split50_Pace500_750 <- sec_to_ms(Split50_Pace500_750)
  Split50_Pace500_800 <- sec_to_ms(Split50_Pace500_800)
  Split50_Pace500_850 <- sec_to_ms(Split50_Pace500_850)
  Split50_Pace500_900 <- sec_to_ms(Split50_Pace500_900)
  Split50_Pace500_950 <- sec_to_ms(Split50_Pace500_950)
  Split50_Pace500_1000 <- sec_to_ms(Split50_Pace500_1000)
  
  
  #100 splits
  Split100_Pace500_100 <- sec_to_ms(Split100_Pace500_100)
  Split100_Pace500_200 <- sec_to_ms(Split100_Pace500_200)
  Split100_Pace500_300 <- sec_to_ms(Split100_Pace500_300)
  Split100_Pace500_400 <- sec_to_ms(Split100_Pace500_400)
  Split100_Pace500_500 <- sec_to_ms(Split100_Pace500_500)
  Split100_Pace500_600 <- sec_to_ms(Split100_Pace500_600)
  Split100_Pace500_700 <- sec_to_ms(Split100_Pace500_700)
  Split100_Pace500_800 <- sec_to_ms(Split100_Pace500_800)
  Split100_Pace500_900 <- sec_to_ms(Split100_Pace500_900)
  Split100_Pace500_1000 <- sec_to_ms(Split100_Pace500_1000)
  
  
  #250 splits
  
  Split250_Pace500_250 <- sec_to_ms(Split250_Pace500_250)
  Split250_Pace500_500 <- sec_to_ms(Split250_Pace500_500)
  Split250_Pace500_750 <- sec_to_ms(Split250_Pace500_750)
  Split250_Pace500_1000 <- sec_to_ms(Split250_Pace500_1000)
  
  
  
  #500 Splits
  Split500_Pace500_500 <- sec_to_ms(Split500_Pace500_500)
  Split500_Pace500_1000 <- sec_to_ms(Split500_Pace500_1000)
  
  ################### 
  
  Split50_Pace1000_50 <- sec_to_ms(Split50_Pace1000_50)
  Split50_Pace1000_100 <- sec_to_ms(Split50_Pace1000_100)
  Split50_Pace1000_150 <- sec_to_ms(Split50_Pace1000_150)
  Split50_Pace1000_200 <- sec_to_ms(Split50_Pace1000_200)
  Split50_Pace1000_250 <- sec_to_ms(Split50_Pace1000_250)
  Split50_Pace1000_300 <- sec_to_ms(Split50_Pace1000_300)
  Split50_Pace1000_350 <- sec_to_ms(Split50_Pace1000_350)
  Split50_Pace1000_400 <- sec_to_ms(Split50_Pace1000_400)
  Split50_Pace1000_450 <- sec_to_ms(Split50_Pace1000_450)
  Split50_Pace1000_500 <- sec_to_ms(Split50_Pace1000_500)
  Split50_Pace1000_550 <- sec_to_ms(Split50_Pace1000_550)
  Split50_Pace1000_600 <- sec_to_ms(Split50_Pace1000_600)
  Split50_Pace1000_650 <- sec_to_ms(Split50_Pace1000_650)
  Split50_Pace1000_700 <- sec_to_ms(Split50_Pace1000_700)
  Split50_Pace1000_750 <- sec_to_ms(Split50_Pace1000_750)
  Split50_Pace1000_800 <- sec_to_ms(Split50_Pace1000_800)
  Split50_Pace1000_850 <- sec_to_ms(Split50_Pace1000_850)
  Split50_Pace1000_900 <- sec_to_ms(Split50_Pace1000_900)
  Split50_Pace1000_950 <- sec_to_ms(Split50_Pace1000_950)
  Split50_Pace1000_1000 <- sec_to_ms(Split50_Pace1000_1000)
  
  #100 splits
  Split100_Pace1000_100 <- sec_to_ms(Split100_Pace1000_100)
  Split100_Pace1000_200 <- sec_to_ms(Split100_Pace1000_200)
  Split100_Pace1000_300 <- sec_to_ms(Split100_Pace1000_300)
  Split100_Pace1000_400 <- sec_to_ms(Split100_Pace1000_400)
  Split100_Pace1000_500 <- sec_to_ms(Split100_Pace1000_500)
  Split100_Pace1000_600 <- sec_to_ms(Split100_Pace1000_600)
  Split100_Pace1000_700 <- sec_to_ms(Split100_Pace1000_700)
  Split100_Pace1000_800 <- sec_to_ms(Split100_Pace1000_800)
  Split100_Pace1000_900 <- sec_to_ms(Split100_Pace1000_900)
  Split100_Pace1000_1000 <- sec_to_ms(Split100_Pace1000_1000)
  
  
  #250 splits
  
  Split250_Pace1000_250 <- sec_to_ms(Split250_Pace1000_250)
  Split250_Pace1000_500 <- sec_to_ms(Split250_Pace1000_500)
  Split250_Pace1000_750 <- sec_to_ms(Split250_Pace1000_750)
  Split250_Pace1000_1000 <- sec_to_ms(Split250_Pace1000_1000)
  
  
  
  #500 Splits
  Split500_Pace1000_500 <- sec_to_ms(Split500_Pace1000_500)
  Split500_Pace1000_1000 <- sec_to_ms(Split500_Pace1000_1000)
  
  ################### 
  
  
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    
    Race2 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "800", "850", "900", "950", "1000", "", "Average"),
                         "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                 Split50_Time_550, Split50_Time_600, Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800, Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000,
                                                 "", Split50_Time_Avg),
                         "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500, "",
                                                  Split100_Time_600, "", Split100_Time_700, "", Split100_Time_800, "", Split100_Time_900, "", Split100_Time_1000,
                                                  "", Split100_Time_Avg),
                         
                         
                         "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500, "", "", "", "", Split250_Time_750, "", "", "", "", Split250_Time_1000,
                                                  "", Split250_Time_Avg),
                         "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500, "", "", "", "", "", "", "", "", "", Split500_Time_1000,
                                                  "", Split500_Time_Avg),
                         "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                             Split50_Pace500_550, Split50_Pace500_600, Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800, Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000,
                                             "", Split50_Pace500_Avg),
                         "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500, "",
                                              Split100_Pace500_600, "", Split100_Pace500_700, "", Split100_Pace500_800, "", Split100_Pace500_900, "", Split100_Pace500_1000,
                                              "", Split100_Pace500_Avg),
                         
                         
                         "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500, "", "", "", "", Split250_Pace500_750, "", "", "", "", Split250_Pace500_1000,
                                              "", Split250_Pace500_Avg),
                         "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500, "", "", "", "", "", "", "", "", "", Split500_Pace500_1000,
                                              "", Split500_Pace500_Avg),
                         
                         "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                              Split50_Pace1000_550, Split50_Pace1000_600, Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800, Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000,
                                              "", Split50_Pace1000_Avg),
                         "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500, "",
                                               Split100_Pace1000_600, "", Split100_Pace1000_700, "", Split100_Pace1000_800, "", Split100_Pace1000_900, "", Split100_Pace1000_1000,
                                               "", Split100_Pace1000_Avg),
                         
                         
                         "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500, "", "", "", "", Split250_Pace1000_750, "", "", "", "", Split250_Pace1000_1000,
                                               "", Split250_Pace1000_Avg),
                         "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500, "", "", "", "", "", "", "", "", "", Split500_Pace1000_1000,
                                               "", Split500_Pace1000_Avg),
                         "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                              Split50_AvVel_550, Split50_AvVel_600, Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800, Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000,
                                              "", Split50_AvVel_Avg),
                         "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500, "",
                                               Split100_AvVel_600, "", Split100_AvVel_700, "", Split100_AvVel_800, "", Split100_AvVel_900, "", Split100_AvVel_1000,
                                               "", Split100_AvVel_Avg),
                         "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500, "", "", "", "", Split250_AvVel_750, "", "", "", "", Split250_AvVel_1000,
                                               "", Split250_AvVel_Avg),
                         "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500, "", "", "", "", "", "", "", "", "", Split500_AvVel_1000,
                                               "", Split500_AvVel_Avg),
                         "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                            Split50_SR_550, Split50_SR_600, Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800, Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000,
                                            "", Split50_SR_Avg),
                         "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500, "",
                                             Split100_SR_600, "", Split100_SR_700, "", Split100_SR_800, "", Split100_SR_900, "", Split100_SR_1000,
                                             "", Split100_SR_Avg),
                         "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500, "", "", "", "", Split250_SR_750, "", "", "", "", Split250_SR_1000,
                                             "", Split250_SR_Avg),
                         "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500, "", "", "", "", "", "", "", "", "", Split500_SR_1000,
                                             "", Split500_SR_Avg))
    
  }else if(input$distance== "Labelled_data_500"){
    
    
    
    Race2 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "", "Average"),
                         "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                 "", Split50_Time_Avg),
                         "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500,
                                                  "", Split100_Time_Avg),
                         "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500,
                                                  "", Split250_Time_Avg),
                         "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500,
                                                  "", Split500_Time_Avg),
                         "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                             "", Split50_Pace500_Avg),
                         "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500,
                                              "", Split100_Pace500_Avg),
                         "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500,
                                              "", Split250_Pace500_Avg),
                         "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500,
                                              "", Split500_Pace500_Avg),
                         
                         "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                              "", Split50_Pace1000_Avg),
                         "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500,
                                               "", Split100_Pace1000_Avg),
                         "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500,
                                               "", Split250_Pace1000_Avg),
                         "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500,
                                               "", Split500_Pace1000_Avg),
                         
                         "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                              "", Split50_AvVel_Avg),
                         
                         "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500,
                                               "", Split100_AvVel_Avg),
                         "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500,
                                               "", Split250_AvVel_Avg),
                         "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500,
                                               "", Split500_AvVel_Avg),
                         "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                            "", Split50_SR_Avg),
                         
                         "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500,
                                             "", Split100_SR_Avg),
                         "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500,
                                             "", Split250_SR_Avg),
                         "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500,
                                             "", Split500_SR_Avg))
    
  }else if(input$distance== "Labelled_data_200"){
    
    Race2 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "", "Average"),
                         "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                                 "", Split50_Time_Avg),
                         "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200,
                                                  "", Split100_Time_Avg),
                         "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                             "", Split50_Pace500_Avg),
                         "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200,
                                              "", Split100_Pace500_Avg),
                         "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                              "", Split50_Pace1000_Avg),
                         "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200,
                                               "", Split100_Pace1000_Avg),
                         
                         "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                              "", Split50_AvVel_Avg),
                         
                         "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200,
                                               "", Split100_AvVel_Avg),
                         "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                            "", Split50_SR_Avg),
                         
                         "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200,
                                             "", Split100_SR_Avg))
  }
  return(Race2)
  
  
  
})

#select
#### Top10 ####
Top10 <- reactive({
  ### Splits Time calculations ####
  
  
  
  Filtered_Time_data <- tab2_Time() %>% select(variable, Average)
  Filtered_Time_data <- column_to_rownames(Filtered_Time_data,'variable')
  #Filtered_Time_data <-  rename(Filtered_Time_data,c(Average = "value"))
  Filtered_Time_data$Average <- as.numeric(Filtered_Time_data$Average)
  #50 splits
  Split50_Time_50 <- Filtered_Time_data["50",]
  Split50_Time_100 <- Filtered_Time_data["100",]-Filtered_Time_data["50",]
  Split50_Time_150 <- Filtered_Time_data["150",]-Filtered_Time_data["100",]
  Split50_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["150",]
  Split50_Time_250 <- Filtered_Time_data["250",]-Filtered_Time_data["200",]
  Split50_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["250",]
  Split50_Time_350 <- Filtered_Time_data["350",]-Filtered_Time_data["300",]
  Split50_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["350",]
  Split50_Time_450 <- Filtered_Time_data["450",]-Filtered_Time_data["400",]
  Split50_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["450",]
  Split50_Time_550 <- Filtered_Time_data["550",]-Filtered_Time_data["500",]
  Split50_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["550",]
  Split50_Time_650 <- Filtered_Time_data["650",]-Filtered_Time_data["600",]
  Split50_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["650",]
  Split50_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["700",]
  Split50_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["750",]
  Split50_Time_850 <- Filtered_Time_data["850",]-Filtered_Time_data["800",]
  Split50_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["850",]
  Split50_Time_950 <- Filtered_Time_data["950",]-Filtered_Time_data["900",]
  Split50_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["950",]
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                     Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                     Split50_Time_450, Split50_Time_500, Split50_Time_550, Split50_Time_600,
                                     Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800,
                                     Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000)
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                     Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400,
                                     Split50_Time_450, Split50_Time_500)
    
  }else if(input$distance== "Labelled_data_200"){
    
    Split50_Time_Data <-  data.table(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200)
    
    
  }
  Split50_Time_Avg <- sec_to_ms(mean(t(Split50_Time_Data)))
  # Split50_Time_Max1 <- max(t(Split50_Time_Data))
  # Split50_Time_Min1 <- min(t(Split50_Time_Data))
  # Split50_Time_DO <-  round((Split50_Time_Max1-Split50_Time_Min1)/Split50_Time_Max1*100,2)
  # Split50_Time_Max <-  sec_to_ms(Split50_Time_Max1)
  # Split50_Time_Min <-  sec_to_ms(Split50_Time_Min1)
  
  #100 splits
  Split100_Time_100 <- Filtered_Time_data["100",]
  Split100_Time_200 <- Filtered_Time_data["200",]-Filtered_Time_data["100",]
  Split100_Time_300 <- Filtered_Time_data["300",]-Filtered_Time_data["200",]
  Split100_Time_400 <- Filtered_Time_data["400",]-Filtered_Time_data["300",]
  Split100_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["400",]
  Split100_Time_600 <- Filtered_Time_data["600",]-Filtered_Time_data["500",]
  Split100_Time_700 <- Filtered_Time_data["700",]-Filtered_Time_data["600",]
  Split100_Time_800 <- Filtered_Time_data["800",]-Filtered_Time_data["700",]
  Split100_Time_900 <- Filtered_Time_data["900",]-Filtered_Time_data["800",]
  Split100_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["900",]
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                      Split100_Time_300,  Split100_Time_400,
                                      Split100_Time_500, Split100_Time_600,
                                      Split100_Time_700, Split100_Time_800,
                                      Split100_Time_900, Split100_Time_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200,
                                      Split100_Time_300,  Split100_Time_400,
                                      Split100_Time_500)
  }else if(input$distance== "Labelled_data_200"){
    Split100_Time_Data <-  data.table(Split100_Time_100, Split100_Time_200)
    
  }
  
  
  Split100_Time_Avg <- sec_to_ms(mean(t(Split100_Time_Data)))
  # Split100_Time_Max1 <- max(t(Split100_Time_Data))
  # Split100_Time_Min1 <- min(t(Split100_Time_Data))
  # Split100_Time_DO <-  round((Split100_Time_Max1-Split100_Time_Min1)/Split100_Time_Max1*100,2)
  # Split100_Time_Max <-  sec_to_ms(Split100_Time_Max1)
  # Split100_Time_Min <-  sec_to_ms(Split100_Time_Min1)
  
  #250 splits
  
  Split250_Time_250 <- Filtered_Time_data["250",]
  Split250_Time_500 <- Filtered_Time_data["500",]-Filtered_Time_data["250",]
  Split250_Time_750 <- Filtered_Time_data["750",]-Filtered_Time_data["500",]
  Split250_Time_1000 <- Filtered_Time_data["1000",]-Filtered_Time_data["750",]
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500,
                                      Split250_Time_750,  Split250_Time_1000)
    Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
  }else if(input$distance== "Labelled_data_500"){
    
    Split250_Time_Data <-  data.table(Split250_Time_250, Split250_Time_500)
    Split250_Time_Avg <- sec_to_ms(mean(t(Split250_Time_Data)))
  }
  
  # Split250_Time_Max1 <- max(t(Split250_Time_Data))
  # Split250_Time_Min1 <- min(t(Split250_Time_Data))
  # Split250_Time_DO <-  round((Split250_Time_Max1-Split250_Time_Min1)/Split250_Time_Max1*100,2)
  # Split250_Time_Max <-  sec_to_ms(Split250_Time_Max1)
  # Split250_Time_Min <-  sec_to_ms(Split250_Time_Min1)
  
  #500 Splits
  Split500_Time_500 <- Filtered_Time_data["500",]
  Split500_Time_1000 <- Filtered_Time_data["1000",] - Filtered_Time_data["500",]
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_Time_Data <-  data.table(Split500_Time_500,
                                      Split500_Time_1000)
    Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
    
  }else if(input$distance== "Labelled_data_500"){
    Split500_Time_Data <-  data.table(Split500_Time_500)
    Split500_Time_Avg <- sec_to_ms(mean(t(Split500_Time_Data)))
    
  }
  
  # Split500_Time_Max1 <- max(t(Split500_Time_Data))
  # Split500_Time_Min1 <- min(t(Split500_Time_Data))
  # Split500_Time_DO <-  round((Split500_Time_Max1-Split500_Time_Min1)/Split500_Time_Max1*100,2)
  # Split500_Time_Max <-  sec_to_ms(Split500_Time_Max1)
  # Split500_Time_Min <-  sec_to_ms(Split500_Time_Min1)
  
  
  ########################
  #AvVel 50 splits
  Split50_AvVel_50 <-  round(((50/Split50_Time_50)*3.6),2)
  Split50_AvVel_100 <-  round(((50/Split50_Time_100)*3.6),2)
  Split50_AvVel_150 <-  round(((50/Split50_Time_150)*3.6),2)
  Split50_AvVel_200 <-  round(((50/Split50_Time_200)*3.6),2)
  Split50_AvVel_250 <-  round(((50/Split50_Time_250)*3.6),2)
  Split50_AvVel_300 <-  round(((50/Split50_Time_300)*3.6),2)
  Split50_AvVel_350 <-  round(((50/Split50_Time_350)*3.6),2)
  Split50_AvVel_400 <-  round(((50/Split50_Time_400)*3.6),2)
  Split50_AvVel_450 <-  round(((50/Split50_Time_450)*3.6),2)
  Split50_AvVel_500 <-  round(((50/Split50_Time_500)*3.6),2)
  Split50_AvVel_550 <-  round(((50/Split50_Time_550)*3.6),2)
  Split50_AvVel_600 <-  round(((50/Split50_Time_600)*3.6),2)
  Split50_AvVel_650 <-  round(((50/Split50_Time_650)*3.6),2)
  Split50_AvVel_700 <-  round(((50/Split50_Time_700)*3.6),2)
  Split50_AvVel_750 <-  round(((50/Split50_Time_750)*3.6),2)
  Split50_AvVel_800 <-  round(((50/Split50_Time_800)*3.6),2)
  Split50_AvVel_850 <-  round(((50/Split50_Time_850)*3.6),2)
  Split50_AvVel_900 <-  round(((50/Split50_Time_900)*3.6),2)
  Split50_AvVel_950 <-  round(((50/Split50_Time_950)*3.6),2)
  Split50_AvVel_1000 <-  round(((50/Split50_Time_1000)*3.6),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                      Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                      Split50_AvVel_450, Split50_AvVel_500, Split50_AvVel_550, Split50_AvVel_600,
                                      Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800,
                                      Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000)
    
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                      Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400,
                                      Split50_AvVel_450, Split50_AvVel_500)
    
  }else if(input$distance== "Labelled_data_200"){
    Split50_AvVel_Data <-  data.table(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200)
    
  }
  Split50_AvVel_Data <-  as.numeric(Split50_AvVel_Data)
  Split50_AvVel_Avg <- round(mean(t(Split50_AvVel_Data)),2)
  # Split50_AvVel_Max <- round(max(t(Split50_AvVel_Data)),2)
  # Split50_AvVel_Min <- round(min(t(Split50_AvVel_Data)),2)
  # Split50_AvVel_DO <-  round((Split50_AvVel_Max-Split50_AvVel_Min)/Split50_AvVel_Max*100,2)
  # AvVel 100 splits
  Split100_AvVel_100 <-  round(((100/Split100_Time_100)*3.6),2)
  Split100_AvVel_200 <-  round(((100/Split100_Time_200)*3.6),2)
  Split100_AvVel_300 <-  round(((100/Split100_Time_300)*3.6),2)
  Split100_AvVel_400 <-  round(((100/Split100_Time_400)*3.6),2)
  Split100_AvVel_500 <-  round(((100/Split100_Time_500)*3.6),2)
  Split100_AvVel_600 <-  round(((100/Split100_Time_600)*3.6),2)
  Split100_AvVel_700 <-  round(((100/Split100_Time_700)*3.6),2)
  Split100_AvVel_800 <-  round(((100/Split100_Time_800)*3.6),2)
  Split100_AvVel_900 <-  round(((100/Split100_Time_900)*3.6),2)
  Split100_AvVel_1000 <-  round(((100/Split100_Time_100)*3.6),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                       Split100_AvVel_300,  Split100_AvVel_400,
                                       Split100_AvVel_500, Split100_AvVel_600,
                                       Split100_AvVel_700, Split100_AvVel_800,
                                       Split100_AvVel_900, Split100_AvVel_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200,
                                       Split100_AvVel_300,  Split100_AvVel_400,
                                       Split100_AvVel_500)
  }else if(input$distance== "Labelled_data_200"){
    Split100_AvVel_Data <-  data.table(Split100_AvVel_100, Split100_AvVel_200)
    
  }
  Split100_AvVel_Data <-  as.numeric(Split100_AvVel_Data)
  Split100_AvVel_Avg <- round(mean(t(Split100_AvVel_Data)),2)
  # Split100_AvVel_Max <- round(max(t(Split100_AvVel_Data)),2)
  # Split100_AvVel_Min <- round(min(t(Split100_AvVel_Data)),2)
  # Split100_AvVel_DO <-  round((Split100_AvVel_Max-Split100_AvVel_Min)/Split100_AvVel_Max*100,2)
  
  
  # AvVel 250 splits
  Split250_AvVel_250 <-  round(((250/Split250_Time_250)*3.6),2)
  Split250_AvVel_500 <-  round(((250/Split250_Time_500)*3.6),2)
  Split250_AvVel_750 <-  round(((250/Split250_Time_750)*3.6),2)
  Split250_AvVel_1000 <-  round(((250/Split250_Time_1000)*3.6),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500,
                                       Split250_AvVel_750,  Split250_AvVel_1000)
    Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
    Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2)
  }else if(input$distance== "Labelled_data_500"){
    Split250_AvVel_Data <-  data.table(Split250_AvVel_250, Split250_AvVel_500)
    Split250_AvVel_Data <-  as.numeric(Split250_AvVel_Data)
    Split250_AvVel_Avg <- round(mean(t(Split250_AvVel_Data)),2) 
    
  }
  
  
  
  # Split250_AvVel_Max <- round(max(t(Split250_AvVel_Data)),2)
  # Split250_AvVel_Min <- round(min(t(Split250_AvVel_Data)),2)
  # Split250_AvVel_DO <-  round((Split250_AvVel_Max-Split250_AvVel_Min)/Split250_AvVel_Max*100,2)
  
  # AvVel 500 splits
  Split500_AvVel_500 <-  round(((500/Split500_Time_500)*3.6),2)
  Split500_AvVel_1000 <-  round(((500/Split500_Time_1000)*3.6),2)
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_AvVel_Data <-  data.table(Split500_AvVel_500,
                                       Split500_AvVel_1000)
    Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
    Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
  }else if(input$distance== "Labelled_data_500"){
    Split500_AvVel_Data <-  data.table(Split500_AvVel_500)
    
    Split500_AvVel_Data <-  as.numeric(Split500_AvVel_Data)
    Split500_AvVel_Avg <- round(mean(t(Split500_AvVel_Data)),2)
  }
  
  # Split500_AvVel_Max <- round(max(t(Split500_AvVel_Data)),2)
  # Split500_AvVel_Min <- round(min(t(Split500_AvVel_Data)),2)
  # Split500_AvVel_DO <-  round((Split500_AvVel_Max-Split500_AvVel_Min)/Split500_AvVel_Max*100,2)
  
  ##############
  
  Filtered_SR_data <-  tab_SR_R1() %>% select(variable, value)
  Filtered_SR_data <- column_to_rownames(Filtered_SR_data,'variable')
  Filtered_SR_data$value <- as.numeric(Filtered_SR_data$value)
  #50 splits
  Split50_SR_50 <- round(((Filtered_SR_data["25",]+Filtered_SR_data["50",])/2),2)
  Split50_SR_100 <- round(((Filtered_SR_data["75",]+Filtered_SR_data["100",])/2),2)
  Split50_SR_150 <- round(((Filtered_SR_data["125",]+Filtered_SR_data["150",])/2),2)
  Split50_SR_200 <- round(((Filtered_SR_data["175",]+Filtered_SR_data["200",])/2),2)
  Split50_SR_250 <- round(((Filtered_SR_data["225",]+Filtered_SR_data["250",])/2),2)
  Split50_SR_300 <- round(((Filtered_SR_data["275",]+Filtered_SR_data["300",])/2),2)
  Split50_SR_350 <- round(((Filtered_SR_data["325",]+Filtered_SR_data["350",])/2),2)
  Split50_SR_400 <- round(((Filtered_SR_data["375",]+Filtered_SR_data["400",])/2),2)
  Split50_SR_450 <- round(((Filtered_SR_data["425",]+Filtered_SR_data["450",])/2),2)
  Split50_SR_500 <- round(((Filtered_SR_data["475",]+Filtered_SR_data["500",])/2),2)
  Split50_SR_550 <- round(((Filtered_SR_data["525",]+Filtered_SR_data["550",])/2),2)
  Split50_SR_600 <- round(((Filtered_SR_data["575",]+Filtered_SR_data["600",])/2),2)
  Split50_SR_650 <- round(((Filtered_SR_data["625",]+Filtered_SR_data["650",])/2),2)
  Split50_SR_700 <- round(((Filtered_SR_data["675",]+Filtered_SR_data["700",])/2),2)
  Split50_SR_750 <- round(((Filtered_SR_data["725",]+Filtered_SR_data["750",])/2),2)
  Split50_SR_800 <- round(((Filtered_SR_data["775",]+Filtered_SR_data["800",])/2),2)
  Split50_SR_850 <- round(((Filtered_SR_data["825",]+Filtered_SR_data["850",])/2),2)
  Split50_SR_900 <- round(((Filtered_SR_data["875",]+Filtered_SR_data["900",])/2),2)
  Split50_SR_950 <- round(((Filtered_SR_data["925",]+Filtered_SR_data["950",])/2),2)
  Split50_SR_1000 <- round(((Filtered_SR_data["975",]+Filtered_SR_data["1000",])/2),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                   Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                   Split50_SR_450, Split50_SR_500, Split50_SR_550, Split50_SR_600,
                                   Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800,
                                   Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000)
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                   Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400,
                                   Split50_SR_450, Split50_SR_500)
    
  }else if(input$distance== "Labelled_data_200"){
    Split50_SR_Data <-  data.table(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200)
    
    
  }
  Split50_SR_Avg <- mean(t(Split50_SR_Data))
  # Split50_SR_Max1 <- max(t(Split50_SR_Data))
  # Split50_SR_Min1 <- min(t(Split50_SR_Data))
  # Split50_SR_DO <-  round((Split50_SR_Max1-Split50_SR_Min1)/Split50_SR_Max1*100,2)
  # Split50_SR_Max <-  sec_to_ms(Split50_SR_Max1)
  # Split50_SR_Min <-  sec_to_ms(Split50_SR_Min1)
  
  #100 splits
  Split100_SR_100 <- round(((Split50_SR_50+Split50_SR_100)/2),2)
  Split100_SR_200 <- round(((Split50_SR_150+Split50_SR_200)/2),2)
  Split100_SR_300 <- round(((Split50_SR_250+Split50_SR_300)/2),2)
  Split100_SR_400 <- round(((Split50_SR_350+Split50_SR_400)/2),2)
  Split100_SR_500 <- round(((Split50_SR_450+Split50_SR_500)/2),2)
  Split100_SR_600 <- round(((Split50_SR_550+Split50_SR_600)/2),2)
  Split100_SR_700 <- round(((Split50_SR_650+Split50_SR_700)/2),2)
  Split100_SR_800 <- round(((Split50_SR_750+Split50_SR_800)/2),2)
  Split100_SR_900 <- round(((Split50_SR_850+Split50_SR_900)/2),2)
  Split100_SR_1000 <- round(((Split50_SR_50+Split50_SR_1000)/2),2)
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                    Split100_SR_300,  Split100_SR_400,
                                    Split100_SR_500, Split100_SR_600,
                                    Split100_SR_700, Split100_SR_800,
                                    Split100_SR_900, Split100_SR_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200,
                                    Split100_SR_300,  Split100_SR_400,
                                    Split100_SR_500)
  }else if(input$distance== "Labelled_data_200"){
    Split100_SR_Data <-  data.table(Split100_SR_100, Split100_SR_200)
    
  }
  
  
  Split100_SR_Avg <- mean(t(Split100_SR_Data))
  # Split100_SR_Max1 <- max(t(Split100_SR_Data))
  # Split100_SR_Min1 <- min(t(Split100_SR_Data))
  # Split100_SR_DO <-  round((Split100_SR_Max1-Split100_SR_Min1)/Split100_SR_Max1*100,2)
  # Split100_SR_Max <-  sec_to_ms(Split100_SR_Max1)
  # Split100_SR_Min <-  sec_to_ms(Split100_SR_Min1)
  
  #250 splits
  
  Split250_SR_250 <- round(((Split50_SR_50+Split50_SR_100+Split50_SR_150+Split50_SR_200+Split50_SR_250)/5),2)
  Split250_SR_500 <- round(((Split50_SR_300+Split50_SR_350+Split50_SR_400+Split50_SR_450+Split50_SR_500)/5),2)
  Split250_SR_750 <- round(((Split50_SR_550+Split50_SR_600+Split50_SR_650+Split50_SR_700+Split50_SR_750)/5),2)
  Split250_SR_1000 <- round(((Split50_SR_800+Split50_SR_850+Split50_SR_900+Split50_SR_950+Split50_SR_1000)/5),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500,
                                    Split250_SR_750,  Split250_SR_1000)
    Split250_SR_Avg <- mean(t(Split250_SR_Data))
  }else if(input$distance== "Labelled_data_500"){
    
    Split250_SR_Data <-  data.table(Split250_SR_250, Split250_SR_500)
    Split250_SR_Avg <- mean(t(Split250_SR_Data)) 
  }
  
  # Split250_SR_Max1 <- max(t(Split250_SR_Data))
  # Split250_SR_Min1 <- min(t(Split250_SR_Data))
  # Split250_SR_DO <-  round((Split250_SR_Max1-Split250_SR_Min1)/Split250_SR_Max1*100,2)
  # Split250_SR_Max <-  sec_to_ms(Split250_SR_Max1)
  # Split250_SR_Min <-  sec_to_ms(Split250_SR_Min1)
  
  #500 Splits
  Split500_SR_500 <- round(((Split250_SR_250+Split250_SR_500)/2),2)
  Split500_SR_1000 <- round(((Split250_SR_750+Split250_SR_1000)/2),2)
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_SR_Data <-  data.table(Split500_SR_500,
                                    Split500_SR_1000)
    Split500_SR_Avg <- mean(t(Split500_SR_Data))
  }else if(input$distance== "Labelled_data_500"){
    Split500_SR_Data <-  data.table(Split500_SR_500)
    Split500_SR_Avg <- mean(t(Split500_SR_Data))
    
    
  }
  
  
  
  ########################
  
  #AvVel 50 splits
  
  Split50_Pace500_50 <-  Split50_Time_50*10
  Split50_Pace500_100 <-  Split50_Time_100*10
  Split50_Pace500_150 <-  Split50_Time_150*10
  Split50_Pace500_200 <-  Split50_Time_200*10
  Split50_Pace500_250 <-  Split50_Time_250*10
  Split50_Pace500_300 <-  Split50_Time_300*10
  Split50_Pace500_350 <-  Split50_Time_350*10
  Split50_Pace500_400 <-  Split50_Time_400*10
  Split50_Pace500_450 <-  Split50_Time_450*10
  Split50_Pace500_500 <-  Split50_Time_500*10
  Split50_Pace500_550 <-  Split50_Time_550*10
  Split50_Pace500_600 <-  Split50_Time_600*10
  Split50_Pace500_650 <-  Split50_Time_650*10
  Split50_Pace500_700 <-  Split50_Time_700*10
  Split50_Pace500_750 <-  Split50_Time_750*10
  Split50_Pace500_800 <-  Split50_Time_800*10
  Split50_Pace500_850 <-  Split50_Time_850*10
  Split50_Pace500_900 <-  Split50_Time_900*10
  Split50_Pace500_950 <-  Split50_Time_950*10
  Split50_Pace500_1000 <-  Split50_Time_1000*10
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                        Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                        Split50_Pace500_450, Split50_Pace500_500, Split50_Pace500_550, Split50_Pace500_600,
                                        Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800,
                                        Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000)
    
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                        Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400,
                                        Split50_Pace500_450, Split50_Pace500_500)
    
  }else if(input$distance== "Labelled_data_200"){
    Split50_Pace500_Data <-  data.table(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200)
    
  }
  Split50_Pace500_Data <-  as.numeric(Split50_Pace500_Data)
  Split50_Pace500_Avg <- sec_to_ms(round(mean(t(Split50_Pace500_Data)),2))
  # Split50_Pace500_Max <- round(max(t(Split50_Pace500_Data)),2)
  # Split50_Pace500_Min <- round(min(t(Split50_Pace500_Data)),2)
  # Split50_Pace500_DO <-  round((Split50_Pace500_Max-Split50_Pace500_Min)/Split50_Pace500_Max*100,2)
  # Pace 100 splits
  Split100_Pace500_100 <-  Split100_Time_100*5
  Split100_Pace500_200 <-  Split100_Time_200*5
  Split100_Pace500_300 <-  Split100_Time_300*5
  Split100_Pace500_400 <-  Split100_Time_400*5
  Split100_Pace500_500 <-  Split100_Time_500*5
  Split100_Pace500_600 <-  Split100_Time_600*5
  Split100_Pace500_700 <-  Split100_Time_700*5
  Split100_Pace500_800 <-  Split100_Time_800*5
  Split100_Pace500_900 <-  Split100_Time_900*5
  Split100_Pace500_1000 <-  Split100_Time_1000*5
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                         Split100_Pace500_300,  Split100_Pace500_400,
                                         Split100_Pace500_500, Split100_Pace500_600,
                                         Split100_Pace500_700, Split100_Pace500_800,
                                         Split100_Pace500_900, Split100_Pace500_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200,
                                         Split100_Pace500_300,  Split100_Pace500_400,
                                         Split100_Pace500_500)
  }else if(input$distance== "Labelled_data_200"){
    Split100_Pace500_Data <-  data.table(Split100_Pace500_100, Split100_Pace500_200)
  }
  Split100_Pace500_Data <-  as.numeric(Split100_Pace500_Data)
  Split100_Pace500_Avg <- sec_to_ms(round(mean(t(Split100_Pace500_Data)),2))
  # Split100_Pace500_Max <- round(max(t(Split100_Pace500_Data)),2)
  # Split100_Pace500_Min <- round(min(t(Split100_Pace500_Data)),2)
  # Split100_Pace500_DO <-  round((Split100_Pace500_Max-Split100_Pace500_Min)/Split100_Pace500_Max*100,2)
  
  
  # Pace 250 splits
  Split250_Pace500_250 <-  Split250_Time_250*2
  Split250_Pace500_500 <-  Split250_Time_500*2
  Split250_Pace500_750 <-  Split250_Time_750*2
  Split250_Pace500_1000 <-  Split250_Time_1000*2
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500,
                                         Split250_Pace500_750,  Split250_Pace500_1000)
    Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
    Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
  }else if(input$distance== "Labelled_data_500"){
    Split250_Pace500_Data <-  data.table(Split250_Pace500_250, Split250_Pace500_500)
    
    Split250_Pace500_Data <-  as.numeric(Split250_Pace500_Data)
    Split250_Pace500_Avg <- sec_to_ms(round(mean(t(Split250_Pace500_Data)),2))
  }
  
  
  
  # Split250_Pace500_Max <- round(max(t(Split250_Pace500_Data)),2)
  # Split250_Pace500_Min <- round(min(t(Split250_Pace500_Data)),2)
  # Split250_Pace500_DO <-  round((Split250_Pace500_Max-Split250_Pace500_Min)/Split250_Pace500_Max*100,2)
  
  # Pace /500 splits
  Split500_Pace500_500 <-  Split500_Time_500*1
  Split500_Pace500_1000 <-  Split500_Time_1000*1
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_Pace500_Data <-  data.table(Split500_Pace500_500,
                                         Split500_Pace500_1000)
    Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
    Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
  }else if(input$distance== "Labelled_data_500"){
    Split500_Pace500_Data <-  data.table(Split500_Pace500_500)
    
    Split500_Pace500_Data <-  as.numeric(Split500_Pace500_Data)
    Split500_Pace500_Avg <- sec_to_ms(round(mean(t(Split500_Pace500_Data)),2))
  }
  
  # Split500_Pace500_Max <- round(max(t(Split500_Pace500_Data)),2)
  # Split500_Pace500_Min <- round(min(t(Split500_Pace500_Data)),2)
  # Split500_Pace500_DO <-  round((Split500_Pace500_Max-Split500_Pace500_Min)/Split500_Pace500_Max*100,2)
  
  
  #######################
  
  
  #AvVel 50 splits
  
  Split50_Pace1000_50 <-  Split50_Time_50*20
  Split50_Pace1000_100 <-  Split50_Time_100*20
  Split50_Pace1000_150 <-  Split50_Time_150*20
  Split50_Pace1000_200 <-  Split50_Time_200*20
  Split50_Pace1000_250 <-  Split50_Time_250*20
  Split50_Pace1000_300 <-  Split50_Time_300*20
  Split50_Pace1000_350 <-  Split50_Time_350*20
  Split50_Pace1000_400 <-  Split50_Time_400*20
  Split50_Pace1000_450 <-  Split50_Time_450*20
  Split50_Pace1000_500 <-  Split50_Time_500*20
  Split50_Pace1000_550 <-  Split50_Time_550*20
  Split50_Pace1000_600 <-  Split50_Time_600*20
  Split50_Pace1000_650 <-  Split50_Time_650*20
  Split50_Pace1000_700 <-  Split50_Time_700*20
  Split50_Pace1000_750 <-  Split50_Time_750*20
  Split50_Pace1000_800 <-  Split50_Time_800*20
  Split50_Pace1000_850 <-  Split50_Time_850*20
  Split50_Pace1000_900 <-  Split50_Time_900*20
  Split50_Pace1000_950 <-  Split50_Time_950*20
  Split50_Pace1000_1000 <-  Split50_Time_1000*20
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                         Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                         Split50_Pace1000_450, Split50_Pace1000_500, Split50_Pace1000_550, Split50_Pace1000_600,
                                         Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800,
                                         Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000)
    
  }else if(input$distance== "Labelled_data_500"){
    
    Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                         Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400,
                                         Split50_Pace1000_450, Split50_Pace1000_500)
    
  }else if(input$distance== "Labelled_data_200"){
    Split50_Pace1000_Data <-  data.table(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200)
    
  }
  Split50_Pace1000_Data <-  as.numeric(Split50_Pace1000_Data)
  Split50_Pace1000_Avg <- sec_to_ms(round(mean(t(Split50_Pace1000_Data)),2))
  # Split50_Pace1000_Max <- round(max(t(Split50_Pace1000_Data)),2)
  # Split50_Pace1000_Min <- round(min(t(Split50_Pace1000_Data)),2)
  # Split50_Pace1000_DO <-  round((Split50_Pace1000_Max-Split50_Pace1000_Min)/Split50_Pace1000_Max*100,2)
  # Pace 100 splits
  Split100_Pace1000_100 <-  Split100_Time_100*10
  Split100_Pace1000_200 <-  Split100_Time_200*10
  Split100_Pace1000_300 <-  Split100_Time_300*10
  Split100_Pace1000_400 <-  Split100_Time_400*10
  Split100_Pace1000_500 <-  Split100_Time_500*10
  Split100_Pace1000_600 <-  Split100_Time_600*10
  Split100_Pace1000_700 <-  Split100_Time_700*10
  Split100_Pace1000_800 <-  Split100_Time_800*10
  Split100_Pace1000_900 <- Split100_Time_900*10
  Split100_Pace1000_1000 <-  Split100_Time_1000*10
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                          Split100_Pace1000_300,  Split100_Pace1000_400,
                                          Split100_Pace1000_500, Split100_Pace1000_600,
                                          Split100_Pace1000_700, Split100_Pace1000_800,
                                          Split100_Pace1000_900, Split100_Pace1000_1000)
  }else if(input$distance== "Labelled_data_500"){
    Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200,
                                          Split100_Pace1000_300,  Split100_Pace1000_400,
                                          Split100_Pace1000_500)
    
    
  }else if(input$distance== "Labelled_data_200"){
    Split100_Pace1000_Data <-  data.table(Split100_Pace1000_100, Split100_Pace1000_200)
    
  }
  Split100_Pace1000_Data <-  as.numeric(Split100_Pace1000_Data)
  Split100_Pace1000_Avg <- sec_to_ms(round(mean(t(Split100_Pace1000_Data)),2))
  # Split100_Pace1000_Max <- round(max(t(Split100_Pace1000_Data)),2)
  # Split100_Pace1000_Min <- round(min(t(Split100_Pace1000_Data)),2)
  # Split100_Pace1000_DO <-  round((Split100_Pace1000_Max-Split100_Pace1000_Min)/Split100_Pace1000_Max*100,2)
  
  
  # Pace 250 splits
  Split250_Pace1000_250 <-  Split250_Time_250*8
  Split250_Pace1000_500 <-  Split250_Time_500*8
  Split250_Pace1000_750 <-  Split250_Time_750*8
  Split250_Pace1000_1000 <-  Split250_Time_1000*8
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500,
                                          Split250_Pace1000_750,  Split250_Pace1000_1000)
    Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
    Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
  }else if(input$distance== "Labelled_data_500"){
    Split250_Pace1000_Data <-  data.table(Split250_Pace1000_250, Split250_Pace1000_500)
    
    Split250_Pace1000_Data <-  as.numeric(Split250_Pace1000_Data)
    Split250_Pace1000_Avg <- sec_to_ms(round(mean(t(Split250_Pace1000_Data)),2))
  }
  
  
  
  # Split250_Pace1000_Max <- round(max(t(Split250_Pace1000_Data)),2)
  # Split250_Pace1000_Min <- round(min(t(Split250_Pace1000_Data)),2)
  # Split250_Pace1000_DO <-  round((Split250_Pace1000_Max-Split250_Pace1000_Min)/Split250_Pace1000_Max*100,2)
  
  # Pace /500 splits
  Split500_Pace1000_500 <-  Split500_Time_500*2
  Split500_Pace1000_1000 <-  Split500_Time_1000*2
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500,
                                          Split500_Pace1000_1000)
    Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
    Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
  }else if(input$distance== "Labelled_data_500"){
    Split500_Pace1000_Data <-  data.table(Split500_Pace1000_500)
    
    Split500_Pace1000_Data <-  as.numeric(Split500_Pace1000_Data)
    Split500_Pace1000_Avg <- sec_to_ms(round(mean(t(Split500_Pace1000_Data)),2))
    
  }
  
  # Split500_Pace1000_Max <- round(max(t(Split500_Pace1000_Data)),2)
  # Split500_Pace1000_Min <- round(min(t(Split500_Pace1000_Data)),2)
  # Split500_Pace1000_DO <-  round((Split500_Pace1000_Max-Split500_Pace1000_Min)/Split500_Pace1000_Max*100,2)
  
  
  #### Convert to ms in table
  Split50_Time_50 <- sec_to_ms(Split50_Time_50)
  Split50_Time_100 <- sec_to_ms(Split50_Time_100)
  Split50_Time_150 <- sec_to_ms(Split50_Time_150)
  Split50_Time_200 <- sec_to_ms(Split50_Time_200)
  Split50_Time_250 <- sec_to_ms(Split50_Time_250)
  Split50_Time_300 <- sec_to_ms(Split50_Time_300)
  Split50_Time_350 <- sec_to_ms(Split50_Time_350)
  Split50_Time_400 <- sec_to_ms(Split50_Time_400)
  Split50_Time_450 <- sec_to_ms(Split50_Time_450)
  Split50_Time_500 <- sec_to_ms(Split50_Time_500)
  Split50_Time_550 <- sec_to_ms(Split50_Time_550)
  Split50_Time_600 <- sec_to_ms(Split50_Time_600)
  Split50_Time_650 <- sec_to_ms(Split50_Time_650)
  Split50_Time_700 <- sec_to_ms(Split50_Time_700)
  Split50_Time_750 <- sec_to_ms(Split50_Time_750)
  Split50_Time_800 <- sec_to_ms(Split50_Time_800)
  Split50_Time_850 <- sec_to_ms(Split50_Time_850)
  Split50_Time_900 <- sec_to_ms(Split50_Time_900)
  Split50_Time_950 <- sec_to_ms(Split50_Time_950)
  Split50_Time_1000 <- sec_to_ms(Split50_Time_1000)
  
  
  #100 splits
  Split100_Time_100 <- sec_to_ms(Split100_Time_100)
  Split100_Time_200 <- sec_to_ms(Split100_Time_200)
  Split100_Time_300 <- sec_to_ms(Split100_Time_300)
  Split100_Time_400 <- sec_to_ms(Split100_Time_400)
  Split100_Time_500 <- sec_to_ms(Split100_Time_500)
  Split100_Time_600 <- sec_to_ms(Split100_Time_600)
  Split100_Time_700 <- sec_to_ms(Split100_Time_700)
  Split100_Time_800 <- sec_to_ms(Split100_Time_800)
  Split100_Time_900 <- sec_to_ms(Split100_Time_900)
  Split100_Time_1000 <- sec_to_ms(Split100_Time_1000)
  
  
  #250 splits
  
  Split250_Time_250 <- sec_to_ms(Split250_Time_250)
  Split250_Time_500 <- sec_to_ms(Split250_Time_500)
  Split250_Time_750 <- sec_to_ms(Split250_Time_750)
  Split250_Time_1000 <- sec_to_ms(Split250_Time_1000)
  
  
  
  #500 Splits
  Split500_Time_500 <- sec_to_ms(Split500_Time_500)
  Split500_Time_1000 <- sec_to_ms(Split500_Time_1000)
  
  ###################
  #### Convert to ms in table
  Split50_Pace500_50 <- sec_to_ms(Split50_Pace500_50)
  Split50_Pace500_100 <- sec_to_ms(Split50_Pace500_100)
  Split50_Pace500_150 <- sec_to_ms(Split50_Pace500_150)
  Split50_Pace500_200 <- sec_to_ms(Split50_Pace500_200)
  Split50_Pace500_250 <- sec_to_ms(Split50_Pace500_250)
  Split50_Pace500_300 <- sec_to_ms(Split50_Pace500_300)
  Split50_Pace500_350 <- sec_to_ms(Split50_Pace500_350)
  Split50_Pace500_400 <- sec_to_ms(Split50_Pace500_400)
  Split50_Pace500_450 <- sec_to_ms(Split50_Pace500_450)
  Split50_Pace500_500 <- sec_to_ms(Split50_Pace500_500)
  Split50_Pace500_550 <- sec_to_ms(Split50_Pace500_550)
  Split50_Pace500_600 <- sec_to_ms(Split50_Pace500_600)
  Split50_Pace500_650 <- sec_to_ms(Split50_Pace500_650)
  Split50_Pace500_700 <- sec_to_ms(Split50_Pace500_700)
  Split50_Pace500_750 <- sec_to_ms(Split50_Pace500_750)
  Split50_Pace500_800 <- sec_to_ms(Split50_Pace500_800)
  Split50_Pace500_850 <- sec_to_ms(Split50_Pace500_850)
  Split50_Pace500_900 <- sec_to_ms(Split50_Pace500_900)
  Split50_Pace500_950 <- sec_to_ms(Split50_Pace500_950)
  Split50_Pace500_1000 <- sec_to_ms(Split50_Pace500_1000)
  
  
  #100 splits
  Split100_Pace500_100 <- sec_to_ms(Split100_Pace500_100)
  Split100_Pace500_200 <- sec_to_ms(Split100_Pace500_200)
  Split100_Pace500_300 <- sec_to_ms(Split100_Pace500_300)
  Split100_Pace500_400 <- sec_to_ms(Split100_Pace500_400)
  Split100_Pace500_500 <- sec_to_ms(Split100_Pace500_500)
  Split100_Pace500_600 <- sec_to_ms(Split100_Pace500_600)
  Split100_Pace500_700 <- sec_to_ms(Split100_Pace500_700)
  Split100_Pace500_800 <- sec_to_ms(Split100_Pace500_800)
  Split100_Pace500_900 <- sec_to_ms(Split100_Pace500_900)
  Split100_Pace500_1000 <- sec_to_ms(Split100_Pace500_1000)
  
  
  #250 splits
  
  Split250_Pace500_250 <- sec_to_ms(Split250_Pace500_250)
  Split250_Pace500_500 <- sec_to_ms(Split250_Pace500_500)
  Split250_Pace500_750 <- sec_to_ms(Split250_Pace500_750)
  Split250_Pace500_1000 <- sec_to_ms(Split250_Pace500_1000)
  
  
  
  #500 Splits
  Split500_Pace500_500 <- sec_to_ms(Split500_Pace500_500)
  Split500_Pace500_1000 <- sec_to_ms(Split500_Pace500_1000)
  
  ################### 
  
  Split50_Pace1000_50 <- sec_to_ms(Split50_Pace1000_50)
  Split50_Pace1000_100 <- sec_to_ms(Split50_Pace1000_100)
  Split50_Pace1000_150 <- sec_to_ms(Split50_Pace1000_150)
  Split50_Pace1000_200 <- sec_to_ms(Split50_Pace1000_200)
  Split50_Pace1000_250 <- sec_to_ms(Split50_Pace1000_250)
  Split50_Pace1000_300 <- sec_to_ms(Split50_Pace1000_300)
  Split50_Pace1000_350 <- sec_to_ms(Split50_Pace1000_350)
  Split50_Pace1000_400 <- sec_to_ms(Split50_Pace1000_400)
  Split50_Pace1000_450 <- sec_to_ms(Split50_Pace1000_450)
  Split50_Pace1000_500 <- sec_to_ms(Split50_Pace1000_500)
  Split50_Pace1000_550 <- sec_to_ms(Split50_Pace1000_550)
  Split50_Pace1000_600 <- sec_to_ms(Split50_Pace1000_600)
  Split50_Pace1000_650 <- sec_to_ms(Split50_Pace1000_650)
  Split50_Pace1000_700 <- sec_to_ms(Split50_Pace1000_700)
  Split50_Pace1000_750 <- sec_to_ms(Split50_Pace1000_750)
  Split50_Pace1000_800 <- sec_to_ms(Split50_Pace1000_800)
  Split50_Pace1000_850 <- sec_to_ms(Split50_Pace1000_850)
  Split50_Pace1000_900 <- sec_to_ms(Split50_Pace1000_900)
  Split50_Pace1000_950 <- sec_to_ms(Split50_Pace1000_950)
  Split50_Pace1000_1000 <- sec_to_ms(Split50_Pace1000_1000)
  
  #100 splits
  Split100_Pace1000_100 <- sec_to_ms(Split100_Pace1000_100)
  Split100_Pace1000_200 <- sec_to_ms(Split100_Pace1000_200)
  Split100_Pace1000_300 <- sec_to_ms(Split100_Pace1000_300)
  Split100_Pace1000_400 <- sec_to_ms(Split100_Pace1000_400)
  Split100_Pace1000_500 <- sec_to_ms(Split100_Pace1000_500)
  Split100_Pace1000_600 <- sec_to_ms(Split100_Pace1000_600)
  Split100_Pace1000_700 <- sec_to_ms(Split100_Pace1000_700)
  Split100_Pace1000_800 <- sec_to_ms(Split100_Pace1000_800)
  Split100_Pace1000_900 <- sec_to_ms(Split100_Pace1000_900)
  Split100_Pace1000_1000 <- sec_to_ms(Split100_Pace1000_1000)
  
  
  #250 splits
  
  Split250_Pace1000_250 <- sec_to_ms(Split250_Pace1000_250)
  Split250_Pace1000_500 <- sec_to_ms(Split250_Pace1000_500)
  Split250_Pace1000_750 <- sec_to_ms(Split250_Pace1000_750)
  Split250_Pace1000_1000 <- sec_to_ms(Split250_Pace1000_1000)
  
  
  
  #500 Splits
  Split500_Pace1000_500 <- sec_to_ms(Split500_Pace1000_500)
  Split500_Pace1000_1000 <- sec_to_ms(Split500_Pace1000_1000)
  
  ################### 
  
  
  
  
  
  if (input$distance== "Labelled_data_3x3x1km"){
    
    
    Top10 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550", "600", "650", "700", "750", "800", "850", "900", "950", "1000", "", "Average"),
                         "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                 Split50_Time_550, Split50_Time_600, Split50_Time_650, Split50_Time_700, Split50_Time_750, Split50_Time_800, Split50_Time_850, Split50_Time_900, Split50_Time_950, Split50_Time_1000,
                                                 "", Split50_Time_Avg),
                         "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500, "",
                                                  Split100_Time_600, "", Split100_Time_700, "", Split100_Time_800, "", Split100_Time_900, "", Split100_Time_1000,
                                                  "", Split100_Time_Avg),
                         
                         
                         "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500, "", "", "", "", Split250_Time_750, "", "", "", "", Split250_Time_1000,
                                                  "", Split250_Time_Avg),
                         "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500, "", "", "", "", "", "", "", "", "", Split500_Time_1000,
                                                  "", Split500_Time_Avg),
                         "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                             Split50_Pace500_550, Split50_Pace500_600, Split50_Pace500_650, Split50_Pace500_700, Split50_Pace500_750, Split50_Pace500_800, Split50_Pace500_850, Split50_Pace500_900, Split50_Pace500_950, Split50_Pace500_1000,
                                             "", Split50_Pace500_Avg),
                         "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500, "",
                                              Split100_Pace500_600, "", Split100_Pace500_700, "", Split100_Pace500_800, "", Split100_Pace500_900, "", Split100_Pace500_1000,
                                              "", Split100_Pace500_Avg),
                         
                         
                         "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500, "", "", "", "", Split250_Pace500_750, "", "", "", "", Split250_Pace500_1000,
                                              "", Split250_Pace500_Avg),
                         "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500, "", "", "", "", "", "", "", "", "", Split500_Pace500_1000,
                                              "", Split500_Pace500_Avg),
                         
                         "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                              Split50_Pace1000_550, Split50_Pace1000_600, Split50_Pace1000_650, Split50_Pace1000_700, Split50_Pace1000_750, Split50_Pace1000_800, Split50_Pace1000_850, Split50_Pace1000_900, Split50_Pace1000_950, Split50_Pace1000_1000,
                                              "", Split50_Pace1000_Avg),
                         "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500, "",
                                               Split100_Pace1000_600, "", Split100_Pace1000_700, "", Split100_Pace1000_800, "", Split100_Pace1000_900, "", Split100_Pace1000_1000,
                                               "", Split100_Pace1000_Avg),
                         
                         
                         "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500, "", "", "", "", Split250_Pace1000_750, "", "", "", "", Split250_Pace1000_1000,
                                               "", Split250_Pace1000_Avg),
                         "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500, "", "", "", "", "", "", "", "", "", Split500_Pace1000_1000,
                                               "", Split500_Pace1000_Avg),
                         "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                              Split50_AvVel_550, Split50_AvVel_600, Split50_AvVel_650, Split50_AvVel_700, Split50_AvVel_750, Split50_AvVel_800, Split50_AvVel_850, Split50_AvVel_900, Split50_AvVel_950, Split50_AvVel_1000,
                                              "", Split50_AvVel_Avg),
                         "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500, "",
                                               Split100_AvVel_600, "", Split100_AvVel_700, "", Split100_AvVel_800, "", Split100_AvVel_900, "", Split100_AvVel_1000,
                                               "", Split100_AvVel_Avg),
                         "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500, "", "", "", "", Split250_AvVel_750, "", "", "", "", Split250_AvVel_1000,
                                               "", Split250_AvVel_Avg),
                         "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500, "", "", "", "", "", "", "", "", "", Split500_AvVel_1000,
                                               "", Split500_AvVel_Avg),
                         "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                            Split50_SR_550, Split50_SR_600, Split50_SR_650, Split50_SR_700, Split50_SR_750, Split50_SR_800, Split50_SR_850, Split50_SR_900, Split50_SR_950, Split50_SR_1000,
                                            "", Split50_SR_Avg),
                         "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500, "",
                                             Split100_SR_600, "", Split100_SR_700, "", Split100_SR_800, "", Split100_SR_900, "", Split100_SR_1000,
                                             "", Split100_SR_Avg),
                         "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500, "", "", "", "", Split250_SR_750, "", "", "", "", Split250_SR_1000,
                                             "", Split250_SR_Avg),
                         "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500, "", "", "", "", "", "", "", "", "", Split500_SR_1000,
                                             "", Split500_SR_Avg))
    
  }else if(input$distance== "Labelled_data_500"){
    
    
    
    Top10 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "", "Average"),
                         "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200, Split50_Time_250, Split50_Time_300, Split50_Time_350, Split50_Time_400, Split50_Time_450, Split50_Time_500,
                                                 "", Split50_Time_Avg),
                         "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200, "", Split100_Time_300, "", Split100_Time_400, "", Split100_Time_500,
                                                  "", Split100_Time_Avg),
                         "250m splits (secs)" = c("", "", "", "",Split250_Time_250, "", "", "", "", Split250_Time_500,
                                                  "", Split250_Time_Avg),
                         "500m splits (secs)" = c("", "", "", "", "", "", "", "", "", Split500_Time_500,
                                                  "", Split500_Time_Avg),
                         "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200, Split50_Pace500_250, Split50_Pace500_300, Split50_Pace500_350, Split50_Pace500_400, Split50_Pace500_450, Split50_Pace500_500,
                                             "", Split50_Pace500_Avg),
                         "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200, "", Split100_Pace500_300, "", Split100_Pace500_400, "", Split100_Pace500_500,
                                              "", Split100_Pace500_Avg),
                         "250m Pace /500" = c("", "", "", "",Split250_Pace500_250, "", "", "", "", Split250_Pace500_500,
                                              "", Split250_Pace500_Avg),
                         "500m Pace /500" = c("", "", "", "", "", "", "", "", "", Split500_Pace500_500,
                                              "", Split500_Pace500_Avg),
                         
                         "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200, Split50_Pace1000_250, Split50_Pace1000_300, Split50_Pace1000_350, Split50_Pace1000_400, Split50_Pace1000_450, Split50_Pace1000_500,
                                              "", Split50_Pace1000_Avg),
                         "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200, "", Split100_Pace1000_300, "", Split100_Pace1000_400, "", Split100_Pace1000_500,
                                               "", Split100_Pace1000_Avg),
                         "250m Pace /1000" = c("", "", "", "",Split250_Pace1000_250, "", "", "", "", Split250_Pace1000_500,
                                               "", Split250_Pace1000_Avg),
                         "500m Pace /1000" = c("", "", "", "", "", "", "", "", "", Split500_Pace1000_500,
                                               "", Split500_Pace1000_Avg),
                         
                         "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200, Split50_AvVel_250, Split50_AvVel_300, Split50_AvVel_350, Split50_AvVel_400, Split50_AvVel_450, Split50_AvVel_500,
                                              "", Split50_AvVel_Avg),
                         
                         "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200, "", Split100_AvVel_300, "", Split100_AvVel_400, "", Split100_AvVel_500,
                                               "", Split100_AvVel_Avg),
                         "250m vel (km/h)" = c("", "", "", "",Split250_AvVel_250, "", "", "", "", Split250_AvVel_500,
                                               "", Split250_AvVel_Avg),
                         "500m vel (km/h)" = c("", "", "", "", "", "", "", "", "", Split500_AvVel_500,
                                               "", Split500_AvVel_Avg),
                         "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200, Split50_SR_250, Split50_SR_300, Split50_SR_350, Split50_SR_400, Split50_SR_450, Split50_SR_500,
                                            "", Split50_SR_Avg),
                         
                         "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200, "", Split100_SR_300, "", Split100_SR_400, "", Split100_SR_500,
                                             "", Split100_SR_Avg),
                         "250m SR (spm)" = c("", "", "", "",Split250_SR_250, "", "", "", "", Split250_SR_500,
                                             "", Split250_SR_Avg),
                         "500m SR (spm)" = c("", "", "", "", "", "", "", "", "", Split500_SR_500,
                                             "", Split500_SR_Avg))
    
    
  }else if(input$distance== "Labelled_data_200"){
    
    Top10 <-  data.table("Distance (m)" = c("50", "100", "150", "200", "", "Average"),
                         "50m splits (secs)" = c(Split50_Time_50, Split50_Time_100, Split50_Time_150, Split50_Time_200,
                                                 "", Split50_Time_Avg),
                         "100m splits (secs)" = c("",Split100_Time_100, "", Split100_Time_200,
                                                  "", Split100_Time_Avg),
                         "50m Pace /500" = c(Split50_Pace500_50, Split50_Pace500_100, Split50_Pace500_150, Split50_Pace500_200,
                                             "", Split50_Pace500_Avg),
                         "100m Pace /500" = c("",Split100_Pace500_100, "", Split100_Pace500_200,
                                              "", Split100_Pace500_Avg),
                         "50m Pace /1000" = c(Split50_Pace1000_50, Split50_Pace1000_100, Split50_Pace1000_150, Split50_Pace1000_200,
                                              "", Split50_Pace1000_Avg),
                         "100m Pace /1000" = c("",Split100_Pace1000_100, "", Split100_Pace1000_200,
                                               "", Split100_Pace1000_Avg),
                         
                         "50m vel (km/h)" = c(Split50_AvVel_50, Split50_AvVel_100, Split50_AvVel_150, Split50_AvVel_200,
                                              "", Split50_AvVel_Avg),
                         
                         "100m vel (km/h)" = c("",Split100_AvVel_100, "", Split100_AvVel_200,
                                               "", Split100_AvVel_Avg),
                         "50m SR (spm)" = c(Split50_SR_50, Split50_SR_100, Split50_SR_150, Split50_SR_200,
                                            "", Split50_SR_Avg),
                         
                         "100m SR (spm)" = c("",Split100_SR_100, "", Split100_SR_200,
                                             "", Split100_SR_Avg))
  }
  return(Top10)
  
  
  
})

# ### Splits Stroke calculations ###
# Filtered_AvStkRate_data <- <-  tab_AvStkRate() %>% select(variable, value)
# Filtered_AvStkRate_data <- column_to_rownames(Filtered_AvStkRate_data,'variable')
# #250 splits
# 
# Split250_AvStkRate_250 <- format(round((mean(Filtered_AvStkRate_data[2:6,])),2), nsmall = 2)
# Split250_AvStkRate_500 <- format(round((mean(Filtered_AvStkRate_data[7:11,])),2), nsmall = 2)
# Split250_AvStkRate_750 <- format(round((mean(Filtered_AvStkRate_data[12:16,])),2), nsmall = 2)
# Split250_AvStkRate_1000 <- format(round((mean(Filtered_AvStkRate_data[17:21,])),2), nsmall = 2)
# Split250_AvStkRate_1250 <- format(round((mean(Filtered_AvStkRate_data[22:26,])),2), nsmall = 2)
# Split250_AvStkRate_1500 <- format(round((mean(Filtered_AvStkRate_data[27:31,])),2), nsmall = 2)
# Split250_AvStkRate_1750 <- format(round((mean(Filtered_AvStkRate_data[32:36,])),2), nsmall = 2)
# Split250_AvStkRate_2000 <- format(round((mean(Filtered_AvStkRate_data[37:41,])),2), nsmall = 2)
# 

# Filtered_AvProgSpeed_data <-  tab_ProgSpeed() %>% select(variable, value)
# Filtered_AvProgSpeed_data <- column_to_rownames(Filtered_AvProgSpeed_data,'variable')
# ### Splits Prog Speed calculations ####      
# # 250 splits prog speed
# Split250_AvProgSpeed_250 <- format(round((mean(Filtered_AvProgSpeed_data[2:6,])),2), nsmall = 2)
# Split250_AvProgSpeed_500 <- format(round((mean(Filtered_AvProgSpeed_data[7:11,])),2), nsmall = 2)
# Split250_AvProgSpeed_750 <- format(round((mean(Filtered_AvProgSpeed_data[12:16,])),2), nsmall = 2)
# Split250_AvProgSpeed_1000 <- format(round((mean(Filtered_AvProgSpeed_data[17:21,])),2), nsmall = 2)
# Split250_AvProgSpeed_1250 <- format(round((mean(Filtered_AvProgSpeed_data[22:26,])),2), nsmall = 2)
# Split250_AvProgSpeed_1500 <- format(round((mean(Filtered_AvProgSpeed_data[27:31,])),2), nsmall = 2)
# Split250_AvProgSpeed_1750 <- format(round((mean(Filtered_AvProgSpeed_data[32:36,])),2), nsmall = 2)
# Split250_AvProgSpeed_2000 <- format(round((mean(Filtered_AvProgSpeed_data[37:41,])),2), nsmall = 2)
# 
# 

#### Data table ####




output$SummaryTable <-  DT::renderDataTable({ 
  if (input$goButton == 0)
    return()
  input$goButton
  isolate({
    #"50m velocities (km/h)"
    #"50m Stroke Rate (spm)"
    if (input$distance== "Labelled_data_200"){
      if (input$Report_Type == "Single Race"){ 
        
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], 
                                        Race1()[,"100m splits (secs)"])
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "Two Races"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], "Race2" = Race2()[,"50m splits (secs)"], 
                                        Race1()[,"100m splits (secs)"], "Race2" = Race2()[,"100m splits (secs)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m splits (secs)","Race2.100m splits (secs)"), new = c("Race 2", "Race 2"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "vs Top 10"){
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], "Ideal" = Top10()[,"50m splits (secs)"], 
                                        Race1()[,"100m splits (secs)"], "Ideal" = Top10()[,"100m splits (secs)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m splits (secs)","Ideal.100m splits (secs)"), new = c("Top 10", "Top 10"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
          
        })
      }
      
      
      
      
    } else {
      if (input$Report_Type == "Single Race"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], 
                                        Race1()[,"100m splits (secs)"], 
                                        Race1()[,"250m splits (secs)"], 
                                        Race1()[,"500m splits (secs)"])
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "Two Races"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], "Race2" = Race2()[,"50m splits (secs)"], 
                                        Race1()[,"100m splits (secs)"], "Race2" = Race2()[,"100m splits (secs)"],
                                        Race1()[,"250m splits (secs)"], "Race2" = Race2()[,"250m splits (secs)"],
                                        Race1()[,"500m splits (secs)"], "Race2" = Race2()[,"500m splits (secs)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m splits (secs)","Race2.100m splits (secs)","Race2.250m splits (secs)","Race2.500m splits (secs)"), new = c("Race 2", "Race 2","Race 2","Race 2"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "vs Top 10"){
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m splits (secs)"], "Ideal" = Top10()[,"50m splits (secs)"], 
                                        Race1()[,"100m splits (secs)"], "Ideal" = Top10()[,"100m splits (secs)"],
                                        Race1()[,"250m splits (secs)"], "Ideal" = Top10()[,"250m splits (secs)"],
                                        Race1()[,"500m splits (secs)"], "Ideal" = Top10()[,"500m splits (secs)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m splits (secs)","Ideal.100m splits (secs)","Ideal.250m splits (secs)","Ideal.500m splits (secs)"), new = c("Top 10", "Top 10","Top 10","Top 10"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
          
        })
      }
      
    }  
  })
  
})

output$SummaryTablePace <-  DT::renderDataTable({ 
  if (input$goButton == 0)
    return()
  input$goButton
  isolate({
    #"50m velocities (km/h)"
    #"50m Stroke Rate (spm)"
    if (input$distance== "Labelled_data_200"){
      if (input$Report_Type == "Single Race"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                        Race1()[,"50m Pace /500"], 
                                        Race1()[,"100m Pace /500"])
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "Two Races"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                        Race1()[,"50m Pace /500"], "Race2" = Race2()[,"50m Pace /500"], 
                                        Race1()[,"100m Pace /500"], "Race2" = Race2()[,"100m Pace /500"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m Pace /500","Race2.100m Pace /500"), new = c("Race 2", "Race 2"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "vs Top 10"){
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                        Race1()[,"50m Pace /500"], "Ideal" = Top10()[,"50m Pace /500"], 
                                        Race1()[,"100m Pace /500"], "Ideal" = Top10()[,"100m Pace /500"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m Pace /500","Ideal.100m Pace /500"), new = c("Top 10", "Top 10"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
          
        })
      }
    } else {
      if (input$Report_Type == "Single Race"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                        Race1()[,"50m Pace /500"], 
                                        Race1()[,"100m Pace /500"], 
                                        Race1()[,"250m Pace /500"], 
                                        Race1()[,"500m Pace /500"])
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "Two Races"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                        Race1()[,"50m Pace /500"], "Race2" = Race2()[,"50m Pace /500"], 
                                        Race1()[,"100m Pace /500"], "Race2" = Race2()[,"100m Pace /500"],
                                        Race1()[,"250m Pace /500"], "Race2" = Race2()[,"250m Pace /500"],
                                        Race1()[,"500m Pace /500"], "Race2" = Race2()[,"500m Pace /500"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m Pace /500","Race2.100m Pace /500","Race2.250m Pace /500","Race2.500m Pace /500"), new = c("Race 2", "Race 2","Race 2","Race 2"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "vs Top 10"){
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                        Race1()[,"50m Pace /500"], "Ideal" = Top10()[,"50m Pace /500"], 
                                        Race1()[,"100m Pace /500"], "Ideal" = Top10()[,"100m Pace /500"],
                                        Race1()[,"250m Pace /500"], "Ideal" = Top10()[,"250m Pace /500"],
                                        Race1()[,"500m Pace /500"], "Ideal" = Top10()[,"500m Pace /500"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m Pace /500","Ideal.100m Pace /500","Ideal.250m Pace /500","Ideal.500m Pace /500"), new = c("Top 10", "Top 10","Top 10","Top 10"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
          
        })
      }
      
    }
    
  })
  
})

# output$SummaryTable2 <-  DT::renderDataTable({ 
#   
#   #"50m velocities (km/h)"
#   #"50m Stroke Rate (spm)"
#   
#   if (input$Report_Type == "Single Race"){ 
#     Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], 
#                                     Race1()[,"100m vel (km/h)"], 
#                                     Race1()[,"250m vel (km/h)"], 
#                                     Race1()[,"500m vel (km/h)"],
#                                     Race1()[,"50m SR (spm)"], 
#                                     Race1()[,"100m SR (spm)"], 
#                                     Race1()[,"250m SR (spm)"], 
#                                     Race1()[,"500m SR (spm)"])
#     ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
#         formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
#         formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
#                     `border-top` = styleEqual('Average', "solid 3px")) %>%
#         formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
#       
#     })
#     
#   } else if (input$Report_Type == "Two Races"){ 
#     Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Race2" = Race2()[,"50m vel (km/h)"], 
#                                     Race1()[,"100m vel (km/h)"], "Race2" = Race2()[,"100m vel (km/h)"],
#                                     Race1()[,"250m vel (km/h)"], "Race2" = Race2()[,"250m vel (km/h)"],
#                                     Race1()[,"500m vel (km/h)"], "Race2" = Race2()[,"500m vel (km/h)"],
#                                     Race1()[,"50m SR (spm)"], "Race2" = Race2()[,"50m SR (spm)"], 
#                                     Race1()[,"100m SR (spm)"], "Race2" = Race2()[,"100m SR (spm)"],
#                                     Race1()[,"250m SR (spm)"], "Race2" = Race2()[,"250m SR (spm)"],
#                                     Race1()[,"500m SR (spm)"], "Race2" = Race2()[,"500m SR (spm)"])
#     
#     Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m vel (km/h)","Race2.100m vel (km/h)","Race2.250m vel (km/h)","Race2.500m vel (km/h)", 
#                                                             "Race2.50m SR (spm)","Race2.100m SR (spm)","Race2.250m SR (spm)","Race2.500m SR (spm)"), new = c("Race 2", "Race 2","Race 2","Race 2", "Race 2", "Race 2","Race 2","Race 2"))
#     ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
#         formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
#         formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
#                     `border-top` = styleEqual('Average', "solid 3px")) %>%
#         formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
#       
#     })
#     
#   } else if (input$Report_Type == "vs Top 10"){
#     Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Ideal" = Top10()[,"50m vel (km/h)"], 
#                                     Race1()[,"100m vel (km/h)"], "Ideal" = Top10()[,"100m vel (km/h)"],
#                                     Race1()[,"250m vel (km/h)"], "Ideal" = Top10()[,"250m vel (km/h)"],
#                                     Race1()[,"500m vel (km/h)"], "Ideal" = Top10()[,"500m vel (km/h)"],
#                                     Race1()[,"50m SR (spm)"], "Ideal" = Top10()[,"50m SR (spm)"], 
#                                     Race1()[,"100m SR (spm)"], "Ideal" = Top10()[,"100m SR (spm)"],
#                                     Race1()[,"250m SR (spm)"], "Ideal" = Top10()[,"250m SR (spm)"],
#                                     Race1()[,"500m SR (spm)"], "Ideal" = Top10()[,"500m SR (spm)"])
#     
#     Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m vel (km/h)","Ideal.100m vel (km/h)","Ideal.250m vel (km/h)","Ideal.500m vel (km/h)", 
#                                                             "Ideal.50m SR (spm)","Ideal.100m SR (spm)","Ideal.250m SR (spm)","Ideal.500m SR (spm)"),  new = c("Top 10", "Top 10","Top 10","Top 10"))
#     ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
#         formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
#         formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
#                     `border-top` = styleEqual('Average', "solid 3px")) %>%
#         formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
#       
#       
#     })
#   }
#   
#   
#   
#   
# })

output$SummaryTableVel <-  DT::renderDataTable({ 
  
  #"50m velocities (km/h)"
  #"50m Stroke Rate (spm)"
  if (input$goButton == 0)
    return()
  input$goButton
  isolate({
    
    if (input$distance== "Labelled_data_200"){
      
      if (input$Report_Type == "Single Race"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], 
                                        Race1()[,"100m vel (km/h)"])
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "Two Races"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Race2" = Race2()[,"50m vel (km/h)"], 
                                        Race1()[,"100m vel (km/h)"], "Race2" = Race2()[,"100m vel (km/h)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m vel (km/h)","Race2.100m vel (km/h)"), new = c("Race 2", "Race 2"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "vs Top 10"){
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Ideal" = Top10()[,"50m vel (km/h)"], 
                                        Race1()[,"100m vel (km/h)"], "Ideal" = Top10()[,"100m vel (km/h)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m vel (km/h)","Ideal.100m vel (km/h)"),  new = c("Top 10", "Top 10"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
          
        })
      }
    } else {
      
      if (input$Report_Type == "Single Race"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], 
                                        Race1()[,"100m vel (km/h)"], 
                                        Race1()[,"250m vel (km/h)"], 
                                        Race1()[,"500m vel (km/h)"])
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "Two Races"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Race2" = Race2()[,"50m vel (km/h)"], 
                                        Race1()[,"100m vel (km/h)"], "Race2" = Race2()[,"100m vel (km/h)"],
                                        Race1()[,"250m vel (km/h)"], "Race2" = Race2()[,"250m vel (km/h)"],
                                        Race1()[,"500m vel (km/h)"], "Race2" = Race2()[,"500m vel (km/h)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m vel (km/h)","Race2.100m vel (km/h)","Race2.250m vel (km/h)","Race2.500m vel (km/h)"), new = c("Race 2", "Race 2","Race 2","Race 2"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "vs Top 10"){
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], Race1()[,"50m vel (km/h)"], "Ideal" = Top10()[,"50m vel (km/h)"], 
                                        Race1()[,"100m vel (km/h)"], "Ideal" = Top10()[,"100m vel (km/h)"],
                                        Race1()[,"250m vel (km/h)"], "Ideal" = Top10()[,"250m vel (km/h)"],
                                        Race1()[,"500m vel (km/h)"], "Ideal" = Top10()[,"500m vel (km/h)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m vel (km/h)","Ideal.100m vel (km/h)","Ideal.250m vel (km/h)","Ideal.500m vel (km/h)"),  new = c("Top 10", "Top 10","Top 10","Top 10"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
          
        })
      }
      
      
    }
    
  })
})


output$SummaryTableSR <-  DT::renderDataTable({ 
  if (input$goButton == 0)
    return()
  #"50m velocities (km/h)"
  #"50m Stroke Rate (spm)"
  input$goButton
  isolate({
    
    if (input$distance== "Labelled_data_200"){
      if (input$Report_Type == "Single Race"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                        Race1()[,"50m SR (spm)"], 
                                        Race1()[,"100m SR (spm)"])
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "Two Races"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                        Race1()[,"50m SR (spm)"], "Race2" = Race2()[,"50m SR (spm)"], 
                                        Race1()[,"100m SR (spm)"], "Race2" = Race2()[,"100m SR (spm)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m SR (spm)","Race2.100m SR (spm)"), new = c("Race 2", "Race 2"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "vs Top 10"){
        Summarydatatable<-  data.table(Race1()[,"Distance (m)"],
                                       Race1()[,"50m SR (spm)"], "Ideal" = Top10()[,"50m SR (spm)"], 
                                       Race1()[,"100m SR (spm)"], "Ideal" = Top10()[,"100m SR (spm)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m SR (spm)","Ideal.100m SR (spm)"),  new = c("Top 10", "Top 10"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
          
        })
      }
    }else {
      if (input$Report_Type == "Single Race"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"],
                                        Race1()[,"50m SR (spm)"], 
                                        Race1()[,"100m SR (spm)"], 
                                        Race1()[,"250m SR (spm)"], 
                                        Race1()[,"500m SR (spm)"])
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,4),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "Two Races"){ 
        Summarydatatable <-  data.table(Race1()[,"Distance (m)"], 
                                        Race1()[,"50m SR (spm)"], "Race2" = Race2()[,"50m SR (spm)"], 
                                        Race1()[,"100m SR (spm)"], "Race2" = Race2()[,"100m SR (spm)"],
                                        Race1()[,"250m SR (spm)"], "Race2" = Race2()[,"250m SR (spm)"],
                                        Race1()[,"500m SR (spm)"], "Race2" = Race2()[,"500m SR (spm)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Race2.50m SR (spm)","Race2.100m SR (spm)","Race2.250m SR (spm)","Race2.500m SR (spm)"), new = c("Race 2", "Race 2","Race 2","Race 2"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
        })
        
      } else if (input$Report_Type == "vs Top 10"){
        Summarydatatable<-  data.table(Race1()[,"Distance (m)"],
                                       Race1()[,"50m SR (spm)"], "Ideal" = Top10()[,"50m SR (spm)"], 
                                       Race1()[,"100m SR (spm)"], "Ideal" = Top10()[,"100m SR (spm)"],
                                       Race1()[,"250m SR (spm)"], "Ideal" = Top10()[,"250m SR (spm)"],
                                       Race1()[,"500m SR (spm)"], "Ideal" = Top10()[,"500m SR (spm)"])
        
        Summarydatatable <-  setnames(Summarydatatable, old = c("Ideal.50m SR (spm)","Ideal.100m SR (spm)","Ideal.250m SR (spm)","Ideal.500m SR (spm)"),  new = c("Top 10", "Top 10","Top 10","Top 10"))
        ({datatable(Summarydatatable,rownames = NULL, options = list(dom='t', ordering=F, scrollX = TRUE, pageLength = 25)) %>% 
            formatStyle(c(2,3,6,7),backgroundColor = 'lightblue') %>%
            formatStyle(0:ncol(Summarydatatable), valueColumns = 'Distance (m)',
                        `border-top` = styleEqual('Average', "solid 3px")) %>%
            formatStyle('Distance (m)', target = 'row', FontWeight = styleEqual('Average','bold'))
          
          
        })
      }
    }
    
    
  })
  
})

#### Output headers ####
output$Summaryhead <- renderText({
  if (input$goButton == 0)
    return()
  input$goButton
  isolate({
    Label_Race1 = paste(input$Name, input$Competition, input$Phase)
    if (input$Report_Type == "Single Race"){ 
      Summaryhead = Label_Race1
    }else if(input$Report_Type == "Two Races"){ 
      Label_Race2 = paste(input$Name2, input$Competition2, input$Phase2)
      Summaryhead = paste(Label_Race1, "vs", Label_Race2) 
      
    }else if (input$Report_Type == "vs Top 10"){ 
      Summaryhead = paste(Label_Race1, "vs Top 10 Average") 
      
    }
    
    {Summaryhead}
  })
})
output$Timehead <- renderText({
  if (input$goButton == 0)
    return()
  input$goButton
  isolate({
    Label_Race1 = paste(input$Name, input$Competition, input$Phase)
    if (input$Report_Type == "Single Race"){ 
      Timehead = paste("25m Splits -", Label_Race1)
    }else if (input$Report_Type == "Two Races"){ 
      Label_Race2 = paste(input$Name2, input$Competition2, input$Phase2)
      
      Timehead = paste("25m Splits -", Label_Race1, "vs", Label_Race2) 
      
    }else if (input$Report_Type == "vs Top 10"){ 
      Timehead = paste("25m Splits -", Label_Race1, "vs Top 10 Average") 
      
    }
    
    {Timehead}
  })
})

output$Summaryheadgap <- renderText({
  paste(" ")
})
output$Splithead <- renderText({paste0("Splits vs Top 10 average")
})

output$BoxTitleWBT <- renderText({
  paste(input$Event, "World Best")
})
output$BoxTitleProg <- renderText({
  paste(input$Event, "Prognostic")
})

output$Plothead1 <- renderText({
  if (input$Report_Type == "Single Race"){ 
    Plothead1 = paste("Velocity and Stroke Rate")
  }else if(input$Report_Type == "Two Races"){ 
    Plothead1 = paste("Velocity") 
    
  }else if (input$Report_Type == "vs Top 10"){ 
    Plothead1 = paste("Velocity") 
    
  }
  
  {Plothead1}
})

output$Plothead2 <- renderText({
  if(input$Report_Type == "Two Races"){ 
    Plothead2 = paste("Stroke Rate") 
    
  }else if (input$Report_Type == "vs Top 10"){ 
    Plothead2 = paste("Stroke Rate") 
    
  }
  
  {Plothead2}
})



output$RaceSummary <-  DT::renderDataTable({
  input$goButton
  isolate(
    if (input$Report_Type == "Single Race"){
      #Race 1
      FinishTime <-  tab_Time_R1() %>% select(value) %>% last() %>% unlist()
      
      
      if (input$distance == "Labelled_data_200"){
        distancea = 200
      }else if (input$distance == "Labelled_data_500"){
        distancea = 500
      }else if (input$distance == "Labelled_data_3x3x1km"){
        distancea = 1000
      }
      ClassProg <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
      
      #ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass=="MK1") %>% filter(Distance=="500")
      
      ClassProgSpeed <- ClassProg[1,'ProgSpeed100']
      PrognosticTime <- ClassProg[1,'ProgTimesec100']
      
      Difference <-  round((PrognosticTime/FinishTime)*100,2)
      AvgVel = round(distancea/FinishTime*3.6,2)
      FinishTime <- sec_to_ms(FinishTime)
      PrognosticTime <- sec_to_ms(PrognosticTime)
      
      ClassProgSpeed2 = round(ClassProgSpeed,2)
      
      
      Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2)
      colnames(Race_1) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
      
      
      ({datatable(Race_1, rownames = c("Race 1"), options = list(dom='t', ordering=F, scrollX = TRUE))})
      
    } else if (input$Report_Type == "Two Races"){
      #Race 1
      FinishTime <-  tab_Time_R1() %>% select(value) %>% last() %>% unlist()
      
      if (input$distance == "Labelled_data_200"){
        distancea = 200
      }else if (input$distance == "Labelled_data_500"){
        distancea = 500
      }else if (input$distance == "Labelled_data_3x3x1km"){
        distancea = 1000
      }
      
      ClassProg <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
      ClassProgSpeed <- ClassProg[1,'ProgSpeed100']
      PrognosticTime <- ClassProg[1,'ProgTimesec100']
      
      Difference <-  round((PrognosticTime/FinishTime)*100,2)
      AvgVel = round(distancea/FinishTime*3.6,2)
      FinishTime <- sec_to_ms(FinishTime)
      PrognosticTime <- sec_to_ms(PrognosticTime)
      ClassProgSpeed2 = round(ClassProgSpeed,2)
      
      #Race 2
      FinishTime_R2 <-  tab_Time_R2() %>% select(value) %>% last() %>% unlist()
      
      if (input$distance == "Labelled_data_200"){
        distancea = 200
      }else if (input$distance == "Labelled_data_500"){
        distancea = 500
      }else if (input$distance == "Labelled_data_3x3x1km"){
        distancea = 1000
      }
      
      ClassProgSpeed_R2 <- ClassProg[1,'ProgSpeed100']
      PrognosticTime_R2 <- ClassProg[1,'ProgTimesec100']
      
      Difference_R2 <-  round(PrognosticTime_R2/FinishTime_R2*100,2)
      AvgVel_R2 = round(distancea/FinishTime_R2*3.6,2)
      FinishTime_R2 <- sec_to_ms(FinishTime_R2)
      PrognosticTime_R2 <- sec_to_ms(PrognosticTime_R2)
      ClassProgSpeed2_R2 = round(ClassProgSpeed_R2,2)
      
      
      
      Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2)
      colnames(Race_1) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
      
      Race_2 = data.frame(FinishTime_R2, PrognosticTime_R2, Difference_R2, AvgVel_R2, ClassProgSpeed2_R2)
      colnames(Race_2) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
      
      RaceSummary <<-  rbind(Race_1, Race_2)
      ({datatable(RaceSummary, rownames = c("Race 1", "Race 2"), options = list(dom='t', ordering=F, scrollX = TRUE))})
      
    }  else if (input$Report_Type == "vs Top 10"){
      
      #Race 1
      FinishTime <-  tab_Time_R1() %>% select(value) %>% last() %>% unlist()
      
      if (input$distance == "Labelled_data_200"){
        distancea = 200
      }else if (input$distance == "Labelled_data_500"){
        distancea = 500
      }else if (input$distance == "Labelled_data_3x3x1km"){
        distancea = 1000
      }
      ClassProg <-  Prognostic.Velocities %>% filter(BoatClass==input$Class) %>% filter(Distance==distancea)
      
      #ClassProgSpeed <-  Prognostic.Velocities %>% filter(BoatClass=="MK1") %>% filter(Distance=="500")
      
      ClassProgSpeed <- ClassProg[1,'ProgSpeed100']
      PrognosticTime <- ClassProg[1,'ProgTimesec100']
      
      Difference <-  round((PrognosticTime/FinishTime)*100,2)
      AvgVel = round(distancea/FinishTime*3.6,2)
      FinishTime <- sec_to_ms(FinishTime)
      PrognosticTime <- sec_to_ms(PrognosticTime)
      
      ClassProgSpeed2 = round(ClassProgSpeed,2)
      
      #Race 2
      FinishTime_Top10 <-  tab2_Time() %>% select(Average) %>% last() %>% unlist()
      
      if (input$distance == "Labelled_data_200"){
        distancea = 200
      }else if (input$distance == "Labelled_data_500"){
        distancea = 500
      }else if (input$distance == "Labelled_data_3x3x1km"){
        distancea = 1000
      }
      ClassProgSpeed_Top10 <- ClassProg[1,'ProgSpeed100']
      PrognosticTime_Top10 <- ClassProg[1,'ProgTimesec100']
      
      Difference_Top10 <-  round(PrognosticTime_Top10/FinishTime_Top10*100,2)
      AvgVel_Top10 <- round(distancea/FinishTime_Top10*3.6,2)
      FinishTime_Top10 <- sec_to_ms(FinishTime_Top10)
      PrognosticTime_Top10 <- sec_to_ms(PrognosticTime_Top10)
      ClassProgSpeed2_Top10 = round(ClassProgSpeed_Top10,2)
      
      
      
      
      Race_1 = data.frame(FinishTime, PrognosticTime, Difference, AvgVel, ClassProgSpeed2)
      colnames(Race_1) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
      
      Race_2 = data.frame(FinishTime_Top10, PrognosticTime_Top10, Difference_Top10, AvgVel_Top10, ClassProgSpeed2_Top10)
      colnames(Race_2) = c("Finish Time", "Prognostic Time", "Prog %", "Average Velocity (km/h)", "Prognostic Velocity (km/h)")
      
      RaceSummary <-  rbind(Race_1, Race_2)
      ({datatable(RaceSummary, rownames = c("Race 1", "Top 10"), options = list(dom='t', ordering=F, scrollX = TRUE))})
      
      
    }
  )
})



}

