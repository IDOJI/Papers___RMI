##############################################################################################
# 0. Loading functions
##############################################################################################
# rm(list=ls())
#=============================================================================================
# Mac
#=============================================================================================
path_OS = "/Users/Ido/"
# path_External.Drive = "/Volumes/Seagate/"
#============================================================================================
# Windows
#============================================================================================
# path_OS = "C:/Users/lleii/"
#============================================================================================
require(tidyverse)
require(dplyr)
require(clipr)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
#=============================================================================================




#===============================================================================
# WD
#===============================================================================
# path_clip()
# setwd("/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data")
setwd(paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data"))
getwd()







#===============================================================================
# Loading Raw Data
#===============================================================================
# Demographic 
# BDI_COls = 280 # Beck Depression Inventory
Demo_Cols = c(4, 6, 7, 11, 12, 24, 250)



# Physiological
Phys_cols = c(158, # "Aver HR_sleep" 
              159, # "High HR_sleep"
              160 # "Low HR_sleep" 
)




# AHI Parameters
AHI_Cols = c(56,65,67,72,74, #AI
             110, 111, 113, 118, 120, #HI,
             126, 127, 129, 134, 136 #AHI
)



# Respiratory parameters
Respiratory_Cols = c(57, 
                     140, #Low o2
                     153 #Desat. T_ODI
)

# RMI Parameters
RMI_Cols = c(359, # Total %RMI of TST
             368, # "RMI Index N1"
             369, # "RMI Index N2"
             370, # "RMI Index N3"
             371, # "RMI Index R...371"
             372, # "RMI Index Sleep"
             375, # "Duration[min] N1"
             376, # "Duration[min] N2"
             377, # "Duration[min] N3"
             378, # "Duration[min] R...378",
             379, # "Duration[min] Sleep"
             396, # "RMI Index S" 
             # 398, # "RMI Index R...398",
             400, # "RMI Index N-S",
             # 401, # "RMI Index A-p",
             402, # "Duration[min] S",
             # 404, # "Duration[min] L",
             407, # "Duration[min] N-S",
             # 408 # "Duration[min] A-p"
)


# Final cols
# Finally_Selected_Cols = c(Demo_Cols, Phys_cols, AHI_Cols, Respiratory_Cols, RMI_Cols)

# The former cols
Finally_Selected_Cols =  c(4,6,7,11,12,24,56,57,64,65,67,72,74,110,111,113,118,120,126,127,129,134,136,140,153,250,359,368,369,370,371,372,375,376,377,378,379,395,400,402,407)

# which_names.list = list()
# which_names.list[[1]] = which_names_2017 = Finally_Selected_Cols
# which_names.list[[2]] = which_names_2018 = Finally_Selected_Cols
# which_names.list[[3]] = which_names_2019 = Finally_Selected_Cols
# which_names.list[[4]] = which_names_2020 = Finally_Selected_Cols
# which_names.list[[5]] = which_names_2021 = Finally_Selected_Cols
# which_names.list[[6]] = which_names_2022 = Finally_Selected_Cols


# loading data
data.list = list()
for(i in 1:6){
  data.list[[i]] = readxl::read_excel(path="CNUH sleep center PSG data_1029.xlsx", sheet=as.character(2016+i))[,Finally_Selected_Cols]
}






# #===============================================================================
# # subset by columns
# #===============================================================================
# data_Sub.list = list()
# names.df = matrix(NA, length(data.list), length(selected_names)) %>% as.data.frame
# for(i in 1:length(data.list)){
#   data_Sub.list[[i]] = data.list[[i]][,selected_names]
#   names.df[i,] = names(data_Sub.list[[i]])
# }









#===============================================================================
# writing selected names to confirm its duplicacy
#===============================================================================
# not_unique_names.df = matrix(NA, 1, nrow(names.df)) %>% as.data.frame
# not_unique_names_ind = c()
# for(k in 1:ncol(names.df)){
#   # k=1
#   kth = names.df[,k] %>% unlist %>% unique
#   if(length(kth)!=1){
#     not_unique_names.df = rbind(not_unique_names.df, names.df[,k])
#     not_unique_names_ind = c(not_unique_names_ind, k)
#   }
# }
# not_unique_names.df = not_unique_names.df[-1,]
# rownames(not_unique_names.df) = NULL
# colnames(not_unique_names.df) = 2017:2022 %>% as.character
# write.csv(x = not_unique_names.df, file = "not_unique_names.csv",row.names=F,fileEncoding = "cp949")








#===============================================================================
# Combining Data
#===============================================================================
col_names = data.list[[6]] %>% names
for(i in 1:length(data.list)){
  names(data.list[[i]]) = col_names
}
data_Combined = do.call(rbind, data.list)[1:1232,]











#===============================================================================
# Change names
#===============================================================================
colnames(data_Combined)[1:3] = c("RID", "Age", "Sex")
which_dot_names = grep("\\...", names(data_Combined))
dot_names = names(data_Combined)[which_dot_names]
dot_names_split = strsplit(dot_names, "\\...")
for(i in 1:length(which_dot_names)){
  names(data_Combined)[which_dot_names[i]] = dot_names_split[[i]][1]
}



#===================
# Changing col names
#===================
# 1) Respiratory variable
data_Change.Names_1 = data_Combined %>% dplyr::rename("RERA"="RERA_I")
data_Change.Names_2 = data_Change.Names_1 %>% dplyr::rename("RAI"="Apnea/Hypopnea Ar_I");names(data_Change.Names_2)
data_Change.Names_3 = data_Change.Names_2 %>% dplyr::rename("RMI Index percentage"="Total %RMI of TST");names(data_Change.Names_3)
data_Change.Names_4 = data_Change.Names_3 %>% dplyr::rename("ODI"="Desat");names(data_Change.Names_4)


# 2) AHI 
# AHI
data_Change.Names_AHI = data_Change.Names_4 %>% 
  dplyr::rename("AHI R":="AHI in REM") %>% 
  dplyr::rename("AHI NR":="AHI in NREM") %>% 
  dplyr::rename("AHI S":="AHI in supine") %>% 
  dplyr::rename("AHI NS":="AHI in non-supine")
# HI
data_Change.Names_HI = data_Change.Names_AHI %>% 
  dplyr::rename("HI" := "Hypopnea index") %>% 
  dplyr::rename("HI R" := "HI in REM") %>% 
  dplyr::rename("HI NR" := "HI in NREM") %>% 
  dplyr::rename("HI S" := "HI in Supine (index)") %>% 
  dplyr::rename("HI NS" := "HI in non-supine (index)")
# AI
data_Change.Names_AI = data_Change.Names_HI %>% 
  dplyr::rename("AI" := "Apnea Index") %>% 
  dplyr::rename("AI R" := "AI in REM (index)") %>% 
  dplyr::rename("AI NR" := "AI in NREM") %>% 
  dplyr::rename("AI S" :="Apnea in supine (index)") %>% 
  dplyr::rename("AI NS" := "Apnea in non-supine (index)")





# 3) RMI
data_Change.Names_RMI = data_Change.Names_AI
ind_RMI_Duration = filter_by(names(data_Change.Names_RMI), include = "Duration", as.ind = T)
names(data_Change.Names_RMI)[ind_RMI_Duration] = paste0("RMI ", names(data_Change.Names_RMI)[ind_RMI_Duration])
names(data_Change.Names_RMI)[ind_RMI_Duration] = gsub(pattern = "\\[min\\]", replacement = "", x = names(data_Change.Names_RMI)[ind_RMI_Duration])
names(data_Change.Names_RMI)
data_Change.Names_RMI = data_Change.Names_RMI %>% 
  rename("RMI Index NS" := "RMI Index N-S") %>% 
  rename("RMI Duration NS" := "RMI Duration N-S")







#===============================================================================
# rearrange cols
#===============================================================================
data_rearranged_1 = data_Change.Names_RMI
data_rearranged_2 = data_rearranged_1 %>% dplyr::relocate("ESS_total", .after="BMI");names(data_rearranged_2)
data_rearranged_3 = data_rearranged_2 %>% dplyr::relocate(starts_with("AHI"), .after=last_col());names(data_rearranged_3)
data_rearranged_4 = data_rearranged_3 %>% dplyr::relocate(starts_with("AI"), .after=last_col());names(data_rearranged_4)
data_rearranged_5 = data_rearranged_4 %>% dplyr::relocate(starts_with("HI"), .after=last_col());names(data_rearranged_5)
data_rearranged_6 = data_rearranged_5 %>% dplyr::relocate(starts_with("RMI"), .after=last_col());names(data_rearranged_6)
data_rearranged_7 = data_rearranged_6 %>% dplyr::relocate(contains("Duration"), .after=last_col());names(data_rearranged_6)
data_rearranged_8 = data_rearranged_7 %>% dplyr::relocate(ends_with("Index Sleep"), .after = `RMI Index percentage`)
data_rearranged_9 = data_rearranged_8 %>% dplyr::relocate(ends_with("Duration Sleep"), .before = `RMI Duration N1`)







#===============================================================================
# Converting Sex as indicator variables
#===============================================================================
Data_Sex = data_rearranged_9
Data_Sex$Sex = ifelse(Data_Sex$Sex == "Male", 1, 0)
# Data_Sex$Sex_Female = ifelse(Data_Sex$Sex == "Female", 1, 0)
# Data_Sex = Data_Sex %>% relocate(starts_with("Sex_"), .after=Sex)










# #===============================================================================
# # Selecting cols
# #===============================================================================
# Demo_num = c("Age", "BMI", "Neck", "Abdomen", "ESS_total")
# Demo_cha = c("Sex")
# 
# RMI_index    = filter_by(names(data_rearranged_6), include = "RMI",      as.ind = F) # percentage 포함
# RMI_duration = filter_by(names(data_rearranged_6), include = "duration", as.ind = F)
# 
# AHI = filter_by(names(data_rearranged_6), include = "AHI", as.ind = F)
# AI  = filter_by(names(data_rearranged_6), include = "AI", exclude = "RAI", as.ind = F) #무호흡
# HI  = filter_by(names(data_rearranged_6), include = "HI", exclude = "AHI", as.ind = F)
# 
# RERA   = filter_by(names(data_rearranged_6), include = "RERA", as.ind = F)
# minSO2 = filter_by(names(data_rearranged_6), include = "Low o2", as.ind = F)
# ODI    = filter_by(names(data_rearranged_6), include = "ODI", as.ind = F)
# RAI    = filter_by(names(data_rearranged_6), include = "RAI", as.ind = F)






#===============================================================================
# change cols' class
#===============================================================================
data_ChangeClass_1 = change_class(Data_Sex, which.col = c(2:ncol(Data_Sex)), what.class = "numeric")







#===============================================================================
# Rem variable
#===============================================================================
# Create Rem variable
names(data_ChangeClass_1)
denominator = data_ChangeClass_1$`RMI Duration N1` + data_ChangeClass_1$`RMI Duration N2` + data_ChangeClass_1$`RMI Duration N3`
numerator = (data_ChangeClass_1$`RMI Index N1` * data_ChangeClass_1$`RMI Duration N1`) + (data_ChangeClass_1$`RMI Index N2` * data_ChangeClass_1$`RMI Duration N1`) + (data_ChangeClass_1$`RMI Index N3` * data_ChangeClass_1$`RMI Duration N3`)
data_NonRem = data_ChangeClass_1
data_NonRem$"RMI Index NR" = numerator/denominator
data_NonRem = data_NonRem %>% relocate(`RMI Index NR`, .after=`RMI Index R`)




# Remove NA
Non_Rem = "RMI Index NR"
# rm_which_col = c(Demo_num, Demo_cha,
#                  RMI_index, RMI_duration,
#                  AHI, AI, HI,
#                  RERA, minSO2, ODI, RAI, Non_Rem)
data_RmNA_1 = exclude_na(data_NonRem, 1:ncol(data_NonRem))
data_RmNA_2 = data_RmNA_1[[1]]
data_RmNA_3 = data_RmNA_1[[2]]




#===============================================================================
# Create AHI groups
#===============================================================================
data_OSAgroups = Split.Data___Grouping.By___Continuous(Data = data_RmNA_2, 
                                                       Var_Name = "AHI", 
                                                       Cut_Points = c(5,15,30), 
                                                       Eq = "<", 
                                                       New_Var_Name = "OSA",
                                                       groups = c("Normal", "Mild OSA", "Moderate OSA", "Severe OSA"))
data_OSAgroups = data_OSAgroups %>% relocate(OSA)





#===============================================================================
# 2.Splitting by Age group
#===============================================================================
Cut_Points = c(18, 25, 35, 45, 50, 55, 60, 65, 70)
data_Age.groups = Split.Data___Grouping.By___Continuous(Data = data_OSAgroups, Var_Name = "Age", Cut_Points, Eq = "<=", New_Var_Name = "Age_Group")
data_Age.groups = data_Age.groups %>% relocate(Age_Group, .after=Age)













#===============================================================================
# 임의 : Duration N-R 계산
#===============================================================================
# 추측 -> 실제 데이터에서의 계산 상 맞아 보임
# Duration Sleep == Duration_R + Duration_N1 + Duration_N2 + Duration_3
# => Duration N-R = Duration_Sleep  - Duration_R
data_Duration  = data_Age.groups 
data_Duration$`RMI Duration NR` = data_Duration$`RMI Duration Sleep`  - data_Duration$`RMI Duration R`
data_Duration = data_Duration %>% relocate("RMI Duration NR", .after="RMI Duration R")










#===============================================================================
# Data saving
#===============================================================================
# final data
data_final_included = data_Duration
data_final_excluded = data_RmNA_3


# saving
CNU_Sleep_Center = data_final_included %>% as_tibble()
CNU_Sleep_Center_Excluded = data_final_excluded %>% as_tibble()
getwd()
saveRDS(CNU_Sleep_Center, file = "CNU_Sleep_Center.rds")
saveRDS(CNU_Sleep_Center_Excluded, file = "CNU_Sleep_Center_Excluded.rds")


