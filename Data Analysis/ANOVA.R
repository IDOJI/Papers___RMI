##############################################################################################
# 0. Loading functions
##############################################################################################
# rm(list=ls())
#=============================================================================================
# Mac
#=============================================================================================
# path_OS = "/Users/Ido/"
# path_External.Drive = "/Volumes/Seagate/"
#============================================================================================
# Windows
#============================================================================================
path_OS = "C:/Users/lleii/"
#============================================================================================
require(tidyverse)
require(dplyr)
require(pROC)
require(ggplot2)
require(clipr)
require(ggstatsplot)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
#=============================================================================================





#===============================================================================
# Load Data
#===============================================================================
path_Data = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data/CNU_Sleep_Center.rds")
# path = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data/CNU_Sleep_Center.rds"
Raw_Data = readRDS(path_Data)
Data = Raw_Data %>% select(-Sex_Male, -Sex_Female)













#===============================================================================
# Define Variables
#===============================================================================
Demographics = c("Age", "Neck", "Abdomen", "BMI", "ESS_total")
Respiratory = c("RAI", "RERA", "Low O2", "ODI")
AHI = names(Data) %>% filter_by(include = c("AHI", "HI", "AI"), any_include = T, exclude = "RAI")
RMI = names(Data) %>% filter_by(include = c("RMI"), any_include = T)
################################################################################
Responses = list(Demographics, Respiratory, AHI, RMI)
names(Responses) = c("Demographics", "Respiratory", "AHI", "RMI")
Group_Vars = c("OSA", "Sex", "Age_Group")

  









#===============================================================================
# ANOVA : OSA
#===============================================================================
# AHI 변수 제거
Responses_OSA = Responses
Responses_OSA[[3]] = Responses_OSA[[3]][Responses_OSA[[3]] != "AHI"]
Group_Var = "OSA"
Group_Var_Type = "Ordinal"
ANOVA___OSA = lapply(seq_along(Responses_OSA), function(i){
  Test___MeanDiff(Data,
                  Response_Vars = Responses_OSA[[i]],
                  Group_Var = Group_Var,
                  Group_Var_Type = Group_Var_Type,
                  p.adjust.method = "TSBH",
                  group.comparison = FALSE,
                  lines.connecting.medians=TRUE,
                  save.path = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/ANOVA___", Group_Var, "___", Group_Var_Type),
                  file.name = paste0("ANOVA___",Group_Var, "___", names(Responses)[i])
                  ) 
})
names(ANOVA___OSA) = names(Responses)
saveRDS(ANOVA___OSA, file = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/ANOVA___", Group_Var, "___", Group_Var_Type, "/ANOVA_", Group_Var,"_Results.rds"))








#===============================================================================
# ANOVA : Age Group
#===============================================================================
# Age 변수 제거
Responses_Age = Responses
Responses_Age[[1]] = Responses_Age[[1]][Responses_Age[[1]] != "Age"]
Group_Var = "Age_Group"
Group_Var_Type = "Nominal"
ANOVA___Age = lapply(seq_along(Responses_Age), function(i){
  Test___MeanDiff(Data,
                  Response_Vars = Responses_Age[[i]],
                  Group_Var = Group_Var,
                  Group_Var_Type = Group_Var_Type,
                  p.adjust.method = "TSBH",
                  group.comparison = FALSE,
                  lines.connecting.medians=TRUE,
                  save.path = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/ANOVA___", Group_Var, "___", Group_Var_Type, "_2"),
                  file.name = paste0("ANOVA___",Group_Var, "___", names(Responses)[i])
                  )  
})
names(ANOVA___Age) = names(Responses)
saveRDS(ANOVA___Age, file = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/ANOVA___", Group_Var, "___", Group_Var_Type, "/ANOVA_", Group_Var,"_Results.rds"))














#===============================================================================
# ANOVA : Sex
#===============================================================================
Group_Var = "Sex"
Group_Var_Type = "Nominal"
ANOVA___Sex = lapply(seq_along(Responses), function(i){
  Test___MeanDiff(Data,
                  Response_Vars = Responses[[i]],
                  Group_Var = Group_Var,
                  Group_Var_Type = Group_Var_Type,
                  p.adjust.method = "TSBH",
                  group.comparison = FALSE,
                  lines.connecting.medians=TRUE,
                  save.path = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/ANOVA___", Group_Var),
                  file.name = paste0("ANOVA___",Group_Var, "___", names(Responses)[i]))  
})
names(ANOVA___Age) = names(Responses)
saveRDS(ANOVA___Age, file = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/ANOVA___", Group_Var, "/ANOVA_", Group_Var,"_Results.rds"))
















