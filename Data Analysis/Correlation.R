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
y_Vars.list = list(Demographics=Demographics, Respiratory=Respiratory, AHI=AHI)
x_Vars = RMI  






#===============================================================================
# Pearson Corr : RMI vs Demographics, AHI, Respiratory
#===============================================================================
p.adjust.method = "Bonferroni"
# p.adjust.method = "TSHB"
type = "parametric"
Corr.list = lapply(seq_along(y_Vars.list), function(i){
  Test___Corr(Data, 
              x_Vars, 
              y_Vars = y_Vars.list[[i]], 
              alpha = 0.05, 
              x_lab = "RMI", 
              y_lab = names(y_Vars.list)[i], 
              type = type, 
              p.adjust.method = p.adjust.method, 
              colors = c("blue", "white", "red"), 
              save.path = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/Correlation_", type, "___", p.adjust.method))  
})






#===============================================================================
# Spearman Corr : RMI vs Demographics, AHI, Respiratory
#===============================================================================
p.adjust.method = "Bonferroni"
# p.adjust.method = "TSHB"
type = "nonparametric"
Corr.list = lapply(seq_along(y_Vars.list), function(i){
  Test___Corr(Data, 
              x_Vars, 
              y_Vars = y_Vars.list[[i]], 
              alpha = 0.05, 
              x_lab = "RMI", 
              y_lab = names(y_Vars.list)[i], 
              type = type, 
              p.adjust.method = p.adjust.method, 
              colors = c("blue", "white", "red"), 
              save.path = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/Correlation_", type, "___", p.adjust.method))  
})












#===============================================================================
# Winsorized Pearson Corr : RMI vs Demographics, AHI, Respiratory
#===============================================================================
Corr.list = lapply(seq_along(y_Vars.list), function(i){
  Test___Corr(Data, 
              x_Vars, 
              y_Vars = y_Vars.list[[i]], 
              alpha = 0.05, 
              x_lab = "RMI", 
              y_lab = names(y_Vars.list)[i], 
              type = "parametric", 
              p.adjust.method = "TSHB", 
              colors = c("blue", "white", "red"), 
              save.path = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/Correlation"))  
})






#===============================================================================
# Winsorized Spearman Corr : RMI vs Demographics, AHI, Respiratory
#===============================================================================
Corr.list = lapply(seq_along(y_Vars.list), function(i){
  Test___Corr(Data, 
              x_Vars, 
              y_Vars = y_Vars.list[[i]], 
              alpha = 0.05, 
              x_lab = "RMI", 
              y_lab = names(y_Vars.list)[i], 
              type = "nonparametric", 
              p.adjust.method = "TSHB", 
              colors = c("blue", "white", "red"), 
              save.path = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/Correlation"))  
})










