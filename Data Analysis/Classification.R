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
require(pROC)
require(ggplot2)
require(clipr)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/ADNIprep/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)
list.files(paste0(path_OS, "Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
#=============================================================================================










#===============================================================================
# Splitting by "AHI_Group" & "Age_Group maintaining proportion
#===============================================================================
path = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data/CNU_Sleep_Center.rds")
# path = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data/CNU_Sleep_Center.rds"
Data = readRDS(path)
Splitted_Data = Split.Data___CV.Fold___Stratified(Data = Data, Var_1="Age_Group", Var_2="OSA", n_fold = 10)









#===============================================================================
# 3.Extract test set  & train index
#===============================================================================
Train = Splitted_Data$Train
Test = Splitted_Data$Test
# Excluding Variables
Exclude_Common = c("AHI", "OSA", "RID", "Strata", "Age_Group")
Exclude_AHI_related_vars = c("AHI S","AHI NS", "AHI R", "AHI NR", 
                             "AI", "AI S", "AI NS", "AI R", "AI NR", 
                             "HI", "HI S", "HI NS", "HI R", "HI NR")

Exclude_Train = c(Exclude_Common, "Which_Fold")
Exclude_Test = Exclude_Common


# Fold index
Train_Folds = Train$Which_Fold
Train_Folds_Index = Splitted_Data$Train_Fold_Index


# Train
X_Train = Train %>% dplyr::select(-!!Exclude_Train)
y_Train = Train %>% dplyr::select(OSA) %>% unlist



# Test
X_Test = Test %>% dplyr::select(-!!Exclude_Test)
y_Test = Test %>% dplyr::select(OSA) %>% unlist









#===============================================================================
# Setting Dataset
#===============================================================================
X_Train.list = list(RAI = X_Train %>% dplyr::select(RAI),
                    ODI = X_Train %>% dplyr::select(ODI),
                    with_AHI = X_Train,
                    without_AHI = X_Train %>% dplyr::select(-all_of(Exclude_AHI_related_vars)),
                    RAI_ODI = X_Train %>% dplyr::select(RAI, ODI),
                    RAI = X_Train %>% dplyr::select(RAI),
                    ODI = X_Train %>% dplyr::select(ODI))


X_Test.list = list(RAI = X_Test %>% dplyr::select(RAI),
                  ODI = X_Test %>% dplyr::select(ODI),
                  with_AHI = X_Test,
                  without_AHI = X_Test %>% dplyr::select(-all_of(Exclude_AHI_related_vars)),
                  RAI_ODI = X_Test %>% dplyr::select(RAI, ODI),
                  RAI = X_Test %>% dplyr::select(RAI),
                  ODI = X_Test %>% dplyr::select(ODI))





#===============================================================================
# Setting save path & arguments
#===============================================================================
path_Export_Common = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/Classification_2/")
# folder by variables
path_Export_Folders = c("Fitting with RAI___MLE",
                        "Fitting with ODI___MLE",
                        "Fitting with AHI-related vars",
                        "Fitting without AHI-related vars",
                        "Fitting with RAI, ODI",
                        "Fitting with RAI",
                        "Fitting with ODI")
path_Export_List = paste0(path_Export_Common, path_Export_Folders)



fitting_method.list = c(rep("mle", 2), rep("elastic", 5)) %>% as.list
x_varname = c(list("RAI", "ODI"), rep(list(NULL), 5))
y_varname = c(rep(list("OSA"), 2), rep(list(NULL), 5))
link.list = c(rep("logistic", 2), rep("logit", 5)) %>% as.list












#===============================================================================
# Model fitting
#===============================================================================
Fitting.list = lapply(seq_along(path_Export_Folders), function(k){
  k=1
  kth_Result = Classification___Logistic(X_Train = X_Train.list[[k]],
                               y_Train = y_Train,
                               X_Test = X_Test.list[[k]],
                               y_Test = y_Test,
                               y_varname = y_varname[[k]],
                               x_varname = x_varname[[k]],
                               standardize = T,
                               fitting.method = fitting_method.list[[k]],
                               penalty_alpha = seq(0, 1, 0.1),
                               penalty_lambda = NULL,
                               response_type = "Ordinal",
                               family = "cumulative",
                               link = link.list[[k]],
                               tuneMethod = "cvMisclass",
                               best.model.criterion = "misclass",
                               Train_Folds_Index = Train_Folds_Index,
                               AUC_in_Legend = F,
                               path_Export = path_Export_List[[k]])
  
  # misclassified
  mis_index = paste0(path_Export_List[[k]], "/2.Misclassified_Subjects.csv") %>% read.csv %>% dplyr::select(Index) %>% unlist()
  Combined = cbind(Predicted = kth_Result$Misclassified_Subjects$Predicted_Class, Test[mis_index,]) %>% 
    relocate(Predicted, .after = "OSA") %>% 
    as_tibble()
  write.csv(Combined, paste0(path_Export_List[[k]], "/2.Misclassified_Subjects_Selected.csv"))
  
  return(kth_Result)
})











#===============================================================================
# Visualize misclassification
#===============================================================================

Combined %>% dplyr::select(all_of(c("OSA", "Predicted", "AHI"))) %>% as.data.frame
Combined$Predicted
# Define a function to categorize AHI values
get_OSA_group <- function(AHI) {
  if (AHI < 5) {
    return("Normal")
  } else if (AHI >= 5 & AHI < 15) {
    return("Mild OSA")
  } else if (AHI >= 15 & AHI < 30) {
    return("Moderate OSA")
  } else {
    return("Severe OSA")
  }
}

# Apply the function to the AHI column
Combined$True_Predicted <- sapply(Combined$AHI, get_OSA_group)

# Identify borderline cases
Combined$Borderline <- ifelse(abs(Combined$AHI - 5) <= 1 | 
                               abs(Combined$AHI - 15) <= 1 | 
                               abs(Combined$AHI - 30) <= 1, "Borderline", "Not Borderline")


library(ggplot2)


library(ggplot2)
# "Misclassification of OSA based on AHI values"
# Scatter plot of AHI values colored by True and Predicted classes
Combined$Predicted = factor(Combined$Predicted, levels = levels(Combined$OSA))
p = ggplot(Combined, aes(x = 1:nrow(Combined), y = AHI, color = Predicted)) +
  geom_point(aes(shape = OSA), size = 3) +
  geom_hline(yintercept = c(5, 15, 30), linetype = "dashed", color = "red") +
  labs(title = "",
       x = "Observation Index",
       y = "AHI Value",
       shape = "True OSA Group",
       color = "Predicted OSA Group") +
  theme_minimal() + 
  theme(
  plot.background = element_rect(fill = "white", color = "white"),
  legend.title = element_text(face="bold", size = 15),
  legend.text = element_text(size = 12),                  # Adjust size of legend labels
  legend.key.size = unit(1, "cm"),     # Adjust size of legend keys
  # panel.background = element_rect(fill = "white"),
  axis.title.x = element_text(size = 14, face="bold"),  # Adjust font size of x-axis label
  axis.title.y = element_text(size = 14, face="bold")   # Adjust font size of y-axis label
  )
p
ggsave(filename = paste0(path_Export_List[[k]], "/2.Misclassified_Subjects_Selected_Scatterplot.png"), dpi = 150, bg="white")


#===============================================================================
# Show results
#===============================================================================
Fitting_1$Confusion_Matrix
Fitting_2$Confusion_Matrix


Fitting_1$Misclassification_Rate
Fitting_2$Misclassification_Rate
Fitting_3$Misclassification_Rate

Fitting_1$Fit_Coef
Fitting_2$Fit_Coef



#====================================
# RAI, ODI
#====================================
# Confusion matrices
Fitting_3$Confusion_Matrix # ODI & RAI
Fitting_4$Confusion_Matrix # ODI
Fitting_5$Confusion_Matrix # RAI


# Coefficients
Fitting_3$Fit_Coef # ODI & RAI
Fitting_4$Fit_Coef # ODI
Fitting_5$Fit_Coef # RAI


# missclassification rate
Fitting_3$Misclassification_Rate # ODI & RAI
Fitting_4$Misclassification_Rate # ODI
Fitting_5$Misclassification_Rate # RAI




#===============================================================================
# Export results
#===============================================================================
path_Export = path_Export = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___2/Classification")
write.csv(x = round(Fitting_1$Fit_Coef, 4), file = paste0(path_Export, "/Coef_1.csv"))
write.csv(x = round(Fitting_2$Fit_Coef, 4), file = paste0(path_Export, "/Coef_2.csv"))


Fitting_2








#===============================================================================

path_Export = paste0(path_OS, "Dropbox/Github/Papers/Papers___Wrting/___Papers___진행중/Data.Analysis___3/Classification_2/Fitting with AHI-related vars")
Fitting_1 = readRDS(list.files(path_Export, full.names=T, pattern = "Fitting"))






















#===============================================================================
# Correlation of ODI and RAI
#===============================================================================
Plot___Scatter(x="ODI", y="RAI", Data, regression.line = T)










