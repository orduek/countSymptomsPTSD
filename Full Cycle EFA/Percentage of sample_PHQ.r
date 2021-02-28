##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 1.0 (15.02.2021)                        #
#                                                                            #
#----------------------------------------------------------------------------#
#                                                                            #
#                             Full Cycle EFA / CFA                           #
#									                                                           #
##############################################################################


###### Table of Contents #####################################################
#----- 1. Load libraries ----------------------------------------------------#
#----- 2. Import and prepare data -------------------------------------------#
#----- 3. Descriptive -------------------------------------------------------#
#----- 4. EFA  --------------------------------------------------------------#
#----- 5. CFA  --------------------------------------------------------------#
#-----  5.1 Define factor structure -----------------------------------------#
#-----  4.2 Function (data split + factor estimation) -----------------------#


# # Data handling + basic calculations
# if(!require("tidyverse")) install.packages("tidyverse")
# if(!require("rsample")) install.packages("rsample")
# if(!require("readxl")) install.packages("readxl")
# 
# #Factor Analysis
# if(!require("lavaan")) install.packages("lavaan")
# if(!require("psych")) install.packages("psych")
# if(!require("GPArotation")) install.packages("GPArotation")
source('/home/or/countSymptomsPTSD/utils.r') # import local functions
if(!require("gt")) install.packages("gt")

###### 2. Import and prepare data ############################################

###### 2.1 Import
# Create new data frame
datax <- read_delim("/home/or/countSymptomsPTSD/Generated_Data/Matched_freq_countPHQ.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

###### 2.2 Training and testing data

allDatas <- splitDat(datax = datax, begin = "PHQN01", end = "PHQN09")
###### 3. Descriptive ########################################################

summary(datax)
summary(allDatas[["data_train"]])
summary(allDatas[["data_test"]])

###### 4. EFA ################################################################
###### 4.2 EFA  ####################################################################
###### 4.2.1 20/80 Split  ##########################################################
### Not relevant in PTSD sample, as 20/80 split yield with empty group (top)
# Define used sample
#cut_1 <- efa_run(cutPoint = allDatas[["Splits_percentage"]][1], datay_train = allDatas[["datay_train"]])

###### 4.2.2 Median Split  ##########################################################
print("RUNNING")
# Define used sample
cut_2 <- efa_run(cutPoint = allDatas[["Splits_percentage"]][2], datay_train = allDatas[["datay_train"]])
# Safe of EFA 
CUT2_t <- cut_2$cut_top
CUT2_l <- cut_2$cut_low

###### 4.2.3 80/20 Split  ##########################################################
# Define used sample
cut_3 <- efa_run(cutPoint = allDatas[["Splits_percentage"]][3], datay_train = allDatas[["datay_train"]])
# Save of EFA 
CUT3_t <- cut_3$cut_top
CUT3_l <- cut_3$cut_low



###### 4.2.4 Full Data  ##########################################################

# Prepare
data_full <- allDatas$datay_train %>% 
  select(-freq)

data_full_cor <- cor(data_full)

# Check assumptions
bar_top <- cortest.bartlett(data_full_cor , n=nrow(data_full))
bar_top

KMO_RES_top <- KMO(data_full_cor )
KMO_RES_top 


# Estimate number of factors
EFA_top <- fa.parallel(data_full_cor , fa = "fa", n.obs = nrow(data_full))

# Estimate factor models
EFA_top_specific <- fa(data_full_cor , 
                       nfactors=EFA_top$nfact,
                       n.obs = nrow(data_full),
                       SMC=TRUE,
                       fm = "ml",
                       rotate="promax",
                       max.iter=1000)

# Results of EFA 
print(EFA_top_specific$loadings, cutoff = 0.3)

# Safe of EFA 
FULL <- EFA_top_specific$loadings


###### 4.2.5 All models  ##########################################################

# print(CUT1_t, cutoff = 0.5) #None
# print(CUT1_l, cutoff = 0.5) #None

print(CUT2_t, cutoff = 0.5) #4 factors
print(CUT2_l, cutoff = 0.5) #4 factors

print(CUT3_t, cutoff = 0.5) #4 factors
print(CUT3_l, cutoff = 0.5) #4

print(FULL, cutoff = 0.2)   #4 factor model 

###### 4.3 Extracted factor models  #################################################

model_CUT2_t<- ' A=~ PHQN03 + PHQN04 + PHQN05
                 B=~ PHQN01 + PHQN02 
'
# PHQ 6, 7, 8, 9 omitted

model_FULL <- '
    A =~ PHQN03 + PHQN04 + PHQN05 
    B =~ PHQN02 + PHQN06 + PHQN09
    C =~ PHQN07 + PHQN08
    D =~ PHQN01
'
# PHQ 1
###### 5. CFA  #####################################################################

# Simulation conditions

###### 5.1 Extracted Model Top 20%  ################################################
## NOT RELEVANT FOR PTSD

###### 5.1.1 20/80 ################################################

###### 5.1.2 Median ################################################

Res_1CUT_Median <- simConditions(nIter = 5, freqCut = allDatas$Splits_percentage[2], 
                                 data_test = allDatas$data_test, model = model_CUT2_t)

###### 5.1.3 80/20 ################################################

Res_1CUT_8020 <- simConditions(nIter = 5, freqCut = allDatas$Splits_percentage[3], 
                               data_test = allDatas$data_test, model = model_CUT2_t) 


###### 5.2 DSM-5 factor Model ################################################

###### 5.2.1 20/80 ################################################
# not relevant for PTSD
###### 5.2.2 Median ################################################
Res_FULL_Median <- simConditions(nIter = 5, freqCut = allDatas$Splits_percentage[2], 
                                   data_test = allDatas$data_test, model = model_FULL)

###### 5.2.3 80/20 ################################################
Res_FULL_8020 <- simConditions(nIter = 5, freqCut = allDatas$Splits_percentage[3], 
                                 data_test = allDatas$data_test, model = model_FULL)


###### 5.4 Factor Model Results ################################################
#Res_1CUT_2080


knitr::kable(Res_1CUT_Median, digits = 3)#, labels = labels)
knitr::kable(Res_1CUT_8020, digits = 3)

#Res_FULL
flextable(Res_Theory_Median)
flextable(Res_Theory_8020)

###### 5.4.1 Q1 ################################################
# Does the factor model derived in the top 20% fit in the remaining population? -> "ok"
#Res_1CUT_2080[8,] #CFI 0.94, #RMSEA 0.08

###### 5.4.2 Q2 ################################################
# Does the factor model derived in the top 20% fit in the lowest 20% -> "marginal"
Res_1CUT_8020[8,] #CFI 0.99 #RMSEA 0.02 #SRMR 0.03

###### 5.4.3 Q3 ################################################
# Does the theory derived factor model fit in the top and low 20% -> yes
#Res_Theory_2080[4,] #CFI 0.99 #RMSEA 0.02 #SRMR 0.03
Res_Theory_8020[8,] #CFI 0.92 #RMSEA 0.055 #SRMR 0.06
