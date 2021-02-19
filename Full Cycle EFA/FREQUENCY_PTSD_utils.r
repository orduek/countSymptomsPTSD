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
#-----  5.2 Function (data split + factor estimation) -----------------------#

## General Notes ##
# datax = dataframe script one with scores, binarized items and frequency profiles
# data_train = 75% of datax
# data_test = 25% of datax
# datay_train = dataframe of items with ordinal ratings AND frequency of symptom profiles (redcued data_train)
# dataz_train = dataframe of items with ordinal ratings WITHOUT frequency of symptom profiles (redcued data_train)

###### 1. Load Libraries #####################################################
# Data handling + basic calculations
source('utils.r') # import local functions
###### 2. Import and prepare data ############################################

###### 2.1 Import
# Create new data frame
datax <- read_delim("Generated_Data/Matched_freq_countPTSD.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)

###### 2.2 Training and testing data
allDatas <- splitDat(datax = datax, begin = PCLN01, end = PCLN20)
###### 3. Descriptive ########################################################

summary(datax)
summary(allDatas[["data_train"]])
summary(allDatas[["data_test"]])
###### 4. EFA ################################################################
###### 4.2 EFA  ####################################################################
###### 4.2.1 Freq= 5  ##########################################################
# Define used sample
cut_1 <- efa_run(cutPoint = allDatas$Splits_frequency[1], datay_train = allDatas[["datay_train"]])

# Safe of EFA 
CUT1_t <- cut_1$cut_top
CUT1_l <- cut_1$cut_low


###### 4.2.2 Freq= 2  ##########################################################

# Define used sample
cut_2 <- efa_run(cutPoint = allDatas$Splits_frequency[2], datay_train = allDatas[["datay_train"]])

# Safe of EFA 
CUT2_t <- cut_2$cut_top
CUT2_l <- cut_2$cut_low

###### 4.2.3 Freq= 1  ##########################################################
# Define used sample
cut_3 <- efa_run(cutPoint = allDatas$Splits_frequency[3], datay_train = allDatas[["datay_train"]])

# Safe of EFA 
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

print(CUT1_t, cutoff = 0.3) #8 factors
print(CUT1_l, cutoff = 0.3) #7 factors

print(CUT2_t, cutoff = 0.3) #8 factors
print(CUT2_l, cutoff = 0.3) #7 factors

print(CUT3_t, cutoff = 0.5) #8 factors
print(CUT3_l, cutoff = 0.5) #7

print(FULL, cutoff = 0.2)   #7 factor model 

###### 4.3 Extracted factor models  #################################################

model_CUT1_t<- ' A=~ PCLN12 + PCLN13 + PCLN14
                 B=~ PCLN01+PCLN02+PCLN03
                 C=~ PCLN09 + PCLN10 + PCLN11
                 D=~ PCLN17 + PCLN18
                 E=~ PCLN19
                 F=~ PCLN06 + PCLN07
                 G=~ PCLN04 + PCLN05
                 H=~ PCLN16
'
# PCL 8 exclued

model_DSM5_PCL <- '
      Rexp =~ PCLN01 + PCLN02 + PCLN03 + PCLN04 + PCLN05
      Av =~ PCLN06 + PCLN07
      An =~ PCLN08 + PCLN09 + PCLN10 + PCLN11 + PCLN12 + PCLN13 + PCLN14 
      Hyper =~ PCLN15 + PCLN16 + PCLN17 + PCLN18 + PCLN19 + PCLN20'

model_7factor <- '
    Rexp =~ PCLN01 + PCLN02 + PCLN03 + PCLN04 + PCLN05
    Av =~ PCLN06 + PCLN07
    Na =~ PCLN08 + PCLN09 + PCLN10 + PCLN11
    An =~ PCLN12 + PCLN13 + PCLN14
    EB =~ PCLN15 + PCLN16
    AA =~ PCLN17 + PCLN18
    DA =~ PCLN19 + PCLN20
'

###### 5. CFA  #####################################################################

# Simulation conditions

###### 5.1 Extracted Model Top 20%  ################################################
model <- model_CUT1_t

###### 5.1.1 Freq= 5 ################################################
Res_1CUT_2080 <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[1], 
                               data_test = allDatas$data_test, model = model_CUT1_t)


###### 5.1.2 Freq= 2 ################################################

Res_1CUT_Median <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[2], 
                                 data_test = allDatas$data_test, model = model_CUT1_t)


###### 5.1.3 Freq= 1 ################################################

Res_1CUT_8020 <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[3], 
                               data_test = allDatas$data_test, model = model_CUT1_t)


###### 5.2 MBI 3 factor Model ################################################
model <- model_DSM5_PCL

###### 5.2.1 Freq= 5 ################################################
Res_Theory_2080 <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[1], 
                                 data_test = allDatas$data_test, model = model_DSM5_PCL)

###### 5.2.2 Freq= 2 ################################################

Res_Theory_Median <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[2], 
                                   data_test = allDatas$data_test, model = model_DSM5_PCL)

###### 5.2.3 Freq= 1 ################################################
Res_Theory_8020 <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[3], 
                                 data_test = allDatas$data_test, model = model_DSM5_PCL)






###### 5.3 MBI 7factor Model ################################################
model_Factor <- model_7factor

###### 5.3.1 Freq= 5 ################################################
Res_FULL_2080 <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[1], 
                               data_test = allDatas$data_test, model = model_7factor)

###### 5.3.2 Freq= 2 ################################################

Res_FULL_Median <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[2], 
                                 data_test = allDatas$data_test, model = model_7factor)

###### 5.3.3 Freq= 1 ################################################

Res_FULL_8020 <- simConditions(nIter = 5, freqCut = allDatas$Splits_frequency[3], 
                               data_test = allDatas$data_test, model = model_7factor)

###### 5.4 Factor Model Results ################################################
Res_1CUT_2080
Res_1CUT_Median
Res_1CUT_8020

Res_Theory_2080
Res_Theory_Median
Res_Theory_8020

Res_FULL_2080
Res_FULL_Median
Res_FULL_8020

###### 5.4.1 Q1 ################################################
# Does the factor model derived in the top 20% fit in the remaining population? -> "ok"
Res_1CUT_2080[8,] #CFI 0.99, #RMSEA 0.024

###### 5.4.2 Q2 ################################################
# Does the factor model derived in the top 20% fit in the lowest 20% -> "marginal"
Res_1CUT_8020[8,] #CFI 0.99 #RMSEA 0.01 #SRMR 0.04

###### 5.4.3 Q3 ################################################
# Does the theory derived factor model fit in the top and low 20% 
Res_Theory_2080[4,] #CFI 0.98 #RMSEA 0.04 #SRMR 0.04
Res_Theory_8020[8,] #CFI 0.92 #RMSEA 0.04 #SRMR 0.06

#### Does 7 factor model holds ####
Res_FULL_2080[4,] #CFI 0.99 #RMSEA 0.02 #SRMR 0.03
Res_FULL_8020[8,] #CFI 0.97 #RMSEA 0.02 #SRMR 0.04



