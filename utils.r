###################################################################
#                                                                 #
#                               Utils                             #
#                                                                 #
#                 Authors: Tobias Spiller & Or Duek               #
#                               Version: 0.1                      #
###################################################################
## load libraries
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("rsample")) install.packages("rsample")
if(!require("readxl")) install.packages("readxl")

#Factor Analysis
if(!require("lavaan")) install.packages("lavaan")
if(!require("psych")) install.packages("psych")
if(!require("GPArotation")) install.packages("GPArotation")

## Utils functions to run the power-law and supplementary analysis

# count frequencies and return a table of different frequencies
countFreqGeneral <- function(data2_counted) {
  
  # this function receives two data sets
  TotalProfiles <- nrow(data2_counted) #8174
  
  ## Endorsed only once
  Number_Endorsed_Once <- sum(data2_counted$freq==1) #6479
  
  ## Endorsed <= 2
  Number_Endorsed_Twice <- sum(data2_counted$freq<= 2) #7207
  
  ## Endorsed <= 100
  Number_Endorsed_Less100 <- sum(data2_counted$freq<= 100) #8139
  
  ## Endorsed > 100
  Endorsed_Higher_100 <- sum(data2_counted$freq> 100) #35
  
  ### 4.2.3 Most common combination 
  Number_MostCommon <- max(data2_counted$freq) #20575
  
  ####
  df <- data.frame(TotalProfiles, Number_Endorsed_Once, Number_Endorsed_Twice, Number_Endorsed_Less100,
                   Endorsed_Higher_100,Number_MostCommon)
  ## Assess the symptom profiles
  data2_counted <- data2_counted %>% 
    arrange(desc(freq))
  
  print(data2_counted[1,]) # all yes
  print(data2_counted[2,]) # only PCL 16 no
  print(data2_counted[3,]) # only PCL8 no
  
  return(df)
}
# Count individual freq (i.e. number of participants, not profiles)
countFreq_Ind <- function(datax) {
  ### 4.2.4 INDIVIDUALS
  ## Median endorsement
  med <- median(datax$freq) #2453
  print(paste0("Q1: ", quantile(datax$freq, probs=0.25), " Q2: ", 
               quantile(datax$freq, probs=0.5), " Q3: ", quantile(datax$freq, probs=0.75)))
  
  #quantile(datax$freq) # Q1 = 25, Q3= 2453, IQR = 68
  print(paste0("IRQ: ",IQR(datax$freq) ))
   
  
  ## Endorsed only once
  Endorsed_once <- sum(datax$freq==1) #6479
  Endorsed_oncePer <- sum(datax$freq==1)/nrow(datax) # 0.123
  
  ## Endorsed <= 5
  Endorsed_Less5 <- sum(datax$freq<= 5) #9863
  Endorsed_Less5Per <- sum(datax$freq<= 5)/nrow(datax) # 0.187
  
  ## Endorsed <= 50
  Endorsed_Less50 <- sum(datax$freq<= 50) #15132
  Endorsed_Less50Per <- sum(datax$freq<= 50)/nrow(datax) # 0.287
  
  ## Endorsed > 100
  Endorsed_More100 <- sum(datax$freq> 100) #35844
  Endorsed_More100Per <- sum(datax$freq> 100)/nrow(datax) # 0.681
  
  ## Endorsed Top 10 frequencies
  top_10 <- (data2_counted$freq[1:10])
  cut_min_top_10 <- min(top_10)
  
  Top10_freq <- sum(datax$freq>= cut_min_top_10) #31634
  Top10_freqPer <- sum(datax$freq>= cut_min_top_10)/nrow(datax) # 0.60
  
  ## Endorsed Top 50 frequencies
  top_50 <- (data2_counted$freq[1:50])
  Top50_freqPer <- cut_min_top_50 <- min(top_50)
  
  Top50_freq <- sum(datax$freq>= cut_min_top_50) #36995
  Top50_freqPer <- sum(datax$freq>= cut_min_top_50)/nrow(datax) # 0.70

  df <- data.frame(Endorsed_once, Endorsed_oncePer, Endorsed_Less5, Endorsed_Less5Per,
                   Endorsed_Less50, Endorsed_Less50Per, Endorsed_More100, Endorsed_More100Per,
                   Top10_freq, Top10_freqPer, Top50_freq, Top50_freqPer)
  
  return(df)
}

####################################################################################################
# Organize train/test splits and return the data sets and split point 
####################################################################################################
splitDat <- function(datax, begin, end) {
  # takes data
  # begin = the first column to take
  # end = the last column - both should be names of column
  ###### 2.2 Training and testing data
  splits <- initial_split(datax, strata = freq) #25/75%
  
  data_train <- training(splits)
  data_test  <- testing(splits)
  
  # Create Splits based on % of sample
  Splits_percentage <- c()
  
  freq_2080 <- data_train %>% 
    arrange(freq) %>% 
    slice_tail(prop = 0.2)
  Splits_percentage[1] <- min(freq_2080$freq)
  
  freq_5050 <- data_train %>% 
    arrange(freq) %>% 
    slice_tail(prop = 0.5)
  Splits_percentage[2] <- min(freq_5050$freq)
  
  freq_8020 <- data_train %>% 
    arrange(freq) %>% 
    slice_tail(prop = 0.8)
  Splits_percentage[3] <- min(freq_8020$freq)
  
  # Create Splits based on frequency
  Splits_frequency <- c()
  Splits_frequency[1] <- 5
  Splits_frequency[2] <- 2
  Splits_frequency[3] <- 1
  
  Splits_percentage
  Splits_frequency
  
  # Prepare dataset for further analysis
  datay_train <- data_train %>% 
    select(PCLN01:PCLN20, freq)
  
  dataz_train <- datay_train %>% 
    select(- freq)
  
  all <- list(Splits_percentage = Splits_percentage, 
              Splits_frequency = Splits_frequency, 
              data_train = data_train,
              data_test= data_test, datay_train= datay_train, dataz_train= dataz_train)
  ## returns a list containing vector of splits by percentage (first element). vector of split by freq(seond)
  ## data train (3rd), data_test (4th), datay_train (5th) and dataz_train (6)
  return (all)
}

efa_run <- function(cutPoint, datay_train) {
# 
  CUT = cutPoint
 
  # Prepare
  data_top <- datay_train %>% 
    filter(freq > CUT) %>% 
    select(-freq)
  
  data_low <- datay_train %>% 
    filter(freq <= CUT) %>% 
    select(-freq)
  
  Share_top <- nrow(data_top)/nrow(datay_train)
  
  data_top_cor <- cor(data_top)
  data_low_cor <- cor(data_low)
  
  # Check assumptions
  bar_top <- cortest.bartlett(data_top_cor, n=nrow(data_top))
  bar_low <- cortest.bartlett(data_low_cor, n=nrow(data_low))
  bar_top$p.value
  bar_low
  
  KMO_RES_top <- KMO(data_top_cor)
  KMO_RES_low <- KMO(data_low_cor)
  KMO_RES_top 
  KMO_RES_low
  
  # Estimate number of factors
  EFA_top <- fa.parallel(data_top_cor, fa = "fa", n.obs = nrow(data_top))
  EFA_low <- fa.parallel(data_low_cor, fa = "fa", n.obs = nrow(data_low))
  
  # Estimate factor models
  EFA_top_specific <- fa(data_top_cor, 
                         nfactors=EFA_top$nfact,
                         n.obs = nrow(data_top),
                         SMC=TRUE,
                         fm = "ml",
                         rotate="promax",
                         max.iter=1000)
  
  EFA_low_specific <- fa(data_low_cor, 
                         nfactors=EFA_low$nfact,
                         n.obs = nrow(data_low),
                         SMC=TRUE,
                         fm = "ml",
                         rotate="promax",
                         max.iter=1000)
  
  # Results of EFA 
  print(EFA_top_specific$loadings, cutoff = 0.3)
  print(EFA_low_specific$loadings, cutoff = 0.3)
  
  # Save of EFA 
  CUT_t <- EFA_top_specific$loadings
  CUT_l <- EFA_low_specific$loadings
  
  efa <- list(cut_top = CUT_t, cut_low = CUT_l)
  return (efa)
}

# iteration CFA function
iterateCFA <- function(data, n_itr, sample_size, freqCUT, model) {
  ## This function takes data, frequency cut point and model of factors (can be theoretical or other)
  ## This function is used by the following function (simConditions)
  results_top = matrix(nrow = n_itr, ncol = 7)
  results_low = matrix(nrow = n_itr, ncol = 7)
  RES <- list()
  for (i in 1:n_itr) {
    dfSamp <- slice_sample(data, n = sample_size) 
    dataSAMP_top <- filter(dfSamp, freq > freqCUT)
    dataSAMP_low  <- filter(dfSamp, freq <= freqCUT)  
    factor_top <- cfa(model, data = dataSAMP_top, estimator = "WLSMV")
    factor_low <- cfa(model, data = dataSAMP_low, estimator = "WLSMV")
    results_top[i,1:6] <- fitMeasures(factor_top, c("chisq","df","pvalue","srmr","cfi","rmsea"))
    results_low[i,1:6] <- fitMeasures(factor_low, c("chisq","df","pvalue","srmr","cfi","rmsea"))
    results_top[i,7] <- nrow(dataSAMP_top) 
    results_low[i,7] <- nrow(dataSAMP_low) 
  }
  RES$results_top_df <- data.frame(chisq = results_top[,1], df = results_top[,2], pvalue = results_top[,3], 
                                   srmr = results_top[,4], cfi = results_top[,5], rmsea = results_top[,6], subsamplesize = results_top[,7])
  RES$results_low_df <- data.frame(chisq = results_low[,1], df = results_low[,2], pvalue = results_low[,3], 
                                   srmr = results_low[,4], cfi = results_low[,5], rmsea = results_low[,6], subsamplesize = results_low[,7])
  return(RES)
}

# Simulation conditions
simConditions <- function(nIter=5, freqCut, data_test, model) {
  size_n <- c(1000, 1500, 2000, 5000)
  nitr <- nIter
  size_top <- c()
  size_low <- c()
  size_top_sd <- c()
  size_low_sd <- c()
  
  CFI_TOP <- c()
  CFI_sd_TOP <- c()
  CFI_LOW <- c()
  CFI_sd_LOW <- c()
  
  rmsea_TOP <- c()
  rmsea_sd_TOP <- c()
  rmsea_LOW <- c()
  rmsea_sd_LOW <- c()
  
  p_TOP <- c()
  p_sd_TOP <- c()
  p_LOW <- c()
  p_sd_LOW <- c()
  
  srmr_TOP <- c()
  srmr_sd_TOP <- c()
  srmr_LOW <- c()
  srmr_sd_LOW <- c()
  
  FinRES <- list()
  
  for (i in 1:length(size_n)){
    test <- iterateCFA(data_test, n_itr = nitr, freqCUT = freqCut, sample_size = size_n[i], model = model)
    
    size_top[i] <- median(test$results_top_df$subsamplesize)
    size_top_sd[i] <- sd(test$results_top_df$subsamplesize)
    size_low[i] <- median(test$results_low_df$subsamplesize)
    size_low_sd[i] <- sd(test$results_low_df$subsamplesize)
    
    CFI_TOP[i] <- median(test$results_top_df$cfi)
    CFI_sd_TOP[i] <- sd(test$results_top_df$cfi)
    CFI_LOW[i] <- median(test$results_low_df$cfi)
    CFI_sd_LOW [i] <- sd(test$results_low_df$cfi)
    
    rmsea_TOP[i] <- median(test$results_top_df$rmsea)
    rmsea_sd_TOP[i] <- sd(test$results_top_df$rmsea)
    rmsea_LOW[i] <- median(test$results_low_df$rmsea)
    rmsea_sd_LOW[i] <- sd(test$results_low_df$rmsea)
    
    p_TOP[i] <- median(test$results_top_df$p)
    p_sd_TOP[i] <- sd(test$results_top_df$p)
    p_LOW[i] <- median(test$results_low_df$p)
    p_sd_LOW[i] <- sd(test$results_low_df$p)
    
    srmr_TOP[i] <- median(test$results_top_df$srmr)
    srmr_sd_TOP[i] <- sd(test$results_top_df$srmr)
    srmr_LOW[i] <- median(test$results_low_df$srmr)
    srmr_sd_LOW[i] <- sd(test$results_low_df$srmr)
  }   
  d_top <- data.frame(size_n, size_top, size_top_sd,
                      CFI_TOP, CFI_sd_TOP,
                      rmsea_TOP,rmsea_sd_TOP,
                      p_TOP, p_sd_TOP,
                      srmr_TOP,srmr_sd_TOP)
  
  d_low <- data.frame(size_n, size_low, size_low_sd,
                      CFI_LOW, CFI_sd_LOW,
                      rmsea_LOW, rmsea_sd_LOW, 
                      p_LOW, p_sd_LOW, 
                      srmr_LOW, srmr_sd_LOW)
  
  d_top$group <- c("top")
  d_low$group <- c("low ")
  colnames(d_low) <- colnames(d_top)
  dx <- rbind(d_top, d_low)
  
  return(dx)


}

