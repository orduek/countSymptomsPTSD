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
#                        Descriptives & Distribution                         #
#									                                                           #
##############################################################################


###### Table of Contents #####################################################
# Script 1: PTSD data
#----- 1. Load libraries ----------------------------------------------------#
#----- 2. Import and prepare data -------------------------------------------#
#----- 3. Descriptive -------------------------------------------------------#
#----- 4. Count symptom profiles --------------------------------------------#
#-----  4.1 Count combinations ----------------------------------------------#
#-----  4.2 Plot ------------------------------------------------------------#
#----- 5. Test distributions ------------------------------------------------#


## General Notes ##
# data2 = binarized dataframe
# datax = dataframe script one with scores, binarized items and frequency profiles
# datay = dataframe of items with ordinal ratings AND frequency of symptom profiles (redcued datax)
# dataz = dataframe of items with ordinal ratings WITHOUT frequency of symptom profiles (redcued datay)

###### 1. Load Libraries #####################################################
# Data handling + basic calculations
if(!require("tidyverse")) install.packages("tidyverse")
#if(!require("foreign")) install.packages("foreign")
if(!require("ReIns")) install.packages("ReIns")

#Power Law
if(!require("poweRlaw")) install.packages("poweRlaw")


###### 2. Import and prepare data ############################################
# Prepare
# Load datax & data2 created in script one!

data2<- read_delim("Generated_Data/freq_countPTSD.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
# Count frequency of profiles
data2_counted <- plyr::count(data2[, ])


datax<- read_delim("Generated_Data/Matched_freq_countPTSD.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

###### 3. Descriptive #######################################################
dfPCL5 <- read.spss('/home/or/Documents/pcl5_vaData/phq_pcl_19/PCLpct19.sav', to.data.frame = TRUE)
data_subset <- dfPCL5[,5:24]
# omitting data just based on PCL NAs
dfPCL5 <- dfPCL5[complete.cases(data_subset), ]
dfPCL5$totalPCL <- dfPCL5$PCLN01 + dfPCL5$PCLN02 + dfPCL5$PCLN03 + dfPCL5$PCLN04 + dfPCL5$PCLN05 + 
  dfPCL5$PCLN06 + dfPCL5$PCLN07 + dfPCL5$PCLN08 + dfPCL5$PCLN09 + dfPCL5$PCLN10 + dfPCL5$PCLN11 +
  dfPCL5$PCLN12 + dfPCL5$PCLN13 + dfPCL5$PCLN14 + dfPCL5$PCLN15 + dfPCL5$PCLN16 + dfPCL5$PCLN17 +
  dfPCL5$PCLN18 + dfPCL5$PCLN19 + dfPCL5$PCLN20
## Datax
summary(datax)
mean(dfPCL5$AGE_OCT01)
sd(dfPCL5$AGE_OCT01)
mean(dfPCL5$totalPCL)
sd(dfPCL5$totalPCL)
hist(dfPCL5$totalPCL) #clinical population...

###### 4. Count symptom profiles ############################################
######  4.1 Count combinations ###############################################
### 4.1.1 COMBINATIONS
### 4.1.2 Total number of unique symptom combination
nrow(data2_counted) #8174

## Endorsed only once
sum(data2_counted$freq==1) #6479

## Endorsed <= 2
sum(data2_counted$freq<= 2) #7207

## Endorsed <= 5
sum(data2_counted$freq<= 5) #7744


## Endorsed <= 100
sum(data2_counted$freq<= 100) #8139

## Endorsed > 100
sum(data2_counted$freq> 100) #35

### 4.2.3 Most common combination 
max(data2_counted$freq) #20575

## Assess the symptom profiles
data2_counted <- data2_counted %>% 
  arrange(desc(freq))

print(data2_counted[1,]) # all yes
print(data2_counted[2,]) # only PCL 16 no
print(data2_counted[3,]) # only PCL8 no

### 4.2.4 INDIVIDUALS
## Median endorsement
med <- median(datax$freq) #2453
quantile(datax$freq) # Q1 = 25, Q3= 20575, IQR = 20550
IQR(datax$freq) # 20550

## Endorsed only once
sum(datax$freq==1) #6479
sum(datax$freq==1)/nrow(datax) # 0.123

## Endorsed <= 5
sum(datax$freq<= 5) #9863
sum(datax$freq<= 5)/nrow(datax) # 0.187

## Endorsed <= 50
sum(datax$freq<= 50) #15132
sum(datax$freq<= 50)/nrow(datax) # 0.287

## Endorsed > 100
sum(datax$freq> 100) #35844
sum(datax$freq> 100)/nrow(datax) # 0.681

## Endorsed Top 10 frequencies
data2_counted <- ordered(data2_counted$freq, decreasing = F)
top_10 <- data2_counted$freq[1:10]
cut_min_top_10 <- min(top_10)

sum(datax$freq>= cut_min_top_10) #31634
sum(datax$freq>= cut_min_top_10)/nrow(datax) # 0.60

## Endorsed Top 50 frequencies
top_50 <- (data2_counted$freq[1:50])
cut_min_top_50 <- min(top_50)

sum(datax$freq>= cut_min_top_50) #36995
sum(datax$freq>= cut_min_top_50)/nrow(datax) # 0.70


######  4.3 Plot ##########################################################
### 4.3.1 Plot the 100 most common profiles
freq1_top100  <- data2_counted %>% 
  arrange(freq) %>% 
  top_n(freq, n = 100)

pdf("PTSD_.pdf", width=8, height=8)
ggplot(freq1_top100, aes(x=as.factor(1:nrow(freq1_top100)),y=freq)) +
  geom_hline(yintercept = c(10, 50, 100, 250, 500, 800), color = "grey", size = 0.3) +
  geom_bar(stat = "identity",fill = "grey26") +
  xlab("Unique symptom combinations") + 
  ylab("Number of endorsements") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks=c(10, 50, 100, 250, 500, 800)) +
  ggtitle("The frequency of the fifty most common symptom combinations")
dev.off()  


### 4.3.1 Plot the 100 most common profiles
# datax_counted <- plyr::count(datax$freq)
# colnames(datax_counted) <- c("freq", "count")
# 
# datax_plot <- dplyr::full_join(datax, datax_counted, by = "freq")
# datax_plot$count
# datax_plot$freq
# 
# pdf("MBI_top_100.pdf", width=15, height=8)
# ggplot(datax_plot, aes(x=freq, y=count)) +
#   geom_bar(stat = "identity",fill = "grey26") +
#   xlab("Endorsment of unique combination") + 
#   ylab("Number of individuals") +
#   theme_classic() +
#   ggtitle("") 
# dev.off()  


### 4.3.2 Plot ordered by frequency and symptom sum
# freq1 <- data2_counted %>% 
#   arrange(sumsymptoms, freq)
# 
# freq1 <- freq1 %>%
#   mutate(row_name = row_number())
# 
# pdf("MBI_freq_sum.pdf", width=15, height=8)
# ggplot(freq1, aes(x=as.factor(1:nrow(freq1)),y=freq)) +
#   geom_hline(yintercept = c(10, 50, 100, 250, 500), color = "grey", size = 0.3) +
#   geom_bar(stat = "identity",fill = "grey26") +
#   xlab("Symptom profiles ") + 
#   ylab("Number of endorsements of a given symptom profile") +
#   theme_classic() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks = element_blank(),
#     plot.title = element_text(hjust = 0.5)) +
#   scale_y_continuous(breaks=c(10, 50, 100, 250, 500)) +
#   ggtitle("Distribution of binarized MBI symptom profiles by their frequency")
# dev.off()  

# ### 4.3.3 Plot Pareto Q-Q plots
# pdf("MBI_ParetoQQ.pdf", width=8, height=8)
# ParetoQQ(data = data2_counted$freq, main = "MBI Full sample")
# dev.off()  

###### 5. Test distributions ################################################
#### Prepare
Distribution <- data2_counted$freq

# Power Law
m_pl = displ$new(Distribution)
est_pl = estimate_xmin(m_pl)
m_pl$setXmin(est_pl)
estimate_xmin(m_pl, pars = seq(1.8, 2.5, 0.01))

## Test if power law is possible
bs_p = bootstrap_p(m_pl, no_of_sims = 1000, threads = 5)
# p value
bs_p$p #0.869 -> power law possible
## get mean x-min from bootsrap
mean(bs_p$bootstraps$xmin) # 5.662
mean(bs_p$bootstraps$pars) # 1.9

pdf("PL_estiamtes_PTSD.pdf", width=8, height=8)
plot(bs_p)
dev.off() 

# Log normal
m_ln = dislnorm$new(Distribution) 
est_ln = estimate_xmin(m_ln)
m_ln$setXmin(est_ln)

# Exponential
m_disexp = disexp$new(Distribution) 
est_disexp = estimate_xmin(m_disexp)
m_disexp$setXmin(est_disexp )

# Poisson
m_pois = dispois$new(Distribution)
est_m_pois = estimate_xmin(m_pois)
m_pois$setXmin(est_m_pois)

# Plot different distrubutions
options(scipen=5)
pdf("qq_distributionsPTSD.pdf", width=8, height=8)
plot(m_pl, main="Distribution of symptom combinations",
     xlab="Number of endorsements", ylab="CDF",panel.first = grid(col = "grey80"))
text(x=200, y=0.5, "Power-Law  ",col="2", font=2, cex=0.8)
text(x=200, y=0.39, "Log-Normal ",col="3", font=2, cex=0.8)
text(x=200, y=0.3, "Exponential ",col="4", font=2, cex=0.8)
lines(m_pl, col = 2) 
lines(m_ln, col = 3) 
lines(m_disexp, col = 4)
lines(m_pois, col = 5)
dev.off()


### Test if power law or log-normal distribution fits better
m_ln$setXmin(m_pl$getXmin())
est_m_ln = estimate_pars(m_ln)
m_ln$setPars(est_m_ln)

comp = compare_distributions(m_pl, m_ln)
comp$p_two_sided #not sig: inconclusive
comp$p_one_sided #not sig: inconclusive






