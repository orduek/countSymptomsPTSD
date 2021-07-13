##############################################################################
#									                                                           #
# 	 The Heterogeneity of Symptoms of Mental Disorders is Heavy-Tailed       #
#                                                                            #
#                         Or Duek & Tobias Spiller                           # 
#                                                                            #
#                       Code Version 2.0 (01.06.2021)                        #
#                                                                            #
#----------------------------------------------------------------------------#
#                                                                            #
#                            Script IV -  Figures                            #
#									                                                           #
##############################################################################


###### Table of Contents #####################################################
#----- 1. Load libraries ----------------------------------------------------#
#----- 2. Figure 1A ---------------------------------------------------------#
#----- 3. Figure 1B ---------------------------------------------------------#
#----- 4. Figure 1 FULL -----------------------------------------------------#
#----- 5. Figure X ----------------------------------------------------------#
#----- 6. Figure X Zoom------------------------------------------------------#
#----- 7. Session Info  -----------------------------------------------------#


## General Notes ##

###### 1. Load Libraries #####################################################
# Data handling + basic calculations
if(!require("tidyverse")) install.packages("tidyverse")

if(!require("poweRlaw")) install.packages("poweRlaw")

if(!require("gridExtra")) install.packages("gridExtra")
if(!require("cowplot")) install.packages("cowplot")
if(!require("scales")) install.packages("scales")
if(!require("ggforce")) install.packages("ggforce")


###### 2. FIGURE 1A ##############################################################
#### Import
## MBI
load("raw_data/freq1_top_MBI.RData")
## DASS
load("raw_data/freq1_top_DASS.RData")

#### Individual Figures
##PANSS
load('Analysis/PANSS/Generated Data/freq1_top_PANSS.RData')
q1 <-ggplot(freq1_top_PANSS, aes(x=as.factor(1:nrow(freq1_top_PANSS)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("Freq") +
  ggtitle("PANSS")+
  theme_minimal_hgrid() +
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

# q1<- ggdraw(q1) + 
#   draw_label("    Draft", color = "#C0A0A0", size = 50, angle = 45)
  
##PCL
load('Analysis/PCL5/Generated_Data/freq1_top_PCL.RData')
q2 <- ggplot(freq1_top_PCL, aes(x=as.factor(1:nrow(freq1_top_PCL)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("Phenotypes") + 
  ylab("") +
  ggtitle("PCL")+
  theme_minimal_hgrid() +
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

# q2<- ggdraw(q2) + 
#   draw_label("    Draft", color = "#C0A0A0", size = 50, angle = 45)

##DASS
q3 <- ggplot(freq1_top_DASS, aes(x=as.factor(1:nrow(freq1_top_DASS)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("") +
  ggtitle("DASS")+
  theme_minimal_hgrid() +
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

##MBI
q4 <-ggplot(freq1_top_MBI, aes(x=as.factor(1:nrow(freq1_top_MBI)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab(" ") + 
  ylab("Freq") +
  ggtitle("MBI")+
  theme_minimal_hgrid() +
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

##PHQ
load('Analysis/PHQ9/Generated Data/freq1_top_PHQ9.RData')
q5 <-ggplot(freq1_top_PHQ, aes(x=as.factor(1:nrow(freq1_top_PHQ)),y=freq)) +
  geom_bar(stat = "identity",fill = "grey26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  xlab("Phenotypes") + 
  ylab("") +
  ggtitle("PQH")+
  theme_minimal_hgrid() +
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

# q5<- ggdraw(q5) + 
#   draw_label("    Draft", color = "#C0A0A0", size = 50, angle = 45)

##DASS
# q6 <- ggplot(freq1_top_DASS, aes(x=as.factor(1:nrow(freq1_top_DASS)),y=freq)) +
#   geom_bar(stat = "identity",fill = "grey26") +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   xlab(" ") + 
#   ylab("") +
#   ggtitle("GAD")+
#   theme_minimal_hgrid() +
#   panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks = element_blank())

# q6<- ggdraw(q6) + 
#   draw_label("    Draft", color = "#C0A0A0", size = 50, angle = 45)

#### Panel
# Cow Plot
pdf("Figure_1B.pdf", width=8, height=8)
plot_grid(q1, q2, q3, q4, q5, labels=c("A", "B", "C", "D", "E"), ncol = 3, nrow = 2)
dev.off()



###### 3. FIGURE 1B ##############################################################
#### Import
## MBI
load("raw_data/res_pl_MBI.RData")
load("raw_data/line_pl_MBI.RData")
load("raw_data/line_ln_MBI.RData")

## DASS
load("raw_data/res_pl_DASS.RData")
load("raw_data/line_pl_DASS.RData")
load("raw_data/line_ln_DASS.RData")

## PANSS
load('Analysis/PANSS/Generated Data/res_pl_PANSS.RData')
load('Analysis/PANSS/Generated Data/line_pl_PANSS.RData')
load('Analysis/PANSS/Generated Data/line_ln_PANSS.RData')

## PCL
load('Analysis/PCL5/Generated_Data/res_pl_PCL.RData')
load('Analysis/PCL5/Generated_Data/line_pl_PCL.RData')
load('Analysis/PCL5/Generated_Data/line_ln_PCL.RData')

## PHQ9
load('Analysis/PHQ9/Generated Data/res_pl_PHQ9.RData')
load('Analysis/PHQ9/Generated Data/line_pl_PHQ9.RData')
load('Analysis/PHQ9/Generated Data/line_ln_PHQ9.RData')
#### Individual Figures

## PANSS
p1 <- ggplot(res_pl_PANSS, aes(x=x,y=y)) +
  geom_point(size = 1)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_line(data = line_pl_PANSS, aes(x=x, y=y), color = "red", size = 0.4) +
  geom_line(data = line_ln_PANSS, aes(x=x, y=y), color = "blue", size = 0.4)+
  xlab("") + 
  ylab("CDF") +
  ggtitle("PANSS")+
  theme_half_open(12)+
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  background_grid() 

# p1<- ggdraw(p1) + 
#   draw_label("    Draft", color = "#C0A0A0", size = 50, angle = 45)

## PCL
p2 <- ggplot(res_pl_PCL, aes(x=x,y=y)) +
  geom_point(size = 1)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_line(data = line_pl_PCL, aes(x=x, y=y), color = "red", size = 0.4) +
  geom_line(data = line_ln_PCL, aes(x=x, y=y), color = "blue", size = 0.4) +
  xlab("Freq") + 
  ylab("") +
  ggtitle("PCL")+
  theme_half_open(12)+
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  background_grid() 

# p2<- ggdraw(p2) + 
#   draw_label("    Draft", color = "#C0A0A0", size = 50, angle = 45)

## DASS
p3 <- ggplot(res_pl_DASS, aes(x=x,y=y)) +
  geom_point(size = 1)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_line(data = line_pl_DASS, aes(x=x, y=y), color = "red", size = 0.4) +
  geom_line(data = line_ln_DASS, aes(x=x, y=y), color = "blue", size = 0.4) +
  xlab("") + 
  ylab("") +
  ggtitle("DASS")+
  theme_half_open(12)+
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  background_grid() 

## MBI
p4 <- ggplot(res_pl_MBI, aes(x=x,y=y)) +
  geom_point(size = 1)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_line(data = line_pl_MBI, aes(x=x, y=y), color = "red", size = 0.4) +
  geom_line(data = line_ln_MBI, aes(x=x, y=y), color = "blue", size = 0.4)+
  xlab("") + 
  ylab("CDF") +
  ggtitle("MBI")+
  theme_half_open(12)+
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  background_grid() 

## PHQ
p5 <- ggplot(res_pl_PHQ, aes(x=x,y=y)) +
  geom_point(size = 1)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_line(data = line_pl_PHQ, aes(x=x, y=y), color = "red", size = 0.4) +
  geom_line(data = line_ln_PHQ, aes(x=x, y=y), color = "blue", size = 0.4)+
  xlab("Freq") + 
  ylab("") +
  ggtitle("PHQ")+
  theme_half_open(12)+
  panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
  background_grid() 

# p5<- ggdraw(p5) + 
#   draw_label("    Draft", color = "#C0A0A0", size = 50, angle = 45)

## GAD
# p6 <- ggplot(res_pl_MBI, aes(x=x,y=y)) +
#   geom_point(size = 1)+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))+
#   geom_line(data = line_pl_MBI, aes(x=x, y=y), color = "red", size = 0.4) +
#   geom_line(data = line_ln_MBI, aes(x=x, y=y), color = "blue", size = 0.4)+
#   xlab("") + 
#   ylab("") +
#   ggtitle("GAD")+
#   theme_half_open(12)+
#   panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE)+
#   background_grid() 
# 
# p6<- ggdraw(p6) + 
#   draw_label("    Draft", color = "#C0A0A0", size = 50, angle = 45)

###### 4. FIGURE 1 FULL #########################################################
pdf("Figure_1_FULL.pdf", width=8, height=12)
plot_grid(
  q1, q2, q3, q4, q5, #q6,
  p1, p2, p3, p4, p5, #p6,
  labels=c("A", "", "", "", "", "",
           "B", "", "", "", "", ""),
  ncol = 3, nrow = 4)
dev.off()


###### 5. Figure X #########################################################
# pdf("Figure_MBI_full.pdf", width=8, height=4)
# ggplot(data2_counted, aes(x=as.factor(1:nrow(data2_counted)),y=freq)) +
#   geom_bar(stat = "identity",fill = "grey26") +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   xlab("Phenotypes") + 
#   ylab("Freq") +
#   ggtitle("MBI")+
#   theme_minimal_hgrid() +
#   panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks = element_blank())
# dev.off()




######  6. FIGURE X Zoom ####################################################
# data2_counted <- read_delim("Raw Data/freq_count_MBI.csv", 
#                              ";", escape_double = FALSE, trim_ws = TRUE)
# 
# data2_counted <- data2_counted %>% 
#   select(freq)
# 
# f1 <- ggplot(data2_counted, aes(x=as.numeric(1:nrow(data2_counted)),y=freq)) +
#   geom_bar(stat = "identity",fill = "grey26") +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   xlab("Number of phenotypes") + 
#   ylab("Freq") +
#   ggtitle("MBI")+
#   theme_cassic() +
#   panel_border(color = "black", size = 0.4, linetype = 1, remove = FALSE) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks = element_blank())
# 
# 
# # Choose which items to display
# f1 + 
#   facet_zoom(xlim = c(0, 100))


######  7. Session info #########################################################
sessionInfo()




#####################################  END  ####################################
