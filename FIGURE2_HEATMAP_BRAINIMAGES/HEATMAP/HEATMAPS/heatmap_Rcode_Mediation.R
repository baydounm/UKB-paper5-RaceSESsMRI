#######################################################################
#######  Making heatmaps for Paper4
#######  Mission: Plot the betas   
#######  Programmer: Yi-Han Hu
#######  Date: Jan. 17 2023
#######################################################################

op <- options(nwarnings = 10000)
# --------------------------------------
# Specify working directory where the script and data files are
# --------------------------------------
WorkingDirectory = "w:/"

# --------------------------------------
# Set working directory
# --------------------------------------
setwd(WorkingDirectory)

# --------------------------------------
# Turn off scientific notation
# --------------------------------------
options(scipen=999)

# --------------------------------------
# Install/load the packages
# --------------------------------------
library(readxl) #read excel file.
library(haven) #read dta file.
library(tidyr) 
library(dplyr)
library(sjmisc) #Convert wide data to long format or vise versa
library(ggplot2)
library(ggnewscale) #Multiple Fill and Colour Scales in 'ggplot2'
library(scico) #Scientific colour map palettes

# ---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# ---------------------------------- Part 1 Data preprocess -----------------------------------#
# ---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# --------------------------------------
# Load data:
# --------------------------------------
Racial_SES_MRI_mediation <- read_dta("Data/HEATMAP_DATA_SMRI_collapsedfin.dta")
dim(Racial_SES_MRI_mediation)
head(Racial_SES_MRI_mediation)
colnames(Racial_SES_MRI_mediation)

# Racial
Racial_MRI_mediation <- Racial_SES_MRI_mediation %>% 
  dplyr::select(roi, te_nonwhite_st, p_te_nonwhite, ie_nonwhite_st, p_ie_nonwhite, de_nonwhite_st, p_de_nonwhite, percent_mediated_nonwhite) %>% 
  dplyr::rename(ROI = roi, TE = te_nonwhite_st, p_TE = p_te_nonwhite, IE = ie_nonwhite_st, p_IE = p_ie_nonwhite,
                DE = de_nonwhite_st, p_DE = p_de_nonwhite, percent_mediated = percent_mediated_nonwhite) %>% 
  mutate("p_mediated" = 0.01)

Racial_MRI_mediation.long <- to_long(Racial_MRI_mediation, keys = 'term',
                                     values = c('estimate','p'), 
                                     c('TE','DE','IE', 'percent_mediated'),
                                     c('p_TE','p_DE','p_IE', 'p_mediated'))

# SES
SES_MRI_mediation <- Racial_SES_MRI_mediation %>% 
  dplyr::select(roi, te_ses_st, p_te_ses, ie_ses_st, p_ie_ses, de_ses_st, p_de_ses, percent_mediated_ses) %>% 
  dplyr::rename(ROI = roi, TE = te_ses_st, p_TE = p_te_ses, IE = ie_ses_st, p_IE = p_ie_ses,
                DE = de_ses_st, p_DE = p_de_ses, "percent_mediated" = "percent_mediated_ses") %>% 
  mutate("p_mediated" = 0.01)

SES_MRI_mediation.long <- to_long(SES_MRI_mediation, keys = 'term',
                                     values = c('estimate','p'), 
                                     c('TE','DE','IE', 'percent_mediated'),
                                     c('p_TE','p_DE','p_IE', 'p_mediated'))

# ---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# ------------------------------------- Part 2 Heat maps --------------------------------------#
# ---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# --------------------------------------
# Heatmap for Race and SES groups separately
# --------------------------------------

# Built a function to plot heat map
heatmap <- function(data, Racial.exposure = 1, coeff.palette = "vik", mediation.palette = 'acton', hide.unsig = TRUE){
  if (Racial.exposure == 1){
    exposure = "Racial minority status"
    } else if(Racial.exposure == 0){
      exposure = "SES"
    }
  
  # Prepare data for heatmap
  data.long <- data %>% 
    mutate(ap=ifelse(p < 0.001, 1,
                     ifelse(p >= 0.001 & p < 0.01, 2, 
                            ifelse(p >= 0.01 & p < 0.05, 3, 4))),
           aq=ifelse(p < 0.05 , "Pass", "insig"),
           bg.line=ifelse(term=="percent_mediated", "White", "Dark Grey"),
           bg.color=ifelse(term=="percent_mediated", "Dark Grey", "White"),
           break.mediation = ifelse(term != "percent_mediated", NaN,
                                    ifelse(abs(estimate) <= 10, 1,
                                           ifelse(abs(estimate) > 10 & abs(estimate) <= 20, 2,
                                                  ifelse(abs(estimate) > 20 & abs(estimate) <= 30, 3,
                                                         ifelse(abs(estimate) > 30 & abs(estimate) <= 40, 4,
                                                                ifelse(abs(estimate) > 40 & abs(estimate) <= 50, 5, 6))))))) %>% 
    arrange(factor(ROI, levels = unique(SES_MRI_mediation.long$ROI)), factor(term, levels = c('TE','DE','IE', 'percent_mediated'))) %>% 
    mutate(term = ifelse(term == "percent_mediated", "%mediated", term),
           break.mediation = factor(break.mediation, levels = c("1", "2", "3", "4", "5", "6", "NaN")))
  
  # Set up color scheme for plot
  pal <- scico(7, palette = mediation.palette)
  
  if (hide.unsig == TRUE){
    p.plot <- ggplot(data = data.long, aes(x = factor(term, levels = c('TE','DE','IE', '%mediated')), y = forcats::fct_rev(factor(ROI, levels = unique(ROI)))))+
      geom_tile(color = data.long$bg.line, fill = data.long$bg.color)+
      geom_point(data= subset(data.long, term %in% c("TE", "DE", "IE")),
                 aes(shape=factor(aq),
                     size=factor(ap), 
                     fill=estimate))+
      scale_shape_manual(values=c('insig'=1, 'Pass'=21), guide = "none")+
      scale_fill_scico(palette = coeff.palette, midpoint = 0, aesthetics = c("colour","fill")) +
      scale_size_manual(values=c(8, 6, 4, 2), labels = c("< .001", "< .01", "< .05", "\u2265 .05"))+
      new_scale_color() +
      geom_point(shape=19, size=6, data = subset(data.long, term %in% c("%mediated")),
                 aes(color=break.mediation))+
      scale_color_manual(breaks=c(1, 2, 3, 4, 5, 6), drop = FALSE, labels = c("\U2264 10%", "10% - 20%", "20% - 30%", "30% - 40%", "40% - 50%", "> 50%"), values=pal)+
      labs(title=paste("Heatmap(", exposure, " and brain sMRI volumetric outcomes)", sep = ""),
           subtitle=paste("Mediation pathways through lifestyle factors", "", sep = ""),
           x="TE: Total effect; DE: Direct effect; IE: Indirect effect.",
           y="Brain ROI (20 measures)",
           size=paste("p-value\nsolid circle: p<.05"), fill=(expression(paste(beta," coefficients"))), color=("% mediated"),
           caption="% mediated is the percent of total effect that is mediated. No p-values were generated.") +
      theme(plot.title = element_text(color="Dark blue", size=15, face="bold.italic", hjust = 0.5),
            plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="Dark blue"),
            plot.caption=element_text(size=10, hjust=0.5, color="Dark grey"),
            axis.title.x = element_text(color="deepskyblue", size=13, face="bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
            aspect.ratio=7/4)+
      coord_fixed()
  } else if (hide.unsig == FALSE){
    p.plot <- ggplot(data = data.long, aes(x = factor(term, levels = c('TE','DE','IE', '%mediated')), y = forcats::fct_rev(factor(ROI, levels = unique(ROI)))))+
      geom_tile(color = data.long$bg.line, fill = data.long$bg.color)+
      geom_point(data= subset(data.long, term %in% c("TE", "DE", "IE")),
                 aes(size=factor(ap), 
                     fill=estimate),shape=21)+
      scale_fill_scico(palette = coeff.palette, midpoint = 0, aesthetics = c("colour","fill")) +
      scale_size_manual(values=c(8, 6, 4, 2), labels = c("< .001", "< .01", "< .05", "\u2265 .05"))+
      new_scale_color() +
      geom_point(shape=19, size=6, data = subset(data.long, term %in% c("%mediated")),
                 aes(color=break.mediation))+
      scale_color_manual(breaks=c(1, 2, 3, 4, 5, 6), drop = FALSE, labels = c("\U2264 10%", "10% - 20%", "20% - 30%", "30% - 40%", "40% - 50%", "> 50%"), values=pal)+
      labs(title=paste("Heatmap(", exposure, " and brain sMRI volumetric outcomes)", sep = ""),
           subtitle=paste("Mediation pathways through lifestyle factors", "", sep = ""),
           x="TE: Total effect; DE: Direct effect; IE: Indirect effect.",
           y="Brain ROI (20 measures)",
           size=paste("p-value"), fill=(expression(paste(beta," coefficients"))), color=("% mediated"),
           caption="% mediated is the percent of total effect that is mediated. No p-values were generated.") +
      theme(plot.title = element_text(color="Dark blue", size=15, face="bold.italic", hjust = 0.5),
            plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="Dark blue"),
            plot.caption=element_text(size=10, hjust=0.5, color="Dark grey"),
            axis.title.x = element_text(color="deepskyblue", size=13, face="bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
            aspect.ratio=7/4)+
      coord_fixed()
  }
  return(p.plot)
}

# Making plots
Racial.hide.unsig <- heatmap(data = Racial_MRI_mediation.long, Racial.exposure = 1, hide.unsig = TRUE)
Racial <- heatmap(data = Racial_MRI_mediation.long, Racial.exposure = 1, hide.unsig = FALSE)
SES.hide.unsig <- heatmap(data = SES_MRI_mediation.long, Racial.exposure = 0, hide.unsig = TRUE)
SES <- heatmap(data = SES_MRI_mediation.long, Racial.exposure = 0, hide.unsig = FALSE)

# Export plots
dir.create(paste(WorkingDirectory,"Output/plot",sep=""), recursive = TRUE)
plot.out.folder <- paste(WorkingDirectory,"Output/plot",sep="")

ggsave(paste(plot.out.folder,"/Paper5_heatmaps_Racial_hide_unsig.jpeg",sep=""), Racial.hide.unsig, width = 8.5, height = 11, units = "in", dpi = 300)
ggsave(paste(plot.out.folder,"/Paper5_heatmaps_Racial.jpeg",sep=""), Racial, width = 8.5, height = 11, units = "in", dpi = 300)
ggsave(paste(plot.out.folder,"/Paper5_heatmaps_SES_hide_unsig.jpeg",sep=""), SES.hide.unsig, width = 8.5, height = 11, units = "in", dpi = 300)
ggsave(paste(plot.out.folder,"/Paper5_heatmaps_SES.jpeg",sep=""), SES, width = 8.5, height = 11, units = "in", dpi = 300)
