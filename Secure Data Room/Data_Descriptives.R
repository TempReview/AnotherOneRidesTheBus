###############################################
### Step 0: Setup
###############################################

#rm(list=ls())
setwd("C:/Users/ra000016/ownCloud/Home/Mesaras")

# Load Relevant Outputs from Data_Prepare_Script

library(ggplot2)
library(dplyr)
library(haven)
library(lmtest)
library(lmtest)
library(corrplot)
library(jtools)
library(modelsummary)
library(Cairo)

load(file="./DataForAnalysis.Rdata")

###############################################
### Step 1: Summary Tables
###############################################


# Shows the distribution of Contentious variables
datasummary_skim(fin[,c("move","commuting_time","total_budget","ticket_available","risk"
                        ,"pref_uni","single_child","female","partner","academic_house",
                        "better_density","better_income","better_cinemas")], output ="./Output/Descriptives/Summary.txt")

# Shows the distribution of Contentious variables
datasummary_skim(fin[fin$move==TRUE, c("move","commuting_time","total_budget","ticket_available","risk"
                        ,"pref_uni","single_child","female","partner","academic_house",
                        "better_density","better_income","better_cinemas")], output ="./Output/Descriptives/SummaryMove.txt")

datasummary_skim(fin[fin$move==FALSE, c("move","commuting_time","total_budget","ticket_available","risk"
                        ,"pref_uni","single_child","female","partner","academic_house",
                        "better_density","better_income","better_cinemas")], output ="./Output/Descriptives/SummaryCommute.txt")



###############################################
### Step 2: Barplots
###############################################

commuters_per_uni <- fin %>% select(uni_place,move,ticket_available,uni) 
commuters_per_uni$commutes <- " - Commuters"
commuters_per_uni$commutes[commuters_per_uni$move==TRUE] <- " - Movers"
commuters_per_uni <- commuters_per_uni %>% mutate(uni_commuters = paste0(uni,commutes))

commuters_per_uni$ticket <- "No"
commuters_per_uni$ticket[commuters_per_uni$ticket_available==TRUE] <- "Yes"

ggplot(commuters_per_uni) + 
  geom_bar(aes(uni_commuters,fill=uni, alpha=ticket)) +
  theme_minimal() +
  scale_alpha_manual(values = c(0.5, 1)) +
  guides(x=guide_axis(angle=90))+
  labs(x="Movers/Commuters per University", y="Number of Observations", fill="University", alpha="Covered \nby student ticket") 
  
ggsave("./Output/Figures/Barplot.png", width= 18, height=12, units="cm", dpi=600)

#produce_log_file("./Data_Descriptives.R", log_file = "./Output/Logs/Data_Descriptives.log")
