###############################################
### Step 0: Setup
###############################################

rm(list=ls())
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

load(file = "./DataForAnalysis.Rdata")

###############################################
### Step 1: Summary Tables
###############################################

# Shows the distribution of Contentious variables

datasummary_skim(fin[, c(
  "move",
  "commuting_time",
  "total_budget",
  "ticket_available",
  "risk",
  "pref_uni",
  "single_child",
  "female",
  "partner",
  "academic_house",
  "better_density",
  "better_migration",
  "better_income",
  "better_pop",
  "better_tourism",
  "better_cinemas",
  "better_swimming",
  "better_age",
  "better_study"
   )], output = "./Output/Descriptives/Summary.txt")

# The Output contains the max value for commuting_time which needs to be censored, this value is listed
# after the median 111.3, thus we need to replace it in such a way that the .log file does not disclose the value

uncensored <- read_file("./Output/Descriptives/Summary.txt")
censored <-
  str_replace(uncensored, "111.3  \\| [1-9][0-9][0-9].[0-9] ", "111.3 |---")
write_file(censored, "./Output/Descriptives/Summary.txt")

# Shows the distribution of Continious variables for the people that moved
datasummary_skim(fin[fin$move == TRUE, c(
  "move",
  "commuting_time",
  "total_budget",
  "ticket_available",
  "risk",
  "pref_uni",
  "single_child",
  "female",
  "partner",
  "academic_house",
  "better_density",
  "better_migration",
  "better_income",
  "better_cinemas",
  "better_pop",
  "better_tourism",
  "better_cinemas",
  "better_swimming",
  "better_age",
  "better_study"
   )], output = "./Output/Descriptives/SummaryMove.txt")

# Shows the distribution of Continious variables for the people that did not move
datasummary_skim(fin[fin$move == FALSE, c(
  "move",
  "commuting_time",
  "total_budget",
  "ticket_available",
  "risk",
  "pref_uni",
  "single_child",
  "female",
  "partner",
  "academic_house",
  "better_density",
  "better_migration",
  "better_income",
  "better_cinemas",
  "better_pop",
  "better_tourism",
  "better_cinemas",
  "better_swimming",
  "better_age",
  "better_study"
   )], output = "./Output/Descriptives/SummaryCommute.txt")

# Show the distribution of regional variables for the different cities.
datasummary(
  uni * ticket_available ~ better_density + better_migration + better_income +
    better_pop + better_tourism + better_cinemas + better_swimming + better_study +
    better_age,
  fin,
  output = "./Output/Descriptives/SummaryRegional.txt"
  )

###############################################
### Step 2: Barplots
###############################################

# Copy relevant Variables and create new Categories for (University x Move)
commuters_per_uni <-
  fin %>% select(uni_place, move, ticket_available, uni)
commuters_per_uni$commutes <- " - Commuters"
commuters_per_uni$commutes[commuters_per_uni$move == TRUE] <-
  " - Movers"
commuters_per_uni <-
  commuters_per_uni %>% mutate(uni_commuters = paste0(uni, commutes))

commuters_per_uni$ticket <- "No"
commuters_per_uni$ticket[commuters_per_uni$ticket_available == TRUE] <-
  "Yes"

#Create Barplot with manual Color Scheme
ggplot(commuters_per_uni) +
  geom_bar(aes(uni_commuters, fill = uni, alpha = ticket)) +
  theme_bw() +
  scale_alpha_manual(values = c(0.5, 1)) +
  guides(x = guide_axis(angle = 90)) +
  labs(x = "Movers/Commuters per University",
       y = "Number of Observations",
       fill = "University",
       alpha = "Covered \nby student ticket") +
  scale_fill_manual(
    values = c(
      "Bielefeld" = "#002244",
      "Clausthal" = "#005522",
      "Dortmund" = "#007000",
      "Halle" = "#808080",
      "Hannover" = "#bb8b60",
      "Magdeburg" = "#ff9922",
      "Muenster" = "#ffd777"
    )
  )

ggsave(
  "./Output/Figures/Barplot.png",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600
  )

###############################################
## Step 3: Correlation Table
###############################################

# Calculate correlation for all numeric values
cor_matrix <-
  cor(fin[, c(10, 12, 20, 21, 25, 26, 27, 38:45)], use = "pairwise.complete.obs")

# Export to csv.
write.csv(cor_matrix, file = "./Output/Descriptives/Cor.csv")


###############################################
## Step 4: Compute Average Speeds
###############################################

# Calculate Average Speed (km/h) for all Connections above 10km
stat <- fin[fin$commuting_distance > 10, ]
stat$speed <- stat$commuting_distance / stat$commuting_time * 60

#Remove NA's
stat <- stat[!is.na(stat$speed), ]
summary(stat$speed, seq(0, 1, 0.1), na.rm = TRUE)
sd(stat$speed)

  