############
# Step 0: Setup
############

# Clear loaded Data and reload
rm(list = ls())
setwd("C:/Users/ra000016/ownCloud/Home/Mesaras")
options(warn = -1)
load(file = "DataForAnalysis.Rdata")

# Load Libraries
library(ggplot2)
library(dplyr)
library(haven)
library(lmtest)
library(aod)
library(jtools)
library(huxtable)
library(sjPlot)
library(margins)
library(lmtest)
library(ResourceSelection)
library(rcompanion)
library(fmsb)
library(sdcR)
library(performance)

# Define some strings for later, based on the quantile
# distribution of variables (20%, 50%, 80%) and (20%,40%,60%,80%)
quantiles_budget <-
  quantile(fin$total_budget,
           probs = seq(0, 1, 0.1),
           na.rm = TRUE)

budget_string <-
  paste0(
    "total_budget [",
    quantiles_budget[3],
    ", ",
    quantiles_budget[6],
    ", ",
    quantiles_budget[9],
    "]"
  )

budget_string2 <-
  paste0(
    "total_budget [",
    quantiles_budget[3],
    ", ",
    quantiles_budget[5],
    ", ",
    quantiles_budget[7],
    ", ",
    quantiles_budget[9],
    "]"
  )


# Add helping function for printing test statistics as a table
# Takes a list of models an for each model prints R2 values and 3 tests.

summarytests <- function(modellist) {
  results <- data.frame(matrix(nrow = 8, ncol = length(modellist)))
  for (i in 1:length(modellist))
  {
    model <- modellist[[i]]
    col <-
      c(
        r2_coxsnell(model),
        r2_nagelkerke(model),
        lrtest(model)[2, 4],
        lrtest(model)[2, 5],
        waldtest(model)[2, 3],
        waldtest(model)[2, 4],
        performance_hosmer(model, n_bins = 3)$chisq,
        performance_hosmer(model, n_bins = 3)$p.value
      )
    results[, i] <- col
  }
  row.names(results) <-
    c("Cox",
      "Nagelkerke",
      "LR X",
      "LR-p",
      "Wald",
      "Wald-p",
      "Hosmer",
      "Hosmer-p")
  return(results)
}

##########
# Step 1: H1 If a student ticket is available,
# the likelihood to move to the place of
# study versus commuting is lower
###########

# Base model
model1 <-
  glm(move ~ commuting_time + ticket_available,
      data = fin,
      family = "binomial")

# Add Budget
model2 <-
  glm(
    move ~  commuting_time + ticket_available + total_budget,
    data = fin,
    family = "binomial"
  )

# Add individual controls
model3 <-
  glm(
    move ~ commuting_time + total_budget + ticket_available +
      risk + pref_uni + single_child + female + partner +
      academic_house ,
    data = fin,
    family = "binomial"
  )

# Add regional controls
model4 <-
  glm(
    move ~ commuting_time + total_budget + ticket_available + risk + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age,
    data = fin,
    family = "binomial"
  )

# Create output tables
######

# Assign approrpiate names
coef_keys <-  c(
  "Commuting Time \n (in Minutes)" = "commuting_time",
  "Student Ticket \n (Yes/No) " = "ticket_availableTRUE",
  "Total Budget \n (in 100€)" = "total_budget",
  "Risk Aversion for moving" = "risk",
  "Preferred Uni \n (Yes/No)" = "pref_uniTRUE",
  "Female \n (Yes/No)" = "femaleTRUE",
  "Partnership \n (Yes/No)" = "partnerTRUE",
  "Single Child \n (Yes/No)" = "single_childTRUE",
  "At least one Parent Academic \n (Yes/No)" = "academic_houseTRUE",
  "Uni region younger \n (Yes/No) " = "better_ageTRUE",
  "Uni faster growing \n (Yes/No)" = "better_migrationTRUE"
)

# Create a List of models
models <- list(model1, model2, model3, model4)

# Print Regression Estimates
export_summs(
  models,
  model.names = c("Model 1", "Model 2" , "Model 3", "Model 4"),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  coefs = c("(Intercept)" = "(Intercept)", coef_keys),
  number_format = "%.3f"
)

#Calculate Test-Statistics for models
summarytests(models)

# Re-compute as Average Marginal Effects
models <- lapply(models, FUN = margins)

# Print Regression Estimates
export_summs(
  models,
  model.names = c("Model 1", "Model 2" , "Model 3", "Model 4"),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  coefs = coef_keys,
  number_format = "%.3f"
)

##########
# Step 1 1/2: Alternative specification
###########

# Calculate alternative set of models, based on financial resources


model2a <- glm(move ~  commuting_time + ticket_available,
               data = fin_bafoeg,
               family = "binomial")


model2b <-
  glm(
    move ~  commuting_time + ticket_available + total_budget,
    data = fin_bafoeg,
    family = "binomial"
  )

model2c <-
  glm(
    move ~  commuting_time + total_budget + ticket_available +
      bafoeg_indicator,
    data = fin,
    family = "binomial"
  )
model2d <-
  glm(
    move ~  commuting_time + total_budget  + bafoeg_share + ticket_available ,
    data = fin_bafoeg,
    family = "binomial"
  )
model2e <- glm(
  move ~    commuting_time + total_budget  + bafoeg_amount + ticket_available ,
  data = fin_bafoeg,
  family = "binomial"
)
model2f <- glm(
  move ~ commuting_time + budget_without_bafoeg  +  bafoeg_amount + ticket_available ,
  data = fin_bafoeg,
  family = "binomial"
)

coef_keys_bafoeg <-  c(
  "Commuting Time \n (in Minutes)" = "commuting_time",
  "Student Ticket \n (Yes/No) " = "ticket_availableTRUE",
  "Total Budget \n (in 100€)" = "total_budget",
  "Bafoeg Available \n (Yes/No)" = "bafoeg_indicatorTRUE",
  "Bafoeg Share \n (in %)" = "bafoeg_share",
  "Bafoeg Amaount \n (in 100 €)" = "bafoeg_amount",
  "Budget without Bafoeg \n (in 100 €)" = "budget_without_bafoeg"
)

# Complete table for Appendix as Odds Ratio not needed in final publication
#export_summs(model2a,model2b,model2c,model2d,model2e,model2f,
#             model.names = c("Model 2(a)", "Model 2(b)", "Model 2(c)", "Model 2(d)", "Model 2(e)", "Model 2(f)"),
#             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
#             coefs= c( "(Intercept)" = "(Intercept)", coef_keys_bafoeg),
#             number_format = "%.3f") # %>% quick_html(file="table1.html")
#Only for Robustness checks

models <- list(model2a, model2b, model2c, model2d, model2e, model2f)

summarytests(models)

models <- lapply(models, FUN = margins)

# Print Regression Estimates
export_summs(
  models,
  model.names = c(
    "Model 2(a)",
    "Model 2(b)",
    "Model 2(c)",
    "Model 2(d)",
    "Model 2(e)",
    "Model 2(f)"
  ),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  coefs = coef_keys_bafoeg,
  number_format = "%.3f"
)

########
# Create  Plots
########

#Switch Names and Values for Printing
coef_keys2 <- names(coef_keys)
names(coef_keys2) <- coef_keys

# Create Plot for Predicted Probabilities
plot_model(
  model4,
  type = "pred",
  terms = c("commuting_time [all]", "ticket_available")
) +
  theme_bw() +
  labs(
    color = "Covered by student ticket",
    y = "Probability of moving",
    x = "commuting time (min)",
    title = NULL
  ) +
  scale_colour_manual(
    labels = c("No", "Yes"),
    values = c("FALSE" = "#ff9922", "TRUE" = "#002244")
  ) +
  xlim(c(0, 200)) + theme(legend.position = "bottom")


# Save Graphics
ggsave(
  "./Output/Figures/Model_4_Predicted.png",
  width = 12,
  height = 12,
  units = "cm",
  dpi = 600
)

##########
# Step 2: H2 The effect of the student tickets on the likelihood
# to move is reduced with increasing commuting time
###########

model5  <-
  glm(
    move ~ commuting_time * ticket_available  + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age ,
    data = fin,
    family = "binomial"
  )


# Assign approrpiate names
coef_keys <-  c(
  "Commuting Time \n (in Minutes)" = "commuting_time",
  "Student Ticket \n (Yes/No) " = "ticket_availableTRUE",
  "Total Budget \n (in 100€)" = "total_budget",
  "Risk Aversion for moving" = "risk",
  "Preferred Uni \n (Yes/No)" = "pref_uniTRUE",
  "Female \n (Yes/No)" = "femaleTRUE",
  "Partnership \n (Yes/No)" = "partnerTRUE",
  "Single Child \n (Yes/No)" = "single_childTRUE",
  "At least one Parent Academic \n (Yes/No)" = "academic_houseTRUE",
  "Uni region younger \n (Yes/No) " = "better_ageTRUE",
  "Uni faster growing \n (Yes/No)" = "better_migrationTRUE"
)

coef_keys_inter <-
  c(coef_keys, "Commuting Time \n x Ticket Available" = "commuting_time:ticket_availableTRUE")
# Complete table for Appendix as Odds Ratio
models <- list(model5)

# Print Regression Estimates
export_summs(
  model5,
  model.names = c("Model 5"),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  coefs = c("(Intercept)" = "(Intercept)", coef_keys_inter),
  number_format = "%.3f"
)

# Compute Test-Statistics
summarytests(models)

# Re-compute as Average Marginal Effects
models <- list(model5)
models <- lapply(models, FUN = margins)

# Print Regression Estimates
export_summs(
  models,
  model.names = c("Model 5"),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  coefs = c(coef_keys),
  number_format = "%.3f"
)

# Create  Plots
coef_keys2 <- names(coef_keys_inter)
names(coef_keys2) <- coef_keys_inter

# Create Plot for Predicted Probabilities
plot_model(
  model5,
  type = "pred",
  terms = c("commuting_time [all]", "ticket_available")
) +
  theme_bw() +
  labs(
    color = "Covered by student ticket",
    y = "Probability of moving",
    x = "commuting time (min)",
    title = NULL
  ) +
  scale_colour_manual(
    labels = c("No", "Yes"),
    values = c("FALSE" = "#ff9922", "TRUE" = "#002244")
  ) +
  xlim(c(0, 200)) + theme(legend.position = "bottom")

# Save Figure
ggsave(
  "./Output/Figures/Model_5_Predicted.png",
  width = 12,
  height = 12,
  units = "cm",
  dpi = 600
)

##########
# Step 4 : Instead of running a triple regression, we remove
#                the commuting distance interaction from our analysis and introduce
#                 an interaction between the social variables and ticket
#########

model6  <-
  glm(
    move ~ total_budget * ticket_available  + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house  +  better_migration +
      better_age ,
    data = fin,
    family = "binomial"
  )

model7  <-
  glm(
    move ~  academic_house * ticket_available  + commuting_time +  total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin,
    family = "binomial"
  )

model8  <-
  glm(
    move ~  risk * ticket_available  + commuting_time +  total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin,
    family = "binomial"
  )

models <- list(model6, model7, model8)
export_summs(
  models,
  model.names = c("Model Int-Budget", "Model Int-Parent", "Model Int-ACA"),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

summarytests(models)
models <- lapply(models, FUN = margins)

# Print Regression Estimates
export_summs(
  models,
  model.names = c("Model Int-Budget", "Model Int-Parent", "Model Int-ACA"),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

## Plot Model 6
p6 <- plot_model(
  model6,
  type = "pred",
  terms = c("total_budget [all]", "ticket_available"),
  ci.lvl = 0.95
)  +
  theme_bw() +
  labs(
    x = "Total Budget \n (in 100€)",
    y = "Probability of moving",
    color = "Covered by student ticket",
    title = NULL
  ) + xlim(c(0, 15)) +
  scale_colour_manual(
    labels = c("No", "Yes"),
    values = c("FALSE" = "#ff9922", "TRUE" = "#002244")
  ) +  theme(legend.position = "bottom")
p6

ggsave(
  "./Output/Figures/Model_6_Predicted.png",
  width = 12,
  height = 12,
  units = "cm",
  dpi = 600
)

## Plot Model 7
p7 <- plot_model(model7,
                 type = "pred",
                 terms = c("academic_house", "ticket_available")) +
  theme_bw() +
  labs(
    color = "Covered by student ticket",
    y = "Probability of moving",
    x = "At least one Parent Academic \n (Yes/No)",
    title = NULL
  ) +
  scale_colour_manual(
    labels = c("No", "Yes"),
    values = c("FALSE" = "#ff9922", "TRUE" = "#002244")
  ) +
  theme(legend.position = "bottom")

p7

ggsave(
  "./Output/Figures/Model_7_Predicted.png",
  width = 12,
  height = 12,
  units = "cm",
  dpi = 600
)

## Plot Model 8
p8 <- plot_model(model8,
                 type = "pred",
                 terms = c("risk[all]", "ticket_available")) +
  theme_bw() +
  
  labs(
    color = "Covered by student ticket",
    y = "Probability of moving",
    x = "Risk Attitute Towards Moving",
    title = NULL
  ) +
  scale_colour_manual(
    labels = c("No", "Yes"),
    values = c("FALSE" = "#ff9922", "TRUE" = "#002244")
  ) +
  theme(legend.position = "bottom",
        plot.margin = margin(0, 50, 0, 0))

p8

ggsave(
  "./Output/Figures/Model_8_Predicted.png",
  width = 16,
  height = 12,
  units = "cm",
  dpi = 600
)


##########
# Step 5 : Instead of running a triple regression, we split
#                the sample in the 4 quartiles based on commuting times
#########


commuting_quartiles <- quantile(fin$commuting_time, c(0.25, 0.5, 0.75))


fin_near <- fin[fin$commuting_time < commuting_quartiles[1], ]
fin_middle <- fin[fin$commuting_time < commuting_quartiles[2] &
                    fin$commuting_time >= commuting_quartiles[1], ]
fin_far <-  fin[fin$commuting_time < commuting_quartiles[3] &
                  fin$commuting_time >= commuting_quartiles[2], ]

commuting_quartiles


# TOTAL BUDGET Interaction with Student ticket
model6a  <-
  glm(
    move ~ ticket_available * total_budget + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_near,
    family = "binomial"
  )
model6b  <-
  glm(
    move ~ ticket_available * total_budget + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_middle,
    family = "binomial"
  )
model6c  <-
  glm(
    move ~ ticket_available * total_budget + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_far,
    family = "binomial"
  )


# ACADEMIC PARENTS Interaction with Student ticket
model7a  <-
  glm(
    move ~  ticket_available * academic_house + commuting_time +  total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_near,
    family = "binomial"
  )

model7b  <-
  glm(
    move ~  ticket_available  * academic_house + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_middle,
    family = "binomial"
  )
model7c  <-
  glm(
    move ~  ticket_available * academic_house + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_far,
    family = "binomial"
  )

# RISK Interaction with Student ticket

model8a  <-
  glm(
    move ~ ticket_available * risk + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_near,
    family = "binomial"
  )

model8b  <-
  glm(
    move ~ ticket_available * risk + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_middle,
    family = "binomial"
  )


model8c  <-
  glm(
    move ~ ticket_available * risk + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age,
    data = fin_far,
    family = "binomial"
  )

# Create List of all models
modellist <-
  list(model6a,
       model6b,
       model6c,
       model7a,
       model7b,
       model7c,
       model8a,
       model8b,
       model8c)

# Print Regression Results
export_summs(
  modellist,
  model.names = c(
    "Model 6a",
    "Model 6b",
    "Model 6c",
    "Model 7a",
    "Model 7b",
    "Model 7c",
    "Model 8a",
    "Model 8b",
    "Model 8c"
  ),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
) # %>% quick_html(file="table8.html")

# Compute Test Statistics
summarytests(modellist)

# Compute AME
models <- lapply(modellist, FUN = margins)

# Print AME REsults
export_summs(
  models,
  model.names = c(
    "Model 6a",
    "Model 6b",
    "Model 6c",
    "Model 7a",
    "Model 7b",
    "Model 7c",
    "Model 8a",
    "Model 8b",
    "Model 8c"
  ),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
) # %>% quick_html(file="table4.html")

###################
# Create Plots for Predicted Probabilities for Budget
###################


######### PLOT Model 6 for middle distances.
plot_model(
  model6b,
  type = "pred",
  terms = c("total_budget [all]", "ticket_available"),
  ci.lvl = 0.95
)
theme_bw() +
  labs(
    x = "Total Budget \n (in 100€)",
    y = "Probability of moving",
    color = "Covered by student ticket",
    title = NULL
  ) + xlim(c(0, 15)) +
  scale_colour_manual(
    labels = c("No", "Yes"),
    values = c("FALSE" = "#ff9922", "TRUE" = "#002244")
  ) +  theme(legend.position = "bottom")


ggsave(
  "./Output/Figures/Model_6_Predicted_middle.png",
  width = 12,
  height = 12,
  units = "cm",
  dpi = 600
)


######### PLOT Model 7 for middle distances.
plot_model(model7b,
           type = "pred",
           terms = c("academic_house", "ticket_available")) +
  theme_bw() +
  labs(
    color = "Covered by student ticket",
    y = "Probability of moving",
    x = "At least one Parent Academic \n (Yes/No)",
    title = NULL
  ) +
  scale_colour_manual(
    labels = c("No", "Yes"),
    values = c("FALSE" = "#ff9922", "TRUE" = "#002244")
  ) +  theme(legend.position = "bottom")


ggsave(
  "./Output/Figures/Model_7_predicted_middle.png",
  width = 12,
  height = 12,
  units = "cm",
  dpi = 600
)


##########
# Step 6: H2 Really big triple interactions
###########

model6X  <-
  glm(
    move ~ commuting_time * ticket_available * total_budget  + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin,
    family = "binomial"
  )

model7X  <-
  glm(
    move ~ commuting_time * ticket_available * academic_house  + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin,
    family = "binomial"
  )


model8X  <-
  glm(
    move ~ commuting_time * ticket_available * risk  + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin,
    family = "binomial"
  )

models <- list(model6X, model7X, model8X)


# Print Regression Estimates
export_summs(
  models,
  model.names = c("Model 6 - FULL", "Model 7 - FULL", "Model 8 - FULL"),
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

# Compute Test-Statistics
summarytests(models)
