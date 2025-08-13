

###############################################
### Step 0: Setup
###############################################

# Clear loaded Data and reload
rm(list = ls())
setwd("C:/Users/ra000016/ownCloud/Home/Mesaras")
options(warn = -1)
load(file = "DataForAnalysis.Rdata")


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


# Add helping function for printing test statistics as a table
# Takes a list of models an for each model prints R2 values and 3 tests.

summarytests <- function(modellist)
   {
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

commuting_quartiles <- quantile(fin$commuting_time, c(0.25, 0.5, 0.75))

########################################
# Robustness Check 1: Only long term
##########################################

fin_robust <- fin[fin$final_acc == TRUE, ]
fin_robust_middle <-
  fin[fin$commuting_time < commuting_quartiles[2] &
        fin$commuting_time >= commuting_quartiles[1], ]
fin_robust_middle <- fin_robust_middle[fin_robust_middle$final_acc == TRUE, ]

model4 <-
  glm(
    move ~ commuting_time + total_budget + ticket_available + risk + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age,
    data = fin_robust,
    family = "binomial"
  )

model5  <-
  glm(
    move ~ commuting_time * ticket_available  + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model6  <-
  glm(
    move ~ total_budget * ticket_available  + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house  +  better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model6b  <-
  glm(
    move ~  total_budget * ticket_available  + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_robust_middle,
    family = "binomial"
  )

model7  <-
  glm(
    move ~ ticket_available  * academic_house   + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house  +  better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model7b  <-
  glm(
    move ~  ticket_available  * academic_house + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_robust_middle,
    family = "binomial"
  )

models <- list(model4, model5, model6, model6b, model7, model7b)

### Print table as Odds Ratio
export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

summarytests(models)

# Compute AMEs
models <- lapply(models, FUN = margins)

export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)


########################################
# Robustness Check 2: Closest to their home place
##########################################

fin_robust <- fin[fin$closest_uni == TRUE]
fin_robust_middle <-   fin[fin$commuting_time < commuting_quartiles[2] &
                             fin$commuting_time >= commuting_quartiles[1], ]
fin_robust_middle <- fin_robust_middle[fin_robust_middle$closest_uni == TRUE, ]

model4 <-
  glm(
    move ~ commuting_time + total_budget + ticket_available + risk + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age,
    data = fin_robust,
    family = "binomial"
  )

model5  <-
  glm(
    move ~ commuting_time * ticket_available  + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model6  <-     glm(
  move ~ total_budget * ticket_available  + commuting_time + total_budget  + risk + pref_uni +
    single_child + female + partner + academic_house  +  better_migration +
    better_age ,
  data = fin_robust,
  family = "binomial"
)

model6b  <-
  glm(
    move ~  total_budget * ticket_available  + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_robust_middle,
    family = "binomial"
  )

model7  <-
  glm(
    move ~ ticket_available  * academic_house   + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house  +  better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model7b  <-
  glm(
    move ~  ticket_available  * academic_house + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_robust_middle,
    family = "binomial"
  )

models <- list(model4, model5, model6, model6b, model7, model7b)

### Print table as Odds Ratio
export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)


summarytests(models)


# Compute AMEs
models <- lapply(models, FUN = margins)

export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

########################################
# Robustness Check 3: Comuting times to physical tistance
##########################################

fin_robust <- fin
dist_quantile <-
  quantile(fin$commuting_distance, c(0.25, 0.5, 0.75))
fin_robust_middle <-
  fin_robust[fin_robust$commuting_distance < dist_quantile[2] &
               fin_robust$commuting_distance >= dist_quantile[1], ]

model4 <-
  glm(
    move ~ commuting_distance + total_budget + ticket_available + risk + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age,
    data = fin_robust,
    family = "binomial"
  )

model5  <-
  glm(
    move ~ commuting_distance * ticket_available  + total_budget  + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model6 <- glm(
  move ~ total_budget * ticket_available + commuting_time + total_budget + risk + pref_uni +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust,
  family = "binomial"
)

model6b <- glm(
  move ~ total_budget * ticket_available + commuting_time + total_budget + risk + pref_uni +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust_middle,
  family = "binomial"
)

model7 <- glm(
  move ~ ticket_available * academic_house + commuting_time + total_budget + risk + pref_uni +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust,
  family = "binomial"
)

model7b <- glm(
  move ~ ticket_available * academic_house + commuting_time + total_budget + risk + pref_uni +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust_middle,
  family = "binomial"
)


models <- list(model4, model5, model6, model6b, model7, model7b)

### Print table as Odds Ratio
export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

summarytests(models)

# Compute AMEs
models <- lapply(models, FUN = margins)

export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

########################################
# Robustness Check 4: Not Preferered Unis
##########################################

fin_robust <- fin[fin$pref_uni == FALSE]
fin_robust_middle <-
  fin[fin$commuting_time < commuting_quartiles[2] &
        fin$commuting_time >= commuting_quartiles[1], ]

fin_robust_middle <- fin_robust_middle[fin_robust_middle$pref_uni == FALSE, ]

model4 <-
  glm(
    move ~ commuting_time + total_budget + ticket_available + risk  +
      single_child + female + partner + academic_house + better_migration +
      better_age,
    data = fin_robust,
    family = "binomial"
  )

model5  <-
  glm(
    move ~ commuting_time * ticket_available  + total_budget  + risk  +
      single_child + female + partner + academic_house + better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model6 <- glm(
  move ~ total_budget * ticket_available + commuting_time + total_budget + risk +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust,
  family = "binomial"
)

model6b <- glm(
  move ~ total_budget * ticket_available + commuting_time + total_budget + risk +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust_middle,
  family = "binomial"
)

model7 <- glm(
  move ~ academic_house * ticket_available + commuting_time + total_budget + risk +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust,
  family = "binomial"
)

model7b <- glm(
  move ~ ticket_available * academic_house + commuting_time + total_budget + risk +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust_middle,
  family = "binomial"
)

models <- list(model4, model5, model6, model6b, model7, model7b)

### Print table as Odds Ratio
export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)


summarytests(models)


# Compute AMEs
models <- lapply(models, FUN = margins)

export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)


########################################
# Robustness Check 5: MOving is riskless
##########################################

fin_robust <- fin[fin$risk == 1]
fin_robust_middle <-
  fin[fin$commuting_time < commuting_quartiles[2] &
        fin$commuting_time >= commuting_quartiles[1], ]
fin_robust_middle <- fin_robust_middle[fin_robust_middle$risk == 1, ]

model4 <-
  glm(
    move ~ commuting_time + total_budget + ticket_available  + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age,
    data = fin_robust,
    family = "binomial"
  )

model5  <-
  glm(
    move ~ commuting_time * ticket_available  + total_budget  + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model6 <- glm(
  move ~ total_budget * ticket_available + commuting_time + total_budget + pref_uni +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust,
  family = "binomial"
)

model6b <- glm(
  move ~ total_budget * ticket_available + commuting_time + total_budget + pref_uni +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust_middle,
  family = "binomial"
)

model7 <- glm(
  move ~ ticket_available * academic_house + commuting_time + total_budget + pref_uni +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust,
  family = "binomial"
)

model7b <- glm(
  move ~ ticket_available * academic_house + commuting_time + total_budget + pref_uni +
    single_child + female + partner + academic_house + better_migration +
    better_age,
  data = fin_robust_middle,
  family = "binomial"
)


models <- list(model4, model5, model6, model6b, model7, model7b)

### Print table as Odds Ratio
export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

summarytests(models)

# Compute AMEs
models <- lapply(models, FUN = margins)

export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)


########################################
# Robustness Budget wihtout controlling for parents education
#####################################

fin_robust <- fin
fin_robust_middle <-
  fin[fin$commuting_time < commuting_quartiles[2] &
        fin$commuting_time >= commuting_quartiles[1], ]


model4 <-
  glm(
    move ~ commuting_time + total_budget + ticket_available + risk + pref_uni +
      single_child + female + partner + better_migration +
      better_age,
    data = fin_robust,
    family = "binomial"
  )

model5  <-
  glm(
    move ~ commuting_time * ticket_available  + total_budget  + risk + pref_uni +
      single_child + female + partner + better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model6  <-
  glm(
    move ~ total_budget * ticket_available  + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner +  better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model6b  <-
  glm(
    move ~  total_budget * ticket_available  + commuting_time + total_budget  + risk + pref_uni +
      single_child + female + partner +  better_migration +
      better_age ,
    data = fin_robust_middle,
    family = "binomial"
  )
models <- list(model4, model5, model6, model6b)

### Print table as Odds Ratio
export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)


summarytests(models)

# Compute AMEs
models <- lapply(models, FUN = margins)

export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

########################################
# Robustness Paranetal Education without Budget
#####################################

fin_robust <- fin
fin_robust_middle <-
  fin_robust[fin_robust$commuting_time < 112 &
               fin_robust$commuting_time >= 64, ]

model4 <-
  glm(
    move ~ commuting_time + ticket_available + risk + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age,
    data = fin_robust,
    family = "binomial"
  )

model5  <-
  glm(
    move ~ commuting_time * ticket_available   + risk + pref_uni +
      single_child + female + partner + academic_house + better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )


model7  <-
  glm(
    move ~ ticket_available  * academic_house   + commuting_time  + risk + pref_uni +
      single_child + female + partner + academic_house  +  better_migration +
      better_age ,
    data = fin_robust,
    family = "binomial"
  )

model7b  <-
  glm(
    move ~  ticket_available  * academic_house + commuting_time + risk + pref_uni +
      single_child + female + partner + academic_house +  better_migration +
      better_age ,
    data = fin_robust_middle,
    family = "binomial"
  )


models <- list(model4, model5, model7, model7b)

### Print table as Odds Ratio
export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)

summarytests(models)

# Compute AMEs
models <- lapply(models, FUN = margins)

export_summs(
  models,
  stars = c(
    `***` = 0.001,
    `**` = 0.01,
    `*` = 0.05,
    `^` = 0.1
  ),
  number_format = "%.3f"
)
