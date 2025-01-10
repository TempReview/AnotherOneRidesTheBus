###############################################
### Step 0: Setup
###############################################

#rm(list=ls())
setwd("C:/Users/ra000016/ownCloud/Home/Mesaras")
options(warn = -1)

load(file="DataForAnalysis.Rdata")

library(ggplot2)
library(dplyr)
library(haven)
library(lmtest)
library(aod)
library(jtools)
library(huxtable)
library(sjPlot)
library(margins)

quantiles_budget <- quantile(fin$total_budget,probs= seq(0,1,0.1),na.rm=TRUE)
budget_string <- paste0("total_budget [",quantiles_budget[3],", ",quantiles_budget[6],
                        ", ",quantiles_budget[10],"]")          


########################################
# Step 1: H1 If a student ticket is avaialbe, the likelihood to move to the place of
# study versus commuting is lower
##########################################

model1 <- glm(move ~ commuting_time + ticket_available, data=fin, family="binomial")
model2 <- glm(move ~  commuting_time + ticket_available + total_budget,
               data=fin, family="binomial")
## Add individual controls
model3 <- glm(move ~ commuting_time + total_budget + ticket_available + 
                risk+ pref_uni + single_child + female + partner +
                academic_house , data=fin, family="binomial")
## Add regional controls
model4 <- glm(move ~ commuting_time + total_budget + ticket_available + risk + pref_uni +
                single_child + female + partner + academic_house + better_income + better_cinemas +
                better_density, data=fin, family="binomial")

## Calculate alternative set of models
model2a <- glm(move ~  commuting_time + ticket_available,
               data=fin_bafoeg, family="binomial")
model2b <- glm(move ~  commuting_time + ticket_available + bafoeg_indicator,
               data=fin_bafoeg, family="binomial")
model2c <-  glm(move ~  commuting_time + total_budget + ticket_available +
                  bafoeg_indicator, data=fin, family="binomial")
model2d <- glm(move ~  commuting_time +  total_budget  + bafoeg_share
               + ticket_available , data=fin_bafoeg, family="binomial")
model2e <- glm(move ~    commuting_time +  total_budget
               + bafoeg_amount + ticket_available , data=fin_bafoeg, family="binomial")
model2f <- glm(move ~ commuting_time +  budget_without_bafoeg  + 
                 bafoeg_amount + ticket_available , data=fin_bafoeg, family="binomial")

# Create output tables
##########################################

# Assign approrpiate names
coef_keys <-  c(
  "Commuting Time \n (in Minutes)" = "commuting_time",
  "Student Ticket \n (Yes/No) " = "ticket_availableTRUE",
  "Total Budget \n (in 100???)" = "total_budget",
  "Risk Aversion for moving" = "risk",
  "Preferred Uni \n (Yes/No)" = "pref_uniTRUE",
  "Female \n (Yes/No)" = "femaleTRUE",
  "Partnership \n (Yes/No)" = "partnerTRUE",
  "Single Child \n (Yes/No)" = "single_childTRUE",
  "At least one Parent Academic \n (Yes/No)" = "academic_houseTRUE",
  "Uni region richer \n (Yes/No)" = "better_incomeTRUE",
  "Uni region denser  \n (Yes/No) " = "better_densityTRUE",
  "Uni region more cinemas \n (Yes/No)" = "better_cinemasTRUE"
  )

coef_keys_bafoeg <-  c(
  "Commuting Time \n (in Minutes)" = "commuting_time",
  "Student Ticket \n (Yes/No) " = "ticket_availableTRUE",
  "Total Budget \n (in 100???)" = "total_budget",
  "Bafoeg Available \n (Yes/No)" = "bafoeg_indicatorTRUE",
  "Bafoeg Share \n (in %)" = "bafoeg_share",
  "Bafoeg Amaount \n (in 100 ???)" = "bafoeg_amount",
  "Budget without Bafoeg \n (in 100 ???)" = "budget_without_bafoeg"
)

### Complete table for Appendix as Odds Ratio
export_summs(model1,model2,model3,model4,
             model.names = c("Model 1","Model 2" ,"Model 3", "Model 4"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             coefs= c( "(Intercept)" = "(Intercept)", coef_keys),
             number_format = "%.3f") # %>% quick_html(file="table1.html")

### Complete table for Appendix as Odds Ratio
export_summs(model2a,model2b,model2c,model2d,model2e,model2f,
             model.names = c("Model 2(a)", "Model 2(b)", "Model 2(c)", "Model 2(d)", "Model 2(e)", "Model 2(f)"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             coefs= c( "(Intercept)" = "(Intercept)", coef_keys_bafoeg),
             number_format = "%.3f") # %>% quick_html(file="table1.html")



### Re-compute as Average Marginal Effects
models <- list(model1,model2,model3,model4)
models <- lapply(models, FUN=margins)
export_summs(models,
             model.names = c("Model 1","Model 2" ,"Model 3", "Model 4"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             coefs= coef_keys,
             number_format = "%.3f") # %>% quick_html(file="table2.html")
 
models <- list(model2a,model2b,model2c,model2d,model2e,model2f)
models <- lapply(models, FUN=margins)
export_summs(models,
             model.names = c("Model 2(a)", "Model 2(b)", "Model 2(c)", "Model 2(d)", "Model 2(e)", "Model 2(f)"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             coefs= coef_keys_bafoeg,
             number_format = "%.3f") # %>% quick_html(file="table2.html")
           
### Create  Plots

#Switch Names and Values for Printing             
coef_keys2 <- names(coef_keys)
names(coef_keys2) <- coef_keys

### Create Plot for Predicted Probabilities
plot_model(model4, type="pred", terms=c("commuting_time [all]","ticket_available")) +
  theme_minimal() +
  labs(color="Covered by student ticket", y="Probability of moving", x="Commuting Time", title = "Predicted probabilities\n  to Model 4") +
  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +
  xlim(c(0,200)) + theme(legend.position="bottom")
ggsave("./Output/Figures/Model_4_Predicted.png", width= 12, height=12, units="cm", dpi=600)     


########################################
# Step 2: H2 The effect of the student tickets on the likelihood
# to move is reduced with increasing commuting time
##########################################

model5  <- glm(move ~ commuting_time * ticket_available  + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin, family="binomial")

# Assign approrpiate names
coef_keys <-  c(
  "Commuting Time \n (in Minutes)" = "commuting_time",
  "Student Ticket \n (Yes/No) " = "ticket_availableTRUE",
  "Total Budget \n (in 100???)" = "total_budget",
  "Risk Aversion for moving" = "risk",
  "Preferred Uni \n (Yes/No)" = "pref_uniTRUE",
  "Female \n (Yes/No)" = "femaleTRUE",
  "Partnership \n (Yes/No)" = "partnerTRUE",
  "Single Child \n (Yes/No)" = "single_childTRUE",
  "At least one Parent Academic \n (Yes/No)" = "academic_houseTRUE",
  "Uni region richer \n (Yes/No)" = "better_incomeTRUE",
  "Uni region denser  \n (Yes/No) " = "better_densityTRUE",
  "Uni region more cinemas \n (Yes/No)" = "better_cinemasTRUE"
)

coef_keys_inter <- c(coef_keys,  "Commuting Time \n x Ticket Available" = "commuting_time:ticket_availableTRUE")
### Complete table for Appendix as Odds Ratio
export_summs(model5,
             model.names = c("Model 5"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             coefs= c( "(Intercept)" = "(Intercept)", coef_keys_inter),
             number_format = "%.3f") # %>% quick_html(file="table3.html")


#### Re-compute as Average Marginal Effects
models <- list(model5)
models <- lapply(models, FUN=margins)

export_summs(models,
             model.names = c("Model 5"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             coefs= c( coef_keys),
             number_format = "%.3f") # %>% quick_html(file="table4.html")

### Create  Plots
coef_keys2 <- names(coef_keys_inter)
names(coef_keys2) <- coef_keys_inter

### Create Plot for Predicted Probabilities
plot_model(model5, type="pred", terms=c("commuting_time [all]","ticket_available")) +
  theme_minimal() +
  labs(color="Covered by student ticket", y="Probability of moving", x="Commuting Time", title = "Predicted probabilities\n  to Model 5") +
  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +
  xlim(c(0,200)) + theme(legend.position="bottom")

ggsave("./Output/Figures/Model_5_Predicted.png", width= 12, height=12, units="cm", dpi=600)     




########################################
# Step 3a - 3e : H3 The effect of the student tickets i
# s higher for risk averse/ academic/ poor students ...
########################################

model6  <- glm(move ~ commuting_time * ticket_available * total_budget + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin, family="binomial")
model7  <- glm(move ~ commuting_time * ticket_available * risk + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin, family="binomial")
model8  <- glm(move ~ commuting_time * ticket_available * female + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin, family="binomial")
model9  <- glm(move ~ commuting_time * ticket_available * single_child + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin, family="binomial")
model10  <- glm(move ~ commuting_time * ticket_available *academic_house + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin, family="binomial")


### Complete table for Appendix as Odds Ratio
export_summs(model6,model7,model8,model9,model10,
             model.names = c("Model 6","Model 7", "Model8", "Model9", "Model10"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             number_format = "%.3f") # %>% quick_html(file="table5.html")


#### Re-compute as Average Marginal Effects
models <- list(model6,model7,model8,model9,model10)
models <- lapply(models, FUN=margins)

export_summs(models,
             model.names = c("Model 6","Model 7", "Model8", "Model9", "Model10"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             coefs= c( coef_keys),
             number_format = "%.3f") # %>% quick_html(file="table6.html")

### Create  Plots
coef_keys2 <- names(coef_keys)
names(coef_keys2) <- coef_keys

### Create Plot for Predicted Probabilities
plot_model(model6, type="pred", 
           terms=c("commuting_time [all]","ticket_available",budget_string)) +
  theme_minimal() +
  labs(color="Covered by student ticket", y="Probability of moving", x="Commuting Time", title = "Predicted probabilities  to Model 6") +
  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +
  xlim(c(0,200)) + theme(legend.position="bottom")
ggsave("./Output/Figures/Model_6_Predicted.png", width= 24, height=12, units="cm", dpi=600)     

plot_model(model7, type="pred", 
           terms=c("commuting_time [all]", "ticket_available", "risk[1,4,7]")) +
  theme_minimal() +
  labs(color="Covered by student ticket", y="Probability of moving", x="Commuting Time", title = "ABCDEFGHIJKLMNOPQRSTUV abcdefghijklmnopqrstuvw") +
  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +
  xlim(c(0,200)) + theme(legend.position="bottom")
ggsave("./Output/Figures/Model_7_Predicted.png", width= 24, height=12, units="cm", dpi=600)       

plot_model(model7, type="pred", 
           terms=c("commuting_time [all]","risk[1,4,7]", "ticket_available")) +
  theme_minimal() +
  labs(color="Covered by student ticket", y="Probability of moving", x="Commuting Time", title = "Alternative Graphical Export") +
#  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +
  xlim(c(0,200)) + theme(legend.position="bottom")
ggsave("./Output/Figures/Model_7a_Predicted.png", width= 24, height=12, units="cm", dpi=600)  


plot_model(model8, type="pred", 
           terms=c("commuting_time [all]","female","ticket_available")) +
  theme_minimal() +
  labs(color="Covered by student ticket", y="Probability of moving", x="Commuting Time", title = "Predicted probabilities  to Model 8") +
  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +
  xlim(c(0,200)) + theme(legend.position="bottom")
ggsave("./Output/Figures/Model_8_Predicted.png", width= 24, height=12, units="cm", dpi=600)      

plot_model(model9, type="pred", 
           terms=c("commuting_time [all]","single_child","ticket_available")) +
  theme_minimal() +
  labs(color="Covered by student ticket", y="Probability of moving", x="Commuting Time", title = "Predicted probabilities  to Model 9") +
  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +
  xlim(c(0,200)) + theme(legend.position="bottom")
ggsave("./Output/Figures/Model_9_Predicted.png", width= 24, height=12, units="cm", dpi=600)   

plot_model(model10, type="pred", 
           terms=c("commuting_time [all]","academic_house","ticket_available")) +
  theme_minimal() +
  labs(color="Covered by student ticket", y="Probability of moving", x="Commuting Time", title = "Predicted probabilities  to Model 10") +
  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +  scale_colour_manual(labels=c("No","Yes"),values=c("FALSE"="red","TRUE"="darkblue")) +
  xlim(c(0,200)) + theme(legend.position="bottom") + theme(legend.position="bottom")
ggsave("./Output/Figures/Model_10_Predicted.png", width= 24, height=12, units="cm", dpi=600)   


########################################
# Step 4a - 4e : Same as above but with no triple interaction
########################################
fin_near <- fin[fin$commuting_time<60] 
fin_far <-  fin[fin$commuting_time>=60] 

model6a  <- glm(move ~ ticket_available * total_budget + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin_near, family="binomial")
model7a  <- glm(move ~ ticket_available * risk + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin_near, family="binomial")
model8a  <- glm(move ~  ticket_available * female + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin_near, family="binomial")
model9a  <- glm(move ~  ticket_available * single_child + total_budget  + risk + pref_uni +
                 single_child + female + partner + academic_house + better_income + better_cinemas +
                 better_density , data=fin_near, family="binomial")
model10a  <- glm(move ~ ticket_available *academic_house + total_budget  + risk + pref_uni +
                  single_child + female + partner + academic_house + better_income + better_cinemas +
                  better_density , data=fin_near, family="binomial")
model6b  <- glm(move ~ ticket_available * total_budget + total_budget  + risk + pref_uni +
                  single_child + female + partner + academic_house + better_income + better_cinemas +
                  better_density , data=fin_far, family="binomial")
model7b  <- glm(move ~ ticket_available * risk + total_budget  + risk + pref_uni +
                  single_child + female + partner + academic_house + better_income + better_cinemas +
                  better_density , data=fin_far, family="binomial")
model8b  <- glm(move ~  ticket_available * female + total_budget  + risk + pref_uni +
                  single_child + female + partner + academic_house + better_income + better_cinemas +
                  better_density , data=fin_far, family="binomial")
model9b  <- glm(move ~  ticket_available * single_child + total_budget  + risk + pref_uni +
                  single_child + female + partner + academic_house + better_income + better_cinemas +
                  better_density , data=fin_far, family="binomial")
model10b  <- glm(move ~ ticket_available *academic_house + total_budget  + risk + pref_uni +
                   single_child + female + partner + academic_house + better_income + better_cinemas +
                   better_density , data=fin_far, family="binomial")

export_summs(model6a,model6b, model7a,model7b, model8a,model8b, 
             model.names = c("Model 6a","Model 6b","Model 7a","Model 7b",
                             "Model 8a","Model 8b"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             number_format = "%.3f") # %>% quick_html(file="table8.html")

export_summs(model9a,model9b, model10a,model10b, 
             model.names = c("Model 9a","Model 9b","Model 10a","Model 10b"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             number_format = "%.3f") # %>% quick_html(file="table8.html")

#produce_log_file("./Data_Analysis.R", log_file = "./Output/Logs/Data_Analysis.log")
