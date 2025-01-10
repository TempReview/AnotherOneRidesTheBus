

###############################################
### Step 0: Setup
###############################################

#rm(list=ls())
setwd("C:/Users/ra000016/ownCloud/Home/Mesaras")
options(warn = -1)
load(file="after_analysis.Rdata")

library(lmtest)
library(ResourceSelection)
library(rcompanion)
library(fmsb)
library(performance)



###############################################
### Step 1:  Add missing depiction from marginal effects from last time
###############################################
models <- list(model6a,model6b, model7a,model7b, model8a,model8b)
models <- lapply(models, FUN=margins)

export_summs(models,
             model.names = c("Model 6a","Model 6b","Model 7a","Model 7b",
                             "Model 8a","Model 8b"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             number_format = "%.3f") # %>% quick_html(file="table8.html")

models <- list(model9a,model9b, model10a,model10b)
models <- lapply(models, FUN=margins)

export_summs(models,
             model.names = c("Model 9a","Model 9b","Model 10a","Model 10b"),
             stars= c(`***`= 0.001, `**` = 0.01, `*` = 0.05, `^`= 0.1),
             number_format = "%.3f") # %>% quick_html(file="table8.html")


###############################################
### Step 2:  Run model diagnostics for nested models
###############################################

#LR Test for nested Models
lrtest(model2,model1)
lrtest(model3,model2)
lrtest(model4,model3)
lrtest(model5,model4)
lrtest(model6,model5)
lrtest(model7,model5)
lrtest(model8,model5)
lrtest(model9,model5)
lrtest(model10,model5)

###############################################
### Step 3: Tests for each model
###############################################

modellist <- list("Model1"=model1,"Model2"=model2 , "Model3"=model3,"Model4"=model4,
                  "Model5"=model5,"Model6"=model6 , "Model7"=model7,"Model8"=model8,
                  "Model9"=model9,"Model10"=model10 , "Model2a"=model2a,"Model2b"=model2b,
                  "Model2c"=model2c,"Model2d"=model2d , "Model2e"=model2e,"Model2f"=model2f,
                  "Model6a"=model6a, "Model7a"=model7a,"Model8a"=model8a,
                  "Model9a"=model9a,"Model10a"=model10 , "Model6b"=model6b, 
                  "Model7b"=model7b,"Model8b"=model8b,
                  "Model9b"=model9b,"Model10b"=model10b 
                  )

#Compute for every model the LR-Test, Wald-Test, Hosmer-Lemeshow Test, 
# As well as the Cos-Snell and Nagelkerke Pseudo R2 Statistics
for(i in 1:length(modellist))
{
  print("----------")
  print("----------")
  print(names(modellist)[i])
  model <- modellist[[i]]
  print("----------")
  print("----------")
  
  print(lrtest(model))
  print(waldtest(model))
  print(performance_hosmer(model, n_bins = 3))
  print(r2_coxsnell(model))
  print(r2_nagelkerke(model))
}
