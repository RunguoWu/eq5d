rm(list = ls())

library(tidyverse)
library(mice)
library(miceadds)
library(parallel)
library(matrixStats)

# data ----
h03040611 <- readRDS("D:/eq5d/data/h03040611.rds")
h17 <- readRDS("D:/eq5d/data/h17.rds")

# check missing
# p_missing <- unlist(lapply(h03040611, function(x) sum(is.na(x))))/nrow(h03040611)
# sort(p_missing[p_missing > 0], decreasing = TRUE)

# select variable for multiple imputation function

h03040611_imp <- h03040611 %>% select(age, male, smk, ethn, bmi, trtbp, imd, edu, marital, mental, cancer, mi_0_1, mi_1_inf, stroke_0_1, stroke_1_inf, crv_0_1, crv_1_inf, dm_0_10_inf, angina, othcvd, mobility, selfcare, usualact, pain, anxiety, year)

# h17_imp <- h17 %>% select(age, male, smk, ethn, bmi, trtbp, imd, edu, marital, mental, cancer, mi_0_1, mi_1_inf, stroke_0_1, stroke_1_inf, crv_0_1, crv_1_inf, dm_0_10_inf, angina, othcvd, mobility, selfcare, usualact, pain, anxiety)


# adjust parametres ----
imp <- mice(h03040611_imp, maxit = 0)

# imp <- mice(h17_imp, maxit = 0)

predM = imp$predictorMatrix
meth = imp$method


# change the distribution for imputing eq5d measure to ordered 
meth[c("mobility", "selfcare", "usualact", "pain", "anxiety")]="polr"

# imputation ----
ptm <- proc.time()
imp <- parlmice(h03040611_imp, m = 20, maxit = 20, 
                n.core = 4, n.imp.core = 5,
                cluster.seed = 1234,
                predictorMatrix = predM, method = meth, print = T)

print(proc.time() - ptm)
print(Sys.time())

# fit model on imputed data
setwd("D:/eq5d/data")

implist <- miceadds::mids2datlist(imp)

save(imp, file = "h03040611_imp.RData")
save(implist, file = "h03040611_implist.RData")

# save(imp, file = "h17_imp.RData")
# save(implist, file = "h17_implist.RData")


# post imputation recoding----
# generate eq-5d index
# function calculate eq5d

get_index <- function(MO, SC, UA, PD, AD) {
  library(eq5d)
  score <- data.frame(MO=MO, SC=SC, UA=UA, PD=PD, AD=AD)
  eq5d_index <- eq5d(score, country = "UK", version = "3L", type = "TTO", ignore.incomplete = T)
  return(eq5d_index)
}

# get_index <- function(MO, SC, UA, PD, AD) {
#   library(eq5d)
#   score <- data.frame(MO=MO, SC=SC, UA=UA, PD=PD, AD=AD)
#   eq5d_index <- eq5d(score, country = "UK", version = "5L", type = "CW", ignore.incomplete = T)
#   return(eq5d_index)
# }


for(i in c(1:20)) {
  implist[[i]]$eq5d_index <- 
    get_index(MO=as.numeric(implist[[i]]$mobility),
              SC=as.numeric(implist[[i]]$selfcare),
              UA=as.numeric(implist[[i]]$usualact),
              PD=as.numeric(implist[[i]]$pain),
              AD=as.numeric(implist[[i]]$anxiety))
  
}

# BMI categories
for (i in 1:20) {
  implist[[i]] <- implist[[i]] %>% 
    mutate(bmi_cat = case_when(bmi<18.5 ~ "<18.5",
                               bmi>=18.5 & bmi<25 ~ "18.5-25",
                               bmi>=25 & bmi<30 ~ "25-30",
                               bmi>=30 & bmi<35 ~ "30-35",
                               bmi>=35 & bmi<40 ~ "35-40",
                               bmi>=40 ~ "40+"))
  
  implist[[i]]$bmi_cat <- as.factor(implist[[i]]$bmi_cat)
  implist[[i]]$bmi_cat <- relevel(implist[[i]]$bmi_cat, ref = "18.5-25")
}

# age categories----
for (i in 1:20) {
  implist[[i]]$age5 <- NA
  implist[[i]]$age5[implist[[i]]$age>=30 & implist[[i]]$age<35] <- "30-34"
  implist[[i]]$age5[implist[[i]]$age>=35 & implist[[i]]$age<40] <- "35-39"
  implist[[i]]$age5[implist[[i]]$age>=40 & implist[[i]]$age<45] <- "40-44"
  implist[[i]]$age5[implist[[i]]$age>=45 & implist[[i]]$age<50] <- "45-49"
  implist[[i]]$age5[implist[[i]]$age>=50 & implist[[i]]$age<55] <- "50-54"
  implist[[i]]$age5[implist[[i]]$age>=55 & implist[[i]]$age<60] <- "55-59"
  implist[[i]]$age5[implist[[i]]$age>=60 & implist[[i]]$age<65] <- "60-64"
  implist[[i]]$age5[implist[[i]]$age>=65 & implist[[i]]$age<70] <- "65-69"
  implist[[i]]$age5[implist[[i]]$age>=70 & implist[[i]]$age<75] <- "70-74"
  implist[[i]]$age5[implist[[i]]$age>=75 & implist[[i]]$age<80] <- "75-79"
  implist[[i]]$age5[implist[[i]]$age>=80 & implist[[i]]$age<85] <- "80-84"
  implist[[i]]$age5[implist[[i]]$age>=85 & implist[[i]]$age<90] <- "85-89"
  implist[[i]]$age5[implist[[i]]$age>=90] <- "90+"
  
  implist[[i]]$age5 <- as.factor(implist[[i]]$age5)
}

# linear age centred at 60 divided by 10

for (i in 1:20) {
  implist[[i]]$CurrAge_cent <- (implist[[i]]$age-60)/10
}

# CVD history----
for (i in 1:20) {

  implist[[i]]$cvd_num <- as.numeric(implist[[i]]$mi_1_inf)-1 + as.numeric(implist[[i]]$stroke_1_inf)-1 + as.numeric(implist[[i]]$angina)-1
  
  implist[[i]] <- implist[[i]] %>% 
    mutate(cvd_hist = case_when(cvd_num==0 ~ "None",
                                cvd_num==1 & angina==1 ~ "Angina ever only",
                                cvd_num==1 & mi_1_inf==1 ~ "MI 1yr+ only",
                                cvd_num==1 & stroke_1_inf==1 ~ "stroke 1yr+ only",
                                cvd_num>1 ~ "Two or more"))
  
  implist[[i]]$cvd_hist <- as.factor(implist[[i]]$cvd_hist)
  implist[[i]]$cvd_hist <- relevel(implist[[i]]$cvd_hist, ref = "None")
}


saveRDS(implist, file="h17_implist_rev.rds")

saveRDS(implist, file="h03040611_implist_rev.rds")
