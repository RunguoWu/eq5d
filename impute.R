rm(list = ls())

library(tidyverse)
library(mice)
library(miceadds)

# data ----
ctt <- readRDS("D:/eq5d/data/ctt.rds")

# check missing
# p_missing <- unlist(lapply(ctt, function(x) sum(is.na(x))))/nrow(ctt)
# sort(p_missing[p_missing > 0], decreasing = TRUE)

# select variable for multiple imputation function

ctt_imp <- ctt %>% select(age, male, smk, ethn, bmi, trtbp, imd, edu, marital, mental, cancer, mi_0_1, mi_1_plus, stroke_0_1, stroke_1_plus, crv_0_1, crv_1_plus, dm_0_10_plus, angina, othcvd, mobility, selfcare, usualact, pain, anxiety, year)

# adjust parametres ----
imp <- mice(ctt_imp, maxit = 0)

predM = imp$predictorMatrix
meth = imp$method

# change the distribution for imputing eq5d measure to ordered 
meth[c("mobility", "selfcare", "usualact", "pain", "anxiety")]="polr"

# imputation ----
imp <- mice(ctt_imp, m = 20, maxit = 20, predictorMatrix = predM, method = meth, print = T)

# fit model on imputed data
setwd("D:/eq5d/data")

implist <- miceadds::mids2datlist(imp)


save(imp, file = "imp.RData")
write.mice.imputation(mi.res=imp, name="imp")

# post imputation recoding----
# generate eq-5d index
# function calculate eq5d

get_index <- function(MO, SC, UA, PD, AD) {
  library(eq5d)
  score <- data.frame(MO=MO, SC=SC, UA=UA, PD=PD, AD=AD)
  eq5d_index <- eq5d(score, country = "UK", version = "3L", type = "TTO", ignore.incomplete = T)
  return(eq5d_index)
}

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
  implist[[i]]$age5[implist[[i]]$age>=30 & implist[[i]]$age<35] <- 1
  implist[[i]]$age5[implist[[i]]$age>=35 & implist[[i]]$age<40] <- 2
  implist[[i]]$age5[implist[[i]]$age>=40 & implist[[i]]$age<45] <- 3
  implist[[i]]$age5[implist[[i]]$age>=45 & implist[[i]]$age<50] <- 4
  implist[[i]]$age5[implist[[i]]$age>=50 & implist[[i]]$age<55] <- 5
  implist[[i]]$age5[implist[[i]]$age>=55 & implist[[i]]$age<60] <- 6
  implist[[i]]$age5[implist[[i]]$age>=60 & implist[[i]]$age<65] <- 7
  implist[[i]]$age5[implist[[i]]$age>=65 & implist[[i]]$age<70] <- 8
  implist[[i]]$age5[implist[[i]]$age>=70 & implist[[i]]$age<75] <- 9
  implist[[i]]$age5[implist[[i]]$age>=75 & implist[[i]]$age<80] <- 10
  implist[[i]]$age5[implist[[i]]$age>=80 & implist[[i]]$age<85] <- 11
  implist[[i]]$age5[implist[[i]]$age>=85 & implist[[i]]$age<90] <- 12
  implist[[i]]$age5[implist[[i]]$age>=90] <- 13
  
  implist[[i]]$age5 <- as.factor(implist[[i]]$age5)
  
  levels(implist[[i]]$age5)[1] <- "30-34"
  levels(implist[[i]]$age5)[2] <- "35-39"
  levels(implist[[i]]$age5)[3] <- "40-44"
  levels(implist[[i]]$age5)[4] <- "45-49"
  levels(implist[[i]]$age5)[5] <- "50-54"
  levels(implist[[i]]$age5)[6] <- "55-59"
  levels(implist[[i]]$age5)[7] <- "60-64"
  levels(implist[[i]]$age5)[8] <- "65-69"
  levels(implist[[i]]$age5)[9] <- "70-74"
  levels(implist[[i]]$age5)[10] <- "75-79"
  levels(implist[[i]]$age5)[11] <- "80-84"
  levels(implist[[i]]$age5)[12] <- "85-89"
  levels(implist[[i]]$age5)[13] <- "90+"

}

# linear age centred at 60 divided by 10

for (i in 1:20) {
  implist[[i]]$CurrAge_cent <- (implist[[i]]$age-60)/10
}

# CVD history----
for (i in 1:20) {

  implist[[i]]$cvd_num <- as.numeric(implist[[i]]$mi_1_plus)-1 + as.numeric(implist[[i]]$stroke_1_plus)-1 + as.numeric(implist[[i]]$angina)-1
  
  implist[[i]] <- implist[[i]] %>% 
    mutate(cvd_hist = case_when(cvd_num==0 ~ "None",
                                cvd_num==1 & angina==1 ~ "Angina ever only",
                                cvd_num==1 & mi_1_plus==1 ~ "MI 1yr+ only",
                                cvd_num==1 & stroke_1_plus==1 ~ "stroke 1yr+ only",
                                cvd_num>1 ~ "Two or more"))
  
  implist[[i]]$cvd_hist <- as.factor(implist[[i]]$cvd_hist)
  implist[[i]]$cvd_hist <- relevel(implist[[i]]$cvd_hist, ref = "None")
}


save(implist, file="implist.RData")


