library(tidyverse)

# path
datafile <- "D:/eq5d/data/raw"


# original data -----------------------------------------------------------


# read data
hse03 <- read.table(file = file.path(datafile, "hse03ai.tab"), 
                    header = T,sep = "\t") 
                      
hse04 <- read.table(file = file.path(datafile, "hse04etha.tab"), 
                    header = T,sep = "\t")                 
                      
hse06 <- read.table(file = file.path(datafile, "hse06ai.tab"), 
                    header = T,sep = "\t") 

hse11 <- read.table(file = file.path(datafile, "hse2011ai.tab"), 
                    header = T,sep = "\t") 

hse17 <- read.table(file = file.path(datafile, "hse17i_eul_v1.tab"), 
                    header = T,sep = "\t") 

# only keep cases aged >=30

hse03 <- subset(hse03, age>=30) 

hse04 <- subset(hse04, age>=30)

# because those aged 65+ and grouped in type 2 did not receive CV questionnaire, remove the 1592 cases altogether
hse06 <- subset(hse06, age>=30 & (samptype==1 | (samptype==2 & age<65))) 

hse11 <- subset(hse11, Age>=30) 

hse17 <- subset(hse17, Age35g>=10)


# baseline ----------------------------------------------------------------

# age----

h03 <- data.frame(age=hse03$age)

h04 <- data.frame(age=hse04$age)

h06 <- data.frame(age=hse06$age)

h11 <- data.frame(age=hse11$Age)

h17 <- data.frame(age5=hse17$Age16g5)

h17$age5 <- as.factor(hse17$Age16g5)
levels(h17$age5)[1] <- "30-34"
levels(h17$age5)[2] <- "35-39"
levels(h17$age5)[3] <- "40-44"
levels(h17$age5)[4] <- "45-49"
levels(h17$age5)[5] <- "50-54"
levels(h17$age5)[6] <- "55-59"
levels(h17$age5)[7] <- "60-64"
levels(h17$age5)[8] <- "65-69"
levels(h17$age5)[9] <- "70-74"
levels(h17$age5)[10] <- "75-79"
levels(h17$age5)[11] <- "80-84"
levels(h17$age5)[12] <- "85-89"
levels(h17$age5)[13] <- "90+"

h17 <- h17 %>% mutate(age = case_when(age5 == "30-34" ~ 32,
                                      age5 == "35-39" ~ 37,
                                      age5 == "40-44" ~ 42,
                                      age5 == "45-49" ~ 47,
                                      age5 == "50-54" ~ 52,
                                      age5 == "55-59" ~ 57,
                                      age5 == "60-64" ~ 62,
                                      age5 == "65-69" ~ 67,
                                      age5 == "70-74" ~ 72,
                                      age5 == "75-79" ~ 77,
                                      age5 == "80-84" ~ 82,
                                      age5 == "85-89" ~ 87,
                                      age5 == "90+" ~ 92))



# sex----

h03$male <- ifelse(hse03$sex==1, 1, 0)

h04$male <- ifelse(hse04$sex==1, 1, 0)

h06$male <- ifelse(hse06$sex==1, 1, 0)

h11$male <- ifelse(hse11$Sex==1, 1, 0)

h17$male <- ifelse(hse17$Sex==1, 1, 0)

# smoke----

#2003
h03$cur_smk <- 0
h03$cur_smk[hse03$cignow==1 | hse03$cigarnow==1 | hse03$pipenowa==1] <- 1
h03$cur_smk[hse03$smkevr<0] <- NA

h03$ex_smk <- NA
h03$ex_smk[hse03$smkevr==1 & h03$cur_smk==0] <- 1
h03$ex_smk[hse03$smkevr==2 | h03$cur_smk==1] <- 0

#2004
h04$cur_smk <- 0
h04$cur_smk[hse04$cignow==1 | hse04$cigarnow==1 | hse04$pipenowa==1] <- 1
h04$cur_smk[hse04$smkevr<0] <- NA 

h04$ex_smk <- NA
h04$ex_smk[hse04$smkevr==1 & h04$cur_smk==0] <- 1
h04$ex_smk[hse04$smkevr==2 | h04$cur_smk==1] <- 0

#2006
h06$cur_smk <- 0
h06$cur_smk[hse06$cignow==1 | hse06$cigarnow==1 | hse06$pipenowa==1] <- 1
h06$cur_smk[hse06$smkevr<0] <- NA 

h06$ex_smk <- NA
h06$ex_smk[hse06$smkevr==1 & h06$cur_smk==0] <- 1
h06$ex_smk[hse06$smkevr==2 | h06$cur_smk==1] <- 0

#2011
h11$cur_smk <- 0
h11$cur_smk[hse11$cignow ==1 | hse11$CigarNow ==1 | hse11$PipeNowA ==1] <- 1
h11$cur_smk[hse11$smkevr<0] <- NA 

h11$ex_smk <- NA
h11$ex_smk[hse11$smkevr==1 & h11$cur_smk==0] <- 1
h11$ex_smk[hse11$smkevr==2 | h11$cur_smk==1] <- 0

#2017
h17$cur_smk <- 0
h17$cur_smk[hse17$cignow ==1 | hse17$CigarNow ==1 | hse17$PipeNowA ==1] <- 1
h17$cur_smk[hse17$smkevr<0] <- NA 

h17$ex_smk <- NA
h17$ex_smk[hse17$smkevr==1 & h17$cur_smk==0] <- 1
h17$ex_smk[hse17$smkevr==2 | h17$cur_smk==1] <- 0

# ethnicity----

#2003
h03$ethn <- NA
h03$ethn[hse03$ethnici==1] <- 1 # white
h03$ethn[hse03$ethnici==3 | hse03$ethnici==4] <- 2 # black
h03$ethn[hse03$ethnici==5 | hse03$ethnici==6] <- 3 # asian
h03$ethn[hse03$ethnici==7 | hse03$ethnici==2] <- 5 # others & mixed

# there is concept overlap between asian and others
# revise using another variable
h03$ethn[(h03$ethn==3 | h03$ethn==5) & hse03$allcult1==15] <- 4 # Chinese
h03$ethn[h03$ethn==3 & (hse03$allcult1 %in% c(1:10) | hse03$allcult1 %in% c(16:19))] <- 5 # recode asian not belong to India, Pakistan and Bangladesh to other or mixed
# combine Chinese and others
h03$ethn[h03$ethn == 4 | h03$ethn == 5] <- 4

#2004
h04$ethn <- NA
h04$ethn[hse04$dmethn04 %in% c(7,8)] <- 1 # "white"
h04$ethn[hse04$dmethn04 %in% c(1,2)] <- 2 # "black"
h04$ethn[hse04$dmethn04 %in% c(3,4,5)] <- 3 # "South Asia"
h04$ethn[hse04$dmethn04==6] <- 4 # "Chinese"
h04$ethn[hse04$dmethn04==9] <- 5 # "others 
# combine Chinese and others
h04$ethn[h04$ethn == 4 | h04$ethn == 5] <- 4

#2006
h06$ethn <- NA
h06$ethn[hse06$ethinda==1] <- 1 # "white"
h06$ethn[hse06$ethinda==4] <- 2 # "black"
h06$ethn[hse06$ethinda==3] <- 3 # "Asia"
h06$ethn[hse06$ethinda==5] <- 4 # "Chinese or other"
h06$ethn[hse06$ethinda==2] <- 5 # "Mixed"

# there is concept overlap between asian and others
# revise using another variable
h06$ethn[h06$ethn==4 & hse06$othcult1==0] <- 5 # not Chinese -> Others
h06$ethn[h06$ethn==3 & (hse06$indcult1!=1 & hse06$indcult2!=1 & hse06$indcult3!=1 & hse06$indcult4!=1)] <- 5 # recode asian not belong to India, Pakistan and Bangladesh to other or mixed
# combine Chinese and others
h06$ethn[h06$ethn==4 | h06$ethn==5] <- 4

#2011
h11$ethn <- 4 # "Others/Mixed"
h11$ethn[hse11$Origin %in% c(1:4)] <- 1 # "white"
h11$ethn[hse11$Origin %in% c(14, 15, 16)] <- 2 # "black"
h11$ethn[hse11$Origin %in% c(9, 10, 11)] <- 3 # "South Asia"
h11$ethn[hse11$Origin<0] <- NA 

#2017
h17$ethn <- hse17$Origin2
h17$ethn[h17$ethn %in% 4:5] <- 4 # "mixed/other"
h17$ethn[hse17$Origin2<0] <- NA


# BMI----

h03$bmi <- hse03$bmival
h03$bmi[h03$bmi<0] <- NA

h04$bmi <- hse04$bmival
h04$bmi[h04$bmi<0] <- NA

h06$bmi <- hse06$bmival
h06$bmi[h06$bmi<0] <- NA

h11$bmi <- hse11$bmival
h11$bmi[h11$bmi<0] <- NA

h17$bmi <- hse17$BMIval
h17$bmi[h17$bmi<0] <- NA

# treatment of HBP----
h03$trtbp <- NA
h03$trtbp[hse03$bp1==2 | hse03$medcinbp==2] <- 0
h03$trtbp[hse03$medcinbp==1 & hse03$bp1==1] <- 1

h04$trtbp <- NA
h04$trtbp[hse04$bp1==2 | hse04$medcinbp==2] <- 0
h04$trtbp[hse04$medcinbp==1 & hse04$bp1==1] <- 1

h06$trtbp <- NA
h06$trtbp[hse06$bp1==2 | hse06$medcinbp==2] <- 0
h06$trtbp[hse06$medcinbp==1 & hse06$bp1==1] <- 1

h11$trtbp <- NA
h11$trtbp[hse11$bp1==2 | hse11$medcinbp==2] <- 0
h11$trtbp[hse11$medcinbp==1 & hse11$bp1==1] <- 1

h17$trtbp <- NA
h17$trtbp[hse17$bp1==2 | hse17$MedBP==2] <- 0
h17$trtbp[hse17$MedBP==1 & hse17$bp1==1] <- 1

# IMD----
h03$imd <- hse03$imd2004
h04$imd <- hse04$imd2004
h06$imd <- hse06$imd2004
h11$imd <- hse11$qimd
h17$imd <- hse17$qimd

# education----

for (i in c("03", "04", "06", "11", "17")) {
  x <- get(paste0("h", i)) %>% mutate(edu = get(paste0("hse", i))[["topqual3"]])
  
  x$edu[x$edu %in% c(2,3)] <- 2
  x$edu[x$edu %in% c(4:6)] <- 3
  x$edu[x$edu == 7] <- 4
  x$edu[x$edu<0] <- NA
  
  x$edu <- as.factor(x$edu)
  levels(x$edu)[1] <- "NVQ4/NVQ5/Degree or equiv"
  levels(x$edu)[2] <- "Higher ed below degree or A Level"
  levels(x$edu)[3] <- "O Level, CSE other grade or Foreign"
  levels(x$edu)[4] <- "No qualification"
  
  assign(paste0("h", i), x)
}

# marital status----

h03$marital <- ifelse(hse03$marstatb %in% c(2, 6), "Married, partner or Cohabitees", 
                      ifelse(hse03$marstatb %in% c(1, 3, 4, 5), "living alone", NA))

h04$marital <- ifelse(hse04$marstatb %in% c(2, 6), "Married, partner or Cohabitees", 
                      ifelse(hse04$marstatb %in% c(1, 3, 4, 5), "living alone", NA))

h06$marital <- ifelse(hse06$marstatc %in% c(2,3,7), "Married, partner or Cohabitees", 
                      ifelse(hse06$marstatc %in% c(1, 4, 5, 6), "living alone", NA))
  
h11$marital <- ifelse(hse11$marstatc %in% c(2,3,7), "Married, partner or Cohabitees", 
                      ifelse(hse11$marstatc %in% c(1, 4, 5, 6), "living alone", NA))

h17$marital <- ifelse(hse17$MarStatD %in% c(2,6), "Married, partner or Cohabitees", 
                      ifelse(hse17$MarStatD %in% c(1,3, 4, 5), "living alone", NA))

# mental health----

# use longstanding illness - mental disorder

h03$mental <- hse03$compm3
h03$mental[h03$mental<0] <- NA

h04$mental <- hse04$compm3
h04$mental[h04$mental<0] <- NA

h06$mental <- hse06$compm3
h06$mental[h06$mental<0] <- NA

h11$mental <- hse11$compm3
h11$mental[h11$mental<0] <- NA

h17$mental <- hse17$complst3
h17$mental[h17$mental<0] <- NA



# time-varied diseases ----------------------------------------------------

# cancer----

h03$cancer <- hse03$compm1
h03$cancer[h03$cancer<0] <- NA

h04$cancer <- hse04$compm1
h04$cancer[h04$cancer<0] <- NA

h06$cancer <- hse06$compm1
h06$cancer[h06$cancer<0] <- NA

h11$cancer <- hse11$compm1
h11$cancer[h11$cancer<0] <- NA

h17$cancer <- hse17$complst1
h17$cancer[h17$cancer<0] <- NA

# MI and stroke----

# 2003
# ever
h03$mi <- NA
h03$mi[hse03$heartdef==1] <- 1
h03$mi[hse03$heartdef==2] <- 0

h03$stroke <- NA
h03$stroke[hse03$strodef==1] <- 1
h03$stroke[hse03$strodef==2] <- 0

# 12 months
h03$mi_0_1 <- NA
h03$mi_0_1[hse03$recheart==2 | h03$mi==0] <- 0
h03$mi_0_1[hse03$recheart==1] <- 1

h03$stroke_0_1 <- NA
h03$stroke_0_1[hse03$recstro==2 | h03$stroke==0] <- 0
h03$stroke_0_1[hse03$recstro==1] <- 1

# 1 year ago
age_event <- hse03$ageheart # this is a numeric variable indicating the age when the event happened 
age_event[hse03$ageheart<0] <- NA
dif <- h03$age - age_event
h03$mi_1_inf <- NA
h03$mi_1_inf[h03$mi==1 & dif>1] <- 1
h03$mi_1_inf[h03$mi==0 | (h03$mi==1 & dif<=1)] <- 0

age_event <- hse03$agestro
age_event[hse03$agestro<0] <- NA 
dif <- h03$age - age_event
h03$stroke_1_inf <- NA
h03$stroke_1_inf[h03$stroke==1 & dif>1] <- 1
h03$stroke_1_inf[h03$stroke==0 | (h03$stroke==1 & dif<=1)] <- 0

# 2004
# ever
h04$mi <- NA
h04$mi[hse04$heartdef==1] <- 1
h04$mi[hse04$heartdef==2] <- 0

h04$stroke <- NA
h04$stroke[hse04$strodef==1] <- 1
h04$stroke[hse04$strodef==2] <- 0

# 12 months
h04$mi_0_1 <- NA
h04$mi_0_1[hse04$recheart==2 | h04$mi==0] <- 0
h04$mi_0_1[hse04$recheart==1] <- 1

h04$stroke_0_1 <- NA
h04$stroke_0_1[hse04$recstro==2 | h04$stroke==0] <- 0
h04$stroke_0_1[hse04$recstro==1] <- 1

# 1 year ago
age_event <- hse04$ageheart 
age_event[hse04$ageheart<0] <- NA 
dif <- h04$age - age_event
h04$mi_1_inf <- NA
h04$mi_1_inf[h04$mi==1 & dif>1] <- 1
h04$mi_1_inf[h04$mi==0 | (h04$mi==1 & dif<=1)] <- 0

age_event <- hse04$agestro
age_event[hse04$agestro<0] <- NA 
dif <- h04$age - age_event
h04$stroke_1_inf <- NA
h04$stroke_1_inf[h04$stroke==1 & dif>1] <- 1
h04$stroke_1_inf[h04$stroke==0 | (h04$stroke==1 & dif<=1)] <- 0

# 2006
# ever
h06$mi <- NA
h06$mi[hse06$heartdef==1] <- 1
h06$mi[hse06$heartdef==2] <- 0

h06$stroke <- NA
h06$stroke[hse06$strodef==1] <- 1
h06$stroke[hse06$strodef==2] <- 0

# 12 months
h06$mi_0_1 <- NA
h06$mi_0_1[hse06$recheart==2 | h06$mi==0] <- 0
h06$mi_0_1[hse06$recheart==1] <- 1

h06$stroke_0_1 <- NA
h06$stroke_0_1[hse06$recstro==2 | h06$stroke==0] <- 0
h06$stroke_0_1[hse06$recstro==1] <- 1

# 1 year ago
age_event <- hse06$ageheart 
age_event[hse06$ageheart<0] <- NA 
dif <- h06$age - age_event
h06$mi_1_inf <- NA
h06$mi_1_inf[h06$mi==1 & dif>1] <- 1
h06$mi_1_inf[h06$mi==0 | (h06$mi==1 & dif<=1)] <- 0

age_event <- hse06$agestro
age_event[hse06$agestro<0] <- NA 
dif <- h06$age - age_event
h06$stroke_1_inf <- NA
h06$stroke_1_inf[h06$stroke==1 & dif>1] <- 1
h06$stroke_1_inf[h06$stroke==0 | (h06$stroke==1 & dif<=1)] <- 0

# 2011
# ever
h11$mi <- NA
h11$mi[hse11$heartdef==1] <- 1
h11$mi[hse11$heartdef==2] <- 0

h11$stroke <- NA
h11$stroke[hse11$strodef==1] <- 1
h11$stroke[hse11$strodef==2] <- 0

# 12 months
h11$mi_0_1 <- NA
h11$mi_0_1[hse11$recheart==2 | h11$mi==0] <- 0
h11$mi_0_1[hse11$recheart==1] <- 1

h11$stroke_0_1 <- NA
h11$stroke_0_1[hse11$recstro==2 | h11$stroke==0] <- 0
h11$stroke_0_1[hse11$recstro==1] <- 1

# 1 year ago
age_event <- hse11$ageheart 
age_event[hse11$ageheart<0] <- NA 
dif <- h11$age - age_event
h11$mi_1_inf <- NA
h11$mi_1_inf[h11$mi==1 & dif>1] <- 1
h11$mi_1_inf[h11$mi==0 | (h11$mi==1 & dif<=1)] <- 0

age_event <- hse11$agestro
age_event[hse11$agestro<0] <- NA 
dif <- h11$age - age_event
h11$stroke_1_inf <- NA
h11$stroke_1_inf[h11$stroke==1 & dif>1] <- 1
h11$stroke_1_inf[h11$stroke==0 | (h11$stroke==1 & dif<=1)] <- 0

# 2017
# ever
h17$mi <- NA
h17$mi[hse17$heartdef==1] <- 1
h17$mi[hse17$heartdef==2] <- 0

h17$stroke <- NA
h17$stroke[hse17$strodef==1] <- 1
h17$stroke[hse17$strodef==2] <- 0

# 12 months
h17$mi_0_1 <- NA
h17$mi_0_1[hse17$recheart==2 | h17$mi==0] <- 0
h17$mi_0_1[hse17$recheart==1] <- 1

h17$stroke_0_1 <- NA
h17$stroke_0_1[hse17$recstro==2 | h17$stroke==0] <- 0
h17$stroke_0_1[hse17$recstro==1] <- 1

# 1 year ago
age_event <- hse17$ageheartg 
age_event[hse17$ageheartg<0] <- NA 
dif <- h17$age - age_event
h17$mi_1_inf <- NA
h17$mi_1_inf[h17$mi==1 & dif>1] <- 1
h17$mi_1_inf[h17$mi==0 | (h17$mi==1 & dif<=1)] <- 0

age_event <- hse17$agestrog
age_event[hse17$agestrog<0] <- NA 
dif <- h17$age - age_event
h17$stroke_1_inf <- NA
h17$stroke_1_inf[h17$stroke==1 & dif>1] <- 1
h17$stroke_1_inf[h17$stroke==0 | (h17$stroke==1 & dif<=1)] <- 0


# CRV----

#2003
# ever
h03$crv <- NA

h03$crv[hse03$surgery== 2 | hse03$surgery== -1 | hse03$whatsurg== 3 | hse03$whatsurg == -1] <- 0 # -1 indicates not applicable to the question due to no illness  

h03$crv[(hse03$whatsurg==1 | hse03$whatsurg==2)] <- 1 # category 1: angioplasty/stenting; category 2: CABG; exclude category 3: others

# 12 months
h03$crv_0_1 <- NA
h03$crv_0_1[h03$crv==0 | hse03$whensurg>0] <- 0
h03$crv_0_1[h03$crv==1 & hse03$whensurg==0] <- 1 # whensurg==0 means CRV happened in last 12 months

# 1 year + 
h03$crv_1_inf <- NA
h03$crv_1_inf[h03$crv==1 & hse03$whensurg>=1] <- 1 # value 1 means surgery happened 1 years ago
h03$crv_1_inf[h03$crv==0 | (h03$crv==1 & hse03$whensurg==0 )] <- 0

# 2004
# ever
h04$crv <- NA

h04$crv[hse04$surgery== 2 | hse04$surgery== -1 | hse04$whatsurg== 3 | hse04$whatsurg == -1] <- 0

h04$crv[(hse04$whatsurg==1 | hse04$whatsurg==2)] <- 1 

# 12 months
h04$crv_0_1 <- NA
h04$crv_0_1[h04$crv==0 | hse04$whensurg>0] <- 0
h04$crv_0_1[h04$crv==1 & hse04$whensurg==0] <- 1 

# 1 year +
h04$crv_1_inf <- NA
h04$crv_1_inf[h04$crv==1 & hse04$whensurg>=1] <- 1 
h04$crv_1_inf[h04$crv==0 | (h04$crv==1 & hse04$whensurg==0 )] <- 0

# 2006
# ever
h06$crv <- NA

h06$crv[hse06$surgery== 2 | hse06$surgery== -1 | hse06$whatsurg== 3 | hse06$whatsurg == -1] <- 0 

h06$crv[(hse06$whatsurg==1 | hse06$whatsurg==2)] <- 1 

# 12 months
h06$crv_0_1 <- NA
h06$crv_0_1[h06$crv==0 | hse06$whensurg>0] <- 0
h06$crv_0_1[h06$crv==1 & hse06$whensurg==0] <- 1 

# 1 year +
h06$crv_1_inf <- NA
h06$crv_1_inf[h06$crv==1 & hse06$whensurg>=1] <- 1 
h06$crv_1_inf[h06$crv==0 | (h06$crv==1 & hse06$whensurg==0 )] <- 0

# 2011
# ever
h11$crv <- NA

h11$crv[hse11$Surgery== 2 | hse11$Surgery== -1 | hse11$WhatSurg== 3 | hse11$WhatSurg == -1] <- 0 

h11$crv[(hse11$WhatSurg==1 | hse11$WhatSurg==2)] <- 1 

# 12 months
h11$crv_0_1 <- NA
h11$crv_0_1[h11$crv==0 | hse11$whensurg>0] <- 0
h11$crv_0_1[h11$crv==1 & hse11$whensurg==0] <- 1 

# 1 year + 
h11$crv_1_inf <- NA
h11$crv_1_inf[h11$crv==1 & hse11$whensurg>=1] <- 1 
h11$crv_1_inf[h11$crv==0 | (h11$crv==1 & hse11$whensurg==0 )] <- 0

# 2017
# ever
h17$crv <- NA

h17$crv[hse17$Surgery== 2 | hse17$Surgery== -1 | hse17$WhatSurg17== 3 | hse17$WhatSurg17 == -1] <- 0 

h17$crv[(hse17$WhatSurg17==1 | hse17$WhatSurg17==2)] <- 1 

# 12 months
h17$crv_0_1 <- NA
h17$crv_0_1[h17$crv==0 | hse17$whensurg>0] <- 0
h17$crv_0_1[h17$crv==1 & hse17$whensurg==0] <- 1 

# 1 year + 
h17$crv_1_inf <- NA
h17$crv_1_inf[h17$crv==1 & hse17$whensurg>=1] <- 1 
h17$crv_1_inf[h17$crv==0 | (h17$crv==1 & hse17$whensurg==0 )] <- 0


# diabetes----

# 2003
h03$dm <- NA
h03$dm[hse03$diabete2==1] <- 1
h03$dm[hse03$diabete2==2] <- 0

age_event <- hse03$ageinfo1
age_event[hse03$ageinfo1<0] <- NA 
dif <- h03$age - age_event
h03$dm_0_10 <- NA
h03$dm_0_10[h03$dm==1 & dif<=10 & dif>=0] <- 1
h03$dm_0_10[h03$dm==0 | (h03$dm==1 & dif>10)] <- 0

h03$dm_10_inf <- NA
h03$dm_10_inf[h03$dm==1 & dif>10] <- 1
h03$dm_10_inf[h03$dm==0 | h03$dm_0_10==1] <- 0

# 2004
h04$dm <- NA
h04$dm[hse04$diabete2==1] <- 1
h04$dm[hse04$diabete2==2] <- 0

age_event <- hse04$ageinfo1
age_event[hse04$ageinfo1<0] <- NA 
dif <- h04$age - age_event
h04$dm_0_10 <- NA
h04$dm_0_10[h04$dm==1 & dif<=10 & dif>=0] <- 1
h04$dm_0_10[h04$dm==0 | (h04$dm==1 & dif>10)] <- 0

h04$dm_10_inf <- NA
h04$dm_10_inf[h04$dm==1 & dif>10] <- 1
h04$dm_10_inf[h04$dm==0 | h04$dm_0_10==1] <- 0


# 2006
h06$dm <- NA
h06$dm[hse06$diabete2==1] <- 1
h06$dm[hse06$diabete2==2] <- 0

age_event <- hse06$ageinfo1
age_event[hse06$ageinfo1<0] <- NA 
dif <- h06$age - age_event
h06$dm_0_10 <- NA
h06$dm_0_10[h06$dm==1 & dif<=10 & dif>=0] <- 1
h06$dm_0_10[h06$dm==0 | (h06$dm==1 & dif>10)] <- 0

h06$dm_10_inf <- NA
h06$dm_10_inf[h06$dm==1 & dif>10] <- 1
h06$dm_10_inf[h06$dm==0 | h06$dm_0_10==1] <- 0

# 2011
h11$dm <- NA
h11$dm[hse11$diabete2r==1] <- 1
h11$dm[hse11$diabete2r==2] <- 0
# add hba1c>=6.5 #03 04 06 have no such information
h11$dm[hse11$diabete3r==3] <- 1

age_event <- hse11$ageinfo1
age_event[hse11$ageinfo1<0] <- NA 
dif <- h11$age - age_event
h11$dm_0_10 <- NA
h11$dm_0_10[h11$dm==1 & dif<=10 & dif>=0] <- 1
h11$dm_0_10[h11$dm==0 | (h11$dm==1 & dif>10)] <- 0
h11$dm_0_10[is.na(age_event) & hse11$diabete3r==3] <- 1

h11$dm_10_inf <- NA
h11$dm_10_inf[h11$dm==1 & dif>10] <- 1
h11$dm_10_inf[h11$dm==0 | h11$dm_0_10==1] <- 0

#2017
h17$dm <- NA
h17$dm[hse17$diabete2r==1] <- 1
h17$dm[hse17$diabete2r==2] <- 0
# add hba1c>=6.5
h17$dm[hse17$diabete3ra==3] <- 1

age_event <- hse17$cdiageg
age_event[hse17$cdiageg<0] <- NA 
dif <- h17$age - age_event
h17$dm_0_10 <- NA
h17$dm_0_10[h17$dm==1 & dif<=10 & dif>=0] <- 1
h17$dm_0_10[h17$dm==0 | (h17$dm==1 & dif>10)] <- 0
h17$dm_0_10[is.na(age_event) & hse17$diabete3ra==3] <- 1

h17$dm_10_inf <- NA
h17$dm_10_inf[h17$dm==1 & dif>10] <- 1
h17$dm_10_inf[h17$dm==0 | h17$dm_0_10==1] <- 0


# angina and other CVD----

# ever
h03$angina <- NA
h03$angina[hse03$angidef==1] <- 1
h03$angina[hse03$angidef==2] <- 0

h04$angina <- NA
h04$angina[hse04$angidef==1] <- 1
h04$angina[hse04$angidef==2] <- 0

h06$angina <- NA
h06$angina[hse06$angidef==1] <- 1
h06$angina[hse06$angidef==2] <- 0

h11$angina <- NA
h11$angina[hse11$angidef==1] <- 1
h11$angina[hse11$angidef==2] <- 0

h17$angina <- NA
h17$angina[hse17$angidef==1] <- 1
h17$angina[hse17$angidef==2] <- 0

h03$othcvd <- NA
h03$othcvd[hse03$ohtdef==1] <- 1
h03$othcvd[hse03$ohtdef==2] <- 0

h04$othcvd <- NA
h04$othcvd[hse04$ohtdef==1] <- 1
h04$othcvd[hse04$ohtdef==2] <- 0

h06$othcvd <- NA
h06$othcvd[hse06$ohtdef==1] <- 1
h06$othcvd[hse06$ohtdef==2] <- 0

h11$othcvd <- NA
h11$othcvd[hse11$ohtdef==1] <- 1
h11$othcvd[hse11$ohtdef==2] <- 0

h17$othcvd <- NA
h17$othcvd[hse17$ohtdef==1] <- 1
h17$othcvd[hse17$ohtdef==2] <- 0


# eq5d dimensions ---------------------------------------------------------

h03$mobility <- hse03$mobility
h03$selfcare <- hse03$selfcare
h03$usualact <- hse03$usualact
h03$pain <- hse03$pain
h03$anxiety <- hse03$anxiety

h04$mobility <- hse04$mobility
h04$selfcare <- hse04$selfcare
h04$usualact <- hse04$usualact
h04$pain <- hse04$pain
h04$anxiety <- hse04$anxiety

h06$mobility <- hse06$mobility
h06$selfcare <- hse06$selfcare
h06$usualact <- hse06$usualact
h06$pain <- hse06$pain
h06$anxiety <- hse06$anxiety

h11$mobility <- hse11$Mobility
h11$selfcare <- hse11$Selfcare
h11$usualact <- hse11$UsualAct
h11$pain <- hse11$Pain
h11$anxiety <- hse11$Anxiety

h17$mobility <- hse17$Mobil17
h17$selfcare <- hse17$SelfCa17
h17$usualact <- hse17$UsualA17
h17$pain <- hse17$Pain17
h17$anxiety <- hse17$Anxiet17


for (i in c("mobility", "selfcare", "usualact", "pain", "anxiety")) {
  h03[[i]][h03[[i]]<0] <- NA
  h04[[i]][h04[[i]]<0] <- NA
  h06[[i]][h06[[i]]<0] <- NA
  h11[[i]][h11[[i]]<0] <- NA
  h17[[i]][h17[[i]]<0] <- NA
}


# combine 03, 04, 06 and 11 -----------------------------------------------

h03$year <- "2003"
h04$year <- "2004"
h06$year <- "2006"
h11$year <- "2011"
h17$year <- "2017"

h03040611 <- rbind(h03, h04, h06, h11)

# calculate eq5d index----

# # a easier funciton
# get_index <- function(MO, SC, UA, PD, AD) {
#   library(eq5d)
#   score <- data.frame(MO=MO, SC=SC, UA=UA, PD=PD, AD=AD)
#   eq5d_index <- eq5d(score, country = "UK", version = "3L", type = "TTO", ignore.incomplete = T)
#   return(eq5d_index)
# }
# 
# h03040611$eq5d_index <- get_index(MO=h03040611$mobility, SC=h03040611$selfcare, UA=h03040611$usualact, PD=h03040611$pain, AD=h03040611$anxiety)

# age band----

h03040611$age5 <- NA
h03040611$age5[h03040611$age>=30 & h03040611$age<35] <- "30-34"
h03040611$age5[h03040611$age>=35 & h03040611$age<40] <- "35-39"
h03040611$age5[h03040611$age>=40 & h03040611$age<45] <- "40-44"
h03040611$age5[h03040611$age>=45 & h03040611$age<50] <- "45-49"
h03040611$age5[h03040611$age>=50 & h03040611$age<55] <- "50-54"
h03040611$age5[h03040611$age>=55 & h03040611$age<60] <- "55-59"
h03040611$age5[h03040611$age>=60 & h03040611$age<65] <- "60-64"
h03040611$age5[h03040611$age>=65 & h03040611$age<70] <- "65-69"
h03040611$age5[h03040611$age>=70 & h03040611$age<75] <- "70-74"
h03040611$age5[h03040611$age>=75 & h03040611$age<80] <- "75-79"
h03040611$age5[h03040611$age>=80 & h03040611$age<85] <- "80-84"
h03040611$age5[h03040611$age>=85 & h03040611$age<90] <- "85-89"
h03040611$age5[h03040611$age>=90] <- "90+"

h03040611$age5 <- as.factor(h03040611$age5)


# # BMI category----
# 
# h03040611 <- h03040611 %>% mutate(bmi_cat = case_when(bmi<18.5 ~ "<18.5",
#                                           bmi>=18.5 & bmi<25 ~ "18.5-25",
#                                           bmi>=25 & bmi<30 ~ "25-30",
#                                           bmi>=30 & bmi<35 ~ "30-35",
#                                           bmi>=35 & bmi<40 ~ "35-40",
#                                           bmi>=40 ~ "40+"
#                                           ))
# 
# h03040611$bmi_cat <- as.factor(h03040611$bmi_cat)
# h03040611$bmi_cat <- relevel(h03040611$bmi_cat, ref = "18.5-25")

# # ethnicity category
h03040611$ethn <- as.factor(h03040611$ethn)
levels(h03040611$ethn)[1] <- "White"
levels(h03040611$ethn)[2] <- "Black"
levels(h03040611$ethn)[3] <- "South Asian"
levels(h03040611$ethn)[4] <- "Chinese/Others"

h17$ethn <- as.factor(h17$ethn)
levels(h17$ethn)[1] <- "White"
levels(h17$ethn)[2] <- "Black"
levels(h17$ethn)[3] <- "South Asian"
levels(h17$ethn)[4] <- "Chinese/Others"

# IMD category----
h03040611$imd <- as.factor(h03040611$imd)
h03040611$imd <- relevel(h03040611$imd, ref=3)

h17$imd <- as.factor(h17$imd)
h17$imd <- relevel(h17$imd, ref=3)


# code categorical smoke and diabetes for impute----
h03040611 <- h03040611 %>% mutate(smk = case_when(ex_smk==1 ~ "ex-smoker",
                                      cur_smk==1 ~ "current smoker",
                                      ex_smk==0 & cur_smk==0 ~ "non-smoker"))

h03040611$smk <- as.factor(h03040611$smk)
h03040611$smk <- relevel(h03040611$smk, ref = "non-smoker")

h17 <- h17 %>% mutate(smk = case_when(ex_smk==1 ~ "ex-smoker",
                            cur_smk==1 ~ "current smoker",
                            ex_smk==0 & cur_smk==0 ~ "non-smoker"))

h17$smk <- as.factor(h17$smk)
h17$smk <- relevel(h17$smk, ref = "non-smoker")


h03040611 <- h03040611 %>% mutate(dm_0_10_inf = case_when(dm_0_10==1 ~ "diabetes <=10 years",
                                      dm_10_inf==1 ~ "diabetes >10 years",
                                      dm_0_10==0 & dm_10_inf==0 ~ "no diabetes"))

h03040611$dm_0_10_inf <- as.factor(h03040611$dm_0_10_inf)
h03040611$dm_0_10_inf <- relevel(h03040611$dm_0_10_inf, ref = "no diabetes")

h17 <- h17 %>% mutate(dm_0_10_inf = case_when(dm_0_10==1 ~ "diabetes <=10 years",
                             dm_10_inf==1 ~ "diabetes >10 years",
                             dm_0_10==0 & dm_10_inf==0 ~ "no diabetes"))

h17$dm_0_10_inf <- as.factor(h17$dm_0_10_inf)
h17$dm_0_10_inf <- relevel(h17$dm_0_10_inf, ref = "no diabetes")

# code to factor for other categorical variable for imputation ---------------

h03040611$year <- as.factor(h03040611$year)
h03040611$trtbp <- as.factor(h03040611$trtbp)
h03040611$mi_0_1 <- as.factor(h03040611$mi_0_1)
h03040611$mi_1_inf <- as.factor(h03040611$mi_1_inf)
h03040611$stroke_0_1 <- as.factor(h03040611$stroke_0_1)
h03040611$stroke_1_inf <- as.factor(h03040611$stroke_1_inf)
h03040611$crv_0_1 <- as.factor(h03040611$crv_0_1)
h03040611$crv_1_inf <- as.factor(h03040611$crv_1_inf)
h03040611$angina <- as.factor(h03040611$angina)
h03040611$edu <- as.factor(h03040611$edu)
h03040611$marital <- as.factor(h03040611$marital)
h03040611$mental <- as.factor(h03040611$mental)
h03040611$cancer <- as.factor(h03040611$cancer)
h03040611$othcvd <- as.factor(h03040611$othcvd)
h03040611$mobility <- as.factor(h03040611$mobility)
h03040611$selfcare <- as.factor(h03040611$selfcare)
h03040611$usualact <- as.factor(h03040611$usualact)
h03040611$pain <- as.factor(h03040611$pain)
h03040611$anxiety <- as.factor(h03040611$anxiety)

h17$year <- as.factor(h17$year)
h17$trtbp <- as.factor(h17$trtbp)
h17$mi_0_1 <- as.factor(h17$mi_0_1)
h17$mi_1_inf <- as.factor(h17$mi_1_inf)
h17$stroke_0_1 <- as.factor(h17$stroke_0_1)
h17$stroke_1_inf <- as.factor(h17$stroke_1_inf)
h17$crv_0_1 <- as.factor(h17$crv_0_1)
h17$crv_1_inf <- as.factor(h17$crv_1_inf)
h17$angina <- as.factor(h17$angina)
h17$edu <- as.factor(h17$edu)
h17$marital <- as.factor(h17$marital)
h17$mental <- as.factor(h17$mental)
h17$cancer <- as.factor(h17$cancer)
h17$othcvd <- as.factor(h17$othcvd)
h17$mobility <- as.factor(h17$mobility)
h17$selfcare <- as.factor(h17$selfcare)
h17$usualact <- as.factor(h17$usualact)
h17$pain <- as.factor(h17$pain)
h17$anxiety <- as.factor(h17$anxiety)
# save --------------------------------------------------------------------
saveRDS(h03040611, file = "D:/eq5d/data/h03040611.rds", compress = F)

saveRDS(h17, file = "D:/eq5d/data/h17.rds", compress = F)
