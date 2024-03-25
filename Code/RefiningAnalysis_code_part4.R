# Refining the Analysis of Medicare Plan Choices and Utilization
# A Disaggregated Approach
# Using MCBS Survey and Cost Supplement Files, 2015-2019
# This code written by Austin Knies
# Ph.D. Candidate, Department of Economics, Indiana University Bloomington
# Last updated: 03/25/2024

# Creates segments of Tables 5-6, Tables 7-8

# 1) Load packages ----

# Set working directory
# setwd("Your File Path")

# Install packages, first by installing pacman (manages packages well in R)
if (!require("pacman")) install.packages("pacman")

# Load desired packages
pacman::p_load(pacman, car, rio, tidyverse, tidyfast, ggplot2, table1, kableExtra, stargazer, sandwich, R.utils, plm, survey, memisc,fixest)

# Settings 
options(scipen = 999)

# 2) Load data -----

# MCBS Data: https://www.cms.gov/data-research/research/medicare-current-beneficiary-survey

## Survey Files #####

### hisumry files #####

# Loading each HISUMRY file in separately
hi2015 <- import("Survey File 2015/Data/CSV Files/hisumry.csv") %>%
  as_tibble()
hi2016 <- import("Survey File 2016/Data/CSV Files/hisumry.csv") %>%
  as_tibble()
hi2017 <- import("Survey File 2017/Data/CSV Files/hisumry.csv") %>%
  as_tibble()
hi2018 <- import("Survey File 2018/Data/CSV Files/hisumry.csv") %>%
  as_tibble()
hi2019 <- import("Survey File 2019/Data/CSV Files/hisumry.csv") %>%
  as_tibble()

### demo files #####

# Loading each DEMO file in separately
demo2015 <- import("Survey File 2015/Data/CSV Files/demo.csv") %>%
  as_tibble()
demo2016 <- import("Survey File 2016/Data/CSV Files/demo.csv") %>%
  as_tibble()
demo2017 <- import("Survey File 2017/Data/CSV Files/demo.csv") %>%
  as_tibble()
demo2018 <- import("Survey File 2018/Data/CSV Files/demo.csv") %>%
  as_tibble()
demo2019 <- import("Survey File 2019/Data/CSV Files/demo.csv") %>%
  as_tibble()

# A few formatting consistency corrections to make so that we can merge these data sets
demo2015$H_DODSRC <- as.numeric(demo2015$H_DODSRC)
demo2017$RACEASAI <- as.character(demo2017$RACEASAI)
demo2017$RACEASCH <- as.character(demo2017$RACEASCH)
demo2017$RACEASFI <- as.character(demo2017$RACEASFI)
demo2017$RACEASJA <- as.character(demo2017$RACEASJA)
demo2017$RACEASKO <- as.character(demo2017$RACEASKO)
demo2017$RACEASVI <- as.character(demo2017$RACEASVI)
demo2017$RACEASOT <- as.character(demo2017$RACEASOT)
demo2015$RACEPIHA <- as.character(demo2015$RACEPIHA)
demo2016$RACEPIHA <- as.character(demo2016$RACEPIHA)
demo2015$RACEPIGU <- as.character(demo2015$RACEPIGU)
demo2016$RACEPIGU <- as.character(demo2016$RACEPIGU)
demo2015$RACEPISA <- as.character(demo2015$RACEPISA)
demo2016$RACEPISA <- as.character(demo2016$RACEPISA)
demo2015$RACEPIOT <- as.character(demo2015$RACEPIOT)
demo2016$RACEPIOT <- as.character(demo2016$RACEPIOT)
demo2015$WHATLANG <- as.character(demo2015$WHATLANG)
demo2016$WHATLANG <- as.character(demo2016$WHATLANG)
demo2017$WHATLANG <- as.character(demo2017$WHATLANG)

### chrncond files #####

# Loading each chrncond file in separately
chrn2015 <- import("Survey File 2015/Data/CSV Files/chrncond.csv") %>%
  as_tibble() %>%
  mutate(across(everything(), as.character)) %>%
  mutate(BASEID=as.integer(BASEID),
         SURVEYYR=as.numeric(SURVEYYR))
chrn2016 <- import("Survey File 2016/Data/CSV Files/chrncond.csv") %>%
  as_tibble() %>%
  mutate(across(everything(), as.character)) %>%
  mutate(BASEID=as.integer(BASEID),
         SURVEYYR=as.numeric(SURVEYYR))
chrn2017 <- import("Survey File 2017/Data/CSV Files/chrncond.csv") %>%
  as_tibble() %>%
  mutate(across(everything(), as.character)) %>%
  mutate(BASEID=as.integer(BASEID),
         SURVEYYR=as.numeric(SURVEYYR))
chrn2018 <- import("Survey File 2018/Data/CSV Files/chrncond.csv") %>%
  as_tibble() %>%
  mutate(across(everything(), as.character)) %>%
  mutate(BASEID=as.integer(BASEID),
         SURVEYYR=as.numeric(SURVEYYR))
chrn2019 <- import("Survey File 2019/Data/CSV Files/chrncond.csv") %>%
  as_tibble() %>%
  mutate(across(everything(), as.character)) %>%
  mutate(BASEID=as.integer(BASEID),
         SURVEYYR=as.numeric(SURVEYYR))

### genhlth files #####

# Loading each genhlth file in separately
gh2015 <- import("Survey File 2015/Data/CSV Files/genhlth.csv") %>%
  as_tibble()
gh2016 <- import("Survey File 2016/Data/CSV Files/genhlth.csv") %>%
  as_tibble()
gh2017 <- import("Survey File 2017/Data/CSV Files/genhlth.csv") %>%
  as_tibble()
gh2018 <- import("Survey File 2018/Data/CSV Files/genhlth.csv") %>%
  as_tibble()
gh2019 <- import("Survey File 2019/Data/CSV Files/genhlth.csv") %>%
  as_tibble()

### Merging hisumry and demo files by year ####
hide15 <- left_join(hi2015,demo2015, by = c("BASEID","SURVEYYR"))
hide16 <- left_join(hi2016,demo2016, by = c("BASEID","SURVEYYR"))
hide17 <- left_join(hi2017,demo2017, by = c("BASEID","SURVEYYR"))
hide18 <- left_join(hi2018,demo2018, by = c("BASEID","SURVEYYR"))
hide19 <- left_join(hi2019,demo2019, by = c("BASEID","SURVEYYR"))

# table(is.na(hidexx$SURVIVE)) # Code to check all merged well, no missing
# Now, we can remove all the individual demo and hisumry files from memory
rm(demo2015,demo2016,demo2017,demo2018,demo2019,hi2015,hi2016,hi2017,hi2018,hi2019)
gc()

### Merging genhlth and chrncond files by year ####
chgh15 <- left_join(gh2015,chrn2015, by = c("BASEID","SURVEYYR"))
chgh16 <- left_join(gh2016,chrn2016, by = c("BASEID","SURVEYYR"))
chgh17 <- left_join(gh2017,chrn2017, by = c("BASEID","SURVEYYR"))
chgh18 <- left_join(gh2018,chrn2018, by = c("BASEID","SURVEYYR"))
chgh19 <- left_join(gh2019,chrn2019, by = c("BASEID","SURVEYYR"))

# table(is.na(chghXX$OCARTERY)) # Code to check all merged well, no missing
# Now, we can remove all the individual chrn and gh files from memory
rm(chrn2015,chrn2016,chrn2017,chrn2018,chrn2019,gh2015,gh2016,gh2017,gh2018,gh2019)
gc()

### Merging hide and chgh files by year ####
surv15 <- left_join(hide15,chgh15, by = c("BASEID","SURVEYYR"))
surv16 <- left_join(hide16,chgh16, by = c("BASEID","SURVEYYR"))
surv17 <- left_join(hide17,chgh17, by = c("BASEID","SURVEYYR"))
surv18 <- left_join(hide18,chgh18, by = c("BASEID","SURVEYYR"))
surv19 <- left_join(hide19,chgh19, by = c("BASEID","SURVEYYR"))

### hitline files #####

# Loading each HITLINE file in separately
ht2015 <- import("Survey File 2015/Data/CSV Files/hitline.csv") %>%
  as_tibble()
ht2016 <- import("Survey File 2016/Data/CSV Files/hitline.csv") %>%
  as_tibble()
ht2017 <- import("Survey File 2017/Data/CSV Files/hitline.csv") %>%
  as_tibble()
ht2018 <- import("Survey File 2018/Data/CSV Files/hitline.csv") %>%
  as_tibble()
ht2019 <- import("Survey File 2019/Data/CSV Files/hitline.csv") %>%
  as_tibble()

ht <- full_join(ht2015,ht2016)
ht <- full_join(ht,ht2017)
ht <- full_join(ht,ht2018)
ht <- full_join(ht,ht2019)

## Cost Supplement Files #####

### Two-Year Longitudinal Weights #####
csl2_16 <- import("Cost Supplement 2016/Data/CSV Files/csl2wgts.csv") %>%
  as_tibble() %>% 
  dplyr::rename(WGTYR = SURVEYYR)
csl2_17 <- import("Cost Supplement 2017/Data/CSV Files/csl2wgts.csv") %>%
  as_tibble() %>% 
  dplyr::rename(WGTYR = SURVEYYR)
csl2_18 <- import("Cost Supplement 2018/Data/CSV Files/csl2wgts.csv") %>%
  as_tibble() %>% 
  dplyr::rename(WGTYR = SURVEYYR)
csl2_19 <- import("Cost Supplement 2019/Data/CSV Files/csl2wgts.csv") %>%
  as_tibble() %>% 
  dplyr::rename(WGTYR = SURVEYYR)

#### 2015-2016 Two Year Panel #####

ps2015 <- import("Cost Supplement 2015/Data/CSV Files/ps.csv") %>%
  as_tibble()

psa <- inner_join(ps2015,surv15, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing

psb <- inner_join(csl2_16,psa, by = c("BASEID"))
# table(is.na(psb$OCARTERY))

ps2015 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2016 <- import("Cost Supplement 2016/Data/CSV Files/ps.csv") %>%
  as_tibble()

psa <- inner_join(ps2016,surv16, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing

psb <- inner_join(csl2_16,psa, by = c("BASEID"))
# table(is.na(psb$OCARTERY))

ps2016 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2_16 <- full_join(ps2015, ps2016)
rm(ps2015,ps2016)
gc()

#### 2016-2017 Two Year Panel #####

ps2016 <- import("Cost Supplement 2016/Data/CSV Files/ps.csv") %>%
  as_tibble()

psa <- inner_join(ps2016,surv16, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing

psb <- inner_join(csl2_17,psa, by = c("BASEID"))
# table(is.na(psb$OCARTERY))

ps2016 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2017 <- import("Cost Supplement 2017/Data/CSV Files/ps.csv") %>%
  as_tibble()

psa <- inner_join(ps2017,surv17, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing

psb <- inner_join(csl2_17,psa, by = c("BASEID"))
# table(is.na(psb$OCARTERY))

ps2017 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2_17 <- full_join(ps2016, ps2017)
rm(ps2016,ps2017)
gc()

#### 2017-2018 Two Year Panel #####

ps2017 <- import("Cost Supplement 2017/Data/CSV Files/ps.csv") %>%
  as_tibble()

psa <- inner_join(ps2017,surv17, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing

psb <- inner_join(csl2_18,psa, by = c("BASEID"))
# table(is.na(psb$OCARTERY))

ps2017 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2018 <- import("Cost Supplement 2018/Data/CSV Files/ps.csv") %>%
  as_tibble()

psa <- inner_join(ps2018,surv18, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing

psb <- inner_join(csl2_18,psa, by = c("BASEID"))
# table(is.na(psb$OCARTERY))

ps2018 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2_18 <- full_join(ps2017, ps2018)
rm(ps2017,ps2018)
gc()


#### 2018-2019 Two Year Panel #####

ps2018 <- import("Cost Supplement 2018/Data/CSV Files/ps.csv") %>%
  as_tibble()

psa <- inner_join(ps2018,surv18, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing

psb <- inner_join(csl2_19,psa, by = c("BASEID"))
# table(is.na(psb$OCARTERY))

ps2018 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2019 <- import("Cost Supplement 2019/Data/CSV Files/ps.csv") %>%
  as_tibble()

psa <- inner_join(ps2019,surv19, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing

psb <- inner_join(csl2_19,psa, by = c("BASEID"))
# table(is.na(psb$OCARTERY))

ps2019 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2_19 <- full_join(ps2018, ps2019)
rm(ps2018,ps2019)
gc()

#### Merge Two-Year Panels Together #####

ps2 <- full_join(ps2_16,ps2_17)
ps2 <- full_join(ps2,ps2_18)
ps2 <- full_join(ps2,ps2_19)
rm(ps2_16,ps2_17,ps2_18,ps2_19) 
gc()

#### Final Construction (Adding Custom Variables and Dropping ESRD) #####
mainps2 <- ps2 %>%
  filter(H_MEDSTA==10) %>% # Dropping beneficiaries with ESRD
  # First, determine if Medicare eligible each month
  mutate(MA_EL01 = case_when(H_MAFF01=="NO"~0, TRUE~1),
         MA_EL02 = case_when(H_MAFF02=="NO"~0, TRUE~1),
         MA_EL03 = case_when(H_MAFF03=="NO"~0, TRUE~1),
         MA_EL04 = case_when(H_MAFF04=="NO"~0, TRUE~1),
         MA_EL05 = case_when(H_MAFF05=="NO"~0, TRUE~1),
         MA_EL06 = case_when(H_MAFF06=="NO"~0, TRUE~1),
         MA_EL07 = case_when(H_MAFF07=="NO"~0, TRUE~1),
         MA_EL08 = case_when(H_MAFF08=="NO"~0, TRUE~1),
         MA_EL09 = case_when(H_MAFF09=="NO"~0, TRUE~1),
         MA_EL10 = case_when(H_MAFF10=="NO"~0, TRUE~1),
         MA_EL11 = case_when(H_MAFF11=="NO"~0, TRUE~1),
         MA_EL12 = case_when(H_MAFF12=="NO"~0, TRUE~1)) %>%
  # Then, sum up how many months in the year of eligibility
  mutate(TOTMA_EL = MA_EL01+MA_EL02+MA_EL03+MA_EL04+MA_EL05+MA_EL06+MA_EL07+MA_EL08+MA_EL09+MA_EL10+MA_EL11+MA_EL12) %>%
  # Now, find number of months of Medicare Advantage
  mutate(TOTMA_MA = case_when(H_MAFF01=="MA"~1,
                              TRUE~0),
         TOTMA_MA = case_when(H_MAFF02=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF03=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF04=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF05=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF06=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF07=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF08=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF09=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF10=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF11=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA),
         TOTMA_MA = case_when(H_MAFF12=="MA"~TOTMA_MA+1,
                              TRUE~TOTMA_MA)) %>%
  # Now calculate share of Medicare eligibility each year spent in MADV
  mutate(SHARE_C = case_when(TOTMA_EL>0~TOTMA_MA/TOTMA_EL,
                             TRUE~-100)) %>%
  # Finally, if share is over half of months in eligibility, they are MADV
  mutate(HAVE_C = case_when(TOTMA_MA>TOTMA_EL/2~1,
                            TRUE~0)) %>%
  mutate(H_AGE_DMND = H_AGE - mean(H_AGE),
         HSEDUC = case_when(SPDEGRCV==1~1,
                            SPDEGRCV==2~1,
                            SPDEGRCV==3~1, 
                            SPDEGRCV==4~2, 
                            SPDEGRCV==5~3, 
                            SPDEGRCV==6~3,
                            SPDEGRCV==7~3,
                            SPDEGRCV==8~3,
                            SPDEGRCV==9~3),
         LOGINCOME = log(INCOME_H+1),
         CBSA_D = case_when(SURVEYYR==2015 & H_URBRUR=="Metro"~"Metro",
                            SURVEYYR==2016 & H_URBRUR=="Metro"~"Metro",
                            SURVEYYR==2017 & H_CBSA=="Metro"~"Metro",
                            SURVEYYR==2018 & H_CBSA=="Metro"~"Metro",
                            SURVEYYR==2019 & H_CBSA=="Metro"~"Metro",
                            SURVEYYR==2015 & H_URBRUR=="Micro"~"Micro",
                            SURVEYYR==2016 & H_URBRUR=="Micro"~"Micro",
                            SURVEYYR==2017 & H_CBSA=="Micro"~"Micro",
                            SURVEYYR==2018 & H_CBSA=="Micro"~"Micro",
                            SURVEYYR==2019 & H_CBSA=="Micro"~"Micro",
                            SURVEYYR==2015 & H_URBRUR=="Rural"~"Rural",
                            SURVEYYR==2016 & H_URBRUR=="Rural"~"Rural",
                            SURVEYYR==2017 & H_CBSA=="Non-CBSA"~"Rural",
                            SURVEYYR==2018 & H_CBSA=="Non-CBSA"~"Rural",
                            SURVEYYR==2019 & H_CBSA=="Non-CBSA"~"Rural",
                            TRUE~"Unknown")
  )
rm(ps2)
gc()

mainps2 <- mainps2 %>% 
  mutate(SURVEYID = paste(BASEID,WGTYR,sep="")) %>% 
  mutate(SURVEYID = as.numeric(SURVEYID))

sum2 <- mainps2 %>% group_by(SURVEYID) %>%
  summarise(n_years = n(), years=toString(SURVEYYR))%>%
  ungroup()
table(sum2$years)

df2 <- left_join(mainps2, sum2, by = c("SURVEYID"))

rm(mainps2,sum2)
gc()

mainps2 <- df2 %>% 
  filter(n_years>1)
table(mainps2$years) 


mainps2 <- mainps2 %>%
  mutate(GENHELTH = case_when(GENHELTH==1~0,GENHELTH==2~1,GENHELTH==3~2,GENHELTH==4~3,GENHELTH==5~4,TRUE~2),
         COMPHLTH = case_when(COMPHLTH==1~0,COMPHLTH==2~1,COMPHLTH==3~2,COMPHLTH==4~3,COMPHLTH==5~4,TRUE~2),
         FUTRHLTH = case_when(FUTRHLTH==1~0,FUTRHLTH==2~1,FUTRHLTH==3~2,FUTRHLTH==4~3,FUTRHLTH==5~4,TRUE~2),
         HELMTACT = case_when(HELMTACT==1~0,HELMTACT==2~1,HELMTACT==3~2,HELMTACT==4~3,TRUE~1),
         BMI_CAT = case_when(BMI_CAT==1~1,BMI_CAT==2~0,BMI_CAT==3~1,BMI_CAT==4~2,BMI_CAT==5~3,TRUE~1),
         IPR_IND = case_when(IPR_IND==1~4,IPR_IND==2~3,IPR_IND==3~2,IPR_IND==4~1,IPR_IND==5~0,TRUE~1),
         HYSTEREC = case_when(HYSTEREC==1~1,TRUE~0),
         OCARTERY = case_when(OCARTERY==1~1,TRUE~0),
         OCHBP = case_when(OCHBP==1~1,TRUE~0),
         OCMYOCAR = case_when(OCMYOCAR==1~1,TRUE~0),
         OCCHD = case_when(OCCHD==1~1,TRUE~0),
         OCCFAIL = case_when(OCCFAIL==1~1,TRUE~0),
         OCCVALVE = case_when(OCCVALVE==1~1,TRUE~0),
         OCRHYTHM = case_when(OCRHYTHM==1~1,TRUE~0),
         OCOTHHRT = case_when(OCOTHHRT==1~1,TRUE~0),
         OCSTROKE = case_when(OCSTROKE==1~1,TRUE~0),
         OCCHOLES = case_when(OCCHOLES==1~1,TRUE~0),
         OCCSKIN = case_when(OCCSKIN==1~1,TRUE~0),
         OCCLUNG = case_when(OCCLUNG==1~1,TRUE~0),
         OCCCOLON = case_when(OCCCOLON==1~1,TRUE~0),
         OCCBREST= case_when(OCCBREST==1~1,TRUE~0),
         OCCUTER= case_when(OCCUTER==1~1,TRUE~0),
         OCCPROST = case_when(OCCPROST==1~1,TRUE~0),
         OCCBLAD = case_when(OCCBLAD==1~1,TRUE~0),
         OCCOVARY = case_when(OCCOVARY==1~1,TRUE~0),
         OCCSTOM = case_when(OCCSTOM==1~1,TRUE~0),
         OCCCERVX = case_when(OCCCERVX==1~1,TRUE~0),
         OCCBRAIN = case_when(OCCBRAIN==1~1,TRUE~0),
         OCCKIDNY = case_when(OCCKIDNY==1~1,TRUE~0),
         OCCTHROA = case_when(OCCTHROA==1~1,TRUE~0),
         OCCHEAD = case_when(OCCHEAD==1~1,TRUE~0),
         OCCBACK = case_when(OCCBACK==1~1,TRUE~0),
         OCCFONEC = case_when(OCCFONEC==1~1,TRUE~0),
         OCCOTHER = case_when(OCCOTHER==1~1,TRUE~0),
         OCARTHRH = case_when(OCARTHRH==1~1,TRUE~0),
         OCARTH = case_when(OCARTH==1~1,TRUE~0),
         OCMENTAL = case_when(OCMENTAL==1~1,TRUE~0),
         OCALZMER = case_when(OCALZMER==1~1,TRUE~0),
         OCDEMENT = case_when(OCDEMENT==1~1,TRUE~0),
         OCDEPRSS = case_when(OCDEPRSS==1~1,TRUE~0),
         OCPSYCHO = case_when(OCPSYCHO==1~1,TRUE~0),
         OCOSTEOP = case_when(OCOSTEOP==1~1,TRUE~0),
         OCBRKHIP = case_when(OCBRKHIP==1~1,TRUE~0),
         OCPARKIN = case_when(OCPARKIN==1~1,TRUE~0),
         OCEMPHYS = case_when(OCEMPHYS==1~1,TRUE~0),
         OCPPARAL = case_when(OCPPARAL==1~1,TRUE~0),
         OCAMPUTE = case_when(OCAMPUTE==1~1,TRUE~0),
         HAVEPROS = case_when(HAVEPROS==1~1,TRUE~0),
         OCBETES = case_when(OCBETES==1~1,TRUE~0),
         TIMESAD = case_when(TIMESAD==1~4,TIMESAD==2~3,TIMESAD==3~2,TIMESAD==4~1,TIMESAD==5~0,TRUE~1),
         LOSTINTR = case_when(LOSTINTR==1~1,TRUE~0),
         LOSTURIN = case_when(LOSTURIN==1~6,LOSTURIN==2~5,LOSTURIN==3~4,LOSTURIN==4~3,LOSTURIN==5~2,LOSTURIN==6~1,LOSTURIN==7~0,LOSTURIN==8~7,TRUE~1),
         HYPETOLD = case_when(HYPETOLD==1~1,TRUE~0)
  )
mainps2 <- mainps2 %>%
  group_by(SURVEYID, SURVEYYR) %>%
  mutate(FULLRISK = sum(GENHELTH,COMPHLTH,FUTRHLTH,HELMTACT,BMI_CAT,IPR_IND,HYSTEREC,OCARTERY,OCHBP,OCMYOCAR,OCCHD,OCCFAIL,OCCVALVE,OCRHYTHM,OCOTHHRT,OCSTROKE,OCCHOLES,
                        OCCSKIN,OCCLUNG,OCCCOLON,OCCBREST,OCCUTER,OCCPROST,OCCBLAD,OCCOVARY,OCCSTOM,OCCCERVX,OCCBRAIN,OCCKIDNY,OCCTHROA,OCCHEAD,OCCBACK,OCCFONEC,OCCOTHER,
                        OCARTHRH,OCARTH,OCMENTAL,OCALZMER,OCDEMENT,OCDEPRSS,OCPSYCHO,OCOSTEOP,OCBRKHIP,OCPARKIN,OCEMPHYS,OCPPARAL,OCAMPUTE,HAVEPROS,OCBETES,TIMESAD,LOSTINTR,
                        LOSTURIN,HYPETOLD,na.rm=TRUE),
         CHRONRISK = sum(HYSTEREC,OCARTERY,OCHBP,OCMYOCAR,OCCHD,OCCFAIL,OCCVALVE,OCRHYTHM,OCOTHHRT,OCSTROKE,OCCHOLES,
                         OCCSKIN,OCCLUNG,OCCCOLON,OCCBREST,OCCUTER,OCCPROST,OCCBLAD,OCCOVARY,OCCSTOM,OCCCERVX,OCCBRAIN,OCCKIDNY,OCCTHROA,OCCHEAD,OCCBACK,OCCFONEC,OCCOTHER,
                         OCARTHRH,OCARTH,OCMENTAL,OCALZMER,OCDEMENT,OCDEPRSS,OCPSYCHO,OCOSTEOP,OCBRKHIP,OCPARKIN,OCEMPHYS,OCPPARAL,OCAMPUTE,HAVEPROS,OCBETES,LOSTURIN,HYPETOLD,na.rm=TRUE),
         PERCRISK = sum(GENHELTH,COMPHLTH,FUTRHLTH,HELMTACT,BMI_CAT,IPR_IND,na.rm=TRUE),
         SADRISK = sum(TIMESAD,LOSTINTR,na.rm=TRUE),
  ) %>% 
  ungroup()
rm(hide15,hide16,hide17,hide18,hide19)
gc()

# 3) Get supplemental coverage ----

## Use HITLINE files to determine based off of existence of plan that is not A, B, C, or D #####
htsup <- ht %>%
  ungroup() %>%
  filter(PLANTYPE>4) %>% #1, 2, 3, 4 CORRESPOND TO A, B, C, OR D
  mutate(SUPCOV = case_when(PLANTYPE>5~1, TRUE~0),
         MEDICAID = case_when(PLANTYPE==5~1,TRUE~0),
         SCDUR = case_when(PLANTYPE>5~COV01+COV02+COV03+COV04+COV05+COV06+COV07+COV08+COV09+COV10+COV11+COV12))
opt1df <- htsup %>%
  group_by(BASEID,SURVEYYR) %>%
  summarize(SUPCOVhit = max(SUPCOV), SCDUR = max(SCDUR), MEDICAID = max(MEDICAID)) 
mainps2 <- left_join(mainps2, opt1df, by = c("BASEID","SURVEYYR"))
rm(opt1df,htsup)
gc()

mainps2 <- mainps2 %>%
  mutate(SUPCOVhit = case_when(is.na(SUPCOVhit)==TRUE~0, # if no hitline file for it
                               SUPCOVhit==0~0, # if hitline file but no supplemental coverage detected
                               SUPCOVhit==1~1,
                               TRUE~NA),
         MEDICAID = case_when(is.na(MEDICAID)==TRUE~0, # if no hitline file for it
                              MEDICAID==0~0, # if hitline file but no Medicaid detected
                              MEDICAID==1~1,
                              TRUE~NA),
         SCDUR = case_when(is.na(SCDUR)==TRUE~0, # if no sup cov or only Medicaid
                           TRUE~SCDUR)
  )

mainps2 <- mainps2 %>%
  mutate(SUPCOV = case_when(HAVE_C==0 & SUPCOVhit==1 & SCDUR>TOTMA_EL/2~1,
                            HAVE_C==1 & SUPCOVhit==1 & SCDUR>TOTMA_EL/2 & SCDUR>=TOTMA_MA~1,
                            TRUE~0)) %>%
  mutate(MCRCOVGROUP = case_when(HAVE_C==0 & SUPCOV==0~1, # TM Only
                                 HAVE_C==0 & SUPCOV==1~2, # TM with SUPCOV
                                 HAVE_C==1~3))           # MA

mainps2 <- mainps2 %>%
  mutate(TM = case_when(HAVE_C==0~1,TRUE~0)) %>%  # TM at all
  mutate(TMONLY = case_when(TM==1 & MEDICAID==0 & SUPCOV==0~1,TRUE~0),
         MAONLY = case_when(TM==0 & MEDICAID==0 & SUPCOV==0~1,TRUE~0),
         TMMEDICAID = case_when(TM==1 & MEDICAID==1~1,TRUE~0),
         MAMEDICAID = case_when(TM==0 & MEDICAID==1~1,TRUE~0),
         TMPLUS = case_when(TM==1 & SUPCOV==1~1,TRUE~0),
         MAPLUS = case_when(TM==0 & SUPCOV==1~1,TRUE~0)
  )


## Use HITLINE files to determine Part D coverage #####
htpartd <- ht %>%
  ungroup() %>%
  filter(PLANTYPE==4) %>% #1, 2, 3, 4 CORRESPOND TO A, B, C, OR D
  mutate(PARTDhit = 1,
         PARTDDUR = COV01+COV02+COV03+COV04+COV05+COV06+COV07+COV08+COV09+COV10+COV11+COV12)
opt1df <- htpartd %>%
  group_by(BASEID,SURVEYYR) %>%
  summarize(PARTDhit = max(PARTDhit), PARTDDUR = max(PARTDDUR))
mainps2 <- left_join(mainps2, opt1df, by = c("BASEID","SURVEYYR"))
rm(htpartd,opt1df)
gc()
mainps2 <- mainps2 %>%
  mutate(PARTDhit = case_when(is.na(PARTDhit)==TRUE~0, # if no hitline file for it
                              PARTDhit==0~0, # if hitline file but no supplemental coverage detected
                              PARTDhit==1~1,
                              TRUE~NA),
         PARTDDUR = case_when(is.na(PARTDDUR)==TRUE~0, # if no sup cov or only Medicaid
                              TRUE~PARTDDUR)
  )

mainps2 <- mainps2 %>%
  mutate(PARTD = case_when(PARTDhit==1 & PARTDDUR>TOTMA_EL/2~1,
                           TRUE~0),
         RACMIN = case_when(H_RTIRCE==1~0,
                            TRUE~1),
         FEMALE = case_when(H_SEX==2~1, TRUE~0))

## Construct MCRCOVGROUP #####
mainps2 <- mainps2 %>%
  mutate(GROUPING = case_when(MAMEDICAID==1~"MA + Medicaid",
                              TMMEDICAID==1~"TM + Medicaid",
                              TMONLY==1~"TM Only",
                              TMPLUS==1~"TM + Sup Cov",
                              MAPLUS==1~"MA + Sup Cov",
                              TRUE~"MA Only"))


# 4) Getting survey variables of interest #####

# Loading each file in separately
swc2015 <- import("Survey File 2015/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY,MCCOSTS,MCAVOID,MCSPECAR,MCDRNSEE)
swc2016 <- import("Survey File 2016/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY,MCCOSTS,MCAVOID,MCSPECAR,MCDRNSEE)
swc2017 <- import("Survey File 2017/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY,MCCOSTS,MCAVOID,MCSPECAR,MCDRNSEE)
swc2018 <- import("Survey File 2018/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY,MCCOSTS,MCAVOID,MCSPECAR,MCDRNSEE)
swc2019 <- import("Survey File 2019/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY,MCCOSTS,MCAVOID,MCSPECAR)

swc <- full_join(swc2015,swc2016)
swc <- full_join(swc,swc2017)
swc <- full_join(swc,swc2018)
swc <- full_join(swc,swc2019)

mainps2 <- left_join(mainps2,swc, by = c("BASEID","SURVEYYR"))
rm(swc)
gc()

mainps2 <- mainps2 %>% 
  mutate(QUALITY = case_when(MCQUALTY==1~1,
                             MCQUALTY==2~2,
                             MCQUALTY==3~3,
                             MCQUALTY==4~4,
                             is.na(MCQUALTY)~NA,
                             TRUE~NA)
  )

# 5) Creating Panel Structure -----

df2 <- pdata.frame(mainps2, index=c("SURVEYID","SURVEYYR"))

df2$TM_t0 = plm::lag(df2$TM)
df2$TM_t1 = df2$TM
df2$GROUPING_t0 = plm::lag(df2$GROUPING)
df2$GROUPING_t1 = df2$GROUPING
df2$MAONLY_t0 = plm::lag(df2$MAONLY)
df2$MAONLY_t1 = df2$MAONLY
df2$TMONLY_t0 = plm::lag(df2$TMONLY)
df2$TMONLY_t1 = df2$TMONLY
df2$MAMEDICAID_t0 = plm::lag(df2$MAMEDICAID)
df2$MAMEDICAID_t1 = df2$MAMEDICAID
df2$TMMEDICAID_t0 = plm::lag(df2$TMMEDICAID)
df2$TMMEDICAID_t1 = df2$TMMEDICAID
df2$MAPLUS_t0 = plm::lag(df2$MAPLUS)
df2$MAPLUS_t1 = df2$MAPLUS
df2$TMPLUS_t0 = plm::lag(df2$TMPLUS)
df2$TMPLUS_t1 = df2$TMPLUS
df2$PARTD_t0 = plm::lag(df2$PARTD)
df2$PARTD_t1= df2$FULLRISK
df2$FULLRISK_t1 = df2$FULLRISK
df2$FULLRISK_t0 = plm::lag(df2$FULLRISK)
df2$FULLRISKdiff = df2$FULLRISK_t1-df2$FULLRISK_t0

df2$QUALITY_t0 = plm::lag(df2$QUALITY)
df2$QUALITY_t1 = df2$QUALITY

# 6) Analysis with quality survey variables of interest -----

sumdf2 <- df2 %>% drop_na(QUALITY,GROUPING,PARTD,FULLRISK,H_AGE_DMND,RACMIN,LOGINCOME,HSEDUC,FEMALE,CBSA_D,SURVEYYR)

## Satisfaction with Quality of Care Survey Response Measures (Table 5, Two-Year Panel Sample) #####

c1<-table1(~QUALITY|SURVEYYR, data=sumdf2)
t1kable(c1, format = "latex") %>% save_kable("Output/summarystatsquality_twoyearpart.tex")
rm(c1)
gc()

## Impact of Medicare Coverage on Satisfaction with Quality of Care (Table 6, Columns 2 and 4) #####

reg.1 <- lm(QUALITY ~ 1+TM+PARTD+FULLRISK+H_AGE_DMND+RACMIN+LOGINCOME+as.factor(HSEDUC)+FEMALE+as.factor(CBSA_D)+as.factor(SURVEYYR), data = df2, weights = CSL2YWGT)
reg.2 <- lm(QUALITY ~ 1+TM+PARTD+FULLRISK+H_AGE_DMND+RACMIN+LOGINCOME+as.factor(HSEDUC)+FEMALE+as.factor(CBSA_D)+as.factor(SURVEYYR), data = df2, weights = CSL2YWGT)
reg.3 <- lm(QUALITY ~ 1+TMONLY+TMPLUS+MAPLUS+TMMEDICAID+MAMEDICAID+PARTD+FULLRISK+H_AGE_DMND+RACMIN+LOGINCOME+as.factor(HSEDUC)+FEMALE+as.factor(CBSA_D)+as.factor(SURVEYYR), data = df2, weights = CSL2YWGT)
reg.4 <- lm(QUALITY ~ 1+TMONLY+TMPLUS+MAPLUS+TMMEDICAID+MAMEDICAID+PARTD+FULLRISK+H_AGE_DMND+RACMIN+LOGINCOME+as.factor(HSEDUC)+FEMALE+as.factor(CBSA_D)+as.factor(SURVEYYR), data = df2, weights = CSL2YWGT)

model.lst = list(reg.1,reg.2,reg.3,reg.4)

stargazer(reg.1,reg.2,reg.3,reg.4, 
          type = "latex", 
          column.separate = c(2, 2),
          se=lapply(model.lst, function(x) sqrt(diag(vcovHC(x, type = "HC1")))),
          no.space = TRUE,
          single.row = FALSE,
          omit.stat=c("f", "ser"),
          float = FALSE,
          font.size = "small",
          notes = "robust SEs in parentheses",
          out = "Output/quality_twoyearpart.tex")

## Quality of Care Survey Response Measures for Those with MA Only in Year 0 (Table 7) #####

c1<-table1(~QUALITY_t0+QUALITY_t1|GROUPING, data=subset(df2,is.na(QUALITY_t0)==FALSE & is.na(QUALITY_t1)==FALSE & MAONLY_t0==1 & (MAONLY_t1==1|TMONLY_t1==1)))
t1kable(c1, format = "latex") %>% save_kable("Output/switchers_quality_maonly.tex")
rm(c1)
gc()

## Quality of Care Survey Response Measures for Those with TM Only in Year 0 (Table 8) #####

c1<-table1(~QUALITY_t0+QUALITY_t1|GROUPING, data=subset(df2,is.na(QUALITY_t0)==FALSE & is.na(QUALITY_t1)==FALSE & TMONLY_t0==1 & (MAONLY_t1==1|TMONLY_t1==1)))
t1kable(c1, format = "latex") %>% save_kable("Output/switchers_quality_tmonly.tex")
rm(c1)
gc()


# 7) Clean Up -----

rm(list=ls())
gc()
