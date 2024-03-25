# Refining the Analysis of Medicare Plan Choices and Utilization
# A Disaggregated Approach
# Using MCBS Survey and Cost Supplement Files, 2015-2019
# This code written by Austin Knies
# Ph.D. Candidate, Department of Economics, Indiana University Bloomington
# Last updated: 03/25/2024

# Creates segments of Tables 5-6

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
hide15 <- left_join(hide15,chgh15, by = c("BASEID","SURVEYYR"))
hide16 <- left_join(hide16,chgh16, by = c("BASEID","SURVEYYR"))
hide17 <- left_join(hide17,chgh17, by = c("BASEID","SURVEYYR"))
hide18 <- left_join(hide18,chgh18, by = c("BASEID","SURVEYYR"))
hide19 <- left_join(hide19,chgh19, by = c("BASEID","SURVEYYR"))

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

### Cost Supplement Weights #####
csw2015 <- import("Cost Supplement 2015/Data/CSV Files/csevwgts.csv") %>%
  as_tibble()
csw2016 <- import("Cost Supplement 2016/Data/CSV Files/csevwgts.csv") %>%
  as_tibble()
csw2017 <- import("Cost Supplement 2017/Data/CSV Files/csevwgts.csv") %>%
  as_tibble()
csw2018 <- import("Cost Supplement 2018/Data/CSV Files/csevwgts.csv") %>%
  as_tibble()
csw2019 <- import("Cost Supplement 2019/Data/CSV Files/csevwgts.csv") %>%
  as_tibble()

### Person Summary Files #####
ps2015 <- import("Cost Supplement 2015/Data/CSV Files/ps.csv") %>%
  as_tibble()
psa <- left_join(ps2015,hide15, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing
psb <- left_join(csw2015,psa, by = c("BASEID","SURVEYYR"))
# table(is.na(psb$SURVIVE))
ps2015 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2016 <- import("Cost Supplement 2016/Data/CSV Files/ps.csv") %>%
  as_tibble()
psa <- left_join(ps2016,hide16, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing
psb <- left_join(csw2016,psa, by = c("BASEID","SURVEYYR"))
# table(is.na(psb$SURVIVE))
ps2016 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2017 <- import("Cost Supplement 2017/Data/CSV Files/ps.csv") %>%
  as_tibble()
psa <- left_join(ps2017,hide17, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing
psb <- left_join(csw2017,psa, by = c("BASEID","SURVEYYR"))
# table(is.na(psb$SURVIVE))
ps2017 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2018 <- import("Cost Supplement 2018/Data/CSV Files/ps.csv") %>%
  as_tibble()
psa <- left_join(ps2018,hide18, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing
psb <- left_join(csw2018,psa, by = c("BASEID","SURVEYYR"))
# table(is.na(psb$SURVIVE))
ps2018 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

ps2019 <- import("Cost Supplement 2019/Data/CSV Files/ps.csv") %>%
  as_tibble()
psa <- left_join(ps2019,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(psa$SURVIVE)) # Code to check all merged well, no missing
psb <- left_join(csw2019,psa, by = c("BASEID","SURVEYYR"))
# table(is.na(psb$SURVIVE))
ps2019 <- psb # Merged Survey Files with Cost Supplement Weights
rm(psa,psb)
gc()

#### Append PS Files across Years #####
ps <- full_join(ps2015, ps2016)
ps <- full_join(ps,ps2017)
ps <- full_join(ps,ps2018)
ps <- full_join(ps,ps2019)
rm(ps2015,ps2016,ps2017,ps2018,ps2019) 
gc()

#### Final Construction (Adding Custom Variables and Dropping ESRD) #####
mainps <- ps %>%
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
rm(ps)
gc()

mainps <- mainps %>%
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
mainps <- mainps %>%
  rowwise() %>%
  mutate(FULLRISK = sum(GENHELTH,COMPHLTH,FUTRHLTH,HELMTACT,BMI_CAT,IPR_IND,HYSTEREC,OCARTERY,OCHBP,OCMYOCAR,OCCHD,OCCFAIL,OCCVALVE,OCRHYTHM,OCOTHHRT,OCSTROKE,OCCHOLES,
                        OCCSKIN,OCCLUNG,OCCCOLON,OCCBREST,OCCUTER,OCCPROST,OCCBLAD,OCCOVARY,OCCSTOM,OCCCERVX,OCCBRAIN,OCCKIDNY,OCCTHROA,OCCHEAD,OCCBACK,OCCFONEC,OCCOTHER,
                        OCARTHRH,OCARTH,OCMENTAL,OCALZMER,OCDEMENT,OCDEPRSS,OCPSYCHO,OCOSTEOP,OCBRKHIP,OCPARKIN,OCEMPHYS,OCPPARAL,OCAMPUTE,HAVEPROS,OCBETES,TIMESAD,LOSTINTR,
                        LOSTURIN,HYPETOLD,na.rm=TRUE),
         CHRONRISK = sum(HYSTEREC,OCARTERY,OCHBP,OCMYOCAR,OCCHD,OCCFAIL,OCCVALVE,OCRHYTHM,OCOTHHRT,OCSTROKE,OCCHOLES,
                         OCCSKIN,OCCLUNG,OCCCOLON,OCCBREST,OCCUTER,OCCPROST,OCCBLAD,OCCOVARY,OCCSTOM,OCCCERVX,OCCBRAIN,OCCKIDNY,OCCTHROA,OCCHEAD,OCCBACK,OCCFONEC,OCCOTHER,
                         OCARTHRH,OCARTH,OCMENTAL,OCALZMER,OCDEMENT,OCDEPRSS,OCPSYCHO,OCOSTEOP,OCBRKHIP,OCPARKIN,OCEMPHYS,OCPPARAL,OCAMPUTE,HAVEPROS,OCBETES,LOSTURIN,HYPETOLD,na.rm=TRUE),
         PERCRISK = sum(GENHELTH,COMPHLTH,FUTRHLTH,HELMTACT,BMI_CAT,IPR_IND,na.rm=TRUE),
         SADRISK = sum(TIMESAD,LOSTINTR,na.rm=TRUE),
  )

### Event Summary Files #####

#### Dental Events, 2015-2019 #####
du2015 <- import("Cost Supplement 2015/Data/CSV Files/due.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTVA, AMTOTH) %>%
  mutate(AMTOTH = AMTVA + AMTOTH, # After 2015, CMS includes AMTVA in AMTOTH amount; only do for 2015 files
         SOURCETYPE = "DU") %>%
  dplyr::select(-AMTVA)
esa <- left_join(du2015,hide15, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2015, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
du2015 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

du2016 <- import("Cost Supplement 2016/Data/CSV Files/due.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "DU")
esa <- left_join(du2016,hide16, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2016, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
du2016 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

du2017 <- import("Cost Supplement 2017/Data/CSV Files/due.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "DU")
esa <- left_join(du2017,hide17, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2017, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
du2017 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

du2018 <- import("Cost Supplement 2018/Data/CSV Files/due.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "DU")
esa <- left_join(du2018,hide18, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2018, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
du2018 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

du2019 <- import("Cost Supplement 2019/Data/CSV Files/due.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "DU")
esa <- left_join(du2019,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
du2019 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

du <- full_join(du2015, du2016)
du <- full_join(du,du2017)
du <- full_join(du,du2018)
du <- full_join(du,du2019)
rm(du2015,du2016,du2017,du2018,du2019)
gc()

#### Facility Events, 2015-2019 #####
fa2015 <- import("Cost Supplement 2015/Data/CSV Files/fae.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT, AMTCARE, AMTCAID, AMTPRVU, AMTOOP, AMTVA, AMTOTH) %>%
  mutate(AMTOTH = AMTVA + AMTOTH, # After 2015, CMS includes AMTVA in AMTOTH amount; only do for 2015 files
         SOURCETYPE = "FA") %>%
  dplyr::select(-AMTVA)
esa <- left_join(fa2015,hide15, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2015, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
fa2015 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

fa2016 <- import("Cost Supplement 2016/Data/CSV Files/fae.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT, AMTCARE, AMTCAID, AMTPRVU, AMTOOP, AMTOTH) %>%
  mutate(SOURCETYPE = "FA")
esa <- left_join(fa2016,hide16, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2016, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
fa2016 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

fa2017 <- import("Cost Supplement 2017/Data/CSV Files/fae.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT, AMTCARE, AMTCAID, AMTPRVU, AMTOOP, AMTOTH) %>%
  mutate(SOURCETYPE = "FA")
esa <- left_join(fa2017,hide17, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2017, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
fa2017 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

fa2018 <- import("Cost Supplement 2018/Data/CSV Files/fae.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT, AMTCAID, AMTPRVU, AMTOOP, AMTOTH) %>%
  mutate(SOURCETYPE = "FA")
esa <- left_join(fa2018,hide18, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2018, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
fa2018 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

fa2019 <- import("Cost Supplement 2019/Data/CSV Files/fae.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT, AMTCAID, AMTPRVU, AMTOOP, AMTOTH) %>%
  mutate(SOURCETYPE = "FA")
esa <- left_join(fa2019,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
fa2019 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

fa <- full_join(fa2015, fa2016)
fa <- full_join(fa,fa2017)
fa <- full_join(fa,fa2018)
fa <- full_join(fa,fa2019)
rm(fa2015,fa2016,fa2017,fa2018,fa2019)
gc()

#### Hearing Events (2019 Only) #####
hu <- import("Cost Supplement 2019/Data/CSV Files/hue.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "HU")
esa <- left_join(hu,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
hu <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

#### Vision Events (2019 Only) #####
vu <- import("Cost Supplement 2019/Data/CSV Files/vue.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "VU")
esa <- left_join(vu,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
vu <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

#### Inpatient Events, 2015-2019 #####
ip2015 <- import("Cost Supplement 2015/Data/CSV Files/ipe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTVA, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(AMTOTH = AMTVA + AMTOTH, # After 2015, CMS includes AMTVA in AMTOTH amount; only do for 2015 files
         SOURCETYPE = "IP") %>%
  dplyr::select(-AMTVA)
esa <- left_join(ip2015,hide15, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2015, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
ip2015 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

ip2016 <- import("Cost Supplement 2016/Data/CSV Files/ipe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "IP")
esa <- left_join(ip2016,hide16, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2016, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
ip2016 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

ip2017 <- import("Cost Supplement 2017/Data/CSV Files/ipe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "IP")
esa <- left_join(ip2017,hide17, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2017, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
ip2017 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

ip2018 <- import("Cost Supplement 2018/Data/CSV Files/ipe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "IP")
esa <- left_join(ip2018,hide18, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2018, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
ip2018 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

ip2019 <- import("Cost Supplement 2019/Data/CSV Files/ipe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "IP")
esa <- left_join(ip2019,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
ip2019 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

ip <- full_join(ip2015, ip2016)
ip <- full_join(ip,ip2017)
ip <- full_join(ip,ip2018)
ip <- full_join(ip,ip2019)
rm(ip2015,ip2016,ip2017,ip2018,ip2019)
gc()

#### Institutional Events, 2015-2019 #####
iu2015 <- import("Cost Supplement 2015/Data/CSV Files/iue.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTVA, AMTOTH) %>%
  mutate(AMTOTH = AMTVA + AMTOTH, # After 2015, CMS includes AMTVA in AMTOTH amount; only do for 2015 files
         SOURCETYPE = "IU") %>%
  dplyr::select(-AMTVA)
esa <- left_join(iu2015,hide15, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2015, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
iu2015 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

iu2016 <- import("Cost Supplement 2016/Data/CSV Files/iue.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "IU")
esa <- left_join(iu2016,hide16, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2016, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
iu2016 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

iu2017 <- import("Cost Supplement 2017/Data/CSV Files/iue.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "IU")
esa <- left_join(iu2017,hide17, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2017, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
iu2017 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

iu2018 <- import("Cost Supplement 2018/Data/CSV Files/iue.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "IU")
esa <- left_join(iu2018,hide18, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2018, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
iu2018 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

iu2019 <- import("Cost Supplement 2019/Data/CSV Files/iue.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "IU")
esa <- left_join(iu2019,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
iu2019 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

iu <- full_join(iu2015, iu2016)
iu <- full_join(iu,iu2017)
iu <- full_join(iu,iu2018)
iu <- full_join(iu,iu2019)
rm(iu2015,iu2016,iu2017,iu2018,iu2019)
gc()

#### Medical Provider Events, 2015-2019 #####
mp2015 <- import("Cost Supplement 2015/Data/CSV Files/mpe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTVA, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(AMTOTH = AMTVA + AMTOTH, # After 2015, CMS includes AMTVA in AMTOTH amount; only do for 2015 files
         SOURCETYPE = "MP") %>%
  dplyr::select(-AMTVA)
esa <- left_join(mp2015,hide15, by = c("BASEID","SURVEYYR"))
table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2015, by = c("BASEID","SURVEYYR"))
table(is.na(esb$CSEVRWGT)) 
esc <- esb %>% # drop observations with no matching weights in cost supplement files
  filter(is.na(esb$CSEVRWGT)==FALSE)
mp2015 <- esc # Merged Survey Files with Cost Supplement Weights
rm(esa,esb,esc)
gc()

mp2016 <- import("Cost Supplement 2016/Data/CSV Files/mpe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "MP")
esa <- left_join(mp2016,hide16, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2016, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
mp2016 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

mp2017 <- import("Cost Supplement 2017/Data/CSV Files/mpe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "MP")
esa <- left_join(mp2017,hide17, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2017, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
mp2017 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

mp2018 <- import("Cost Supplement 2018/Data/CSV Files/mpe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "MP")
esa <- left_join(mp2018,hide18, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2018, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
mp2018 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

mp2019 <- import("Cost Supplement 2019/Data/CSV Files/mpe.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "MP")
esa <- left_join(mp2019,hide19, by = c("BASEID","SURVEYYR"))
table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
table(is.na(esb$CSEVRWGT))
mp2019 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

mp <- full_join(mp2015, mp2016)
mp <- full_join(mp,mp2017)
mp <- full_join(mp,mp2018)
mp <- full_join(mp,mp2019)
rm(mp2015,mp2016,mp2017,mp2018,mp2019)
gc()

#### Outpatient Events, 2015-2019 #####
op2015 <- import("Cost Supplement 2015/Data/CSV Files/ope.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTVA, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(AMTOTH = AMTVA + AMTOTH, # After 2015, CMS includes AMTVA in AMTOTH amount; only do for 2015 files
         SOURCETYPE = "OP") %>%
  dplyr::select(-AMTVA)
esa <- left_join(op2015,hide15, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2015, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
op2015 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

op2016 <- import("Cost Supplement 2016/Data/CSV Files/ope.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "OP")
esa <- left_join(op2016,hide16, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2016, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
op2016 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

op2017 <- import("Cost Supplement 2017/Data/CSV Files/ope.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "OP")
esa <- left_join(op2017,hide17, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2017, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
op2017 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

op2018 <- import("Cost Supplement 2018/Data/CSV Files/ope.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "OP")
esa <- left_join(op2018,hide18, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2018, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
op2018 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

op2019 <- import("Cost Supplement 2019/Data/CSV Files/ope.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCOV,AMTNCOV, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH, OREVTYPE, SOWMP) %>%
  mutate(SOURCETYPE = "OP")
esa <- left_join(op2019,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
op2019 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

op <- full_join(op2015, op2016)
op <- full_join(op,op2017)
op <- full_join(op,op2018)
op <- full_join(op,op2019)
rm(op2015,op2016,op2017,op2018,op2019)
gc()

#### Prescribed Medicine Events, 2015-2019 #####
pm2015 <- import("Cost Supplement 2015/Data/CSV Files/pme.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT, AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTVA, AMTOTH) %>%
  mutate(AMTOTH = AMTVA + AMTOTH, # After 2015, CMS includes AMTVA in AMTOTH amount; only do for 2015 files
         SOURCETYPE = "PM") %>%
  dplyr::select(-AMTVA)
esa <- left_join(pm2015,hide15, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2015, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
pm2015 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

pm2016 <- import("Cost Supplement 2016/Data/CSV Files/pme.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "PM")
esa <- left_join(pm2016,hide16, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2016, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
pm2016 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

pm2017 <- import("Cost Supplement 2017/Data/CSV Files/pme.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "PM")
esa <- left_join(pm2017,hide17, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2017, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
pm2017 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

pm2018 <- import("Cost Supplement 2018/Data/CSV Files/pme.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "PM")
esa <- left_join(pm2018,hide18, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2018, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
pm2018 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

pm2019 <- import("Cost Supplement 2019/Data/CSV Files/pme.csv") %>%
  as_tibble() %>%
  dplyr::select(BASEID,SURVEYYR,AMTTOT,AMTCARE, AMTCAID, AMTMADV, AMTHMOP, AMTPRVE, AMTPRVI, AMTPRVU, AMTOOP, AMTDISC, AMTOTH) %>%
  mutate(SOURCETYPE = "PM")
esa <- left_join(pm2019,hide19, by = c("BASEID","SURVEYYR"))
# table(is.na(esa$SURVIVE)) # Code to check all merged well, no missing
esb <- left_join(esa, csw2019, by = c("BASEID","SURVEYYR"))
# table(is.na(esb$CSEVRWGT))
pm2019 <- esb # Merged Survey Files with Cost Supplement Weights
rm(esa,esb)
gc()

pm <- full_join(pm2015,pm2016)
pm <- full_join(pm,pm2017)
pm <- full_join(pm,pm2018)
pm <- full_join(pm,pm2019)
rm(pm2015,pm2016,pm2017,pm2018,pm2019)
gc()

#### Append ES across Years #####
all <- full_join(du,fa)
all <- full_join(all,hu)
all <- full_join(all,vu)
all <- full_join(all,ip)
all <- full_join(all,iu)
all <- full_join(all,mp)
all <- full_join(all,op)
all <- full_join(all,pm)
rm(du,fa,hu,vu,ip,iu,mp,op,pm) 
gc()

#### Final Construction (Adding Custom Variables and Dropping ESRD) #####
mainall <- all %>%
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
rm(all)
gc()

mainall <- mainall %>%
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
mainall <- mainall %>%
  rowwise() %>%
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
rm(csw2015,csw2016,csw2017,csw2018,csw2019,hide15,hide16,hide17,hide18,hide19)
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
mainps <- left_join(mainps, opt1df, by = c("BASEID","SURVEYYR"))
mainall <- left_join(mainall, opt1df, by = c("BASEID","SURVEYYR"))
rm(opt1df,htsup)
gc()

mainps <- mainps %>%
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

mainall <- mainall %>%
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


mainps <- mainps %>%
  mutate(SUPCOV = case_when(HAVE_C==0 & SUPCOVhit==1 & SCDUR>TOTMA_EL/2~1,
                            HAVE_C==1 & SUPCOVhit==1 & SCDUR>TOTMA_EL/2 & SCDUR>=TOTMA_MA~1,
                            TRUE~0)) %>%
  mutate(MCRCOVGROUP = case_when(HAVE_C==0 & SUPCOV==0~1, # TM Only
                                 HAVE_C==0 & SUPCOV==1~2, # TM with SUPCOV
                                 HAVE_C==1~3))            # MA

mainall <- mainall %>%
  mutate(SUPCOV = case_when(HAVE_C==0 & SUPCOVhit==1 & SCDUR>TOTMA_EL/2~1,
                            HAVE_C==1 & SUPCOVhit==1 & SCDUR>TOTMA_EL/2 & SCDUR>=TOTMA_MA~1,
                            TRUE~0)) %>%
  mutate(MCRCOVGROUP = case_when(HAVE_C==0 & SUPCOV==0~1, # TM Only
                                 HAVE_C==0 & SUPCOV==1~2, # TM with SUPCOV
                                 HAVE_C==1~3))            # MA

mainps <- mainps %>%
  mutate(TM = case_when(HAVE_C==0~1,TRUE~0)) %>%  # TM at all
  mutate(TMONLY = case_when(TM==1 & MEDICAID==0 & SUPCOV==0~1,TRUE~0),
         MAONLY = case_when(TM==0 & MEDICAID==0 & SUPCOV==0~1,TRUE~0),
         TMMEDICAID = case_when(TM==1 & MEDICAID==1~1,TRUE~0),
         MAMEDICAID = case_when(TM==0 & MEDICAID==1~1,TRUE~0),
         TMPLUS = case_when(TM==1 & SUPCOV==1~1,TRUE~0),
         MAPLUS = case_when(TM==0 & SUPCOV==1~1,TRUE~0)
  )

mainall <- mainall %>%
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
mainps <- left_join(mainps, opt1df, by = c("BASEID","SURVEYYR"))
mainall <- left_join(mainall, opt1df, by = c("BASEID","SURVEYYR"))
rm(htpartd,opt1df)
gc()
mainps <- mainps %>%
  mutate(PARTDhit = case_when(is.na(PARTDhit)==TRUE~0, # if no hitline file for it
                              PARTDhit==0~0, # if hitline file but no supplemental coverage detected
                              PARTDhit==1~1,
                              TRUE~NA),
         PARTDDUR = case_when(is.na(PARTDDUR)==TRUE~0, # if no sup cov or only Medicaid
                              TRUE~PARTDDUR)
  )

mainall <- mainall %>%
  mutate(PARTDhit = case_when(is.na(PARTDhit)==TRUE~0, # if no hitline file for it
                              PARTDhit==0~0, # if hitline file but no supplemental coverage detected
                              PARTDhit==1~1,
                              TRUE~NA),
         PARTDDUR = case_when(is.na(PARTDDUR)==TRUE~0, # if no sup cov or only Medicaid
                              TRUE~PARTDDUR)
  )

mainps <- mainps %>%
  mutate(PARTD = case_when(PARTDhit==1 & PARTDDUR>TOTMA_EL/2~1,
                           TRUE~0),
         RACMIN = case_when(H_RTIRCE==1~0,
                            TRUE~1),
         FEMALE = case_when(H_SEX==2~1, TRUE~0))

mainall <- mainall %>%
  mutate(PARTD = case_when(PARTDhit==1 & PARTDDUR>TOTMA_EL/2~1,
                           TRUE~0),
         RACMIN = case_when(H_RTIRCE==1~0,
                            TRUE~1),
         FEMALE = case_when(H_SEX==2~1, TRUE~0))

## Construct MCRCOVGROUP #####
mainps <- mainps %>%
  mutate(GROUPING = case_when(MAMEDICAID==1~"MA + Medicaid",
                              TMMEDICAID==1~"TM + Medicaid",
                              TMONLY==1~"TM Only",
                              TMPLUS==1~"TM + Sup Cov",
                              MAPLUS==1~"MA + Sup Cov",
                              TRUE~"MA Only"))

mainall <- mainall %>%
  mutate(GROUPING = case_when(MAMEDICAID==1~"MA + Medicaid",
                              TMMEDICAID==1~"TM + Medicaid",
                              TMONLY==1~"TM Only",
                              TMPLUS==1~"TM + Sup Cov",
                              MAPLUS==1~"MA + Sup Cov",
                              TRUE~"MA Only"))


# 4) Getting survey variables of interest -----

# Loading each file in separately
swc2015 <- import("Survey File 2015/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY)
swc2016 <- import("Survey File 2016/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY)
swc2017 <- import("Survey File 2017/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY)
swc2018 <- import("Survey File 2018/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY)
swc2019 <- import("Survey File 2019/Data/CSV Files/satwcare.csv") %>%
  as_tibble() %>% 
  dplyr::select(BASEID,SURVEYYR,MCQUALTY)

swc <- full_join(swc2015,swc2016)
swc <- full_join(swc,swc2017)
swc <- full_join(swc,swc2018)
swc <- full_join(swc,swc2019)

mainps <- left_join(mainps,swc, by = c("BASEID","SURVEYYR"))

rm(swc)
gc()

mainps <- mainps %>% 
  mutate(QUALITY = case_when(MCQUALTY==1~1,
                             MCQUALTY==2~2,
                             MCQUALTY==3~3,
                             MCQUALTY==4~4,
                             is.na(MCQUALTY)~NA,
                             TRUE~NA)
  )

# 5) Analysis with quality survey variables of interest -----

summainps <- mainps %>% drop_na(QUALITY,GROUPING,PARTD,FULLRISK,H_AGE_DMND,RACMIN,LOGINCOME,HSEDUC,FEMALE,CBSA_D,SURVEYYR)

## Satisfaction with Quality of Care Survey Response Measures (Table 5, Cross-Section Sample) #####

c1<-table1(~QUALITY|SURVEYYR, data=summainps)
t1kable(c1, format = "latex") %>% save_kable("Output/summarystatsquality_crosssectionpart.tex")
rm(c1)
gc()


## Impact of Medicare Coverage on Satisfaction with Quality of Care (Table 6, Columns 1 and 3) #####

reg.1 <- lm(QUALITY ~ 1+TM+PARTD+FULLRISK+H_AGE_DMND+RACMIN+LOGINCOME+as.factor(HSEDUC)+FEMALE+as.factor(CBSA_D)+as.factor(SURVEYYR), data = mainps, weights = CSEVRWGT)
reg.2 <- lm(QUALITY ~ 1+TM+PARTD+FULLRISK+H_AGE_DMND+RACMIN+LOGINCOME+as.factor(HSEDUC)+FEMALE+as.factor(CBSA_D)+as.factor(SURVEYYR), data = mainps, weights = CSEVRWGT)
reg.3 <- lm(QUALITY ~ 1+TMONLY+TMPLUS+MAPLUS+TMMEDICAID+MAMEDICAID+PARTD+FULLRISK+H_AGE_DMND+RACMIN+LOGINCOME+as.factor(HSEDUC)+FEMALE+as.factor(CBSA_D)+as.factor(SURVEYYR), data = mainps, weights = CSEVRWGT)
reg.4 <- lm(QUALITY ~ 1+TMONLY+TMPLUS+MAPLUS+TMMEDICAID+MAMEDICAID+PARTD+FULLRISK+H_AGE_DMND+RACMIN+LOGINCOME+as.factor(HSEDUC)+FEMALE+as.factor(CBSA_D)+as.factor(SURVEYYR), data = mainps, weights = CSEVRWGT)

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
          out = "Output/quality_crosssectionpart.tex")

# 6) Clean Up -----

rm(list=ls())
gc()
