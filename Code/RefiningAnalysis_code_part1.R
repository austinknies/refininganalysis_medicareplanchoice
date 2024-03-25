# Refining the Analysis of Medicare Plan Choices and Utilization
# A Disaggregated Approach
# Using MCBS Survey and Cost Supplement Files, 2015-2019
# This code written by Austin Knies
# Ph.D. Candidate, Department of Economics, Indiana University Bloomington
# Last updated: 03/25/2024

# Creates Figure 1

# 1) Load packages ----

# Set working directory
# setwd("Your File Path")

# Install packages, first by installing pacman (manages packages well in R)
if (!require("pacman")) install.packages("pacman")

# Load desired packages
pacman::p_load(pacman, rio, tidyverse, ggplot2, table1, kableExtra, stargazer, sandwich, R.utils, plm, survey, memisc, lmtest, RColorBrewer,modelsummary)

# 2) Load data -----

# Data can be found at the following address:
# https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-advantagepart-d-contract-and-enrollment-data/monthly-enrollment-contract

# NOTE: Filtering by organization type so that we only consider MA Enrollment
# CMS files also include Part D Plan enrollment and other numbers

cms2015m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-01")

cms2015m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-02")

cms2015m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-03")

cms2015m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-04")

cms2015m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-05")

cms2015m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-06")

cms2015m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-07")

cms2015m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-08")

cms2015m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-09")

cms2015m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-10")

cms2015m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-11")

cms2015m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-12")

cms2016m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-01")

cms2016m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-02")

cms2016m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-03")

cms2016m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-04")

cms2016m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-05")

cms2016m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-06")

cms2016m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-07")

cms2016m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-08")

cms2016m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-09")

cms2016m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-10")

cms2016m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-11")

cms2016m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-12")

cms2017m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-01")

cms2017m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-02")

cms2017m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-03")

cms2017m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-04")

cms2017m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-05")

cms2017m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-06")

cms2017m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-07")

cms2017m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-08")

cms2017m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-09")

cms2017m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-10")

cms2017m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-11")

cms2017m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-12")

cms2018m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-01")

cms2018m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-02")

cms2018m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-03")

cms2018m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-04")

cms2018m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-05")

cms2018m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-06")

cms2018m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-07")

cms2018m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-08")

cms2018m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-09")

cms2018m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-10")

cms2018m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-11")

cms2018m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-12")

cms2019m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-01")

cms2019m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-02")

cms2019m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-03")

cms2019m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-04")

cms2019m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-05")

cms2019m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-06")

cms2019m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-07")

cms2019m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-08")

cms2019m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-09")

cms2019m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-10")

cms2019m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-11")

cms2019m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(PlanType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-12")

cmsdflist <- list(cms2015m1,
                  cms2015m2,
                  cms2015m3,
                  cms2015m4,
                  cms2015m5,
                  cms2015m6,
                  cms2015m7,
                  cms2015m8,
                  cms2015m9,
                  cms2015m10,
                  cms2015m11,
                  cms2015m12,
                  cms2016m1,
                  cms2016m2,
                  cms2016m3,
                  cms2016m4,
                  cms2016m5,
                  cms2016m6,
                  cms2016m7,
                  cms2016m8,
                  cms2016m9,
                  cms2016m10,
                  cms2016m11,
                  cms2016m12,
                  cms2017m1,
                  cms2017m2,
                  cms2017m3,
                  cms2017m4,
                  cms2017m5,
                  cms2017m6,
                  cms2017m7,
                  cms2017m8,
                  cms2017m9,
                  cms2017m10,
                  cms2017m11,
                  cms2017m12,
                  cms2018m1,
                  cms2018m2,
                  cms2018m3,
                  cms2018m4,
                  cms2018m5,
                  cms2018m6,
                  cms2018m7,
                  cms2018m8,
                  cms2018m9,
                  cms2018m10,
                  cms2018m11,
                  cms2018m12,
                  cms2019m1,
                  cms2019m2,
                  cms2019m3,
                  cms2019m4,
                  cms2019m5,
                  cms2019m6,
                  cms2019m7,
                  cms2019m8,
                  cms2019m9,
                  cms2019m10,
                  cms2019m11,
                  cms2019m12)

cmsdf <- data.table::rbindlist(cmsdflist)

rm(cms2015m1,
   cms2015m2,
   cms2015m3,
   cms2015m4,
   cms2015m5,
   cms2015m6,
   cms2015m7,
   cms2015m8,
   cms2015m9,
   cms2015m10,
   cms2015m11,
   cms2015m12,
   cms2016m1,
   cms2016m2,
   cms2016m3,
   cms2016m4,
   cms2016m5,
   cms2016m6,
   cms2016m7,
   cms2016m8,
   cms2016m9,
   cms2016m10,
   cms2016m11,
   cms2016m12,
   cms2017m1,
   cms2017m2,
   cms2017m3,
   cms2017m4,
   cms2017m5,
   cms2017m6,
   cms2017m7,
   cms2017m8,
   cms2017m9,
   cms2017m10,
   cms2017m11,
   cms2017m12,
   cms2018m1,
   cms2018m2,
   cms2018m3,
   cms2018m4,
   cms2018m5,
   cms2018m6,
   cms2018m7,
   cms2018m8,
   cms2018m9,
   cms2018m10,
   cms2018m11,
   cms2018m12,
   cms2019m1,
   cms2019m2,
   cms2019m3,
   cms2019m4,
   cms2019m5,
   cms2019m6,
   cms2019m7,
   cms2019m8,
   cms2019m9,
   cms2019m10,
   cms2019m11,
   cms2019m12, cmsdflist)
gc()

# 3) Plot trends -----

cmsdf <- cmsdf %>% 
  group_by(YEARMONTH) %>% 
  mutate(MonthlyTotal = sum(as.numeric(Enrollment))) %>% 
  ungroup() %>% 
  mutate(Share = Enrollment/MonthlyTotal)

ggplot(data=cmsdf)+
  geom_line(aes(x=YEARMONTH, y = Share, group=PlanType, color = PlanType), size = 1.5)+
  labs(x = 'Year-Month', y = 'Share of Total Monthly Enrollment', 
       title = 'Enrollment in Medicare Advantage by Plan Type, 2015-2019',
       subtitle = 'Source: CMS Medicare Advantage Contract and Enrollment Data')+
  scale_color_brewer(name = "Plan Type", palette = "Set1") + 
  theme_bw()+
  scale_x_discrete(breaks=c("2015-06","2016-06","2017-06","2018-06","2019-06"))+
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size=12),
        title = element_text(size = 12),
        legend.position = c(0.85,0.70),
        legend.text=element_text(size=12),
        legend.background = element_rect(fill="papayawhip",
                                         size=0.5, linetype="solid", 
                                         colour ="black"))
ggsave("CMS Monthly Enrollment by Contract/cmsenrollment_plantype.pdf",width = 9, height = 6.5)


# 4) Run it back but for Organization Type -----


cms2015m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-01")

cms2015m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-02")

cms2015m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-03")

cms2015m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-04")

cms2015m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-05")

cms2015m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-06")

cms2015m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-07")

cms2015m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-08")

cms2015m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-09")

cms2015m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-10")

cms2015m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-11")

cms2015m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2015_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2015-12")

cms2016m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-01")

cms2016m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-02")

cms2016m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-03")

cms2016m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-04")

cms2016m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-05")

cms2016m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-06")

cms2016m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-07")

cms2016m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-08")

cms2016m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-09")

cms2016m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-10")

cms2016m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-11")

cms2016m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2016_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2016-12")

cms2017m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-01")

cms2017m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-02")

cms2017m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-03")

cms2017m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-04")

cms2017m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-05")

cms2017m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-06")

cms2017m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-07")

cms2017m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-08")

cms2017m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-09")

cms2017m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-10")

cms2017m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-11")

cms2017m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2017_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2017-12")

cms2018m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-01")

cms2018m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-02")

cms2018m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-03")

cms2018m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-04")

cms2018m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-05")

cms2018m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-06")

cms2018m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-07")

cms2018m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-08")

cms2018m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-09")

cms2018m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-10")

cms2018m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-11")

cms2018m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2018_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2018-12")

cms2019m1 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_01.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-01")

cms2019m2 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_02.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-02")

cms2019m3 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_03.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-03")

cms2019m4 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_04.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-04")

cms2019m5 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_05.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-05")

cms2019m6 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_06.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-06")

cms2019m7 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_07.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-07")

cms2019m8 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_08.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-08")

cms2019m9 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_09.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-09")

cms2019m10 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_10.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-10")

cms2019m11 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_11.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-11")

cms2019m12 <- import("CMS Monthly Enrollment by Contract/Monthly_Report_By_Contract_2019_12.csv") %>%
  as_tibble() %>% rename("Organization Type" = "OrganizationType", "Plan Type" = "PlanType") %>% 
  filter(OrganizationType %in% c("Local CCP", "MSA", "PFFS", "Regional CCP")) %>% 
  group_by(OrganizationType) %>% 
  summarize(Enrollment = sum(as.numeric(Enrollment), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(YEARMONTH = "2019-12")

cmsdflist <- list(cms2015m1,
                  cms2015m2,
                  cms2015m3,
                  cms2015m4,
                  cms2015m5,
                  cms2015m6,
                  cms2015m7,
                  cms2015m8,
                  cms2015m9,
                  cms2015m10,
                  cms2015m11,
                  cms2015m12,
                  cms2016m1,
                  cms2016m2,
                  cms2016m3,
                  cms2016m4,
                  cms2016m5,
                  cms2016m6,
                  cms2016m7,
                  cms2016m8,
                  cms2016m9,
                  cms2016m10,
                  cms2016m11,
                  cms2016m12,
                  cms2017m1,
                  cms2017m2,
                  cms2017m3,
                  cms2017m4,
                  cms2017m5,
                  cms2017m6,
                  cms2017m7,
                  cms2017m8,
                  cms2017m9,
                  cms2017m10,
                  cms2017m11,
                  cms2017m12,
                  cms2018m1,
                  cms2018m2,
                  cms2018m3,
                  cms2018m4,
                  cms2018m5,
                  cms2018m6,
                  cms2018m7,
                  cms2018m8,
                  cms2018m9,
                  cms2018m10,
                  cms2018m11,
                  cms2018m12,
                  cms2019m1,
                  cms2019m2,
                  cms2019m3,
                  cms2019m4,
                  cms2019m5,
                  cms2019m6,
                  cms2019m7,
                  cms2019m8,
                  cms2019m9,
                  cms2019m10,
                  cms2019m11,
                  cms2019m12)

cmsdf <- data.table::rbindlist(cmsdflist)

rm(cms2015m1,
   cms2015m2,
   cms2015m3,
   cms2015m4,
   cms2015m5,
   cms2015m6,
   cms2015m7,
   cms2015m8,
   cms2015m9,
   cms2015m10,
   cms2015m11,
   cms2015m12,
   cms2016m1,
   cms2016m2,
   cms2016m3,
   cms2016m4,
   cms2016m5,
   cms2016m6,
   cms2016m7,
   cms2016m8,
   cms2016m9,
   cms2016m10,
   cms2016m11,
   cms2016m12,
   cms2017m1,
   cms2017m2,
   cms2017m3,
   cms2017m4,
   cms2017m5,
   cms2017m6,
   cms2017m7,
   cms2017m8,
   cms2017m9,
   cms2017m10,
   cms2017m11,
   cms2017m12,
   cms2018m1,
   cms2018m2,
   cms2018m3,
   cms2018m4,
   cms2018m5,
   cms2018m6,
   cms2018m7,
   cms2018m8,
   cms2018m9,
   cms2018m10,
   cms2018m11,
   cms2018m12,
   cms2019m1,
   cms2019m2,
   cms2019m3,
   cms2019m4,
   cms2019m5,
   cms2019m6,
   cms2019m7,
   cms2019m8,
   cms2019m9,
   cms2019m10,
   cms2019m11,
   cms2019m12, cmsdflist)
gc()

# 5) Plot trends (Figure 1) -----

cmsdfavg <- cmsdf %>% 
  group_by(YEARMONTH) %>% 
  mutate(MonthlyTotal = sum(as.numeric(Enrollment))) %>% 
  ungroup() %>% 
  mutate(Share = Enrollment/MonthlyTotal)

ggplot(data=cmsdfavg)+
  geom_hline(yintercept=max(cmsdfavg$Share), linetype="dotted")+
  geom_line(aes(x=YEARMONTH, y = Share, group=OrganizationType, color = OrganizationType), size = 1.5)+
  annotate("text", label = "93.9%", fontface = "bold.italic",
           x = "2015-04", y = 0.96, size = 4)+
  labs(x = 'Year-Month', y = 'Share of Total Monthly Enrollment', 
       title = 'Enrollment in Medicare Advantage by Organization Type, 2015-2019',
       subtitle = 'Source: CMS Medicare Advantage Contract and Enrollment Data')+
  scale_color_brewer(name = "Organization Type", palette = "Set1") + 
  theme_bw()+
  scale_x_discrete(breaks=c("2015-06","2016-06","2017-06","2018-06","2019-06"))+
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size=12),
        title = element_text(size = 12),
        legend.position = c(0.85,0.70),
        legend.text=element_text(size=12),
        legend.background = element_rect(fill="papayawhip",
                                         size=0.5, linetype="solid", 
                                         colour ="black"))
ggsave("CMS Monthly Enrollment by Contract/cmsenrollment_orgtype.pdf",width = 9, height = 6.5)


# 6) Clean Up -----

rm(list=ls())
gc()
