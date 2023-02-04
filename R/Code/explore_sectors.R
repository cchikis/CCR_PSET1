#################################################################
# Filename: explore_sectors.R
# Author: Craig A. Chikis
# Date: 01/22/2023
# Note(s):
#################################################################
rm(list = ls(all.names = TRUE))
gc()

# You might need to change this
# setwd("")

packages = c("haven", "fixest", "Rfast", "sf", "quantmod", "readxl",
             "cowplot", "tigris", "lubridate", "tidyverse")

new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new_packages)

library(haven)
library(fixest)
library(Rfast)
library(sf)
library(quantmod)
library(readxl)
library(cowplot)
library(tigris)
library(lubridate)
library(tidyverse)

# Load ipums
acs0 = read_dta("Data/usa_00007.dta")

# Select what I need
acs_work = acs0 %>%
    filter(as.integer(year) == 2019L) %>%
    filter(as.character(sample) == "201901") %>%
    select(sample, serial, pernum, hhwt, perwt, statefip, countyfip, educ, pwcounty, pwstate2, incwage, race, age) %>%
    arrange(sample, serial, pernum) %>%
    group_by(sample, serial) %>%
    mutate(id = row_number()) %>%
    ungroup()

if (any(acs_work$pernum != acs_work$id)) { 
    stop("You don't know what you're doing.")
} else {
    acs_work = acs_work %>%
        select(-id)
}

acs_work = acs_work %>%
    mutate(res = paste0(statefip, "-", countyfip),
           work = paste0(pwstate2, "-", pwcounty)) %>%
    arrange(res, work) 

# Just need Chicago and Cook County
acs_work_chicago = acs_work %>%
    filter(res == "17-31" & work == "17-31")

summary_acs = function(acs_work) {
    # College/noncollege \times race
    race_educ = acs_work %>%
        filter(age >= 25) %>%
        mutate(educ_new = ifelse(educ >= 7, "College", "No College")) %>%
        group_by(sample, race) %>%
        summarise(frac_college = sum(perwt/sum(perwt, na.rm = TRUE) * (educ_new == "College"), na.rm = TRUE),
                frac_nocollege = sum(perwt/sum(perwt, na.rm = TRUE) * (educ_new == "No College"), na.rm = TRUE),
                frac_total = sum(perwt/sum(perwt, na.rm = TRUE))) %>%
        ungroup()

    # College/noncollege \times race
    educ = acs_work %>%
        filter(age >= 25) %>%
        mutate(educ_new = ifelse(educ >= 7, "College", "No College")) %>%
        group_by(sample) %>%
        summarise(frac_college = sum(perwt/sum(perwt, na.rm = TRUE) * (educ_new == "College"), na.rm = TRUE),
                frac_nocollege = sum(perwt/sum(perwt, na.rm = TRUE) * (educ_new == "No College"), na.rm = TRUE),
                frac_total = sum(perwt/sum(perwt, na.rm = TRUE))) %>%
        ungroup()


    # Breakdown between college and noncollege educated workers the average wage
    acs_work = acs_work %>%
        filter(age >= 25) %>%
        mutate(educ_new = ifelse(educ >= 7, "College", "No College")) %>%
        group_by(educ_new) %>%
        mutate(wt = perwt / sum(perwt, na.rm = TRUE)) %>% 
        ungroup()

    acs_work_2 = acs_work %>%
        group_by(educ_new) %>%
        summarise(avg_wage = sum( (perwt/sum(perwt, na.rm = TRUE)) * incwage, na.rm = TRUE),
                avg_wage_2 = weighted.mean(incwage, w = wt)) %>%
        ungroup()

    acs_work_3 = acs_work %>%
        summarise(avg_wage = sum( (perwt/sum(perwt, na.rm = TRUE)) * incwage, na.rm = TRUE))

    ratio_educ_noeduc = acs_work_2$avg_wage[acs_work_2$educ_new == "College"] / acs_work_2$avg_wage[acs_work_2$educ_new == "No College"] 

    return(list(acs_work_2 = acs_work_2, acs_work_3 = acs_work_3, ratio_educ_noeduc = ratio_educ_noeduc, 
                race_educ = race_educ, educ = educ))


}
# ACS results
acs_ret = map(.x = list(natl = acs_work, chicago = acs_work_chicago), .f = summary_acs)