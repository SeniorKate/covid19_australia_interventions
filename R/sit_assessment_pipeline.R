
#Situational assessment data preparation and reff model pipeline

####PART 1 - Prepare all data for the weekly situational assessment report ####

source("R/lib.R")

source("R/functions.R")

#Section A) Macrodistancing, microdistancing and survey data analysis
#Dependent on survey data update (the numbered file must be manually
# copied to data/survey_raw):
# this usually comes on Monday/Tuesday so do this first

# 1. parse all contact rate data, automatically remove duplicates, and visually
# check for duplicate ages [~60s]
source("R/parse_covid_doh_surveys.R")
# will light up cells with any overly large counts. If this happens check raw data

# 2. Microdistancing [~20 min]
#Run microdistancing model and output the following on Mediaflux (by Thursday night)
#   - 3 microdistancing figures 
#   - data/microdistancing/Barometer wave <wave> compliance.csv
#   - data/contacts/barometer/contact numbers wave <wave>.csv) and face_covering
# If microdistancing GAM fails to converge, change second optimiser term until it does
source("R/microdistancing_change.R")

# 3. Macrodistancing [~10 min]
#Run macrodistancing model and output the following on Mediaflux (by Thursday night)
#   - three macrodistancing figures 
source("R/macrodistancing_change.R")

# 4. Run test seeking behaviour and rat reporting analysis and output the following on Mediaflux
#   - 11 csv file outputs (before Thursday - Rob needs some of these for his models)
#   - Survey analysis figures (by Thursday night)
source("R/survey_analysis_slack.R")

# 5. Post the following figures on Slack by Thursday morning with any notes on survey data issues: 
#   - at_least_one_sym_states.png, at_least_one_core_states.png
#   - at_least_one_sym_states_testtype_PCR_only.png, at_least_one_core_states_testtype_PCR_only.png
#   - at_least_one_sym_states_testtype_RAT_only.png, at_least_one_core_states_testtype_RAT_only.png
#   - macrodistancing_effect.png, microdistancing_effect.png
#   - report_positive_rat.png, report_positive_rat_nsw_vic_qld.png

#Section B) Prepare linelist and/or state summary data for modelling  - Thursday morning
# Dependent on NNDSS, NSW and Victoria data updates on Thursday morning (usually provided by 10am:

# 1. Read in and process NINDSS data and (optionally) linelists from NSW and Victoria
# read in Victorian summary data if using instead of linelist and replace relevant bits
source("R/read_in_weekly_linelist_data.R")

# 2. Fix data errors within linelist and output processed linelist
# IMPORTANT - run line by line to check plots and make changes as you go
source("R/fix_linelist_errors.R")

### PART 2 - REFF model and associated tasks

#run script to check completion probability delay is acceptable
#at the moment need to manually change date of most recent linelist and add in new ones every week
source("R/explore_reporting.R")

# hooray - you're done!
