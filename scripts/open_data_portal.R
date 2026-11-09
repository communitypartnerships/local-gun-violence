# Script with instructions on downloading and formatting Charlottesville Open Data Portal data
# https://opendata.charlottesville.org/

# Setup ----
library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)

# Incidents ----
# Download instructions: 
# 1. Navigate to Cville ODP Crime dataset: https://opendata.charlottesville.org/datasets/d1877e350fad45d192d233d2b2600156_6/explore
# 2. Select filter criteria:
#   - Date range: as wide as available, currently 11/7/2021 - 11/4/2026
#   - Agencies: Charlottesville Police Department
#   - Incident criteria: 
#     - Shots fired/illegal hunting
#     - Aggravated assault
#     - Homicide/murder
#     - Death Investigation - DOA
#     - Suicide Investigation
#     - Suicide Attempt
#     - Weapons violations

incidents <- read_csv("data/raw/Crime_Data (1).csv") %>%
  clean_names() %>%
  mutate(incident_id = format(incident_id, scientific = FALSE), 
         agency_name = "Charlottesville Police Department",
         date = as.Date(ymd_hms(date_reported)),
         type = "incident") %>%
  select(category = offense, incident_id, block_number, street_name, agency_name, date, type)

# Visualize categories over time
incidents %>%
  mutate(month_year = floor_date(date, unit = "month")) %>%
  group_by(category) %>%
  count(month_year, name = "incidents") %>% 
  ggplot(aes(month_year, incidents, color = category)) +
  geom_smooth(se = 0)

# Arrests ----
# # Download instructions: 
# 1. Navigate to Cville ODP Arrests dataset: https://opendata.charlottesville.org/datasets/d558ab0e09fe4f509280bedf6f8793ed_22/explore
# 2. Select filter criteria:
#   - Date range: as wide as available, currently 11/9/2021 - 11/8/2026
#   - Agencies: Charlottesville Police Department
#   - Statute description: 
#     - Armed robbery with a firearm
#     - Carry Concealed Weapon (1st and 2nd offenses)
#     - Carry Concealed Firearm
#     - Possession of any other illegal weapon/firearm
#     - Weapons Law Violations
#     - Simultaneous possession of weapon and narcotic
#     - Weapon: shoot across road or street
#     - Violation of protective order armed w/deadly weapon
#     - Discharge firearm, missile, in/att occ./school
#     - Firearm: purchase/transport firearm while protection order in effect
#     - Firearm/etc: pointing/brandishing
#     - Firearm: allow child <12 to use w/o adult supervision
#     - Firearm: poss/transport by felon w/violent offense
#     - Firearm: possess at school/etc
#     - Firearm/etc: pointing/brandishing on/near school
#     - Firearm: possess by felon nonviolent w/in 10 years
#     - Firearm: possess on/about person w/sch I/II drug
#     - Firearm: possess wile selling 1+ lb marijuana
#     - Firearm: possess by person <18Y
#     - Firearm: possession w/sch I or II drug
#     - Firearm: receive stolen or aid in concealing
#     - Firearm: possess by non violent felon >10 yrs
#     - Firearm: reckless handling
#     - Firearm: remove/alter serial numbers
#     - Firearm: shoot from vehicles
#     - Firearm: shoot in public place, not cause injury
#     - Firearm: school on school grounds
#     - Firearm: shoot w/in 1000 ft of school
#     - Firearm: use in commission of felony (1st and 2nd offenses)
#     - Firearm: reckless handling causes serious injury
#     - Firearm: purchase/transport by protective order subject
#     - Carry handgun in public under the influence
#     - Machine gun: possess/use for aggressive purpose
#     - Gun dealer: unlawful transfer
#     - Assault: shoot, stab, etc. in commission of a felony
#     - Maliciously shoot/throw object in/at dwelling


arrests <- read_csv("data/raw/Arrests.csv") %>%
  clean_names()




