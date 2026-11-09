# Script with instructions on downloading and formatting Citizen Connect alerts data from
# https://cua911.gov/transparency/

# Setup ----
library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)

# Incidents ----
# Download instructions: 
# 1. Navigate to CUA911 transparency site with incident map (https://cua911.gov/transparency/)
# 2. Select filter criteria:
#   - Date range: 01/01/2018 - 09/30/2026
#   - Agencies: Charlottesville Police Department, Albemarle Police Department, UVA Police Department
#   - Incident criteria: 
#     - Assault with weapon
#     - Disorder with weapon
#     - Domestic with weapon
#     - Gunshot wound
#     - Mental person with weapon
#     - Robbery with weapon
#     - Shooting/stabbing
#     - Shots fired

incidents <- read_excel("data/raw/cua/incidents_details20261026.xlsx") %>%
  clean_names() %>%
  mutate(date = mdy(str_extract(incident_date_time, "^[A-Za-z]+ \\d{1,2}, \\d{4}")),
         incident_id = str_remove(incident_number, "-")) %>%
  select(-incident_date_time, -incident_number) %>%
  mutate(agency_name = str_to_title(agency_name),
         type = "incident")

# Visualize shots fired
# Combined across all divisions: 
incidents %>%
  filter(category == "Shots Fired") %>%
  mutate(month_year = floor_date(date, unit = "month")) %>%
  count(month_year, name = "incidents") %>%
  arrange(month_year) %>%
  ggplot(aes(month_year, incidents)) +
  geom_line() +
  labs(title = "All Shots-Fired 911 Calls")

incidents %>% 
  filter(category == "Shots Fired") %>%
  mutate(month_year = floor_date(date, unit = "month")) %>%
  group_by(agency_name) %>%
  count(month_year, name = "incidents") %>%
  ggplot(aes(month_year, incidents, color = agency_name)) +
  geom_line() +
  theme(legend.position = "bottom") +
  labs(title = "Shots-Fired 911 Calls by Department")

  
# Cases ----
# - NOTE 1: you will need to download each department's data separately, otherwise it isn't included 
# - NOTE 2: the agencies differ in how they report CUA data, see below
# - NOTE 3: weapons violations and homicide-murder are the only case categories that span Cville, Alb, and UVA 

## Albemarle ----
# Download instructions: 
# 1. Navigate to CUA911 transparency site with incident map (https://cua911.gov/transparency/)
# 2. Select filter criteria:
#   - Date range: 01/01/2018 - 09/30/2026
#   - Agencies: Albemarle Police Department
#   - Case criteria: 
#     - Assault Aggravated 
#     - Homicide-murder/non-negligent manslaughter
#     - Shots fired / illegal hunting 
#     - Weapons violations 

cases_alb <- read_excel("data/raw/cua/alb_cases_details20261105.xlsx") %>%
  clean_names() %>%
  mutate(date = mdy(str_extract(reported_date_time, "^[A-Za-z]+ \\d{1,2}, \\d{4}")),
         agency_name = "Albemarle County Police Department") %>%
  select(-reported_date_time)

## Charlottesville ----
# Download instructions: 
# 1. Navigate to CUA911 transparency site with incident map (https://cua911.gov/transparency/)
# 2. Select filter criteria:
#   - Date range: 01/01/2018 - 09/30/2026
#   - Agencies: Charlottesville Police Department
#   - Case criteria: 
#     - Assault Aggravated (0 cases reported, might need to look at alternative data sources)
#     - Homicide-murder/non-negligent manslaughter 
#     - Weapons violations 
#     - Attempted Suicide
#     - Suicide Investigation
#     - Death Investigation - DOA 
 
cases_cvl <- read_excel("data/raw/cua/cvl_cases_details20261105.xlsx") %>%
  clean_names() %>%
  mutate(date = mdy(str_extract(reported_date_time, "^[A-Za-z]+ \\d{1,2}, \\d{4}")),
         agency_name = "Charlottesville City Police Department") %>%
  select(-reported_date_time)

## UVA ----
# Download instructions: 
# 1. Navigate to CUA911 transparency site with incident map (https://cua911.gov/transparency/)
# 2. Select filter criteria:
#   - Date range: 01/01/2018 - 09/30/2026
#   - Agencies: UVA Police Department
#   - Case criteria: 
#     - Assault Aggravated 
#     - Shots fired / illegal hunting
#     - Weapons violations 

cases_uva <- read_excel("data/raw/cua/uva_cases_details20261105.xlsx") %>%
  clean_names() %>%
  mutate(date = mdy(str_extract(reported_date_time, "^[A-Za-z]+ \\d{1,2}, \\d{4}")),
         agency_name = "University Of Virginia Police Department") %>%
  select(-reported_date_time)

# Combine and save ----

cases <- bind_rows(cases_alb, cases_cvl, cases_uva) %>%
  mutate(type = "case")

cua <- bind_rows(cases, incidents) 

write_csv(cua, "data/cua.csv")

# Visualize offense types:
cases %>%
  filter(category %in% c("Assault Aggravated", "Homicide-murder/non-negligent", "Shots Fired/Illegal Hunting", "Weapons Violations")) %>%
  mutate(month_year = floor_date(date, unit = "month")) %>%
  group_by(category) %>%
  count(month_year, name = "cases") %>% 
  ggplot(aes(month_year, cases, color = category)) + 
  geom_smooth(se = 0)
