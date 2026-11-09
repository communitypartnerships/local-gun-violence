# Script with instructions on downloading and formatting CDC WONDER data from
# https://wonder.cdc.gov/

# Setup ----
library(janitor)
library(lubridate)
library(readxl)
library(tidycensus)
library(tidyverse)

# Suicide in Charlottesville and Albemarle ----
# Download instructions: 
# 1. Navigate to CDC WONDER Underlying Cause of Death data query page: https://wonder.cdc.gov/deaths-by-underlying-cause.html
# 2. Select filter criteria
#   - Database: Underlying Cause of Death, Single Race Results
#   - Date range: 2018-2024
#   - Localities: Charlottesville City, Albemarle County 
#   - Group by: Injury Intent; Year
#   - ICD-10 codes: 
#       - U01.4 (Terrorism involving firearms)
#       - W20 (Struck by thrown, projected or falling object)
#       - W32 (Handgun discharge)
#       - W33 (Rifle, shotgun and larger firearm discharge)
#       - W34 (Discharge from other and unspecified firearms) 
#       - X72	(Intentional self-harm by handgun discharge)
#       - X73 (Intentional self-harm by rifle, shotgun and larger firearm discharge)
#       - X74	(Intentional self-harm by other and unspecified firearm discharge)
#       - X93 (Assault by handgun discharge)
#       - X94 (Assault by rifle,shotgun and larger firearm discharge)
#       - X95 (Assault by other and unspecified firearm discharge)
#       - Y22 (Handgun discharge, undetermined intent)
#       - Y23 (Rifle, shotgun and larger firearm discharge, undetermined intent)
#       - Y24 (Other and unspecified firearm discharge, undetermined intent)
#       - Y35.0 (Legal intervention involving firearm discharge)
#       - Y36.4 (War operations involving firearm discharge and other forms of conventional warfare)					

local_suicides <- read_excel("data/raw/cvl_Underlying Cause of Death, 2018-2024, Single Race.xlsx", n_max = 6)	%>%
  clean_names() %>%
  select(-notes, -injury_intent_code, -year_code) %>%
  mutate(locality = "Charlottesville and Albemarle",
         deaths = as.character(deaths))

# 2019 values are suppressed (1-9) so we need to manually calculate the upper and lower limits and input back

combined_pop_2019 <- 156596
# Note this number comes directly from the CDC report. If we calculate it using the ACS-5 then
# it's ~2k lower that what the CDC reported. So for consistency we'll work with the 
# CDC-provided pop estimates. 

local_suicides <- local_suicides %>% add_row(
  injury_intent = "Suicide",
  year = 2019, 
  deaths = "1-9", 
  population = combined_pop_2019,
  crude_rate = NA, 
  crude_rate_lower_95_percent_confidence_interval = round((1 / combined_pop_2019) * 100000, 1),
  crude_rate_upper_95_percent_confidence_interval = round((9 / combined_pop_2019) * 100000, 1),
  locality = "Charlottesville and Albemarle")

# Visualize suicide rates over time
local_suicides %>%
  ggplot(aes(year, crude_rate)) +
  geom_point() +
  geom_line()

