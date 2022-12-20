#--------------------
# Objective: PPHA 30536 Final Project-Data Cleaning
# Date: 29th Nov, 2022
#--------------------
  
# Clear Global Environment
rm(list = ls())
options(
  scipen = 999,
  digits = 3
)
# Setting the Working Directory
setwd("~/Desktop/Fall Quarter/PPHA-30536/Final Project/final-project-weiluj")

# Load packages
library(readr)
library(data.table)
library(haven)
library(tidycensus) # Get Census Data
library(dataverse) # API download data form Dataverse
library(fredr) # API download data from St Louis Fed
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)

# 1. Data Cleaning
## 1.1 US-Current Population Survey
### 1.1.1 Load Data
# Load Data
files <- list.files(pattern = "*.dta")
data_list <- lapply(files, read_dta)
names(data_list) <- str_replace(files, "\\..*", "")
# Clean Data
i <- 1
for (i in 1:length(data_list)) {
  data_list[[i]] <- data_list[[i]] %>%
    # Filter target population
    filter(
      prcitshp %in% 1:3 & age >= 16 # US native citizens older than 16
      & ftpt94 %in% 2:4 & lfsr94 == 1 # employed usually full time workers
      & class94 < 6 # exclude self-employed individuals
      & uhourse > 0 # working hours should be positive
      & race <= 5 # remove multiracial individual samples
      & ownchild <= 3 # remove outliers
    ) %>%
    # Select informative columns
    select(
      stfips, # geo location
      age, sex, race, grade92, marital, ownchild, # personal traits
      uhourse, earnwke, # earnings
      class94, dind02, docc00 # sector, industry, occupation
    ) %>%
    # Format column classes
    mutate(across(c(age, grade92, uhourse), as.numeric),
           age_fct = factor(
             as.factor(ifelse(
               age <= 24, # Age 16-24
               1,
               ifelse(
                 age <= 34, # Age 25-34
                 2,
                 ifelse(
                   age <= 44, # Age 35-44
                   3,
                   ifelse(age <= 54, # Age 45-54
                          4,
                          ifelse(age <= 64, # Age 55-64
                                 5,
                                 6 # Age 65 and Older
                          )
                   )
                 )
               )
             )),
             labels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65 and older")
           ),
           sex = factor(
             as.factor(sex),
             labels = c(
               "Male", # Male = 1
               "Female" # Female = 2
             )
           ),
           grade92_fct = factor(
             as.factor(
               ifelse(grade92 <= 38, # Less than High school
                      1,
                      ifelse(grade92 == 39, # High school or equivalent GED
                             2,
                             ifelse(grade92 %in% 40:42, # Some college or associate's degree
                                    3,
                                    ifelse(grade92 == 43, # Bachelor degree
                                           4,
                                           ifelse(grade92 == 44, # Master's degree
                                                  5,
                                                  6 # Advanced degree, including professional and PhD
                                           ) 
                                    )
                             )
                      )
               )
             ),
             labels = c(
               "Less than High school",
               "High school or equivalent GED",
               "Some college or associate's degree",
               "Bachelor degree",
               "Master degree",
               "Advanced degree"
             )
           ),
           race = factor(
             as.factor(ifelse(race == 5, # Hawaiian/Pacific Islander
                              4, # Change to Asian and Pacific Islander category
                              race
             )),
             labels = c(
               "White",
               "Black",
               "American Indian/Alaskan Native",
               "Asian/Pacific Islanders"
             )
           ),
           marital = factor(
             as.factor(ifelse(marital == 7, # Never Married
                              1,
                              ifelse(marital %in% 1:3, # Married
                                     2,
                                     3 # Widowed/Divorced
                              )
             )),
             labels = c(
               "Never Married",
               "Married",
               "Widowed/Divorced"
             )
           ),
           ownchild_fct = factor(
             as.factor(ifelse(ownchild >= 1, # with children
                              1,
                              0 # without children
             )
             ),
             labels = c(
               "Have Children",
               "No Child"
             )
           )
    )
  i <- i + 1
}

### 1.1.2 Merge and Check dataset
# Merge
merged_cps <- rbindlist(data_list, idcol = "index")
# Check dataset
glimpse(merged_cps) # data structure
# Check missing values
colSums(is.na(merged_cps))
#Missing number of earnings per week
merged_cps <- merged_cps %>%
  drop_na(earnwke) # drop all data with missing earnings
# Format Year Index
merged_cps <- merged_cps %>%
  mutate(year = as.numeric(str_c("20", gsub("morg", "", index)))) %>% # remove "morg", add "20"
  select(-index)

# Write dataset to csv
write_csv(merged_cps, "merged_cps.csv")

## 1.2 State Level Socioeconomic Data
### 1.2.1 Partisanship by Presidential Election Data
# Download data with API
partisanship <- get_dataframe_by_name(
  filename = "1976-2020-president.tab",
  dataset = "10.7910/DVN/42MVDX",
  server = "dataverse.harvard.edu"
)
# Clean data
glimpse(partisanship) # Check data
partisanship <- partisanship %>%
  filter(between(year, 2000, 2020)) %>% # select data from 2000 to 2020
  mutate(
    state_name = str_to_title(state),
    party = str_to_title(party_simplified)
  ) %>%
  group_by(year, state_name) %>%
  top_n(1, candidatevotes) %>% # find winner of each year in each state
  select(year, state_name, state_po, state_fips, party) %>%
  rename(
    state = "state_po", # rename for merging purpose
    stfips = "state_fips"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = party
  )
# Calculate political index
partisanship <- rowSums(partisanship == "Democrat") %>%
  cbind(partisanship) %>%
  pivot_longer(`2000`:`2020`,
               names_to = "year",
               names_transform = list(year = as.numeric),
               values_to = "party") %>%
  rename(party_index = "...1") %>%
  mutate(party_fct = ifelse(
    party_index > 3,
    "Dem",
    ifelse(party_index < 3,
           "Rep",
           "Neutral")
  )
  ) %>%
  group_by(state) %>%
  fill(party_index) %>%
  select(year, state_name:stfips, everything())
# Write to csv
write_csv(partisanship, "partisanship_clean.csv")

### 1.2.2 State Level Economic Data
# us state basic info
fips_codes <- fips_codes %>%
  distinct(state, .keep_all = TRUE) %>%
  select(-contains("county")) %>% # keep state level info only
  filter(!state_code %in% c(60:78)) # remove states outside of continental states

# Get GDP, Population, Unemployment Rate and Poverty Rate data from Fed
fredr_set_key("2d9fa20dc68845c8b94dadaf8fad04b3")
get_state <- function(stfips, st) {
  s <- map_dfr(
    c(
      paste0(st, "NGSP"), # GDP, millions dollar
      paste0(st, "POP"), # Population, thousands
      paste0("LAUST", stfips, "0000000000003A"), # Unemployment Rate
      paste0("PPAA", st, stfips, "000A156NCEN") # Poverty Rate
    ),
    fredr
  )
}

data <- list()
i <- 1
for (i in 1:nrow(fips_codes)) {
  data[[i]] <- get_state(fips_codes$state_code[i], fips_codes$state[i])
  i <- i + 1
}

# Combine data list elements to data frame
state_gdpum <- rbindlist(data, idcol = "index") %>%
  filter(between(date, as.Date("2000-01-01"), as.Date("2020-01-01"))) %>% # select time
  select(-contains("time")) %>%
  separate(series_id, into = c("state", "variable"), sep = 2) %>%
  mutate(
    year = year(date),
    state = ifelse(
      variable %in% c("NGSP", "POP"),
      state,
      NA
    )
  ) %>%
  arrange(index, state) %>% # NA values will always at the bottom within group
  group_by(index) %>%
  fill(state) %>% # replace na values with the same state name in the same group
  mutate(index = case_when(
    variable == "NGSP" ~ "gdp",
    variable == "POP" ~ "pop",
    grepl("AA", variable) ~ "poverty_rate",
    grepl("UST", variable) ~ "unemp_rate"
  )) %>%
  select(-variable, -date) %>%
  pivot_wider(
    names_from = index,
    values_from = value
  ) %>%
  mutate(gdp_per_capita = gdp/pop*1000)
# Write to csv
write_csv(state_gdpum, "state_gdp_umeployment.csv")

### 1.2.3 State Level Fertility Data
# Load fertility data
files <- list.files(pattern = "*.txt")
data_list <- lapply(files, read_delim, show_col_types = FALSE)
names(data_list) <- parse_number(files)
# Clean data
i <- 1
for (i in 1:length(data_list)){
  data_list[[i]] <- data_list[[i]] %>%
    select(State, `State Code`, `Fertility Rate`) %>%
    na.omit()
  i <- i + 1
}
# Merge dataset
state_fertility <- rbindlist(data_list, idcol = "index")
# Pivot data to tidy format
state_fertility <- state_fertility %>%
  mutate(
    year = as.numeric(index),
    stfips = as.numeric(`State Code`)
  ) %>%
  rename(
    state_name = "State",
    fertility_rate = `Fertility Rate`
  ) %>%
  select(year, state_name, stfips, fertility_rate) %>%
  left_join(fips_codes, by = "state_name") %>%
  select(-state_code)
# Write to csv
write_csv(state_fertility, "state_fertility_03_20.csv")

### 1.2.4 Merge all socioeconomic data
# Merge state level socioeconomic data
state_socioecon <- state_gdpum %>%
  left_join(state_fertility, by = c("state", "year")) %>%
  left_join(partisanship, by = c("state", "year")) %>%
  select(-ends_with(c(".x", ".y"))) %>%
  left_join(fips_codes, by = "state") %>%
  group_by(state) %>%
  mutate(state_code = as.numeric(state_code),
         avg_fertility = mean(fertility_rate, na.rm = T),
         avg_gdp_per_capita = mean(gdp_per_capita)) %>%
  select(state_code, state_name, everything())
# Write to csv
write_csv(state_socioecon, "state_socioeconomic_index.csv")

## 1.3 Export Final data
merged_cps <- merged_cps %>%
  left_join(state_socioecon,
            by = c("year", "stfips" = "state_code")) %>%
  mutate(party = as_factor(party)) %>%
  select(year, state_name, state, stfips, age, sex, race, grade92, marital,
         ownchild, age_fct:grade92_fct, uhourse, earnwke, class94:dind02, gdp,
         gdp_per_capita, unemp_rate, poverty_rate, fertility_rate, pop, everything())
# Write to csv
write_csv(merged_cps, "merged_data.csv")

## 1.4 Basic Data Checking
# Top cases
top_cases <- vector("list")
get_top_cases <- function(df){
  for (i in names(df)){
    top_cases[[i]] <- df %>%
      group_by(df[[i]]) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)
    colnames(top_cases[[i]]) <- c(i, "count")
  }
  top_cases
}
get_top_cases(merged_cps)