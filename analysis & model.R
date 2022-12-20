#--------------------
# Objective: PPHA 30536 Final Project-Analysis
# Date: 29th Nov, 2022
#--------------------
  
# Clear Global Environment
options(
  scipen = 999,
  digits = 3
)
# Setting the Working Directory
setwd("~/Desktop/Fall Quarter/PPHA-30536/Final Project/final-project-weiluj")

# Load packages
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(lubridate)
library(statar)
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(ggthemes)
library(ggthemr)
library(plotly) # Interactive mapping
library(choroplethr)
library(RColorBrewer)
library(colorspace)
library(rcstatebin)
library(maps)
merged_cps <- read_csv("merged_cps.csv")
# 2. Exploratory Data Analysis
## 2.1 Functions to get median wage by gender and pay gap
# function
data_list <- list()
#one disaggregation variable
get_mwage_pct <- function(col, df) {
  # get data frame
  data_ts <- df %>%
    group_by(sex, df[[col]]) %>%
    summarise(median_wage = median(earnwke, na.rm = T)) %>%
    pivot_wider(
      names_from = sex,
      values_from = median_wage
    ) %>%
    mutate(pct = Female / Male) %>%
    pivot_longer(Female:Male,
                 names_to = "sex",
                 values_to = "median_wage"
    )
  colnames(data_ts) <- c(col, "pct", "sex", "median_wage")
  
  data <- df %>%
    group_by(year, sex, df[[col]]) %>%
    summarise(median_wage = median(earnwke, na.rm = T)) %>%
    pivot_wider(
      names_from = sex,
      values_from = median_wage
    ) %>%
    mutate(pct = Female / Male) %>%
    pivot_longer(Female:Male,
                 names_to = "sex",
                 values_to = "median_wage"
    )
  colnames(data) <- c("year", col, "pct", "sex", "median_wage")
  data_list <- list(data_ts, data)
  data_list
}

#multiple disaggregation variables
get_mwage_pct_db <- function(col1, col2, df) {
  # get data frame
  data_ts <- df %>%
    group_by(sex, df[[col1]], df[[col2]]) %>%
    summarise(median_wage = median(earnwke, na.rm = T)) %>%
    pivot_wider(
      names_from = sex,
      values_from = median_wage
    ) %>%
    mutate(pct = Female / Male) %>%
    pivot_longer(Female:Male,
                 names_to = "sex",
                 values_to = "median_wage"
    )
  colnames(data_ts) <- c(col1, col2, "pct", "sex", "median_wage")
  
  data <- df %>%
    group_by(year, sex, df[[col1]], df[[col2]]) %>%
    summarise(median_wage = median(earnwke, na.rm = T)) %>%
    pivot_wider(
      names_from = sex,
      values_from = median_wage
    ) %>%
    mutate(pct = Female / Male) %>%
    pivot_longer(Female:Male,
                 names_to = "sex",
                 values_to = "median_wage"
    )
  colnames(data) <- c("year", col1, col2, "pct", "sex", "median_wage")
  data_list <- list(data_ts, data)
  data_list
}
# Generate Plots
plt <- list()
get_plt <- function(var, df) {
  # filter data frame
  df <- df %>%
    filter(sex == "Female")
  # median wage ts
  plt_mwage <- df %>%
    ggplot() +
    geom_line(
      aes(year, median_wage,
          color = factor(df[[var]])
      )
    ) +
    lab_a +
    scale_x_continuous(breaks = seq(2000, 2020, 2)) +
    scale_color_discrete_sequential(name = str_to_title(gsub("\\d+|_.*", "", var)), palette = "Heat") +
    theme
  # wage gap ts
  plt_gap <- df %>%
    ggplot() +
    geom_line(
      aes(year, pct,
          color = factor(df[[var]])
      )
    ) +
    lab_b +
    scale_x_continuous(breaks = seq(2000, 2020, 2)) +
    scale_color_discrete_sequential(name = str_to_title(gsub("\\d+|_.*", "", var)), palette = "Heat") +
    theme
  
  plt <- ggarrange(plt_mwage, plt_gap,
                   common.legend = T,
                   nrow = 2
  )
  plt
}

# Set plot theme
theme <- 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 9, face = "bold"),
    plot.caption = element_text(size = 6, face = "italic", hjust = 0),
    axis.text = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  )
theme_a <- theme_economist() + theme
lab_a <- labs(
  x = "Year",
  y = "Median Wage"
)
lab_b <- labs(
  x = "Year",
  y = "Wage Ratio"
)

## 2.2 Overall Distribution
### Marital Status
#Total wage
merged_cps %>%
  group_by(sex, marital, year) %>%
  summarise(median_wage = median(earnwke)) %>%
  ggplot(aes(year, median_wage,
             color = marital,
             group = interaction(marital, sex))) +
  geom_line() + geom_point(aes(pch = sex)) +
  labs(title = "Median Wage by marital status and Sex") +
  scale_shape_manual(values = c(16,1)) +
  legend_top() +
  theme

### Distribution Among Married Groups
merged_cps %>%
  group_by(sex, marital, age_fct, ownchild_fct) %>%
  filter(marital == "Married") %>%
  summarise(median_wage = median(earnwke)) %>%
  ggplot() +
  geom_col(aes(sex, median_wage,
               fill = ownchild_fct), position= "dodge") +
  facet_wrap(~age_fct)

## 2.3 Time Series
### Wage Pct
wage_ts <- get_mwage_pct("year", merged_cps)
### Pay gap line plot
wage_ts[[1]] %>%
  ggplot(aes(year, pct)) +
  geom_line(size = 0.8) +
  labs(
    title = "Women's-to-Men's Earning Ratio",
    subtitle = "Until 2020, females were paid 20 cents less for every dollar in male wages",
    caption = "Source: US Census, Current Population Survey.\nWage calculated by weekly earnings and only includes 16+ years old US citizens"
  ) +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) + 
  scale_y_continuous(breaks = seq(0.65, 0.8, 0.05),
                     limits = c(0.65, 0.82)) +
  theme_a
### Overall Wage Trend
wage_ts[[1]] %>%
  ggplot(aes(
    x = year, y = median_wage,
    color = sex
  )) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Median Wages Trend 2000 to 2020",
    subtitle = "Womens' Median Wages Increase while Lagging behind Men",
    x = "Year",
    y = "Median Wage",
    caption = "Source: US Census"
  ) +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(breaks = seq(600, 1200, 200),
                     limits = c(500, 1200)) +
  theme_a +
  scale_color_economist()
# write_csv(wage_ts[[1]], "ts.csv")

#### 2.3.1 Age
# Age Data frame
age_ts <- get_mwage_pct("age_fct", merged_cps)
# Plot
plt_age <- get_plt("age_fct", age_ts[[2]])
annotate_figure(plt_age,
                top = text_grob("Women between 35-54 Earns the Most with Highest Unqueal Ratio", face = "bold", size = 12)
)

#### 2.3.2 Race
# Get data frame
race_ts <- get_mwage_pct("race", merged_cps)
# Plot
plt_race <- get_plt("race", race_ts[[2]])
annotate_figure(plt_race,
                top = text_grob("Black Women Shares Lowest Gender Pay Gap with Low Wage", face = "bold", size = 12)
)

#### 2.3.4 Marital and Children
# Marital and Children
plt_ma_chi_1 <- get_mwage_pct_db("marital", "ownchild_fct", merged_cps)[[2]] %>%
  filter(sex == "Female") %>%
  ggplot(aes(year, median_wage,
             color = factor(marital),
             group = interaction(marital, ownchild_fct))
  ) +
  geom_line() + geom_point(aes(pch = ownchild_fct)) +
  lab_a +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  guides(color = guide_legend(title = "Marital")) +
  theme

plt_ma_chi_2 <- get_mwage_pct_db("marital", "ownchild_fct", merged_cps)[[2]] %>%
  filter(sex == "Female") %>%
  ggplot(aes(year, pct,
             color = factor(marital),
             group = interaction(marital, ownchild_fct))
  ) +
  geom_line() + geom_point(aes(pch = ownchild_fct)) +
  lab_b +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  theme 
plt_ma_chi <- ggarrange(plt_ma_chi_1, plt_ma_chi_2,
                        common.legend = T,
                        nrow = 2
)
annotate_figure(plt_ma_chi, top = text_grob("Married Mothers Experince Largest Pay Gap", face = "bold", size = 12))

#### 2.3.5 Education
# Get data frame
edu_ts <- get_mwage_pct("grade92_fct", merged_cps)
# Plot
plt_edu <- get_plt("grade92_fct", edu_ts[[2]])
annotate_figure(plt_edu,
                top = text_grob("Women with Advanced Degree Earns the Most and Shares Lowest Gender Pay Gap", face = "bold", size = 12)
)

### 2.3.6 By Sector & Industry & Occupation
# Most popular occupation by women
merged_cps <- merged_cps %>%
  mutate(docc00 = str_replace(docc00, "occupations", ""))

top_occ <- merged_cps %>%
  filter(sex == "Female") %>%
  group_by(docc00) %>%
  summarise(count = n()) %>%
  top_n(5, count)

top_occ %>%
  ggplot() +
  geom_col(aes(fct_reorder(to_factor(docc00), count), count),
               fill = "#7A6752") +
  scale_x_discrete(labels = function(x) str_replace(x, "occupations", "")) +
  coord_flip() +
  labs(
    title = "Administrative Work Remains the Most Popular Job for Women",
    x = "Occupation",
    y = "Count"
  ) +
  theme +
  theme(plot.title = element_text(size = 12, hjust = 1))
#### Occupation with the highest and lowest gap overtime/by year
occ_class <- get_mwage_pct("docc00", merged_cps)
occ_class[[1]] %>%
  mutate(avg_gap = mean(pct),
         gap_group = pct - avg_gap,
         gap_type = ifelse(
           gap_group > 0,
           "Above Average",
           "Below Average"
         )) %>% 
  distinct(docc00, .keep_all = T) %>%
ggplot() +
  geom_col(aes(fct_reorder(to_factor(docc00), gap_group),
               gap_group,
               fill = gap_type),
           width = .5) +
  scale_x_discrete(labels = function(x) str_replace(x, "occupations", "")) +
  coord_flip() +
  theme
sf <- read.csv("state_map.csv") %>%
  left_join(st, by = "NAME") %>%
  select(-c(STATENS:GEOID, LSAD:INTPTLON))
st_write(sf, "sf.shp")
#### Pct
merged_cps %>%
  filter(docc00 %in% top_occ$docc00) %>%
  group_by(year, sex, docc00) %>%
  summarise(median_wage = median(earnwke)) %>%
  pivot_wider(names_from = sex,
              values_from = median_wage) %>%
  mutate(pct = Female / Male) %>%
  ggplot() +
  geom_line(aes(year, pct,
                color = to_factor(docc00))) +
  labs(
    title = "Women Earns Less than 80% as to Male in Sales and Management"
  ) +
  theme +
  scale_color_discrete_sequential(name = "Occupation", palette = "Heat",
  label = c("Management", "Healthcare practitioner and technical", "Sales", "Office and administrative support", "Education, training, and library")) +
  legend_top()

#### Industry
ind_ts <- get_mwage_pct("dind02", merged_cps)
ind_ts[[1]] %>%
  filter(sex == "Female") %>%
  arrange(pct) %>%
  mutate(ind = to_factor(dind02)) %>%
  head(20)

##### Interactive Plot by Year
plt2_ts_al_mpay <- plt2_ts_al_mpay + transition_reveal(year)
anim_save("shiny_ts_al_mpay.gif", plt2_ts_al_mpay)

# 3. State-Level Disaggreagation Analysis
### By Party Analysis
merged_cps %>%
  drop_na(party) %>%
  group_by(year, party, sex) %>%
  mutate(median_wage = median(earnwke)) %>%
  distinct(party, sex, year, median_wage) %>%
  pivot_wider(names_from = sex,
              values_from = median_wage) %>%
  mutate(pct = Female/Male) %>%
ggplot() +
  geom_line(aes(year, pct,
            color = party),
            size = 0.8, alpha = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2020, 4)) +
  scale_colour_manual(name = "Elected Party",
                      values = c("#e15759", "#4e79a7")) +
  lab_b +
  labs(
    title = "Republican States Have Larger Pay Gap",
    caption = "Source: MIT Election Lab, data only includes presidential election from 2000 to 2020"
  ) +
  theme_a

### By GDP per capita, party
merged_cps %>%
  filter(state != "DC" & !is.na(party)) %>%
  group_by(state) %>%
  fill(party_index) %>%
  group_by(state, sex) %>%
  mutate(median_wage = median(earnwke)) %>%
  distinct(state, sex, avg_gdp_per_capita, median_wage, party_fct) %>%
  pivot_wider(names_from = sex,
              values_from = median_wage) %>%
  mutate(pct = Female/Male) %>%
  ggplot(aes(avg_gdp_per_capita, pct)) +
  geom_point(aes(color = factor(party_fct))) +
  geom_smooth(method = "lm", se = F) +
  scale_colour_manual(name = "Elected Party",
                      values = c("#4e79a7", "#7A6752", "#e15759")) +
  labs(
    title = "Gender Wage Ratio by State Partisanship and Avg GDP",
    x = "GDP Per Capita",
    y = "Wage Ratio") +
  theme

### fertility & percentage wage
merged_cps %>%
  group_by(state) %>%
  fill(party_fct) %>%
  drop_na(fertility_rate) %>%
  group_by(year, state, sex) %>%
  mutate(median_wage = median(earnwke)) %>%
  distinct(year, state, sex, median_wage, fertility_rate, party_fct) %>%
  pivot_wider(names_from = sex,
              values_from = median_wage) %>%
  mutate(pct = Female/Male) %>%
  ggplot(aes(fertility_rate, pct)) +
  geom_point(aes(frame = year, ids = state,
                 color = party_fct), alpha = 0.8) +
  geom_smooth(aes(frame = year), se = F, size = 0.8,
              method = "lm") +
  scale_color_manual(name = "Elected Party",
                     values = c("#4e79a7", "#7A6752", "#e15759"),
                     labels = c("Dem", "Neutral", "Rep")) +
  labs(title = "Gender Wage Ratio by State Partisanship and Fertlity rate",
    x = "Fertility Rate",
       y = "Wage Ratio") +
  theme

### gdp per capita
plt_gdp_group <- merged_cps %>%
  group_by(year) %>%
  mutate(gdp_group = as.factor(cut_interval(gdp_per_capita, 5))) %>%
  group_by(year, gdp_group, sex) %>%
  mutate(
    median_wage = median(earnwke),
    earnwke = winsorise(earnwke, probs = c(0.05, 0.95))
  )

plt1 <- plt_gdp_group %>%
  distinct(year, gdp_group, sex, median_wage) %>%
  pivot_wider(
    names_from = sex,
    values_from = median_wage
  ) %>%
  mutate(pct = (Female / Male)) %>%
  ggplot() +
  geom_line(aes(gdp_group, pct*3000,
    group = 1
  ), size = 0.8, color = "#7A6752")
plt1 +
  geom_boxplot(data = plt_gdp_group,
               aes(gdp_group, earnwke,
                   fill = sex), outlier.shape = NA) +
  labs(
    title = "Median Wage and Ratio by GDP Per Capia Range",
    x = "GDP Range",
    y = "Meidan Wage"
  ) +
  facet_wrap(~year,
    scales = "free_x"
  ) +
  theme(
    axis.text.x = element_blank()
  )

### gdp growth, poverty rate and percentage increase
state_gdp_growth <- merged_cps %>%
  group_by(state) %>%
  distinct(year, .keep_all = T) %>%
  mutate(gdp_growth = ((gdp / lag(gdp)) - 1) * 100) %>%
  select(state, year, gdp_growth)
merged_cps <- left_join(merged_cps, state_gdp_growth,
  by = c("year", "state")
)

merged_cps %>%
  group_by(year, state, sex) %>%
  mutate(median_wage = median(earnwke)) %>%
  distinct(year, state, sex, median_wage, unemp_rate, gdp_growth) %>%
  pivot_wider(
    names_from = sex,
    values_from = median_wage
  ) %>%
  mutate(pct = (Female / Male) * 15) %>%
  pivot_longer(
    cols = c(unemp_rate, gdp_growth, pct),
    names_to = "index",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_line(aes(year, value,
    color = index
  )) +
  scale_x_continuous(labels = function(x) str_sub(x, 3, 4)) +
  facet_geo(~state) +
  theme_bw()

### State level Analysis by Race
data_race_state <- get_mwage_pct_db("state", "race", merged_cps)
# analysis
data_race_state[[1]] %>%
  group_by(state) %>%
  distinct(state, race, .keep_all = T) %>%
  slice_min(order_by = pct) %>%
  ungroup() %>%
  count(race)
data_race_state[[2]] %>%
  distinct(state, year, race, .keep_all = T) %>%
  slice_min(order_by = pct) %>%
  group_by(year) %>%
  count(race) %>%
  ggplot() +
  geom_col(aes(year, n,
               fill = race),
           stat = "identity")
wh_as_gap_st <- data_race_state[[2]] %>%
  group_by(state, year) %>%
  distinct(state, year, race, .keep_all = T) %>%
  slice_min(order_by = pct) %>%
  filter(race %in% c("White", "Asian/Pacific Islanders")) %>%
  ungroup() %>%
  count(state, race) %>%
  arrange(race, desc(n)) %>%
  group_by(race) %>%
  top_n(5)

### state analysis
wh_as_top_st <- data_race_state[[1]] %>%
  group_by(state) %>%
  distinct(state,race, .keep_all = T) %>%
  filter(race != "American Indian/Alaskan Native") %>%
  select(-c(pct:sex)) %>%
  pivot_wider(names_from = race,
              values_from = median_wage) %>%
  mutate(race_gap_white = Black/White,
         race_gap_Asian = Black/`Asian/Pacific Islanders`,
         state = as.factor(state)) %>%
  left_join(wh_as_gap_st, by = "state") %>%
  mutate(index = ifelse(
    is.na(race) == TRUE,
    "No",
    as.character(race)
  )) %>%
  select(state, race_gap_Asian:race_gap_white, index) %>%
  group_by(index) %>%
  summarise(mean_White = mean(race_gap_white),
            mean_Asian = mean(race_gap_Asian)
            ) %>%
  pivot_longer(mean_White:mean_Asian,
               names_to = "black female wage as %",
               values_to = "percentage") %>%
  ggplot() +
  geom_line(aes(index, percentage,
                group = `black female wage as %`, color = `black female wage as %`))

### gap plot
data_race_state[[1]] %>%
  ggplot() +
  geom_line(aes(race, pct,
                group = 1)) +
  scale_x_discrete(labels = function(x) str_replace(x, "/.*", "")) +
  facet_geo(~state) +
  theme

### percentage analysis
data_race_state[[1]] %>%
  group_by(state) %>%
  distinct(state, race, .keep_all = T) %>%
  slice_min(order_by = pct) %>%
  ungroup() %>%
  count(race)

### State Level overall Distribution
data_state <- get_mwage_pct("state", merged_cps)
statebins(data_state[[1]],
          value_col = "pct",
          palette = "RdPu",
          name = "%",
          direction = 1) +
  labs(
    title = "Women's Wage % of Men Statewide: Year 2000-2020"
  ) +
  theme_statebins(legend_position = "right") +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.justification = c(0, 0.5))

### prepare the data fo choropleth
us <- st_read("cb_2018_us_state_500k.shp")
us <- us %>%
  select(-geometry)
data_st <- left_join(data_state[[2]], us, by = c("state" = "STUSPS")) %>%
  select(-c(STATENS:GEOID, LSAD:geometry)) %>%
  mutate(region = tolower(NAME))

to_map <- data_st %>%
  filter(sex == "Female")
# choropleth
to_map$region <- tolower(to_map$NAME)
to_map$value <- to_map$pct
# Export dataset
write_csv(to_map, "state_map.csv")
### Plot
state_choropleth(filter(to_map, year == 2001), legend = "Wage %", num_colors = 5, zoom = NULL)

# 4. Model Regression
## Without control
reg <- lm(earnwke ~ sex + year, merged_cps)
summary(reg)
reg <- lm(earnwke ~ sex + year + docc00, merged_cps)
summary(reg)

## State level control
reg_state <- lm(earnwke ~ sex + year + gdp + gdp_per_capita + unemp_rate + poverty_rate + fertility_rate + pop + party_index, merged_cps)
summary(reg_state)
## Personal level control
reg_p <- lm(earnwke ~ sex + year + age + grade92 + marital*ownchild_fct + uhourse +
              industry, merged_cps)
summary(reg_p)

## State level control
reg_full <- lm(earnwke ~ sex + year + gdp + gdp_per_capita + unemp_rate + poverty_rate +
                 pop + party_index + age + race + grade92, merged_cps)
summary(reg_state) %>%
  tidy()