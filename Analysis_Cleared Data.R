#Name: Alena Ploshchansky
#Date: 2026-01-27
#Title: R-bootcamp Project Script
#Description: This script performs data analysis and visualization using R.

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(janitor)
library(stringi)
library(stringr)
library(ggmap)
library(tidygeocoder)
library(writexl)
library(readr) # added to safely parse numbers

#### Data set ####
## load data and inspection

## Prices data set
prices <- read_excel("Prices.xlsx")
head(prices)
dim(prices)

## Density data set
density <- read_excel("Density.xlsx", sheet="TABLE")
head(density)
dim(density)

## Median wage data set
median_wage <- read_excel("MedianWage.xlsx", sheet="TABLE")
head(median_wage)
dim(median_wage)

## Data check
glimpse(prices)
glimpse(density)
glimpse(median_wage)

#### Data manipulation ####
## Data cleaning and transformation

### Prices data set cleaning ###

## Check for missing values
sum(is.na(prices))
## Remove and change polish letters, convert to lower case
clean_prices <- prices %>%
  clean_names() %>%
  mutate(across(where(is.character),
            ~ tolower(stri_trans_general(., "Latin-ASCII"))))

## Check cleaned data
glimpse(clean_prices)

## Remove unnecessary columns
clean_prices <- clean_prices %>%
  select(-c("zrodlo_informacji", "cena_wartosc", "waluta", "numer_budynku"))


## Coordinates data type conversion and save

## Coordinates data type conversion and save
if (!file.exists("streets_geo.rds")) {

streets_geo <- clean_prices %>%
distinct(ulica) %>%
mutate(address = paste0(ulica, ", Warsaw, Poland")) %>%
geocode(
address = address,
method = "osm",
lat = latitude,
long = longitude
)

saveRDS(streets_geo, "streets_geo.rds")
}

# Load saved geo data
streets_geo <- readRDS("streets_geo.rds")

# Clean Prices
clean_prices <- clean_prices %>%
  left_join(streets_geo, by = "ulica")

## Remove unnecessary columns
clean_prices <- clean_prices %>%
    select(-address)

## Check final price data for missing values
sum(is.na(clean_prices))
glimpse(clean_prices)

## Delete rows with missing coordinates
clean_prices <- clean_prices %>%
  filter(!is.na(latitude) & !is.na(longitude))

## Create building age groups
clean_prices <- clean_prices %>%
  mutate(
    building_age_group = case_when(
      rok_budowy < 1960 ~ "very_old",
      rok_budowy >= 1960 & rok_budowy < 2010 ~ "old",
      rok_budowy >= 2010 & rok_budowy <= 2025 ~ "new",
      TRUE ~ NA_character_))

## Date conversion
clean_prices <- clean_prices %>%
  mutate(quarter_only = str_extract(data_transakcji_wyceny, "q[1-4]")) %>% # we left quarter only as all the data is from 2025
  select(-data_transakcji_wyceny)

clean_prices <- clean_prices %>%
    relocate(quarter_only, .before = cena_wartosc_1m2) %>%
  mutate(log_price_sqm = log(cena_wartosc_1m2)) %>%
  relocate(log_price_sqm, .after = cena_wartosc_1m2)

### Clean Density data set ###
#Cleaning the Polish letters and NAs from Density data set
cleaned_density_data <- density %>%
 clean_names() %>%
 select(-code) %>%
  mutate(across(where(is.character),
             ~ tolower(stri_trans_general(., "Latin-ASCII"))))%>%
  # remove missing values
na.omit()

# Erase first 2 rows and Distric 8 from the name
cleaned_density_data <- cleaned_density_data %>%
  slice(-c(1, 2)) %>%                                   # remove first 2 rows
  mutate(
    name = str_remove(name, "\\s*-\\s*district\\s*\\(8\\)")
  )


## Clean Wages data set

# Removing missing Values and unnecessary columns
median_wage_clean <- median_wage %>%
  select(-Code, -Name) %>%
  slice(-c(1, 2, 3, 4))

#Wide to Long
median_wage_clean<- median_wage_clean %>%
  pivot_longer(
    cols = everything(),
    names_to = "month",
    values_to = "median_wage"
  ) %>%
  mutate(median_wage = readr::parse_number(median_wage))

# Convert wage median to quarterly median

wage_quarterly <- median_wage_clean %>%
  mutate(
    quarter = case_when(
      month %in% c("January", "February", "March") ~ "q1",
      month %in% c("April", "May", "June")         ~ "q2",
      month %in% c("July", "August", "September")  ~ "q3",
    ))
wage_quarterly <- wage_quarterly %>%
  group_by(quarter) %>%
  summarise(
    median_wage_q = mean(median_wage, na.rm = TRUE),
    .groups = "drop"
  )

# Add log median wage quarterly
wage_quarterly <- wage_quarterly %>%
  mutate(log_median_wage_q = log(median_wage_q))


# H2: Affordability by Vistula River bank (Warsaw)

left_bank <- c(
  "bemowo", "bielany", "mokotow", "ochota",
  "srodmiescie", "ursynow", "wola",
  "zoliborz", "wlochy", "ursus"
)

right_bank <- c(
  "bialoleka", "praga-polnoc", "praga-poludnie",
  "targowek", "rembertow", "wawer", "wesola"
)

# Add river bank to clean_prices
clean_prices_h2 <- clean_prices %>%
  mutate(
    river_bank = case_when(
      dzielnica %in% left_bank  ~ "Left bank",
      dzielnica %in% right_bank ~ "Right bank",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(river_bank))

#  Calculate average prices per quarter and river bank
clean_prices_h2 <- clean_prices_h2 %>%
  left_join(
    wage_quarterly,
    by = c("quarter_only" = "quarter")
  )

clean_prices_h2 <- clean_prices_h2 %>%
  mutate(
    log_affordability = log_price_sqm - log(median_wage_q)
  )

#Descriptive check
clean_prices_h2 %>%
  group_by(river_bank) %>%
  summarise(
    mean_log_affordability = mean(log_affordability, na.rm = TRUE),
    n = n()
  )

# Boxplot visualization - Maybe we change the style?

ggplot(clean_prices_h2,
       aes(x = river_bank, y = log_affordability)) +
  geom_boxplot() +
  labs(
    title = "Housing affordability by Vistula River bank",
    x = "River bank",
    y = "Log affordability (price per sqm / income)"
  ) +
  theme_minimal()

# T-test H2 - Needed?
t.test(
  log_affordability ~ river_bank,
  data = clean_prices_h2,
  alternative = "greater"
)

# Mean + CI calculation

affordability_summary <- clean_prices_h2 %>%
  group_by(river_bank) %>%
  summarise(
    mean_affordability = mean(log_affordability, na.rm = TRUE),
    sd_affordability   = sd(log_affordability, na.rm = TRUE),
    n                  = n(),
    se                 = sd_affordability / sqrt(n),
    ci_lower           = mean_affordability - qt(0.975, df = n - 1) * se,
    ci_upper           = mean_affordability + qt(0.975, df = n - 1) * se
  )

ggplot(affordability_summary,
       aes(x = river_bank, y = mean_affordability)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15,
    linewidth = 0.8
  ) +
  labs(
    title = "Mean housing affordability by Vistula River bank",
    x = "River bank",
    y = "Mean log affordability (price per sqm / income)"
  ) +
  theme_minimal()

#H3: Building age and affordability

#ANOVA test
anova_h3 <- aov(log_affordability ~ building_age_group, data = clean_prices_h2)
summary(anova_h3)

# Post-hoc Tukey test
tukey_h3 <- TukeyHSD(anova_h3)
print(tukey_h3)

# Boxplot visualization H3