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
library(ggmap)
library(tidygeocoder)
library(writexl)

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

## Prices data set cleaning

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


## Coordinates data type conversion
streets_geo <- clean_prices %>%
  distinct(ulica) %>%
  mutate(address = paste0(ulica, ", Warsaw, Poland")) %>%
  geocode(address = address,
          method = "osm",
          lat = latitude,
          long = longitude)

final_prices <- clean_prices %>%
  left_join(streets_geo, by = "ulica")

## Remove unnecessary columns
final_prices <- final_prices %>%
    select(-address)

## Check final price data for missing values
sum(is.na(final_prices))
glimpse(final_prices)

## Delete rows with missing coordinates
final_prices <- final_prices %>%
  filter(!is.na(latitude) & !is.na(longitude))


## Clean Density data set

#Cleaning the Polish letters and NAs from Density data set
cleaned_density_data <- density %>%
 clean_names() %>%
 select(-code) %>%
  mutate(across(where(is.character),
              ~ tolower(stri_trans_general(., "Latin-ASCII")))) %>%

  # remove missing values
na.omit()


## Clean Wages data set

# Removing missing Values and unnecessary columns
median_wage_clean <- median_wage %>%
  select(-Code, -Name) %>%
  slice(-c(1, 2, 3, 4))
