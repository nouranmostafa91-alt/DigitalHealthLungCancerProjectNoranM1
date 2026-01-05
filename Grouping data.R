###############################################
# 1. Setup: Install and load required packages
###############################################

# Install packages once (run only if not installed)
# install.packages("tidyverse")
# install.packages("shiny")

# Load libraries
library(tidyverse)  # includes dplyr, ggplot2, tidyr, readr, etc.
library(shiny)      # for Shiny web application
###############################################
# 2. Import data
###############################################

# Optionally set working directory
# setwd("C:/path/to/your/folder")

lung <- read_csv("survey_lung_cancer.csv")

# Inspect the first rows and structure
head(lung)
str(lung)
glimpse(lung)
summary(lung)
###############################################
# 3. Data cleaning and transformation
###############################################

# Check for missing values in each column
colSums(is.na(lung))

# Convert character columns to factors (GENDER and LUNG_CANCER)
lung <- lung %>% 
  mutate(
    GENDER      = factor(GENDER),
    LUNG_CANCER = factor(LUNG_CANCER, levels = c("NO", "YES"))
  )

# Recode binary-coded columns (1/2) into factors with labels
lung <- lung %>% 
  mutate(
    SMOKING = factor(
      SMOKING,
      levels = c(1, 2),
      labels = c("No", "Yes")
    ),
    `ALCOHOL CONSUMING` = factor(
      `ALCOHOL CONSUMING`,
      levels = c(1, 2),
      labels = c("No", "Yes")
    ),
    COUGHING = factor(
      COUGHING,
      levels = c(1, 2),
      labels = c("No", "Yes")
    ),
    `SHORTNESS OF BREATH` = factor(
      `SHORTNESS OF BREATH`,
      levels = c(1, 2),
      labels = c("No", "Yes")
    )
  )

# Verify the structure after recoding
glimpse(lung)
###############################################
# 4. Descriptive statistics using dplyr pipes
###############################################

# Overall count and proportion of lung cancer
lung %>% 
  count(LUNG_CANCER) %>% 
  mutate(prop = n / sum(n))
# Lung cancer by gender
lung %>% 
  group_by(GENDER, LUNG_CANCER) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  mutate(prop = n / sum(n))

# Lung cancer by smoking status
lung %>% 
  group_by(SMOKING, LUNG_CANCER) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  ) %>% 
  mutate(prop = n / sum(n))

# Mean age by lung cancer status
lung %>% 
  group_by(LUNG_CANCER) %>% 
  summarise(
    mean_age = mean(AGE),
    sd_age   = sd(AGE),
    n        = n(),
    .groups  = "drop"
  )
