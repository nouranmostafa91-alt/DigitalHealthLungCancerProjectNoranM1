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
