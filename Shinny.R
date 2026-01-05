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
###############################################
# 5. Pivoting with tidyr (wide to long)
###############################################

# Select only variables of interest
lung_symptoms_long <- lung %>% 
  select(GENDER, AGE, LUNG_CANCER, SMOKING,
         COUGHING, `SHORTNESS OF BREATH`) %>% 
  pivot_longer(
    cols = c(COUGHING, `SHORTNESS OF BREATH`),
    names_to = "Symptom",
    values_to = "Presence"
  )

# Inspect the long format
head(lung_symptoms_long)
# Example: pivoting back to wide format (PresenceYes as dummy)
lung_symptoms_wide <- lung_symptoms_long %>% 
  mutate(PresenceYes = if_else(Presence == "Yes", 1, 0)) %>% 
  select(-Presence) %>% 
  pivot_wider(
    names_from  = Symptom,
    values_from = PresenceYes
  )

head(lung_symptoms_wide)
###############################################
# 6. Visualization with ggplot2
###############################################

# Bar plot: lung cancer frequency
ggplot(lung, aes(x = LUNG_CANCER)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Lung Cancer Status",
    x     = "Lung Cancer",
    y     = "Count"
  ) +
  theme_minimal()

# Stacked bar: lung cancer by gender
ggplot(lung, aes(x = GENDER, fill = LUNG_CANCER)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Lung Cancer by Gender",
    x     = "Gender",
    y     = "Proportion"
  ) +
  theme_minimal()
# Histogram of age by lung cancer status
ggplot(lung, aes(x = AGE, fill = LUNG_CANCER)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 15) +
  labs(
    title = "Age Distribution by Lung Cancer Status",
    x     = "Age",
    y     = "Frequency"
  ) +
  theme_minimal()
###############################################
# Heatmap: Symptom Presence vs Lung Cancer
###############################################

library(tidyverse)

# Pivot symptoms into long format
lung_long <- lung %>%
  pivot_longer(
    cols = c(COUGHING, `SHORTNESS OF BREATH`),
    names_to = "Symptom",
    values_to = "Presence"
  )

# Summarise proportion of "Yes" for each symptom
symptom_summary <- lung_long %>%
  group_by(LUNG_CANCER, Symptom, Presence) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(LUNG_CANCER, Symptom) %>%
  mutate(prop = n / sum(n)) %>%
  filter(Presence == "Yes")

# Heatmap
ggplot(symptom_summary, aes(x = Symptom, y = LUNG_CANCER, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "Heatmap of Symptom Presence by Lung Cancer Status",
    x = "Symptom",
    y = "Lung Cancer",
    fill = "Proportion"
  ) +
  theme_minimal()

# Boxplot: age by lung cancer status
ggplot(lung, aes(x = LUNG_CANCER, y = AGE, fill = LUNG_CANCER)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Age by Lung Cancer Status",
    x     = "Lung Cancer",
    y     = "Age"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
###############################################
# 7. Shiny app
###############################################

library(shiny)
library(tidyverse)

# Load data (reuse the cleaned version if in same session; otherwise re-run cleaning)
lung <- read_csv("survey_lung_cancer.csv") %>% 
  mutate(
    GENDER      = factor(GENDER),
    LUNG_CANCER = factor(LUNG_CANCER, levels = c("NO", "YES")),
    SMOKING = factor(SMOKING, levels = c(1, 2), labels = c("No", "Yes")),
    `ALCOHOL CONSUMING` = factor(`ALCOHOL CONSUMING`, levels = c(1, 2), labels = c("No", "Yes")),
    COUGHING = factor(COUGHING, levels = c(1, 2), labels = c("No", "Yes")),
    `SHORTNESS OF BREATH` = factor(`SHORTNESS OF BREATH`, levels = c(1, 2), labels = c("No", "Yes"))
  )

ui <- fluidPage(
  titlePanel("Lung Cancer Survey Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "age_range",
        label   = "Select age range:",
        min     = min(lung$AGE),
        max     = max(lung$AGE),
        value   = c(min(lung$AGE), max(lung$AGE))
      ),
      
      selectInput(
        inputId  = "smoking_filter",
        label    = "Smoking status:",
        choices  = c("All", levels(lung$SMOKING)),
        selected = "All"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary table", tableOutput("summary_table")),
        tabPanel("Plot", plotOutput("gender_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered dataset using pipes and filter()
  filtered_data <- reactive({
    df <- lung %>% 
      filter(
        AGE >= input$age_range[1],
        AGE <= input$age_range[2]
      )
    
    if (input$smoking_filter != "All") {
      df <- df %>% filter(SMOKING == input$smoking_filter)
    }
    
    df
  })
  
  # Summary table: lung cancer counts and proportions
  output$summary_table <- renderTable({
    filtered_data() %>% 
      count(LUNG_CANCER) %>% 
      mutate(prop = n / sum(n))
  })
  
  # Plot: lung cancer by gender (stacked proportion)
  output$gender_plot <- renderPlot({
    filtered_data() %>% 
      ggplot(aes(x = GENDER, fill = LUNG_CANCER)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Proportion of Lung Cancer by Gender (Filtered)",
        x     = "Gender",
        y     = "Proportion"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
