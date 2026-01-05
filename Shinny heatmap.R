###############################################
# Lung Cancer Survey Analysis & Shiny Dashboard
###############################################

# 1. Load required packages
library(tidyverse)
library(shiny)
library(reshape2)
library(scales)

# 2. Load and clean data
lung <- read_csv("survey_lung_cancer.csv") %>%
  mutate(
    GENDER = factor(GENDER),
    LUNG_CANCER = factor(LUNG_CANCER, levels = c("NO", "YES")),
    SMOKING = factor(SMOKING, levels = c(1, 2), labels = c("No", "Yes")),
    `ALCOHOL CONSUMING` = factor(`ALCOHOL CONSUMING`, levels = c(1, 2), labels = c("No", "Yes")),
    COUGHING = factor(COUGHING, levels = c(1, 2), labels = c("No", "Yes")),
    `SHORTNESS OF BREATH` = factor(`SHORTNESS OF BREATH`, levels = c(1, 2), labels = c("No", "Yes"))
  )

# 3. Helper: convert selected factors to numeric for correlation
convert_to_numeric <- function(df) {
  df %>%
    mutate(
      SMOKING_num = as.numeric(SMOKING == "Yes"),
      ALCOHOL_CONSUMING_num = as.numeric(`ALCOHOL CONSUMING` == "Yes"),
      COUGHING_num = as.numeric(COUGHING == "Yes"),
      SHORTNESS_OF_BREATH_num = as.numeric(`SHORTNESS OF BREATH` == "Yes"),
      LUNG_CANCER_num = as.numeric(LUNG_CANCER == "YES")
    ) %>%
    select(AGE, SMOKING_num, ALCOHOL_CONSUMING_num, COUGHING_num, SHORTNESS_OF_BREATH_num, LUNG_CANCER_num)
}

# 4. UI
ui <- fluidPage(
  titlePanel("Lung Cancer Survey Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "age_range", "Select age range:",
        min = min(lung$AGE), max = max(lung$AGE),
        value = c(min(lung$AGE), max(lung$AGE))
      ),
      selectInput(
        "smoking_filter", "Smoking status:",
        choices = c("All", levels(lung$SMOKING)), selected = "All"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", tableOutput("summary_table")),
        tabPanel("Gender Plot", plotOutput("gender_plot")),
        tabPanel("Correlation Heatmap", plotOutput("cor_heatmap"))
      )
    )
  )
)

# 5. Server
server <- function(input, output, session) {
  
  # Reactive: filtered dataset
  filtered_data <- reactive({
    df <- lung %>%
      filter(AGE >= input$age_range[1], AGE <= input$age_range[2])
    if (input$smoking_filter != "All") df <- df %>% filter(SMOKING == input$smoking_filter)
    df
  })
  
  # Summary table: counts and proportions of lung cancer
  output$summary_table <- renderTable({
    filtered_data() %>%
      count(LUNG_CANCER) %>%
      mutate(prop = round(n / sum(n), 2))
  })
  
  # Plot: lung cancer by gender
  output$gender_plot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = GENDER, fill = LUNG_CANCER)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = percent) +
      labs(
        title = "Proportion of Lung Cancer by Gender (Filtered)",
        x = "Gender", y = "Proportion"
      ) +
      theme_minimal()
  })
  
  # Plot: correlation heatmap (numeric variables only)
  output$cor_heatmap <- renderPlot({
    numeric_df <- convert_to_numeric(filtered_data())
    cor_matrix <- cor(numeric_df, use = "complete.obs")
    cor_melt <- melt(cor_matrix)
    
    ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                           limits = c(-1, 1), name = "Correlation") +
      geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
      labs(title = "Correlation Heatmap of Lung Cancer Dataset", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# 6. Run the Shiny App
shinyApp(ui = ui, server = server)
