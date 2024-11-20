# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(GGally)
library(shinythemes)
library(corrplot)
library(tidyr)

# Read the dataset with appropriate NA handling
student_data <- read.csv("StudentPerformanceFactors.csv", na.strings = c(""))

# Define categorical and numeric columns
categorical_cols <- c(
  "Parental_Involvement", "Access_to_Resources", "Extracurricular_Activities",
  "Motivation_Level", "Internet_Access", "Family_Income", "Teacher_Quality",
  "School_Type", "Peer_Influence", "Learning_Disabilities",
  "Parental_Education_Level", "Distance_from_Home", "Gender"
)

numeric_cols <- c("Exam_Score", "Hours_Studied", "Sleep_Hours", "Previous_Scores", 
                  "Tutoring_Sessions", "Physical_Activity", "Attendance")

# Replace NA with "Other" only in categorical columns
student_data <- student_data %>%
  mutate(across(all_of(categorical_cols), ~ replace_na(., "Other")))

# Convert categorical columns to factors with appropriate levels
student_data <- student_data %>%
  mutate(
    Parental_Involvement = factor(Parental_Involvement, levels = c("Low", "Medium", "High")),
    Access_to_Resources = factor(Access_to_Resources, levels = c("Low", "Medium", "High")),
    Extracurricular_Activities = factor(Extracurricular_Activities, levels = c("No", "Yes")),
    Motivation_Level = factor(Motivation_Level, levels = c("Low", "Medium", "High")),
    Internet_Access = factor(Internet_Access, levels = c("No", "Yes")),
    Family_Income = factor(Family_Income, levels = c("Low", "Medium", "High")),
    Teacher_Quality = factor(Teacher_Quality, levels = c("Low", "Medium", "High", "Other")),
    School_Type = factor(School_Type, levels = c("Public", "Private")),
    Peer_Influence = factor(Peer_Influence, levels = c("Negative", "Neutral", "Positive")),
    Learning_Disabilities = factor(Learning_Disabilities, levels = c("No", "Yes")),
    Parental_Education_Level = factor(Parental_Education_Level, levels = c("High School", "College", "Postgraduate", "Other")),
    Distance_from_Home = factor(Distance_from_Home, levels = c("Near", "Moderate", "Far", "Other")),
    Gender = factor(Gender, levels = c("Male", "Female"))
  )

# Define UI for the application
ui <- dashboardPage(
  
  # Dashboard Header
  dashboardHeader(title = "Student Performance Dashboard"),
  
  # Dashboard Sidebar with Main Features as Menu Items
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analytical Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Interactive Data Explorer", tabName = "data_explorer", icon = icon("table"))
    )
  ),
  
  # Dashboard Body with Corresponding Tabs
  dashboardBody(
    # Apply a theme for better aesthetics
    shinythemes::shinytheme("flatly"),
    tabItems(
      
      # Main Feature 1: Performance Overview
      # This feature provides a high-level summary of student performance through key metrics and aggregate visualizations.
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Key Metrics",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    valueBoxOutput("avgExamScore"),
                    valueBoxOutput("avgHoursStudied"),
                    valueBoxOutput("avgSleepHours"),
                    valueBoxOutput("avgPreviousScores"),
                    valueBoxOutput("avgPhysicalActivity"),
                    valueBoxOutput("avgTutoringSessions")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Summary Visualizations",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  tabBox(
                    width = 12,
                    tabPanel("Exam Scores by Motivation Level", plotlyOutput("motivationBoxPlot")),
                    tabPanel("Exam Scores by Teacher Quality", plotlyOutput("teacherQualityPlot")),
                    tabPanel("Average Exam Scores by School Type", plotlyOutput("schoolTypeBarPlot")),
                    tabPanel("Exam Scores by Parental Education Level", plotlyOutput("parentalEducationPlot"))
                  )
                )
              )
      ),
      
      # Main Feature 2: Analytical Visualizations
      # This feature offers in-depth analytical tools and visualizations to explore relationships and correlations between various factors influencing student performance.
      tabItem(tabName = "visualizations",
              fluidRow(
                box(
                  title = "Correlation Heatmap",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,  # Half width for side-by-side layout
                  plotlyOutput("corrHeatmap", height = "400px")
                ),
                box(
                  title = "Scatter Plot: Hours Studied vs. Exam Score",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,  # Half width for side-by-side layout
                  plotlyOutput("scatterPlot", height = "400px")
                )
              )
      ),
      
      # Main Feature 3: Interactive Data Explorer
      # This feature enables users to interactively explore and manipulate the dataset based on various criteria, facilitating targeted analysis.
      tabItem(tabName = "data_explorer",
              fluidRow(
                box(
                  title = "Filters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  # Multiple Filters
                  selectInput("genderFilter", "Gender:",
                              choices = c("All", levels(student_data$Gender)),
                              selected = "All"),
                  selectInput("schoolTypeFilter", "School Type:",
                              choices = c("All", levels(student_data$School_Type)),
                              selected = "All"),
                  selectInput("parentEducationFilter", "Parental Education Level:",
                              choices = c("All", levels(student_data$Parental_Education_Level)),
                              selected = "All"),
                  selectInput("motivationFilter", "Motivation Level:",
                              choices = c("All", levels(student_data$Motivation_Level)),
                              selected = "All"),
                  selectInput("internetAccessFilter", "Internet Access:",
                              choices = c("All", levels(student_data$Internet_Access)),
                              selected = "All"),
                  sliderInput("hoursStudiedFilter", "Hours Studied:",
                              min = min(student_data$Hours_Studied, na.rm = TRUE),
                              max = max(student_data$Hours_Studied, na.rm = TRUE),
                              value = c(min(student_data$Hours_Studied, na.rm = TRUE),
                                        max(student_data$Hours_Studied, na.rm = TRUE))),
                  sliderInput("sleepHoursFilter", "Sleep Hours:",
                              min = min(student_data$Sleep_Hours, na.rm = TRUE),
                              max = max(student_data$Sleep_Hours, na.rm = TRUE),
                              value = c(min(student_data$Sleep_Hours, na.rm = TRUE),
                                        max(student_data$Sleep_Hours, na.rm = TRUE)))
                ),
                box(
                  title = "Filtered Data Table",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  # Display the total number of students after filtering and the download button
                  fluidRow(
                    column(6, textOutput("studentCount")),
                    column(6, downloadButton("downloadData", "Download CSV"))
                  ),
                  br(),
                  DTOutput("filteredTable")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # --- Main Feature 1: Performance Overview ---
  
  # Summary Statistics - Average Exam Score
  # Displays the average exam score across all students.
  output$avgExamScore <- renderValueBox({
    avg_score <- round(mean(student_data$Exam_Score, na.rm = TRUE), 2)
    valueBox(
      paste(avg_score),
      "Average Exam Score",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  # Summary Statistics - Average Hours Studied
  # Shows the average number of hours studied by students.
  output$avgHoursStudied <- renderValueBox({
    avg_hours <- round(mean(student_data$Hours_Studied, na.rm = TRUE), 2)
    valueBox(
      paste(avg_hours, "hrs"),
      "Average Hours Studied",
      icon = icon("book"),
      color = "green"
    )
  })
  
  # Summary Statistics - Average Sleep Hours
  # Presents the average number of sleep hours by students.
  output$avgSleepHours <- renderValueBox({
    avg_sleep <- round(mean(student_data$Sleep_Hours, na.rm = TRUE), 2)
    valueBox(
      paste(avg_sleep, "hrs"),
      "Average Sleep Hours",
      icon = icon("bed"),
      color = "purple"
    )
  })
  
  # Summary Statistics - Average Previous Scores
  # Highlights the average of previous scores among students.
  output$avgPreviousScores <- renderValueBox({
    avg_prev <- round(mean(student_data$Previous_Scores, na.rm = TRUE), 2)
    valueBox(
      paste(avg_prev),
      "Average Previous Scores",
      icon = icon("clipboard-list"),
      color = "yellow"
    )
  })
  
  # Summary Statistics - Average Physical Activity
  # Indicates the average level of physical activity among students.
  output$avgPhysicalActivity <- renderValueBox({
    avg_physical <- round(mean(student_data$Physical_Activity, na.rm = TRUE), 2)
    valueBox(
      paste(avg_physical),
      "Average Physical Activity",
      icon = icon("running"),
      color = "teal"
    )
  })
  
  # Summary Statistics - Average Tutoring Sessions
  # Shows the average number of tutoring sessions attended by students.
  output$avgTutoringSessions <- renderValueBox({
    avg_tutoring <- round(mean(student_data$Tutoring_Sessions, na.rm = TRUE), 2)
    valueBox(
      paste(avg_tutoring),
      "Average Tutoring Sessions",
      icon = icon("chalkboard-teacher"),
      color = "navy"
    )
  })
  
  # Exam Scores by Motivation Level
  # Illustrates how different motivation levels correlate with exam scores.
  output$motivationBoxPlot <- renderPlotly({
    p <- ggplot(student_data, aes(x = Motivation_Level, y = Exam_Score, fill = Motivation_Level,
                                  text = paste("Motivation Level:", Motivation_Level,
                                               "<br>Exam Score:", Exam_Score))) +
      geom_boxplot() +
      labs(title = "Exam Scores by Motivation Level",
           x = "Motivation Level",
           y = "Exam Score") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  # Exam Scores by Teacher Quality
  # Examines the relationship between perceived teacher quality and student exam performance.
  output$teacherQualityPlot <- renderPlotly({
    p <- ggplot(student_data, aes(x = Teacher_Quality, y = Exam_Score, color = Teacher_Quality,
                                  text = paste("Teacher Quality:", Teacher_Quality,
                                               "<br>Exam Score:", Exam_Score))) +
      geom_jitter(width = 0.2, alpha = 0.7) +
      labs(title = "Exam Scores by Teacher Quality",
           x = "Teacher Quality",
           y = "Exam Score") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  # Average Exam Scores by School Type
  # Compares average exam scores between different types of schools (Public vs. Private).
  output$schoolTypeBarPlot <- renderPlotly({
    avg_scores <- student_data %>%
      group_by(School_Type) %>%
      summarise(Average_Score = mean(Exam_Score, na.rm = TRUE))
    
    p <- ggplot(avg_scores, aes(x = School_Type, y = Average_Score, fill = School_Type,
                                text = paste("School Type:", School_Type,
                                             "<br>Average Score:", round(Average_Score, 2)))) +
      geom_bar(stat = "identity") +
      labs(title = "Average Exam Scores by School Type",
           x = "School Type",
           y = "Average Exam Score") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  # Exam Scores by Parental Education Level
  # Analyzes how parental education levels impact student exam scores.
  output$parentalEducationPlot <- renderPlotly({
    p <- ggplot(student_data, aes(x = Parental_Education_Level, y = Exam_Score, fill = Parental_Education_Level,
                                  text = paste("Parental Education Level:", Parental_Education_Level,
                                               "<br>Exam Score:", Exam_Score))) +
      geom_boxplot() +
      labs(title = "Exam Scores by Parental Education Level",
           x = "Parental Education Level",
           y = "Exam Score") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  # --- Main Feature 2: Analytical Visualizations ---
  
  # Correlation Heatmap of Key Numerical Variables
  # Visualizes Pearson correlation coefficients among key numerical variables to identify significant relationships.
  output$corrHeatmap <- renderPlotly({
    # Define numerical variables
    numeric_vars <- c("Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores", 
                      "Tutoring_Sessions", "Physical_Activity", "Exam_Score")
    
    # Select only complete cases for the correlation
    corr_data <- student_data %>%
      select(all_of(numeric_vars)) %>%
      drop_na()
    
    corr_matrix <- cor(corr_data, use = "complete.obs")
    
    # Create a heatmap
    corr_df <- as.data.frame(as.table(corr_matrix))
    names(corr_df) <- c("Var1", "Var2", "Correlation")
    
    p <- ggplot(corr_df, aes(Var1, Var2, fill = Correlation, 
                             text = paste("Correlation:", round(Correlation, 2)))) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name="Pearson\nCorrelation") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
        axis.title = element_blank(),
        axis.ticks = element_blank()
      ) +
      coord_fixed()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Scatter Plot: Hours Studied vs. Exam Score
  # Examines the direct relationship between study hours and exam performance.
  output$scatterPlot <- renderPlotly({
    p <- ggplot(student_data, aes(x = Hours_Studied, y = Exam_Score, color = Gender,
                                  text = paste("Gender:", Gender,
                                               "<br>Hours Studied:", Hours_Studied,
                                               "<br>Exam Score:", Exam_Score))) +
      geom_point(alpha = 0.7) +
      labs(title = "Scatter Plot: Hours Studied vs. Exam Score",
           x = "Hours Studied",
           y = "Exam Score") +
      theme_minimal()
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(title = list(text = '<b>Gender</b>')))
  })
  
  # --- Main Feature 3: Interactive Data Explorer ---
  
  # Dynamic Data Table with Comprehensive Filtering
  # Enables users to filter data based on multiple criteria for targeted analysis.

  # Reactive expression for filtered data
  filtered_data <- reactive({
    student_data %>%
      filter(
        (Gender == input$genderFilter | input$genderFilter == "All"),
        (School_Type == input$schoolTypeFilter | input$schoolTypeFilter == "All"),
        (Parental_Education_Level == input$parentEducationFilter | input$parentEducationFilter == "All"),
        (Motivation_Level == input$motivationFilter | input$motivationFilter == "All"),
        (Internet_Access == input$internetAccessFilter | input$internetAccessFilter == "All"),
        Hours_Studied >= input$hoursStudiedFilter[1],
        Hours_Studied <= input$hoursStudiedFilter[2],
        Sleep_Hours >= input$sleepHoursFilter[1],
        Sleep_Hours <= input$sleepHoursFilter[2]
      )
  })
  
  # Render the filtered data table
  output$filteredTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Render the total number of students after filtering
  output$studentCount <- renderText({
    count <- nrow(filtered_data())
    paste("Total Number of Students:", count)
  })
  
  # Download handler for the filtered data
  output$downloadData <- downloadHandler(
    filename = "filtered_student_data.csv",
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
