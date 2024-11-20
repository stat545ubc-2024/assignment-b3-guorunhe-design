
# Student Performance Dashboard

![Shiny App](https://img.shields.io/badge/Shiny-App-4F5B93?logo=shiny&logoColor=white)

## [Click Here: Live Demo](https://guorunhe.shinyapps.io/assignment-b3-guorunhe-design/)

## Description

The **Student Performance Dashboard** is an interactive Shiny application designed to analyze and visualize various factors influencing student exam performance. By integrating key metrics, aggregate visualizations, and dynamic data exploration tools, this dashboard provides educators, researchers, and stakeholders with valuable insights into the determinants of student success.

### Features

1. **Performance Overview**
   - **Key Metrics Display:** Showcases average exam scores, hours studied, sleep hours, physical activity levels, and tutoring sessions.
   - **Summary Visualizations:** Includes visual representations such as exam score distributions across motivation levels, exam scores by teacher quality, average exam scores by school type, and exam scores by parental education level.

2. **Analytical Visualizations**
   - **Correlation Heatmap:** Visualizes Pearson correlation coefficients among key numerical variables to identify significant relationships.
   - **Scatter Plot:** Explores relationship between study hours and exam scores to uncover patterns and trends.

3. **Interactive Data Explorer**
   - **Dynamic Filtering:** Allows users to filter the dataset based on multiple criteria including gender, school type, parental education level, motivation level, internet access, hours studied, and sleep hours.
   - **Filtered Data Table:** Displays the filtered dataset in an interactive table format for detailed examination and analysis.

## Dataset

The application utilizes the `StudentPerformanceFactors.csv` dataset, which contains comprehensive data on various factors affecting student performance.

### Source

- **Title:** Student Performance Factors
- **Source:** https://www.kaggle.com/datasets/lainguyn123/student-performance-factors
- **License:** CC0: Public Domain

## Installation

To run the **Student Performance Dashboard** locally, follow these steps:

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/stat545ubc-2024/assignment-b3-guorunhe-design
   ```

2. **Navigate to the Directory:**

   ```bash
   cd assignment-b3-guorunhe-design
   ```

3. **Install Required R Packages:**

   Ensure you have the following R packages installed. You can install any missing packages using the `install.packages()` function in R.

   ```r
   install.packages(c(
     "shiny",
     "shinydashboard",
     "ggplot2",
     "plotly",
     "DT",
     "dplyr",
     "GGally",
     "shinythemes",
     "corrplot",
     "tidyr"
   ))
   ```

4. **Run the Application:**

   Open R or RStudio, set the working directory to the cloned repository, and run:

   ```r
   library(shiny)
   runApp("app.R")
   ```

   Alternatively, if you're using RStudio, you can open `app.R` and click the "Run App" button.
