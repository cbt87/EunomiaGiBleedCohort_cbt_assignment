#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


#importing libraries and connecting to the database
library(shiny)
library(shinythemes)
library(tidyverse)
library(forcats)
library(lubridate)

# Setting a global theme
theme_set(
  theme_minimal(base_size = 14))


connectionDetails <- Eunomia::getEunomiaConnectionDetails()
# connect to Eunomia GiBleed
connection <- DatabaseConnector::connect(connectionDetails)

# Build GiBleed Cohorts
Eunomia::createCohorts(connectionDetails = connectionDetails)

# creating necessary dataframes
sql <- "
SELECT concept.concept_name, nested.* FROM (

SELECT cohort.COHORT_START_DATE, co.* FROM cohort INNER JOIN person ON
cohort.SUBJECT_ID = person.PERSON_ID INNER JOIN condition_occurrence co ON
cohort.SUBJECT_ID = co.PERSON_ID
WHERE co.CONDITION_START_DATE <  cohort.COHORT_START_DATE AND 
cohort.COHORT_DEFINITION_ID = 4
) nested INNER JOIN concept ON
nested.CONDITION_CONCEPT_ID = concept.concept_id
;"

baseline_conditions <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()

#creating dataframe that remove non-distinct combos of concept names and person ids
baseline_conditions_distinct <- baseline_conditions |>
  distinct(CONCEPT_NAME, PERSON_ID, .keep_all=TRUE)




sql <- "
SELECT concept.concept_name, nested.* FROM (

SELECT cohort.COHORT_START_DATE, co.* FROM cohort INNER JOIN person ON
cohort.SUBJECT_ID = person.PERSON_ID INNER JOIN condition_occurrence co ON
cohort.SUBJECT_ID = co.PERSON_ID
WHERE co.CONDITION_START_DATE >=  cohort.COHORT_START_DATE AND 
cohort.COHORT_DEFINITION_ID = 4
) nested INNER JOIN concept ON
nested.CONDITION_CONCEPT_ID = concept.concept_id
;"

outcomes <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
  tibble::as_tibble()

#creating dataframe that remove non-distinct combos of concept names and person ids
outcomes_distinct <- outcomes |>
  distinct(CONCEPT_NAME, PERSON_ID, .keep_all=TRUE)


# Define UI for application 
ui <- fluidPage(
    theme = shinytheme("flatly"),

    # Application title
    titlePanel("NSAID Cohort Characterization"),

    # Sidebar with a slider input for number of conditions 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Top n conditions:",
                        min = 1,
                        max = 50,
                        value = 30)
        ,
            radioButtons("count_method", "Choose a method with which to count most common conditions:", 
                        choices = c("Count of Condition Occurrences" = "condition_occurrence", 
                               "Count of Patients with Condition" = "patient"),
                   selected = "condition_occurrence")
    ),
        # Show barcharts
        mainPanel(
          uiOutput("plot_ui")
        )
    )
)

# Define server logic 
server <- function(input, output) {
  
  output$plot_ui <- renderUI({
    if (input$count_method == "condition_occurrence") {
      tagList(
        plotOutput("baseline_occurrence"),
        plotOutput("outcomes_occurence")
      )
    } else {
      tagList(
        plotOutput("baseline_patient"),
        plotOutput("outcomes_patient")
      )
    }
  })
  
  #default plots to render
  
  output$baseline_occurrence <- renderPlot({
    ggplot(baseline_conditions, aes(x = fct_rev(fct_lump_n(fct_infreq(CONCEPT_NAME), n = input$n)))) + 
      geom_bar(fill = "steelblue", color = "black") +
      coord_flip() +
      labs(x = "Condition", y = "Count of Condition Occurrences", 
           title = paste("Top", input$n, "Baseline Conditions by Occurrence Count for NSAID Cohort"))
  })
  
  output$outcomes_occurence <- renderPlot({
    ggplot(outcomes, aes(x = fct_rev(fct_lump_n(fct_infreq(CONCEPT_NAME), n = input$n)))) + 
      geom_bar(fill = "steelblue", color = "black") +
      coord_flip() +
      labs(x = "Condition", y = "Count of Condition Occurrences", 
           title = paste("Top", input$n, "Outcomes by Occurrence Count post-index for NSAID Cohort"))
  })
  
  # logic to determine what happens if the radio buttons are selected
  
  observeEvent(input$count_method, {
    
    if (input$count_method == "condition_occurrence") {
      
      output$baseline_occurrence <- renderPlot({
        ggplot(baseline_conditions, aes(x = fct_rev(fct_lump_n(fct_infreq(CONCEPT_NAME), n = input$n)))) + 
          geom_bar(fill = "steelblue", color = "black")+
          coord_flip()+
          labs(x = "Condition", y = "Count of Condition Occurrences", 
               title = paste("Top", input$n, "Baseline Conditions by Occurrence Count for NSAID Cohort"))
      })
      
      output$outcomes_occurence <- renderPlot({
        ggplot(outcomes, aes(x = fct_rev(fct_lump_n(fct_infreq(CONCEPT_NAME), n = input$n)))) + 
          geom_bar(fill = "steelblue", color = "black")+
          coord_flip() +
          labs(x = "Condition", y = "Count of Condition Occurrences", 
               title = paste("Top", input$n, "Outcomes by Occurrence Count post-index for NSAID Cohort"))
      })
      
      # Clear the other output if not needed
      output$baseline_patient <- renderPlot(NULL)
      output$outcomes_patient <- renderPlot(NULL)
      
    } else if (input$count_method == "patient") {
      
      output$baseline_patient <- renderPlot({
        ggplot(baseline_conditions_distinct, aes(x = fct_rev(fct_lump_n(fct_infreq(CONCEPT_NAME), n = input$n)))) + 
          geom_bar(fill = "steelblue", color = "black")+
          coord_flip()+
          labs(x = "Condition", y = "Count of patients exhibiting condition at least once", 
               title = str_wrap(paste("Top", input$n, "Baseline Conditions by Distinct Patient Count for NSAID Cohort"), width = 40))
      })
      
      output$outcomes_patient <- renderPlot({
        ggplot(outcomes_distinct, aes(x = fct_rev(fct_lump_n(fct_infreq(CONCEPT_NAME), n = 30)))) + 
          geom_bar(fill = "steelblue", color = "black")+
          coord_flip() + 
          labs(x = "Condition", y = "Count of patients exhibiting condition at least once",
               title = str_wrap(paste("Top", input$n,"Outcomes by Distinct Patient Count Post-Index for NSAID Cohort"), width = 40))
        
      })
      
      # Clear unnecessary plots
      output$baseline_occurrence <- renderPlot(NULL)
      output$outcomes_occurence <- renderPlot(NULL)
    }
    
  }, ignoreInit = TRUE)  # Ignore initial event to avoid unnecessary computation at startup
}
# Run the application 
shinyApp(ui = ui, server = server)
