#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(forcats)
library(lubridate)
library(DescTools)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
# connect to Eunomia GiBleed
connection <- DatabaseConnector::connect(connectionDetails)

# Build GiBleed Cohorts
Eunomia::createCohorts(connectionDetails = connectionDetails)

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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NSAID Cohort Characterization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Top n conditions:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barchart1"),
           plotOutput("barchart2"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barchart1 <- renderPlot({
        ggplot(baseline_conditions, aes(x = fct_rev(fct_lump_n(fct_infreq(CONCEPT_NAME), n = input$n)))) + 
          geom_bar()+
          coord_flip()+
          labs(x = "Condition", y = "Count of Condition Occurrences", title = "Top 30 Baseline Conditions by Occurrence Count for NSAID Cohort")
    })
    
    output$barchart2 <- renderPlot({
      ggplot(outcomes, aes(x = fct_rev(fct_lump_n(fct_infreq(CONCEPT_NAME), n = input$n)))) + 
        geom_bar()+
        coord_flip() + 
        theme_minimal()+
        labs(x = "Condition", y = "Count of Condition Occurrences", title = "Top 30 Outcomes by Occurrence Count post-index for NSAID Cohort")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
