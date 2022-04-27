
#Packages
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)


#get necessary data
if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

######################Userinterface
ui <- fluidPage(
#Selecting input data
  #Allow user the choice between visualizing the number of injuries or the population-standardized rate
  #using selectInput() because it makes both states explicit, and it would be easy to add new states in the future
  #set default to rate
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
#Table format
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),

#Plot format
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),

#Narrative UI (adding the written notes for injuries)
  #add an action button to trigger a new story, 
  #and put the narrative in a textOutput()
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

#Truncate the tables. 
  #convert the variable to a factor, 
  #order by the frequency of the levels, 
  #and then lump together all levels after the top 5.
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

############Server

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
#Table-server
  #Truncated tables output +
  #format to take up the maximum width (i.e. fill the column that they appear in): width = 100%
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  #>>
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
#Plot-server  
  #added select input() condition from UI to generate plot (Rate v. Count), default set to rate. 
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  #>>
  
#<< narrative-server
  # use eventReactive() to create a reactive that only updates when the button is clicked or the underlying data changes
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())
  #>>
}


# Run the application 
shinyApp(ui = ui, server = server)
