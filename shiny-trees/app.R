library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)

# load tree models and task data
data_list = readRDS(file = 'models.rds') # see `models.R`
complexities = sapply(data_list, function(el) { el$cp })
task_data = readRDS(file = 'spam_data.rds')

get_treeplot = function(cp, data_list) {
  for (list_el in data_list) {
    if (list_el$cp == cp) return(list_el$tree_plot)
  }
}

# Define UI
ui = fluidPage(

    # Application title
    titlePanel('Effect of complexity value on Tree size'),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          sliderTextInput(
            inputId = 'cp',
            label = 'Complexity value (cp):',
            choices = complexities,
            grid = TRUE
          )
        ),

        # Show a plot of the trained model tree
        mainPanel(
           plotOutput('treePlot')
        )
    ),

    basicPage(
      hr(style = "border-top: 1px solid #000000;"),
      DT::dataTableOutput('spamDataTable')
    )
)

# Define server logic
server = function(input, output) {
    output$spamDataTable = DT::renderDataTable(expr =
      DT::datatable(
        data = task_data,
        caption = 'Sample of spam dataset from UCI ML repo (4601 x 58)',
        options = list(searching = FALSE)
      ) %>%
        DT::formatStyle(columns = 'type', backgroundColor =
          DT::styleEqual(c('spam', 'nonspam'), c('#f18384', '#9cd49a')))
      )

    output$treePlot = renderPlot({
        cp = input$cp
        get_treeplot(cp, data_list)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
