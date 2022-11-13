library(dplyr)
library(mlr3verse)
library(shiny)
library(shinyWidgets)
library(DT)
library(ggplot2)

# make dataset
set.seed(42)
n = 100
df = data.table::data.table(X1 = runif(n), X2 = runif(n))
df$target = factor(ifelse(df$X1 - 1.2*df$X2 < 0, 'A', 'B'),
  levels = c('A', 'B'))
df[26]$target = 'A'

task = TaskClassif$new(id = 'Almost Linear Binary Task',
  backend = df, target = 'target', positive = 'A')

df %>% ggplot(aes(x = X1, y = X2, color = target)) +
  geom_point()

# Define UI
ui = fluidPage(

    # Application title
    titlePanel('Effect of cost value on SVM model'),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          sliderTextInput(
            inputId = 'cost',
            label = 'cost (C) value:',
            choices = c(0.1, 0.5, 1, 10, 100, 1000),
            grid = TRUE,
            selected = 1000
          ),
          span(textOutput(outputId = 'nSVs'), style='font-size:30px')
        ),

        # SVM plot
        mainPanel(
           plotOutput('svmPlot')
        )
    )
)

# Define server logic required to draw a svm plot
server = function(input, output) {
    # train and save SVM learner
    trainedSVM = reactive({
      learner = lrn('classif.svm', scale = FALSE, kernel = 'linear',
        type = 'C-classification', cost = input$cost)
      learner$train(task)
      learner$model
    })

    output$nSVs = renderText({
      paste('Number of support vectors: ', sum(trainedSVM()$nSV))
    })

    output$svmPlot = renderPlot({
      # get trained SVM model
      svm_model = trainedSVM()

      # calculate slope, intercept of decision boundary from weight vector and svm model
      w = t(svm_model$coefs) %*% svm_model$SV
      slope = -w[1]/w[2]
      intercept = svm_model$rho/w[2]
      M = 1/w[2] # margin

      task$data() %>%
        ggplot(aes(x = X1, y = X2, color = target)) +
        geom_point() +
        # add Support Vectors
        geom_point(data = as.data.frame(svm_model$SV),
          aes(x = X1, y = X2),
          color = '#ff80ff', size = 7, alpha = 0.3) +
        # add decision boundary
        geom_abline(slope = slope, intercept = intercept, color = 'purple', linewidth = 1) +
        # add margin boundaries
        geom_abline(slope = slope, intercept = intercept - M, linetype = 'dashed') +
        geom_abline(slope = slope, intercept = intercept + M, linetype = 'dashed') +
        theme_classic(base_size = 14) +
        scale_color_manual(values = c('red', 'blue'))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
