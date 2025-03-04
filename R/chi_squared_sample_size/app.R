library(shiny)
library(ggplot2)

##### UI
ui <- fluidPage(
    titlePanel("Chi-Squared Significance"),

    sidebarLayout(
        sidebarPanel(
        	textOutput('message'),
            sliderInput("k",
                        "Number of groups:",
                        min = 2,
                        max = 10,
                        value = 3),
            actionButton(inputId = 'run',
            			 label = 'Run',
            			 icon = icon('person-running'))
        ),

        mainPanel(
           plotOutput("plot")
        )
    )
)

##### Server
server <- function(input, output) {
	message <- reactiveVal('')

	output$message <- renderText({
		message()
	})

	get_data <- reactive({
		# if(all(probs[1] == probs)) {
		# 	# All the probabilities are the same
		# }
	})

	output$prob_input <- renderUI({
		sliders <- list()
	})

	output$plot <- renderPlot({

	})
}

##### Run the application
shinyApp(ui = ui, server = server)
