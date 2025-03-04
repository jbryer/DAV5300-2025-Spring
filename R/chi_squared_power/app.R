library(shiny)
library(ggplot2)

source('chi_squared_power.R')

##### UI
ui <- fluidPage(
    titlePanel("Chi-Squared Power"),

    sidebarLayout(
        sidebarPanel(
        	textOutput('message'),
        	sliderInput(inputId = 'dimensions',
        				label = 'Dimensions',
        				min = 1,
        				max = 2,
        				step = 1,
        				value = 1),
            sliderInput(inputId = "k",
                        label = "Number of groups:",
                        min = 2,
                        max = 10,
                        value = 3,
            			step = 1
            ),
        	conditionalPanel(
        		condition = "input.dimensions == '2'",
        		sliderInput(
        			inputId = 'k2',
        			label = "Number of groups for variable 2",
        			min = 2,
        			max = 10,
        			value = 2,
        			step = 1
        		)
        	),
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
