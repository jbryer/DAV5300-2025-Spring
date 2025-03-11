library(shiny)
library(ggplot2)

source('chi_squared_power.R')

##### UI
ui <- fluidPage(
    titlePanel("Chi-Squared Power"),

    sidebarLayout(
        sidebarPanel(
        	textOutput('message'),
        	radioButtons(
        		inputId = 'dimensions',
        		label = 'Dimensions',
        		choices = c("One" = 1,
        					"Two" = 2)
        	),
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
        			max = 4,
        			value = 2,
        			step = 1
        		)
        	),
        	# hr(),
        	# strong("Probabilities"),
        	# uiOutput('prob_input'),
            actionButton(inputId = 'run',
            			 label = 'Run',
            			 icon = icon('person-running'))
        ),

        mainPanel(
        	tabsetPanel(
        		tabPanel(
        			title = 'Prababilities',
        			uiOutput('prob_input'),
        		),
        		tabPanel(
        			title = 'Plot',
        			plotOutput("plot")
        		)
        	)
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
		n_rows <- input$k
		n_cols <- ifelse(input$dimensions == 1, 1, input$k2)
		for(row in 1:n_rows) {
			row_inputs <- list()
			for(col in 1:n_cols) {
				row_inputs[[col]] <- column(
					width = 12 / n_cols,
					numericInput(
						inputId = paste0('prob', row, col),
						label = "",#paste0(row, ', ', col),
						min = 0, max = 1,
						value = 0
					)
				)
			}
			sliders[[row]] <- do.call(fluidRow, row_inputs)
		}
		do.call(div, sliders)
	})

	output$plot <- renderPlot({

	})
}

##### Run the application
shinyApp(ui = ui, server = server)
