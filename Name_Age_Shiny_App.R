
 # This Shiny app is a simple web application that allows users to 
 # input their name and age. The interface consists of two input 
 # fields: one for the name and another for the age. After users
 # enter their information and click elsewhere, a greeting message 
 # is displayed below the input fields, addressing the user by name 
 # and stating their age. This app demonstrates the basic functionality of 
 # capturing user input and generating dynamic output in a Shiny application.


#####################################
# Load  library
library(shiny)

# Define the user interface (UI)
ui <- fluidPage(
  
  # Title panel at the top of the app
  titlePanel("Name and Age Input"),
  
  # Sidebar layout with input fields
  sidebarLayout(
    sidebarPanel(
      # Text input for entering the name
      textInput("name", "Enter your name:", "Karam"),
      
      # Numeric input for entering the age
      numericInput("age", "Enter your age:", value = 23, min = 0)
    ),
    
    # Main panel to display output
    mainPanel(
      textOutput("output")  # Output area for displaying the greeting message
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Render the output based on the input values
  output$output <- renderText({
    paste("Hello,", input$name, "! You are", input$age, "years old.")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
