#Name shiny App

# Shiny app is a basic web application designed to capture the user's name.
# It features a single input field where users can enter their name. 
# Upon entering their name, a personalized greeting message is displayed 
# below the input field. This app demonstrates a simple interaction between 
# the user interface and server logic in Shiny, showcasing how user input 
# can be captured and used to generate dynamic output.

###########################
#load library
library(shiny)

ui<-fluidPage(
  
  titlePanel("Name Input"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput("name","ENter Name :","Karam")
    ),#sidebarPanel
    
    
    mainPanel(
      textOutput("output")
    )
    
  )#sidebarLayout
)#fluidPage


#server logic
server <-function(input,output){
  output$output<-renderText({
    paste("Hello , ",input$name ," your welcome ! ")
  })
}

shinyApp(ui , server = server)

