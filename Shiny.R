# # Load library


library(shiny)
library(shinythemes)

# Define UI
ui <-fluidPage(theme = shinytheme("cosmo"),
               navbarPage(
  "My first app",
  tabPanel("Navebar 1",
           sidebarPanel(
             tags$h3("input"),
             textInput("txt1","given Name: ",''),
             textInput("txt2", "surname:",""),

           ), #sidebarPanel
           mainPanel(
             h1("Header 1 "),
             h3("Output 1"),
             verbatimTextOutput("txtout"),
           )#main panel
           
           ),# Navbar  1 #tabPanel 
  tabPanel("Navbar 2 ",  "This is panel left blank"),
  tabPanel("Navbar 3 ",  "This is panel left blank")
)# navbar Page
)#fluidPage

server<-function()

# Run the app
shinyApp(ui, server)


#server function
server <-function(input,output){
  output$txtout<-renderText({
    paste(input$txt1 , input$txt2 ,sep = " ")
  })
  
}#server

#shiny object
shinyApp(ui=ui , server=server)
