###################### ###
#Create a Simple R Shiny Dashboard 

#load library
library(shiny)
library(shinydashboard)


ui<-dashboardPage(
  dashboardHeader(title = "My dashboard"),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("corr_plot") , width = 10)
    
  )
)



server<-function(input,output){
  output$corr_plot <-renderPlot({
    plot(iris$Sepal.Length , iris$Petal.Length)
    
  })
}

shinyApp(ui , server)
