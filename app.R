# Load the required libraries
library(shiny)
library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud2)
library(plotly)
library(stringr)
library(curl)
library(rsconnect)

# Load the dataset
data <- read.csv("D:/Materials/4/DATA VISUALIZATION/R/Project/sentimentdataset.csv")

# Preprocess text data
preprocess_text <- function(text) {
  text <- tolower(text)
  text <- gsub("\\d+", "", text)
  text <- gsub("[^[:alnum:] ]", "", text)
  return(text)
}
data$Text <- lapply(data$Text, preprocess_text)

# Preprocess country names
data$Country <- tolower(trimws(data$Country))  # Convert to lowercase and remove leading/trailing spaces
data$Country <- gsub(" ", "", data$Country)   # Remove spaces in country names
data$Country <- stringr::str_to_title(data$Country)  # Convert to title case

# Preprocess sentiment column
data$Sentiment <- str_trim(data$Sentiment)  # Remove leading and trailing spaces
data$Sentiment <- str_to_title(data$Sentiment)  # Convert to title case

# Map sentiments to main categories
data$Sentiment <- ifelse(data$Sentiment %in% c("Joy", "Happiness", "Love", "Amusement", "Enjoyment", "Admiration",
                                               "Affection", "Awe", "Elation", "Euphoria", "Contentment", "Serenity",
                                               "Gratitude", "Hope", "Empowerment", "Compassion", "Tenderness", "Arousal",
                                               "Enthusiasm", "Fulfillment", "Reverence", "Inspiration", "Motivation",
                                               "Contemplation", "Joyfulreunion", "Satisfaction", "Blessed", "Appreciation",
                                               "Confidence", "Accomplishment", "Wonderment", "Optimism", "Pride", "Positive",
                                               "Positivity", "Kindness", "Friendship", "Success", "Harmony", "Grateful",
                                               "Empathetic", "Compassionate", "Playful", "Free-spirited", "Inspired",
                                               "Confident", "Hopeful", "Proud", "Zest", "Contentment", "Serenity",
                                               "Joy In Baking", "Renewed Effort", "Challenge", "Solace", "Breakthrough",
                                               "Harmony", "Engagement", "Touched", "Suspense", "Satisfaction", "Admiration",
                                               "Triumph", "Heartwarming", "Relief", "Embarrassed", "Happy"),
                         "Positive",
                         ifelse(data$Sentiment %in% c("Anger", "Fear", "Sadness", "Disgust", "Disappointed", "Despair",
                                                      "Grief", "Loneliness", "Jealousy", "Resentment", "Frustration", "Boredom",
                                                      "Anxiety", "Intimidation", "Helplessness", "Envy", "Regret", "Negative",
                                                      "Hate", "Bad", "Mischievous", "Overwhelmed", "Dismissive", "Disgust",
                                                      "Heartbreak", "Betrayal", "Suffering", "Emotionalstorm", "Lostlove", "Melancholy",
                                                      "Exhaustion", "Sorrow", "Darkness", "Desperation", "Ruins", "Desolation",
                                                      "Miscalculation", "Obstacle", "Pressure", "Challenge", "Betrayal", "Heartbreak",
                                                      "Resilience", "Sorrow", "Loss", "Heartache", "Solitude", "Sad", "Hate", "Bad"),
                                "Negative", "Neutral"))



# Define UI for application
ui <- fluidPage(
  titlePanel("Social Media Sentiment Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("platform", "Select Platform:", c("All", unique(data$Platform))),
      selectInput("sentiment", "Select Sentiment:", c("All", unique(data$Sentiment))),
      selectInput("country", "Select Country (Optional):", c("All", unique(data$Country))),
      dateRangeInput("date_range", "Select Date Range:", start = min(data$Timestamp), end = max(data$Timestamp)),
      sliderInput("hour_range", "Select Hour Range:", min = 0, max = 23, value = c(0, 23), step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment Analysis", plotlyOutput("sentiment_plot")),
        tabPanel("Word Cloud", wordcloud2Output("wordcloud")),
        tabPanel("Platform Distribution", plotlyOutput("platform_distribution")),
        tabPanel("Sentiment Distribution", plotlyOutput("sentiment_distribution_plot")),
        tabPanel("Hourly Activity", plotlyOutput("hourly_activity")),
        tabPanel("Sentiment by Country", plotlyOutput("sentiment_country_plot")),
        tabPanel("Sentiment by Platform", plotlyOutput("sentiment_platform_plot")),
        tabPanel("Treemap Plot", plotlyOutput("treemap_plot")),
        tabPanel("Nested Pie Chart", plotlyOutput("nested_pie_chart"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    filtered <- data
    if(input$platform != "All") {
      filtered <- filtered[filtered$Platform == input$platform, ]
    }
    if(input$sentiment != "All") {
      filtered <- filtered[filtered$Sentiment == input$sentiment, ]
    }
    if(input$country != "All") {
      filtered <- filtered[filtered$Country == input$country, ]
    }
    filtered <- filtered[filtered$Timestamp >= input$date_range[1] & filtered$Timestamp <= input$date_range[2], ]
    filtered <- filtered[filtered$Hour >= input$hour_range[1] & filtered$Hour <= input$hour_range[2], ]
    return(filtered)
  })
  
  # Group data by month and calculate sentiment counts
  monthly_sentiment_counts <- reactive({
    filtered <- filtered_data()
    
    if (!is.null(filtered)) {
      sentiment_data <- filtered %>%
        mutate(Month = format(as.Date(Timestamp), "%Y-%m")) %>%
        count(Month, Sentiment) %>%
        spread(Sentiment, n, fill = 0)
      
      return(sentiment_data)
    } else {
      # Return a dataframe with zero counts for all sentiments
      return(data.frame(Month = as.Date(character()), Positive = integer(), Negative = integer(), Neutral = integer()))
    }
  })
  
  # Plot sentiment analysis with trend lines
  output$sentiment_plot <- renderPlotly({
    sentiment_data <- monthly_sentiment_counts()
    
    plot_ly(sentiment_data, x = ~Month) %>%
      add_trace(y = ~Positive, name = "Positive", type = "scatter", mode = "lines", line = list(color = "green")) %>%
      add_trace(y = ~Negative, name = "Negative", type = "scatter", mode = "lines", line = list(color = "red")) %>%
      add_trace(y = ~Neutral, name = "Neutral", type = "scatter", mode = "lines", line = list(color = "grey")) %>%
      layout(title = "Sentiment Analysis Over Months",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Count"))
  })
  
  # Generate word cloud
  output$wordcloud <- renderWordcloud2({
    wordcloud_data <- filtered_data() %>%
      unnest_tokens(word, Text) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      head(100)
    wordcloud2(wordcloud_data, size = 0.6, color = "random-dark")
  })
  
  # Generate platform distribution plot
  output$platform_distribution <- renderPlotly({
    platform_dist <- filtered_data() %>%
      count(Platform) %>%
      plot_ly(labels = ~Platform, values = ~n, type = "pie", textinfo = "label+percent", hoverinfo = "text", text = ~paste(Platform, ": ", n))
    platform_dist <- platform_dist %>% layout(title = "Platform Distribution")
    return(platform_dist)
  })
  
  # Generate sentiment distribution plot
  output$sentiment_distribution_plot <- renderPlotly({
    sentiment_dist <- filtered_data() %>%
      count(Sentiment) %>%
      plot_ly(x = ~Sentiment, y = ~n, type = "bar", color = ~Sentiment, colors = c("green", "grey", "red")) %>%
      layout(title = "Sentiment Distribution", xaxis = list(title = "Sentiment"), yaxis = list(title = "Count"))
    return(sentiment_dist)
  })
  
  # Generate hourly activity plot
  output$hourly_activity <- renderPlotly({
    hourly_activity <- filtered_data() %>%
      mutate(Hour = factor(Hour)) %>%
      count(Hour) %>%
      plot_ly(x = ~Hour, y = ~n, type = "bar", text = ~paste("Hour: ", Hour, "<br>Count: ", n),
              hoverinfo = "text", marker = list(color = "rgb(158,202,225)", line = list(color = "rgb(8,48,107)", width = 1.5))) %>%
      layout(title = "Hourly Activity", xaxis = list(title = "Hour"), yaxis = list(title = "Count"))
    return(hourly_activity)
  })
  
  # Dynamically generate plots comparing sentiment with country
  output$sentiment_country_plot <- renderPlotly({
    sentiment_country_data <- filtered_data() %>%
      count(Sentiment, Country) %>%
      plot_ly(x = ~Country, y = ~n, color = ~Sentiment, type = "bar", text = ~paste("Country: ", Country, "<br>Sentiment: ", Sentiment, "<br>Count: ", n),
              hoverinfo = "text") %>%
      layout(title = "Sentiment Distribution by Country", xaxis = list(title = "Country"), yaxis = list(title = "Count"))
    return(sentiment_country_data)
  })
  
  
  # Generate nested pie chart for sentiment distribution by country
  output$nested_pie_chart <- renderPlotly({
    nested_pie_data <- filtered_data() %>%
      count(Country, Sentiment) %>%
      plot_ly(labels = ~Country, parents = ~Sentiment, values = ~n, type = 'sunburst', branchvalues = 'total') %>%
      layout(title = "Sentiment Distribution by Country")
    return(nested_pie_data)
  })
  
  # Generate treemap plot for sentiment distribution by country
  output$treemap_plot <- renderPlotly({
    treemap_data <- filtered_data() %>%
      count(Country, Sentiment) %>%
      plot_ly(type = 'treemap', labels = ~Country, parents = ~Sentiment, values = ~n, 
              hoverinfo = "label+value+percent rootpercent")
    treemap_data <- treemap_data %>% layout(title = "Sentiment Distribution by Country",
                                            margin = list(l = 10, r = 10, b = 10, t = 50, pad = 4))
    return(treemap_data)
  })
  
  
  # Dynamically generate plots comparing sentiment with platform
  output$sentiment_platform_plot <- renderPlotly({
    sentiment_platform_data <- filtered_data() %>%
      count(Sentiment, Platform) %>%
      plot_ly(x = ~Platform, y = ~n, color = ~Sentiment, type = "bar", text = ~paste("Platform: ", Platform, "<br>Sentiment: ", Sentiment, "<br>Count: ", n),
              hoverinfo = "text") %>%
      layout(title = "Sentiment Distribution by Platform", xaxis = list(title = "Platform"), yaxis = list(title = "Count"))
    return(sentiment_platform_data)
  })
}

# Run the application
rsconnect::setAccountInfo(name='karamalhanatleh', token='AFE66A28EAC7081CA0E14AA2A7CA4F07', secret='EPBMwV01aelxFPDu0IZDlnc8qwtRw5PiLvG7IwFB')
shinyApp(ui = ui, server = server)


deployApp(appDir = "/path/to/your/app/directory")
