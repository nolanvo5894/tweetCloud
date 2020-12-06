library('shiny')
library('tidytext')
library('rtweet')
library('dplyr')
library('wordcloud2')


ui <- fluidPage(
  
  # app title
  titlePanel('Make Your Own Tweet Clouds'),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = 'n_tweets',
                   label = 'Number of Tweets',
                   value = 100),
      textInput(inputId = 'topic',
                label = 'Topic You Care'),
      actionButton(inputId = 'submit',
                   label = 'Make Cloud')
      ),
    
    mainPanel(
    h1('Fun with Tweets'),
    wordcloud2Output(outputId = 'tweetCloud')
  )
)
)


server <- function(input, output){
  
  observeEvent(input$submit, {
    
  tweets <- search_tweets(input$topic, 
                         n = input$n_tweets)
  
  gsub("https\\S*", "", tweets$text) 
  gsub("@\\S*", "", tweets$text) 
  gsub("amp", "", tweets$text) 
  gsub("[\r\n]", "", tweets$text)
  gsub("[[:punct:]]", "", tweets$text)
  
  
  tweets_words <-  tweets %>%
    select(text) %>%
    unnest_tokens(word, text)
  
  
  words <- tweets_words %>% count(word, sort=TRUE)
  
  
  output$tweetCloud <- renderWordcloud2({
    wordcloud2(data = words, 
               size = 1.6,
               color = 'random-light')})
  })
}


shinyApp(ui = ui, server = server)
  
  
  
  
  
  
  
  