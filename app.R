library(shiny)
library(ggplot2)
library(dplyr)

titanic <- read.csv("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")
titanic$Survived <- as.factor(titanic$Survived)

ui <- fluidPage(    
  
  titlePanel("Demographics by Passenger Class"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with pclass input
    sidebarPanel(
      selectInput("pclass", "Passenger Class:", 
                  choices=unique(titanic$Pclass)),
      hr(),
      helpText("Ticket class of Titanic passengers.")
    ),
    
    # Create spots for plots
    mainPanel(
      plotOutput("agePlot"),
      plotOutput("survivalPlot"),
      plotOutput("farePlot")
    )
    
  )
)


server <- function(input, output) {
  
  output$agePlot <- renderPlot({
    
    ggplot(titanic %>% filter(Pclass == input$pclass) %>% select(Age)) +
      geom_histogram(mapping = aes(x = Age, fill=Age)) +
      labs(title = "Distribution of Age")
    
  })
  
  output$survivalPlot <- renderPlot({
    
    ggplot(titanic %>% filter(Pclass == input$pclass) %>% group_by(Survived) %>% summarise(number = n())) +
      geom_bar(mapping = aes(x = Survived, y = number, fill=Survived), stat="identity") +
      labs(title = "Number of Survivors/Deaths")
    
  })
  
  output$farePlot <- renderPlot({
    
    ggplot(titanic %>% filter(Pclass == input$pclass) %>% select(Fare)) +
      geom_histogram(mapping = aes(x = Fare)) +
      labs(title = "Distribution of Fare")
  })
}

shinyApp(ui = ui, server = server)
