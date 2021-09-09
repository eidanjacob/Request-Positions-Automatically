library(shiny)
library(readr)
library(dplyr)
library(stringr)

sentences_df = read_csv("../sentences.csv")
tags = paste(sentences_df$Tags, collapse = " ") %>%
    str_split(" ") %>%
    unlist() %>%
    unique() %>%
    sort()

filter_tag = function(includes, excludes){
    if(includes == "Show All"){
        return(sentences_df %>% filter(
            sapply(excludes, function(x){ !str_detect(Tags, x) })
        ))
    }
    sentences_df %>% filter(
        sapply(includes, function(x){ str_detect(Tags, x) }) &
            sapply(excludes, function(x){ !str_detect(Tags, x) })
    )
}

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Cover Letter Drafter"),
    
    # Sidebar with inputs 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("includes", 
                        "Show sentences with the tag:",
                        choices = c("Show All", tags),
                        selected = "Opening"),
            
            selectInput("excludes",
                        "Don't show sentences the tag:",
                        choices = c("-", tags)
            ),
            
            textInput("org",
                      "Organization Name"),
            
            textInput("role",
                      "Role"),
            
            textInput("cause",
                      "Cause")
        ),
        
        # Show generated text
        mainPanel(
            wellPanel(
                
            textOutput("cover")
            ),
            dataTableOutput("sentences")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$cover = renderText("test")
    output$sentences = renderDataTable(
        filter_tag(input$includes, input$excludes),
        options = list(
            pageLength = 20
        )
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
