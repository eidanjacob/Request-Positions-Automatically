library(shiny)
library(readr)
library(dplyr)
library(stringr)

sentences_df = read_csv("../sentences.csv")
sentences_df$Id = 1:nrow(sentences_df)
sentences_df = sentences_df %>% mutate(Id, Sentences, Tags)

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
                htmlOutput("cover")
            ),
            wellPanel(
                textInput("selections",
                          "Sentence Ids")
            ),
            dataTableOutput("sentences")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$cover = renderUI(
        {
            sentence_ids = str_split(input$selections, ", ") %>% unlist() %>% as.numeric()
            selected_sentences = sentences_df$Sentences[sentence_ids]
            
            selected_sentences = gsub("<org>", input$org, selected_sentences)
            selected_sentences = gsub("<role>", input$role, selected_sentences)
            selected_sentences = gsub("<cause>", input$cause, selected_sentences)
            HTML(paste(c(selected_sentences, "Eidan Jacob"), collapse = "<br/><br/>"))
        }
    )
    
    output$sentences = renderDataTable(
        filter_tag(input$includes, input$excludes),
        options = list(
            pageLength = 20
        )
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
