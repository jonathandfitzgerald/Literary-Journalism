library(shiny)
library(ggplot2)  # for the diamonds dataset
library(shinyjs)
library(tidyverse)
library(plotly)
packageVersion('plotly')
useShinyjs()
allBib <- read_csv(file = "allbib.csv")
skinnyBib <- read_csv(file = "skinnybib.csv")
skinnyBib <- skinnyBib[c(-1)]
timelineBib <- read_csv(file = "timelinebib.csv")

server <- function(input, output) {
  
  
  observeEvent(input$resetYear, {
    reset("Year")
  })
  
  # Bibliography Tab
  # choose columns to display
  data = skinnyBib
  
  # Filter data based on selections
  output$mytable1 <- DT::renderDataTable(DT::datatable({
    if (input$Source != "All") {
      data <- data[data$Source == input$Source,]
    }
    if (input$`Author Last Name` != "All") {
      data <- data[data$`Author Last Name` == input$`Author Last Name`,]
    }
    if (input$Type != "All") {
      data <- data[data$Type == input$Type,]
    }
    data <- data[data$Year <= input$Year[2],]
    data <- data[data$Year >= input$Year[1],]
    
  }))
  
  
  
  
  # Timeline Tab
  # customize the length drop-down menu; display 5 rows per page by default
  output$allBibPlot <- renderPlotly({
    
    if (input$Type2 == "All") {
      allTime <- timelineBib %>%
        filter(Year >= input$Year2[1]) %>%
        filter(Year <= input$Year2[2]) %>%
        #filter(Type == input$Type2) %>%
        group_by(Year) %>% 
        mutate(Count = n()) %>%
        ggplot() +
        geom_point(alpha=.8) + 
        geom_line() +
        aes(x=Year,y=Count, color=Type) + 
        ggtitle("Number of Primary and Critical Works of Literary Journalism Published by Year") 
    }
    
    if (input$Type2 != "All") {
      allTime <- timelineBib %>%
        filter(Year >= input$Year2[1]) %>%
        filter(Year <= input$Year2[2]) %>%
        filter(Type == input$Type2) %>%
        group_by(Year) %>% 
        mutate(Count = n()) %>%
        ggplot() +
        geom_point(alpha=.8) + 
        geom_line() +
        aes(x=Year,y=Count, color=Type) + 
        ggtitle("Number of Primary and Critical Works of Literary Journalism Published by Year") 
    }
    allTime
  })
  
  # Gender tab
  # sorted columns are colored now because CSS are attached to them
  output$GenderPlot <- renderPlotly({

    
 
      # Render a barplot
      if (input$gender == "All") {
        genderFilter = allBib %>%
          filter(Year >= input$Year3[1]) %>%
          filter(Year <= input$Year3[2]) %>%
          filter(gender != "Anonymous", Year >= 1800) %>%
          group_by(gender) %>% 
          mutate(gendercount = n()) %>% 
          ggplot() +
          geom_bar() +
          aes(x=Year, fill=gender) + 
          if (input$sepgraph == TRUE) {facet_wrap( ~ gender,ncol = 1)  
          }
      }
      
      if (input$gender != "All") {
        genderFilter = allBib %>%
          filter(Year >= input$Year3[1]) %>%
          filter(Year <= input$Year3[2]) %>%
          filter(gender != "Anonymous", Year >= 1800) %>%
          filter(gender == input$gender) %>%
          group_by(gender) %>% 
          mutate(gendercount = n()) %>% 
          ggplot() +
          geom_bar() +
          aes(x=Year, fill=gender)  
          
      }

    
    genderFilter
  })
  
}


ui <- fluidPage(
  title = 'The Continuous Line: Visualizing the History of American Literary Journalism',
  titlePanel("The Continuous Line: Visualizing the History of American Literary Journalism "),
  p("A work-in-progress by Jonathan D. Fitzgerald."),
  sidebarLayout(
    sidebarPanel(
      
      # BIBLIOGRAPHY PANEL
      conditionalPanel(
        
        'input.dataset === "Bibliography"',
        div(
          id = "tab1",
          selectInput("Source",
                      "Source:",
                      c("All",
                        sort(unique(as.character(skinnyBib$Source))))),
          selectInput("Author Last Name",
                      "Author Last Name:",
                      c("All",
                        sort(unique(as.character(skinnyBib$`Author Last Name`))))),
          
          selectInput("Type",
                      "Primary/Secondary Source:",
                      c("All",
                        sort(unique(as.character(skinnyBib$Type))))),
          
          sliderInput("Year", "Year:",
                      min = 1800, max = 2017, value = c(1800,2017),sep = ""),
          
          #actionButton("resetYear", "Reset Year"),
          
          hr()
        )
      ),
      
      # LINE PANEL
      conditionalPanel(
        'input.dataset === "The Line"',
        selectInput("Type2",
                    "Primary/Secondary Source:",
                    c("All",
                      sort(unique(as.character(skinnyBib$Type))))),
        sliderInput("Year2", "Year:",
                    min = 1800, max = 2017, value = c(1800,2017),sep = ""),
        hr(),
        helpText('Display 5 records by default.')
      ),
      
      # GENDER PANEL
      conditionalPanel(
        'input.dataset === "Gender"',
        selectInput("gender", "Gender:", 
                    c("All",
                      "female","male")),
        sliderInput("Year3", "Year:",
                    min = 1800, max = 2017, value = c(1800,2017),sep = ""),
        checkboxInput("sepgraph", "Separate graphs", FALSE),
        verbatimTextOutput("value"),
        hr()
        
        #        helpText('Click the column header to sort a column.')
        
        
      ),
      
      p("This project is in BETA. Email", a("Fitz", href="mailto:jonathanfitzgerald@me.com"), "with any suggestions, bug reports, etc.")
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('Bibliography', DT::dataTableOutput('mytable1')),
        tabPanel('The Line', plotlyOutput("allBibPlot")),
        tabPanel('Gender', plotlyOutput("GenderPlot"))
      )
    )
  )
)

shinyApp(ui = ui, server = server)