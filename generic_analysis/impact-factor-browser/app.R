#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggthemes)
library(viridis)

# load in the data and set some factors 
impactSourcesToCompare <-
  readRDS("impactSourcesToCompare.RData")
myPivotedTable <-
  readRDS("myPivotedTable.RData")
materialList <-
  impactSourcesToCompare %>%
  pull(material) %>%
  unique()
dispositionList <-
  impactSourcesToCompare %>%
  pull(disposition) %>%
  unique()
impactGroupList <-
  impactSourcesToCompare %>%
  pull(impactGroup) %>%
  unique()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Comparing WIC and WARM impact factors"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        width=3,
        selectInput(
          inputId = "materialChoice",
          label = "choose a material",
          choices = materialList,
          selected = "Cardboard"
        ),
        checkboxGroupInput(
          inputId = "dispositionChoice",
          label = "choose a disposition",
          choices = dispositionList,
          selected = "production",
          inline=TRUE
        ),
        radioButtons(
          inputId = "impactGroupChoice",
          label = "choose an impact type",
          choices = impactGroupList,
          selected = "Energy",
          inline = TRUE
        ),
        downloadButton(
          outputId = "all_factor_csv",
          label = "Download all factors (CSV)"
        )
      ), # close sidebarPanel   
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "chart",
            plotOutput("impactPlot")
          ),
          tabPanel(
            title = "table",
            DT::dataTableOutput("impactTable")
          )
        ) # close tabsetPanel
      )  #close mainPanel
   ) # close sidebarLayout
   ) #close fluidPage

# Define server logic 
server <- function(input, output) {
  
  output$all_factor_csv <-
    downloadHandler(
      filename = "all_factors.csv",
      content = function(file) {
        write.csv(
          myPivotedTable,
          file=file,
          row.names=FALSE
        )
      }
    )
  
  myPlotData <- reactive({
    impactSourcesToCompare %>%
    filter(
      material %in% input$materialChoice &
      disposition %in% input$dispositionChoice &
      impactGroup %in% input$impactGroupChoice
    )
  })
  
  output$impactTable <- DT::renderDataTable({myPlotData()})
  
  myPlotUnits <- reactive({
    pull(myPlotData(), impactUnits) %>%
    unique()
  })
  
  myPlotWarmMaterial <- reactive({
    pull(myPlotData(), warmMaterial) %>%
    unique()
  })
  
  myPlotTitle <- reactive({
    paste(
      input$impactGroupChoice,
      " impacts",
      " (in ",
      myPlotUnits(),
      "), for\n",
      str_to_upper(input$materialChoice),
      "\n(WARM material: ",
      myPlotWarmMaterial(),
      ")",
      sep=""
    )
  })
   
  myPlot <- reactive({
    ggplot()+
    theme_fivethirtyeight()+
    ggtitle(myPlotTitle())+
    geom_bar(
      data=myPlotData(),
      aes(
        x = impactCategory,
        y = impactFactor,
        fill = ifSource
      ),
      stat="identity"
    )+
    scale_fill_viridis(
      begin=0.3, 
      end=0.9, 
      discrete = TRUE,
      option="D"
    )+
    facet_grid(disposition~.)+
    coord_flip()+
    theme(
      rect=element_rect(fill="transparent")
    )
  })
  
  output$impactPlot <-
    renderPlot({myPlot()},height=525)

}

# Run the application 
shinyApp(ui = ui, server = server)

