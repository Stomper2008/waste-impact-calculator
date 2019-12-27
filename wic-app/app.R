# file app.R
# working draft of waste impact calculator app

# currently I am working on a "free input page"
# where anyone can enter any tonnage for any available material
# and see the impacts calculated as tons and as impacts

# load packages
library(shiny)
library(tidyverse)
library(DT)

# load data
no_records <- 2
impact_factors <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/impact_factors_deq.Rdata"
    ) %>%
  as.data.frame()
mat_disp_combos <- 
  impact_factors %>%
  filter(LCstage == "endOfLife") %>% 
  sample_n(no_records) %>%
  unique() %>%
  select(material, disposition, impliedMiles) %>%
  mutate(
    baseline=round(runif(n=no_records, min=0, max=10),1),
    baselineMiles=impliedMiles,
    alternative=round(runif(n=no_records, min=0, max=10),1),
    alternativeMiles=impliedMiles
  ) %>%
  select(-impliedMiles) %>%
  as.data.frame()

# user interface
ui <- 
  fluidPage(
    tabsetPanel(
      tabPanel(title="something"),
      tabPanel(
        title="enter your own",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId="eyoImpactCategoryChoice",
              label="impact category",
              choices = unique(impact_factors$impactCategory),
              selected="Smog"
            ),
            h2("the original table"),
            tableOutput("x0"),
            h2("An editable table"),
            DTOutput("x1"),
            h2("Edits preserved and transformed"),
            DTOutput("x2"),
            h2("tons"),
            tableOutput("x2a"),
            h2("mileage"),
            tableOutput("x2b"),
            h2("combined"),
            tableOutput("x2c"),
            width=6
          ),
          mainPanel(
            h2("Edits used in graphic output"),
            plotOutput(outputId="x3"),
            h2("table with impacts"),
            tableOutput("x4"),
            width=6
          )
        )
      ),
      tabPanel(title="something else")
    )
  ) #close fluidpage
# end ui definition

# server
server <- function(input, output) {
  
  output$x0 <- renderTable(mat_disp_combos)
  
  output$x1 <- renderDT(mat_disp_combos, 
                        selection = 'none',
                        editable = TRUE
  )
  
  proxy <- dataTableProxy('x1')
  
  observeEvent(input$x1_cell_edit, {
    info <- input$x1_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    
    #limits editing to columns specified by j
    if ( j >=3 & j <= 6) {
      mat_disp_combos[i, j] <<- DT::coerceValue(v, mat_disp_combos[i, j])
      replaceData(proxy, mat_disp_combos, resetPaging = FALSE)  # important
    } else {}
    mat_disp_combos
    
  })
  
  y <- reactive({
    input$x1_cell_edit
    # here is where the joins and so on would go in
    mat_disp_combos %>% mutate(baseline=baseline*10) %>%
      pivot_longer(
        baseline:alternativeMiles,
        names_to="tonnageType",
        values_to="tons"
      )
  })
  
  ya <- reactive({
    filter(
      y(),
      tonnageType=="baseline" | tonnageType=="alternative"
    ) %>%
      mutate(LCstage="endOfLife")
  })
  
  yb <- reactive({
    filter(
      y(),
      tonnageType=="baselineMiles" | tonnageType=="alternativeMiles"
    ) %>%
      mutate(LCstage="endOfLifeTransport")
  })
  
  yc <- reactive({
    bind_rows(ya(),yb()) %>%
      arrange(material, LCstage, disposition)
  })
  
  output$x2 <- renderDT(y(),
                        selection = 'none')
  
  output$x2a <- renderTable(ya())
  output$x2b <- renderTable(yb())
  output$x2c <- renderTable(yc())
  
  
  output$x3 <- renderPlot({
    ggplot()+
      geom_point(
        data=y(),
        aes(x=material, y=tons, fill=tonnageType, color=tonnageType),
        size=5
      )
  })
  
  z <- reactive({
    left_join(
      y(),
      impact_factors,
      by=c("material", "disposition")
    ) %>%
      mutate(impact=tons*impactFactor)
  })
  
  output$x4 <- renderTable(
    z() %>% filter(impactCategory==input$eyoImpactCategoryChoice)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)