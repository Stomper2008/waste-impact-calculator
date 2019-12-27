# file app.R
# working draft of waste impact calculator app

# currently I am working on a "free input page"
# where anyone can enter any tonnage for any available material
# and see the impacts calculated as tons and as impacts

# load packages
library(shiny)
library(tidyverse)
library(ggthemes)
library(DT)

# importing impact factor data
fe_no_records <- 2  #this is just a limiter during app development
impact_factors <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/impact_factors_deq.Rdata"
  ) %>%
  as.data.frame()

# creating a blank small set of material-disposition combinations to work with
# during testing
fe_mat_disp_combos <- 
  impact_factors %>%
  filter(LCstage == "endOfLife") %>% 
  sample_n(fe_no_records) %>%
  unique() %>%
  select(material, disposition, impliedMiles) %>%
  mutate(
    baseline_tons=round(runif(n=fe_no_records, min=0, max=10),1),
    baseline_miles=impliedMiles,
    alternative_tons=round(runif(n=fe_no_records, min=0, max=10),1),
    alternative_miles=impliedMiles
  ) %>%
  select(-impliedMiles) %>%
  as.data.frame() 

# user interface
ui <- 
  fluidPage(
    tabsetPanel(
      tabPanel(title="something"),
      tabPanel(
        title="free entry",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId="fe_ImpactCategoryChoice",
              label="impact category",
              choices = unique(impact_factors$impactCategory),
              selected="Energy demand"
            ),
            h2("the original table"),
            tableOutput("x0"),
            h2("An editable table"),
            DTOutput("x1"),
            h2("Edits preserved and transformed"),
            tableOutput("x2"),
            h3("Edited data with production tons"),
            tableOutput("x3"),
            h3("Edited data with impacts"),
            tableOutput("x4"),
            width=6
          ),
          mainPanel(
            h2("Edits used in graphic output"),
            h2("table with impacts"),
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
  
  output$x0 <- renderTable(fe_mat_disp_combos)
  
  output$x1 <- 
    renderDT(
      fe_mat_disp_combos,
      filter = "top",
      selection = 'none',
      rownames = FALSE,
      editable = TRUE
    )
  
  proxy <- dataTableProxy('x1')
  
  observeEvent(input$x1_cell_edit, {
    info <- input$x1_cell_edit
    str(info)
    i <- info$row
    j <- info$col + 1 #offset because row names are not being used
    v <- info$value
    
    #limits editing to columns specified by j
    if ( j >=3 & j <= 6) {
      fe_mat_disp_combos[i, j] <<- DT::coerceValue(v, fe_mat_disp_combos[i, j])
      replaceData(
        proxy, fe_mat_disp_combos, resetPaging = FALSE, rownames=FALSE
      )  # important
    } else {}
    fe_mat_disp_combos
    
  })
  
  fe_mat_disp_combos_2 <- reactive({
    input$x1_cell_edit
    # here is where the joins and so on would go in
    # flipping so the form is 
    # scenario - material - disposition - tons - miles
      pivot_longer(fe_mat_disp_combos,baseline_tons:alternative_miles, 
                   names_to=c("scenario","thing"), names_sep="_",  
                   values_to=c("quant")) %>% 
      pivot_wider(values_from = "quant", names_from = "thing") %>%
      arrange(desc(scenario), material, disposition) %>%
      select(scenario, material, disposition, tons, miles)
  })
  
  output$x2 <- renderTable(fe_mat_disp_combos_2())
  
  #so, mat_disp_combos_2 has the end-of-life information I need.
  #but production information isn't included.
  #so all I really need to do is double it and change the disposition to production.
  fe_mat_disp_combos_3 <-
    reactive({
      bind_rows(
        fe_mat_disp_combos_2(),
        mutate(
          fe_mat_disp_combos_2(),
          disposition="production",
          miles=0
        )
      ) %>%
      arrange(desc(scenario), material, disposition)
    })  
  
  output$x3 <- renderTable(fe_mat_disp_combos_3())
  
  # merging with impact factors
  fe_combos_with_impacts_detailed <-
    reactive({
      left_join(
        fe_mat_disp_combos_3(),
        impact_factors,
        by = c("material", "disposition")
      ) %>%
      mutate(miles=ifelse(LCstage!="endOfLifeTransport",impliedMiles,miles)) %>%
      arrange(impactCategory, material, scenario, LCstage, disposition) %>%
      mutate(
        impact=tons*impactFactor*miles/impliedMiles
      )
    })
  
  output$x4 <- renderTable(fe_combos_with_impacts_detailed())
  
  } # close server

# Run the application 
shinyApp(ui = ui, server = server)