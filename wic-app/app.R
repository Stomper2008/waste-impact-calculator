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
# fe_no_records <- 5  #this is just a limiter during app development
impact_factors <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/impact_factors_deq.Rdata"
  ) %>%
  as.data.frame()


# DATA FOR THE FREE ENTRY PAGE
# creating the list of materials and end-of-life dispositions
# available for users to enter weights and mileages.
fe_mat_disp_combos <- 
  impact_factors %>%
  filter(LCstage == "endOfLife") %>% 
#  sample_n(fe_no_records) %>%
  select(material, disposition, impliedMiles) %>%
  unique() %>%
  mutate(
#    baseline_tons=round(runif(n=fe_no_records, min=0, max=10),1),
    baseline_tons=0,
    baseline_miles=impliedMiles,
#    alternative_tons=round(runif(n=fe_no_records, min=0, max=10),1),
    alternative_tons=0,
    alternative_miles=impliedMiles
  ) %>%
  select(-impliedMiles) %>%
  as.data.frame() %>%
  arrange(material, disposition)

# A TABLE FORMAT FOR THE FREE ENTRY PAGE
fe_sketch <-
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Material'),
        th(rowspan=2, 'Disposition'),
        th(colspan = 2, 'BASELINE scenario', style="text-align:center"),
        th(colspan = 2, 'ALTERNATIVE scenario', style="text-align:center")
      ),
      tr(
        lapply(rep(c('tons', 'miles'), 2), th)
      )
    )
  )
)


# DEFINE USER INTERFACE
ui <- 
  fluidPage(
#    theme="sandstone.css",
    tabsetPanel(
      tabPanel(title="WASTE IMPACT CALCULATOR"),
      tabPanel(
        title="free entry",
        fluidRow(
          column(
            5,
            wellPanel(
              h3("Enter your solid waste data"),
              DTOutput("x1")
            )
          ),
          column(
            6,
            selectInput(
                inputId="fe_ImpactCategoryChoice",
                label=h3("Choose your impact category"),
                choices = unique(impact_factors$impactCategory),
                selected="Energy demand",
                width="100%"
            ),
            fluidRow(
              column(
                3,
                h3("Weights"),
                plotOutput("x5") #weight chart
              ),
              column(
                3,
                offset=3,
                h3("Impacts"),
                plotOutput("x6") #impact chart
              )
            )
          )
        ),
        fluidRow(
          h3("Your data with impacts"),
          DTOutput("x4")
        )
      ), # close tabPanel "free entry"
      tabPanel(title="something else")
    ) #close tabsetPanel
  ) #close fluidpage
# end ui definition

# server
server <- function(input, output) {
  
  output$x1 <- 
    renderDT(
      fe_mat_disp_combos,
      container=fe_sketch, #use header format defined previously
      # filter = "top",
      selection = 'none',
      rownames = FALSE,
      editable = TRUE,
      style="bootstrap",
      class="table table-sm"
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
      fe_mat_disp_combos[i, j] <<- 
        DT::coerceValue(v, fe_mat_disp_combos[i, j])
      replaceData(
        proxy, fe_mat_disp_combos, resetPaging = FALSE, rownames=FALSE
      )  # important
    } else {}
    # fe_mat_disp_combos
    
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
      select(scenario, material, disposition, tons, miles) %>%
      filter(tons>0)
  })
  
#  output$x2 <- renderTable(fe_mat_disp_combos_2())
  
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
  
#  output$x3 <- renderTable(fe_mat_disp_combos_3())
  
  # merging with impact factors
  fe_combos_with_impacts_detailed <-
    reactive({
      left_join(
        fe_mat_disp_combos_3(),
        impact_factors,
        by = c("material", "disposition")
      ) %>%
      mutate(
        miles=ifelse(LCstage!="endOfLifeTransport",impliedMiles,miles)
      ) %>%
      arrange(
        impactCategory, material, scenario, LCstage, disposition
      ) %>%
      mutate(
        impact=tons*impactFactor*miles/impliedMiles
      )
    })
  
  output$x4 <- 
    renderDT(
      fe_combos_with_impacts_detailed(),
      filter="top",
      rownames=F
      )
  
  # summing tons, for tons chart
  fe_combos_with_tons <- reactive({
    summarise(
      group_by(
        fe_mat_disp_combos_2(),
        scenario,
        disposition
      ),
      tons=sum(tons)
    ) %>% 
    ungroup() %>%
    arrange(scenario, disposition) %>%
    mutate(
      scenario=factor(scenario, levels=c("baseline", "alternative"))
    )
  })
  
  output$x5 <- renderPlot({
    # draw the weight chart
    ggplot()+
      ggtitle("short tons")+
      theme_fivethirtyeight()+
      geom_bar(
        data=fe_combos_with_tons(),
        aes(x=scenario, y=tons, fill=disposition),
        alpha=0.7,
        stat="identity",
        position="stack"
      )+
#      coord_flip()+
      theme(legend.position="bottom")
    },
    height=500,
    width=375
    ) #close renderPlot
  
  # summing impacts, for the impacts chart
  # here are impacts by LC stage
  fe_combos_with_impacts <- reactive({
    summarise(
      group_by(
        fe_combos_with_impacts_detailed(),
        impactCategory,
        impactUnits,
        scenario,
        LCstage
      ),
      impact=sum(impact)
    ) %>% 
    ungroup() %>%
    arrange(impactCategory, scenario, LCstage) %>%
    mutate(
      scenario=
        factor(scenario, levels=c("baseline", "alternative")),
      LCstage=
        factor(
          LCstage, 
          levels=c("endOfLifeTransport", "endOfLife", "production")
        )
    )
  }) #close reactive
  
  # and here are total impacts for the scenario
  fe_summed_impacts <- reactive({
    summarise(
      group_by(
        fe_combos_with_impacts(), 
        impactCategory, impactUnits, scenario
      ),
      impact=sum(impact)
    ) %>%
    ungroup() %>%
    arrange(impactCategory, scenario)
  }) # close reactive
  
  # figuring the impact chart title
  fe_impact_chart_title <- reactive({
    paste(
      # impact category name
      unique(
        filter(
          fe_summed_impacts(), 
          impactCategory==input$fe_ImpactCategoryChoice)$impactCategory
      ),
      " (",
      # impact category units
      unique(
        filter(
          fe_summed_impacts(), 
          impactCategory==input$fe_ImpactCategoryChoice)$impactUnits
      ),
      ")",
      sep=""
    )
  }) # close reactive
  
  # draw the impact chart
  output$x6 <- renderPlot({
  ggplot()+
    ggtitle(fe_impact_chart_title())+
    theme_fivethirtyeight()+
    geom_bar(
      data=filter(
        fe_combos_with_impacts(), 
        impactCategory==input$fe_ImpactCategoryChoice
      ),
      aes(x=scenario, y=impact, fill=LCstage, color=LCstage),
      alpha=0.7,
      stat="identity",
      position="stack"
    )+
    geom_point(
      data=filter(
        fe_summed_impacts(), 
        impactCategory==input$fe_ImpactCategoryChoice
      ),
      aes(x=scenario, y=impact),
      shape=21,
      size=10,
      fill="orange"
    )
    # +
    # coord_flip()  
  },
  height=500,
  width=375
  )  # close renderPlot
  
  } # close server

# Run the application 
shinyApp(ui = ui, server = server)