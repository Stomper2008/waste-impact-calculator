# file app.R
# working draft of waste impact calculator app

# this is a pretty big shiny app
# about the relationship between solid waste and life cycle 
# environmental impacts

# it's a whole tutorial and laboratory in a single app!
# so maybe it's best to list the tabs in the tabset, because
# they summarize the main functions of the app.

# perhaps the main motif that the app presents is "keeping
# the total in mind"... we want to actually lower the total
# impact.  so individual results should be in the context 
# of the total.

# "WASTE IMPACT CALCULATOR" is the first tab.  Yes, it's the
# name of the app.  Here users can read a few words about the 
# app and watch an instructional video about how to use the 
# other tabs and the basic "perspective" of the app...
# which is converting solid waste stream info into estimated
# life cycle impacts.

# "Context" is the next tab.  It shows how the life cycle 
# impacts of materials are not the same as the life cycle 
# impacts of the waste stream.  Users may select to see
# estimates of the difference between waste and materials 
# for several Oregon "wastesheds" (counties, etc.).
# (So here, shiny is filtering
# a pre-existing data frame based on user choice.)

# The "Weight vs. Impacts" tab gets more interactive.  Drawing from 
# a large pre-prepared file of weights and impact calculations, 
# the user can see how the weight of materials in chosen waste-
# sheds compares to the life cycle impacts of materials.  This 
# can help them spot materials with disproportionately high or 
# low impacts... materials they may want to pursue or ignore.

# The "Recycling vs. Reduction" tab allows users to play with
# the differences between recycling and reduction.  They 
# choose one or more materials and wastesheds.  They see 
# estimated life cycle impacts for those materials for three
# scenarios: zero recovery, the current mix of recovery and
# disposal, and a "maximized recovery" scenario.  Usually
# there aren't big differences between these.  They can also
# move one or more sliders to see how reducing X tons of 
# waste has the same benefit as recycling Y tons of it.  
# Reduction is Y/X times as beneficial as recovery.

# The "Enter your own waste" tab allows users to 
# compare the life cycle impacts of waste materials using
# a free entry format.  They can enter tonnages of various
# materials, alone or in combination, and compare the 
# impacts associated with them.  They can also enter
# mileages for end-of-life transport to see how those
# affect things (not much, usually).



# load packages
library(shiny)
library(tidyverse)
library(ggthemes)
library(DT)
library(plotly)

# importing some graphic conventions (to be expanded later)
source(file="../oregon_deq/resources/theme_539.R")

# creating a default chart style
theme_539 <- function() {
  theme_fivethirtyeight() +
  theme(
    rect=element_rect(fill=NA)
  )
}

# importing impact factor data
impact_factors <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/impact_factors_deq.Rdata"
  ) %>%
  as.data.frame()

# importing data for a key chart
weight_vs_impact_chart_data <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/weight_vs_impact_chart_data.RData"
  )

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
      tabPanel(title="Context"),
      tabPanel(
        title="Weight vs. Impacts",
        column(
          3,
          wellPanel(
            selectInput(
              inputId="wvi_wasteshed_choice",
              label="choose a wasteshed",
              choices=unique(weight_vs_impact_chart_data$wasteshed),
              selected = "Metro"
            ),
            selectInput(
              inputId="wvi_impact_cat_choice",
              label="choose an impact category",
              choices = unique(weight_vs_impact_chart_data$impactCategory),
              selected="Global warming"
            )
          )
        ),
        column(
          9,
          plotOutput("wvi_chart")
        )
      ), # close tabPanel "Weight vs. Impacts"
      tabPanel(title="Recycling vs. Recovery"),
      tabPanel(
        title="Enter your own waste",
        fluidRow(
          column(
            5,
            wellPanel(
              h3("Enter your solid waste data"),
              DTOutput("x1")
            ),
            wellPanel(
              h3("Download results"),
              downloadButton(
                outputId="fe_summaryOfTonsAndImpactsFile",
                label="summary of weight and impacts"
              ),
              downloadButton(
                outputId="fe_impactDetailsFile",
                label="impact calculation details"
              ),
              downloadButton(
                outputId="fe_formattedReport",
                label="nicely formatted report"
              )
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
      ), # close tabPanel "Enter your own"
      tabPanel(title="Documentation")
    ) #close tabsetPanel
  ) #close fluidpage
# end ui definition

# server
server <- function(input, output) {
  
  output$wvi_chart <-
    renderPlot({
      ggplot()+
        theme_539()+
        geom_bar(
          data=
            weight_vs_impact_chart_data %>%
            filter(
              (impactCategory=="Weight" | 
                impactCategory == input$wvi_impact_cat_choice)
              & wasteshed == input$wvi_wasteshed_choice
            ),
          aes(x=material, y=pctOfTotal, color=datatype, fill=datatype),
          alpha=0.5,
          stat="identity",
          position="stack"
        )+
        facet_grid(.~datatype)+
        coord_flip()+
        theme(legend.position = "none")
    })
  
  output$x1 <- 
    renderDT(
      fe_mat_disp_combos,
      options=list(pageLength=7),
      container=fe_sketch, #use header format defined previously
      # filter = "top",
      selection = 'none',
      rownames = FALSE,
      editable = TRUE
      # ,
      # style="bootstrap",
      # class="table table-sm"
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
      theme(legend.position="bottom")
    },
    height=500,
    width=375
    ) 
  
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
  },
  height=500,
  width=375
  )  # close renderPlot
  
  # create the file output
  output$fe_impactDetailsFile <-
    downloadHandler(
      filename="wic_enter_your_own_impact_details.csv",
      content = function(file) {
        write.csv(fe_combos_with_impacts_detailed(), file, row.names = F)
      }
    )
  
  } # close server

# Run the application 
shinyApp(ui = ui, server = server)