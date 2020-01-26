# file app.R
# working draft of waste impact calculator app

# GENERAL SETUP OF THIS CODE
# The app is layed out as a web site with a navigation
# bar across the top, with drop-down options under each
# navigation bar entry.

# Each navigation bar entry and/or drop-down option corresponds
# to a full-page web display.  E.g. the "weights & impacts" 
# navigation bar leads to several pages, for example one 
# called "where impacts come from."

# The code mostly follows this structure.  Within 3 sections
# of code (SETUP, UI, and SERVER), it tries to keep objects
# and procedures arranged by page display, more or less in the
# order of the navbar entries and drop-down options.

# Objects associated with a particular page display are named
# using a prefix to help distinguish them.  E.g. objects 
# associated with the "where impacts come from" page display
# have a prefix "wcif", e.g. "wcif_weight_data".

# SETUP SECTION

# load packages 
library(shiny)
library(tidyverse)
library(ggthemes)
library(DT)
library(plotly)
library(heatmaply)
library(scales)

# importing some graphic conventions (to be expanded later)
source(file="../oregon_deq/resources/theme_539.R")

# importing impact factor data
impact_factors <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/impact_factors.Rdata"
  ) %>%
  # readRDS(
  #   "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/impact_factors_deq.Rdata"
  # ) %>%
  as.data.frame()

# DATA SPECIFIC TO THE WEIGHT AND RECOVERY RATE PAGE

masses_eol_by_umbDisp <- readRDS(
  "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/masses_eol_by_umbDisp.RData"
)

masses_eol_with_rr <- readRDS(
  "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/masses_eol_with_rr.RData"
)

material_sort_order <- readRDS(
  "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/material_sort_order.RData"
)

# DATA FOR THE WEIGHT VS. IMPACT TAB
weight_vs_impact_chart_data <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/weight_vs_impact_chart_data.RData"
  )

# DATA FOR THE WHERE IMPACTS COME FROM PAGE
wicf_weight_summaries <- readRDS(
  "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/wicf_weight_summaries.RData"
)

wicf_weights <- readRDS(
  "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/wicf_weights.RData"
)
wicf_impacts_net <- readRDS(
  "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/wicf_impacts_net.RData"
)
wicf_impacts_by_lcstage <- readRDS(
  "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/wicf_impacts_by_lcstage.RData"
)

# DATA FOR THE HEATMAP PAGE
# getting the weight portion flipped
hm_11 <- 
  weight_vs_impact_chart_data %>%
  filter(datatype=="weight") %>%
  rename(tons=magnitude, pctTons=pctOfTotal) %>%
  select(-datatype, -impactCategory, -impactUnits)
# getting the impact portion flipped
hm_12 <-
  weight_vs_impact_chart_data %>%
  filter(datatype=="impact") %>%
  rename(impact=magnitude, pctImpact=pctOfTotal) %>%
  select(-datatype)
# now merging them
hm_13 <-
  full_join(
    hm_12,
    hm_11,
    by=c("year","wasteshed","material")
  ) %>%
  mutate(
    proportionateness = pctImpact/pctTons,
    impactCategory = 
      factor(impactCategory, levels=unique(impactCategory))
  )
# limiting to columns I need
# there are two tables because each one heatmap can only have one 
# dependent
hm_14a <-
  hm_13 %>% 
  select(impactCategory, wasteshed, material, pctImpact)
hm_14b <-
  hm_13 %>% 
  select(impactCategory, wasteshed, material, proportionateness)

# DATA SPECIFIC TO RECYCLING VS. REDUCTION TAB
arr_summary_data_1 <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/arr_summary_data_1.RData"
  )
arr_summary_data_2 <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/arr_summary_data_2.RData"
  )
arr_summary_data_3 <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/arr_summary_data_3.RData"
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
  
  # structure ui as a navbar with drop-down selections possible
  navbarPage(
    title="WASTE IMPACT CALCULATOR",
    
    # lay out the introduction page (no drop-down choices)
    tabPanel(
      title="Introduction",
      "A video will go here, for people who don't like to read.
      Or perhaps 2 videos -- an introduction to the 
      idea of a life cycle assessment of the solid waste stream,
      and then an introduction to how to use the app."
    ), #close introduction page
    
    # lay out weights and impacts section
    # (multiple pages selected via drop-down menu)
    navbarMenu(
      title="Weights and impacts",
      
      # lay out contents of waste stream page
      tabPanel(
        title="total weight & impacts of the waste stream",
        sidebarLayout(
          sidebarPanel(
            width=3,
            h5("WHAT THIS CHART SHOWS"),
            "This chart shows how the waste stream can
            differ from the total effects of materials.",
            h5("THINGS TO LOOK OUT FOR"),
            "The total effects of materials are very 
            large.  The materials in waste create some, but not all, of 
            those impacts.  This is because many impactful 
            materials are too long-lived to be noted in
            solid waste statistics."
          ),
          mainPanel(
            width=9,
            "Here is the pie chart, that shows all the stuff not
            in the waste stream.  Probably won't be many options 
            for this -- perhaps Oregon & Metro, and only for GHGs.
            Question: should it include the recovery slice?"
          ) # close mainPanel for contents of waste stream page
        ) # close sidebarlayout for contents of the waste stream page
      ),  # close tabPanel for contents of waste stream page
      
      # lay out weights, recovery rates, & impacts page
      tabPanel(
        title="weights, recovery rates, & impacts",
        sidebarLayout(
        sidebarPanel(
          width=3,
          h5("WHAT THIS PAGE SHOWS"),
          "blah blah",
          h5("WHAT IT MEANS"),
          "blah blah blah"
        ), # close sidebar panel for weights recovery rates & impacts page
        mainPanel(
          width=9,
          fluidRow(
            column(
              width=4,
              plotOutput("wrr_chart")
            ),
            column(
              width=5,
              plotOutput("wvi_chart")
            )
          ),
          fluidRow(
            column(
              width=3,
              selectInput(
                inputId="wvi_wasteshed_choice",
                label="choose a wasteshed",
                choices=unique(wicf_weight_summaries$wasteshed),
                selected = "Metro"
              )
            ),
            column(
              width=3,
              selectInput(
                inputId="wvi_impact_cat_choice",
                label="choose an impact category",
                choices = unique(wicf_impacts_net$impactCategory),
                selected="Global warming"
              )
            ),
            column(
              width=3,
              selectInput(
                inputId="wvi_scenario_choice",
                label="choose a management scenario",
                choices=unique(wicf_weight_summaries$scenario),
                selected="actual"
              )
            )
          )
        ) #close mainpanel for sidebar layout of weights & recovery rates display
        ) # close sidebarlayout
        ), #close weights and recovery rates display
      
        # lay out weights vs. impacts page
        tabPanel(
          title="where impacts come from",
          sidebarLayout(
            sidebarPanel(
              width=3,
              h5("WHAT THIS SHOWS"),
              "The impact chart on the right 
              shows how net life cycle impacts 
              of materials are calculated.  The net 
              impact (the heavy black lines) are the sum of impacts
              for the life cycle stages of production, end-of-life 
              transport, and end-of-life treatment.  If there 
              has been recycling or other recovery activity, 
              then the end-of-life impact may be negative, which
              lowers the net impact.  You can try changing the 
              management scenario to see if increasing 
              disposal or recovery will substantially change the 
              net impacts.",
              h5("WHAT TO LOOK OUT FOR"),
              "A bit of experimenting with this chart and its 
              options will start to suggest several things.  
              First, the great bulk of impacts associated with 
              materials come from production. Recycling activity 
              usually reduces those impacts but can't entirely
              eliminate them.  Second, end-of-life transportation 
              impacts are usually relatively small."
            ),
            mainPanel(
              width=9,
              fluidRow(
                column(
                  width=4,
                  plotOutput("wicf_weight_chart")
                ),
                column(
                  width=5,
                  plotOutput("wicf_impact_chart")
                )
              ),
              fluidRow(
                column(
                  width=3,
                  selectInput(
                    inputId="wicf_wasteshed_choice",
                    label="choose a wasteshed",
                    selected="Metro",
                    choices=unique(wicf_weights$wasteshed)
                  )
                ),
                column(
                  width=3,
                  selectInput(
                    inputId="wicf_impactCategory_choice",
                    label="choose an impact category",
                    selected="Smog",
                    choices=unique(wicf_impacts_net$impactCategory)
                  )
                ),
                column(
                  width=3,
                  selectInput(
                    inputId="wicf_scenario_choice",
                    label="choose a management scenario",
                    selected="actual",
                    choices=unique(wicf_impacts_net$scenario)
                  )
                )
              )
            ) #close mainpanel of sidebarlayout for weights vs impacts display
          ) #close sidebarlayout for weights vs. impacts display
        ) # close tabPanel for weights vs. impacts display
      ), #close navbarmenu for "weights & impacts" section
    
        # user interface for the impact hotspot (heatmap) section
        navbarMenu(
          title="Impact hotspots",
          
          tabPanel(
            title="Impact intensities",  
          sidebarLayout(
            sidebarPanel(
            width=3,
            h5("WHAT THIS CHART SHOWS"),
            "This chart makes it easier to evaluate impacts for 
            many different impact categories (for example, 
            global warming and water use) in a single glance. 
            The more impact a material has within an impact 
            category, the brighter the color.  For example, 
            food waste often represents a large portion of 
            water use impacts.  Each column totals to 100% of 
            the wasteshed's total impacts for that impact 
            category.  Float your pointer over a 
            square to see the numeric data.",
            "In addition, this chart tries to group 
            similarly sized impacts together.  For example, 
            high water consumption impacts are often 
            grouped with high eutrophication impacts.  
            These groupings are expressed by the wiry 
            dendrogram across the top of the chart.",
            h5("WHAT TO LOOK OUT FOR"),
            "This chart is likely to show you that 
            materials are not at all equal!  Certain kinds 
            of impacts are likely to be concentrated among 
            one or two materials.  In addition, sometimes 
            impacts go together."
          ), # end sidebarPanel
          mainPanel(
            width=9,
            fluidRow(
              plotlyOutput(outputId = "hm_chart_pctImpact")
            ),
            fluidRow(
              wellPanel(
                selectInput(
                  inputId="hm_wasteshed_choice",
                  label="choose a wasteshed",
                  choices = unique(hm_13$wasteshed),
                  selected = "Metro"
                )
              )
            )
            #,
            #tableOutput("hm_matrix")
          ) # end mainPanel
        ) #end sidebarlayout
        ), #end impact intensities page

        # lay out impact disproportionality page        
        tabPanel(
          title="impact disporportionality",
          "blah blah blah"
        ) # end impact disproportionality page
        
        ), #end impact hotspots section
      
      # ui for the Ways to reduce impacts section
      navbarMenu(
        title="Ways to reduce impacts",
        
        tabPanel(
          title="recycling and its limits (ARR)"
        ),
        
        tabPanel(
        title="recycling vs. reduction",
        sidebarLayout(
        sidebarPanel(
          width=3,
          wellPanel(
            h4("WHAT THIS PAGE SHOWS"),
            "blah blah",
            textOutput("rvr_statement")
          ),
          wellPanel(
            h4("WHAT IT MEANS"),
            "blah blah"
          )
        ), # close sidebarPanel
        mainPanel(
        fluidRow(
          column(
            width=6,
#            "weight chart with labels goes here",
            plotOutput("rvr_weight_chart")
            # ,
            # tableOutput("rvr_weight_table"),
            # tableOutput("rvr_weight_table_alt")
            ),
          column(
            width=6,
#            "impact chart without labels goes here",
            plotOutput("rvr_impact_chart")
            # ,
            # tableOutput("rvr_impact_table"),
            # tableOutput("rvr_impact_table_alt")
          )
        ),
      # fluidRow(
      #   tableOutput("rvr_impact_model_table")
      # ),
      fluidRow(
        wellPanel(
          selectInput(
            inputId = "rvr_wasteshed_choice",
            label = "choose a wasteshed",
            choices = unique(arr_summary_data_1$wasteshed),
            selected = "Benton"
          ),
          selectInput(
            inputId = "rvr_impactCategory_choice",
            label = "choose an impact category and standard",
            choices = unique(arr_summary_data_1$impactCategory),
            selected = "Global warming"
          )
        ),# close wellPanel
        wellPanel(
          h4("YOUR MANAGEMENT SCENARIO"),
          sliderInput(
            inputId="rvr_wbrr",
            label="your weight-based recovery rate (%)",
            min = 0, 
            max = 100,
            value=0, 
            step = 2
          ),
          sliderInput(
            inputId="rvr_wbrdr",
            label="reduce waste generation to (%)",
            min=0,
            max=150,
            value=100,
            step=2
          )
        ) # close wellPanel
      )
      ) # close mainpanel
      )  # close sidebarLayout
      ),  # close the page layout

      tabPanel(
        title="strategy chart",
        "put evolving strategy chart here, where there is the 
        radial strategy chart, and you can add things to it by 
        clicking on individual materials and/or impact categories."
      )

      ), # close navbar menu for the ways to reduce impacts section
      
      # ui for the "enter your own" tab
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
    
    # defining ui for the documentation page
    tabPanel(
      title="Documentation",
      "Links to pdf documents will go here."
    )
) #close navbarpage

# end ui definition

# server
server <- function(input, output) {
  
  # generating output for the weight and recovery rate tab
  output$wrr_chart <- renderPlot({
  ggplot()+
    ggtitle("Weights and recovery rates")+
    theme_539()+
    geom_bar(
      data=wicf_weights %>% 
        filter(
          wasteshed==input$wvi_wasteshed_choice,
          scenario==input$wvi_scenario_choice,
          optVariant %in% 
            c("actual", "dispose_all", input$wvi_impact_cat_choice)
          ),
      aes(
        x = factor(material, levels=material_sort_order), 
        y = tons, color=umbDisp, fill=umbDisp, alpha=umbDisp
      ),
      stat="identity"
    )+
    geom_text(
      data=wicf_weight_summaries %>% 
        filter(
          wasteshed==input$wvi_wasteshed_choice,
          scenario==input$wvi_scenario_choice,
          optVariant %in% 
            c("actual", "dispose_all", input$wvi_impact_cat_choice)
        ),
      aes(
        x=material, 
        y=tons,
        label=percent(round(wbrr, 2))
      ),
      hjust=-0.1
    )+
    scale_y_continuous(name="short tons", labels=comma)+
    coord_flip()+
    scale_colour_manual(values=rev(c("aquamarine4","steelblue4")))+
    scale_fill_manual(values=rev(c("aquamarine4", "steelblue4")))+
    scale_alpha_manual(values=c(0.25,0.75))+
    theme(
      legend.position=c(0.7,0.2),
      axis.title=element_text(),
      axis.title.y=element_blank(),
      axis.title.x=element_text()
    )
  })
  
  # generating output for the weight vs. impacts tab
  output$wvi_chart <-
    renderPlot({
      ggplot()+
        ggtitle("life cycle impacts")+
        theme_539()+
        geom_bar(
          data=
            wicf_impacts_net %>%
            filter(
              impactCategory==input$wvi_impact_cat_choice,
              wasteshed == input$wvi_wasteshed_choice,
              scenario == input$wvi_scenario_choice,
              optVariant %in%
                c("actual", "dispose_all", input$wvi_impact_cat_choice)
            ),
          aes(
            x=factor(material, levels=material_sort_order), 
            y=impact
            ),
          alpha=0.5,
          stat="identity",
          position="stack"
        )+
        coord_flip()+
        theme(legend.position = "none")
    })
  
  # generating output for the where impacts come from page

  output$wicf_weight_chart <- renderPlot({
    ggplot()+
    theme_539()+
    ggtitle("Weight chart")+
    geom_bar(
      data=
        wicf_weights %>%
        filter(
          wasteshed==input$wicf_wasteshed_choice,
          scenario==input$wicf_scenario_choice,
          optVariant %in% 
            c("actual", "dispose_all", 
              input$wicf_impactCategory_choice
              )
        ),
      aes(
        x=material,
        y=tons,
        color=umbDisp,
        fill=umbDisp,
        alpha=umbDisp
      ),
      stat="identity",
      position="stack"
    ) +
    coord_flip()
  }
  ) # close plot definition for wicf_weight_chart

  output$wicf_impact_chart <- renderPlot({  
    ggplot()+
    theme_539()+
    ggtitle("impact chart")+
    geom_bar(
      data=
        wicf_impacts_by_lcstage %>%
        filter(
          wasteshed==input$wicf_wasteshed_choice,
          scenario==input$wicf_scenario_choice,
          optVariant %in% c(
            "actual", "dispose_all", input$wicf_impactCategory_choice
          ),
          impactCategory==input$wicf_impactCategory_choice
        ),
      aes(
        x=factor(material, levels=material_sort_order),
        y=impact,
        color=LCstage,
        fill=LCstage
      ),
      stat="identity",
      position="stack",
      alpha=0.3
    )+
    geom_bar(
      data=
        wicf_impacts_net %>%
        filter(
          wasteshed==input$wicf_wasteshed_choice,
          scenario==input$wicf_scenario_choice,
          optVariant %in% c(
            "actual", "dispose_all", input$wicf_impactCategory_choice
          ),
          impactCategory==input$wicf_impactCategory_choice
        ),
      aes(
        x=material,
        y=impact
      ),
      color="black",
      fill=NA,
      size=2,
      stat="identity"
    )+
    coord_flip()
  }) # close definition of wicf_impact_chart
  
  # REACTIVE OBJECTS FOR THE HEATMAP PAGE
  # second try: make a plotly heatmap from input-filtered data
  hm_15aa <- reactive({
    hm_14a %>%
      filter(wasteshed==input$hm_wasteshed_choice) %>%
      select(-wasteshed) %>%
      pivot_wider(
        names_from = "impactCategory",
        values_from = "pctImpact"
      ) %>%
      as.data.frame() %>%
      mutate(material=as.character(material)) %>%
      mutate(material=factor(material, levels = material_sort_order)) %>%
      arrange(desc(material)) %>%
      mutate(material=as.character(material))
  })
  
  # i need to get the ordered list of materials to use as row names
  # later, so..
  hm_15ab <- reactive({
    hm_15aa()$material
  })
  
  hm_15ac <- reactive({
    hm_15aa() %>%
      select(-material) %>%
      as.matrix()
  })
  
  output$hm_matrix <- 
    renderTable(hm_15aa())
  # second try: make a plotly heatmap with selected data
  # and external names
  output$hm_chart_pctImpact <-
    renderPlotly(
      heatmaply(
        x=hm_15ac(), labRow = hm_15ab(),
        main="heatmap of impact intensities",
        colors = viridis( n=256, begin=0, end=1, option="plasma"),
        Rowv = FALSE
      )
    )
  
  # GENERATING OUTPUT FOR THE RECYCLING VS REDUCTION TAB
  
  rvr_weight_data <- reactive({
    arr_summary_data_2 %>% 
      filter(
        wasteshed==input$rvr_wasteshed_choice &
        optVariant %in% 
          c("actual", "dispose_all", input$rvr_impactCategory_choice)
      )
  })
  
  rvr_weight_total <- reactive({
    sum(
      filter(rvr_weight_data(), optVariant=="actual")$tons
    )
  })
  
  rvr_weight_data_alt <- reactive({
    rvr_weight_data() %>%
    mutate(
      scenario=ifelse(scenario=="optimal", "your own", optVariant),
      scenario=factor(
        scenario, 
        levels=rev(c("dispose_all", "actual", "your own"))
      ),
      tons=case_when(
        scenario=="your own" & umbDisp=="recovery" ~
          rvr_weight_total()*(input$rvr_wbrr/100)*
          (input$rvr_wbrdr/100),
        scenario=="your own" & umbDisp=="disposal" ~
          rvr_weight_total()*(1-(input$rvr_wbrr/100))*
          (input$rvr_wbrdr/100),
        TRUE ~ tons
      )
    )
  })
  
  rvr_impact_data <- reactive({
    arr_summary_data_3 %>%
      filter(
        wasteshed==input$rvr_wasteshed_choice &
          optVariant %in% 
          c("actual", "dispose_all", input$rvr_impactCategory_choice) &
        impactCategory==input$rvr_impactCategory_choice
      )
  })
  
  rvr_impact_model <- reactive({
    arr_summary_data_1 %>%
    filter(
      wasteshed==input$rvr_wasteshed_choice & 
      impactCategory==input$rvr_impactCategory_choice
    )
  })
  
  output$rvr_impact_model_table <-
    renderTable(rvr_impact_model())
  
  rvr_impact_data_alt <- reactive({
    rvr_impact_data() %>%
    mutate(
      scenario=ifelse(scenario=="optimal", "your own", optVariant),
      scenario=factor(
        scenario, 
        levels=rev(c("dispose_all", "actual", "your own"))
      ),
      impact=ifelse(
        scenario=="your own",
        (input$rvr_wbrdr/100)*
        (rvr_impact_model()$rr_intercept +
          rvr_impact_model()$rr_slope*(input$rvr_wbrr/100)),
        impact
      )
    )
  })
  
  output$rvr_statement <- 
    renderText({
      paste("For the ", 
            input$rvr_wasteshed_choice, 
            " wasteshed, the weight-based recovery rate is ",
            round(filter(arr_summary_data_1, 
                    wasteshed==input$rvr_wasteshed_choice,
                    impactCategory==input$rvr_impactCategory_choice)
             $wb_rr, 2),
            ".  That recovery reduces life cycle ",
            (filter(arr_summary_data_1, 
                    wasteshed==input$rvr_wasteshed_choice,
                    impactCategory==input$rvr_impactCategory_choice)
            )$impactCategory,
            " impacts by ",
            round((filter(arr_summary_data_1, 
                    wasteshed==input$rvr_wasteshed_choice,
                    impactCategory==input$rvr_impactCategory_choice)
            )$curr_impact_reduction, 2),
            ".",
            sep=""
            )
    })
  
  
  output$rvr_weight_chart <-
    renderPlot({
      ggplot()+
      theme_539()+
      geom_bar(
        data=rvr_weight_data_alt(),
        aes(
          x=scenario, y=tons, 
          color=scenario, fill=scenario, alpha=umbDisp
        ),
        stat="identity",
        position="stack"
      )+
      scale_alpha_manual(values=c(0.25,0.75))+
      coord_flip()+
      theme(legend.position="none")
    })
  
  output$rvr_weight_table <-
    renderTable(rvr_weight_data())
  output$rvr_weight_table_alt <-
    renderTable(rvr_weight_data_alt())
  
  output$rvr_impact_chart <- renderPlot({
    ggplot()+
    theme_539()+
    geom_bar(
      data=rvr_impact_data_alt(),
      aes(
        x=scenario, y=impact, color=scenario, fill=scenario),
      stat="identity",
      position="stack"
    )+
    coord_flip()+
    theme(
      legend.position = "none"
    )
  })
  
  output$rvr_impact_table <-
    renderTable(rvr_impact_data())
  output$rvr_impact_table_alt <-
    renderTable(rvr_impact_data_alt())

  # REACTIVE OBJECTS AND OUTPUTS FOR THE ENTER-YOUR-OWN PAGE  
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