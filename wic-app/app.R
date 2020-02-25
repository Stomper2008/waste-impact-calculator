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
# library(plotly)
# library(heatmaply)
library(scales)
library(openxlsx)
library(knitr)
library(ggforce)

# importing some graphic conventions (to be expanded later)
source(file="../oregon_deq/resources/theme_539.R")

# a two-step palette based on viridis
virPal2 <- viridis_pal()(2)
names(virPal2) <- c("darkdark", "lightlight")

# a four-step palette based on viridis
myPal <- 
  deqPal15[c("blue_bold", "green_bold", "lime_bold", "yellow_bold")]
myPal2 <- as.character(myPal)

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
# this is a new version... it probably could be done back in the pre-
# processing and included in wicf_impacts_net.  
# but at this point i'm just trying it.
hm_data_1 <-
  wicf_impacts_net %>%
  filter(scenario=="actual") %>%
  group_by(wasteshed, impactCategory, impactUnits) %>%
  summarise(impact=sum(impact)) %>%
  rename(actualImpact = impact) %>%
  ungroup()

hm_data_2 <-
  left_join(
    wicf_impacts_net,
    hm_data_1,
    by=c("wasteshed", "impactCategory", "impactUnits")
  ) %>%
  mutate(pctActual = impact/actualImpact)

normalized_impacts_1 <-
  hm_data_2 %>%
  group_by(wasteshed, impactCategory, impactUnits, scenario) %>%
  summarise(
    impact=sum(impact, na.rm=T)
  ) %>%
  ungroup()

normalized_impacts_2 <-
  normalized_impacts_1 %>% 
  filter(scenario=="actual") %>%
  rename(actualImpact = impact) %>%
  select(-scenario)

normalized_impacts_3 <-
  left_join(
    normalized_impacts_1,
    normalized_impacts_2,
    by = c("wasteshed", "impactCategory", "impactUnits")
  ) %>%
  mutate(pctImpact = impact/actualImpact)

# older more complex version of heatmap data..
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

# DATA SPECIFIC TO THE "RECYCLING AND ITS LIMITS" PAGE
arr_summary_data_1 <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/arr_summary_data_1.RData"
  ) %>%
  mutate(
    curr_impact_reduction_abs = dispose_all-actual,
    curr_impact_reduction_pct = curr_impact_reduction_abs/dispose_all,
    poss_impact_reduction_abs = dispose_all-optimal
  )
arr_summary_data_2 <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/arr_summary_data_2.RData"
  )
arr_summary_data_3 <-
  readRDS(
    "../oregon_deq/projects/arr_scenarios_deq_factors/intermediate_output/arr_summary_data_3.RData"
  )

# DATA FOR THE STRATEGY PICKER
# the strategy picker relies on a "widened" version of wicf_impacts_net
# that also includes a summary of impacts in the dispose_all scenario
sp_data_summaries <-
  wicf_impacts_net %>%
  filter(scenario == "dispose_all") %>%
  group_by(wasteshed, impactCategory, impactUnits) %>%
  summarise(
    totalCategoryImpact = sum(impact),
    maxMaterialImpact = max(impact)
  ) %>%
  ungroup()

sp_data_1 <- 
  wicf_impacts_net %>%
  select(-optVariant) %>%
  pivot_wider(
    values_from = "impact",
    names_from = "scenario"
  ) %>%
  mutate(
    potentialSavingsAbsolute = dispose_all - optimal,
    realizedSavingsAbsolute = dispose_all - actual,
    unrealizedSavingsAbsolute = actual - optimal
  )

sp_data_2 <-
  left_join(
    sp_data_1,
    sp_data_summaries,
    by = c("wasteshed", "impactCategory", "impactUnits")
  ) %>%
  mutate(
    potentialSavingsPct = potentialSavingsAbsolute/totalCategoryImpact,
    totalMaterialImpactPct = dispose_all/totalCategoryImpact
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
      sidebarLayout(
        sidebarPanel(
          width=3,
          h3("INTRODUCTION"),
          "This section contains some resources to help you understand
          what this model is about."
        ),
        mainPanel(
          width=9,
          tabsetPanel(
            tabPanel(
              title = "Video",
              "insert youtube video here"
            ),
            tabPanel(
              title = "Understanding the impacts of materials",
              "insert essay about materials lifecycle, cbei results,
              etc. here"
            ),
            tabPanel(
              title = "Glossary and conventions",
              "insert some verbiage here -- could i put it in an rmd?"
            )
          )
        )
      ) # close sidebarlayout for introduction page
    ), #close introduction page
    
    # lay out weights and impacts section
    # (multiple pages selected via drop-down menu)
    navbarMenu(
      title="Weights and impacts",
      
      # lay out weights, recovery rates, & impacts page
      tabPanel(
        title="weights vs. impacts",
        sidebarLayout(
        sidebarPanel(
          h3("WEIGHTS VS. IMPACTS"),
          width=3,
          "This page shows how the weights of solid waste 
          materials (left side of the page) relate 
          to the total life cycle environmental impacts for 
          those same materials (right side of the page).",
          h5(""),
          "You'll probably notice that weight
          doesn't always do a good job of indicating 
          impacts.  For example, electronics typically have
          extremely large life cycle impacts compared to
          weight, whereas yard debris has low impacts 
          compared to weight."
        ), # close sidebar panel for weights recovery rates & impacts page
        mainPanel(
          width=9,
          fluidRow(
            column(
              width=6,
              plotOutput("wrr_chart")
            ),
            column(
              width=6,
              plotOutput("wvi_chart")
            )
          ),
          fluidRow(
            column(
              width=4,
              selectInput(
                inputId="wvi_wasteshed_choice",
                label="choose a wasteshed",
                choices=unique(wicf_weight_summaries$wasteshed),
                selected = "Metro"
              )
            ),
            column(
              width=4,
              selectInput(
                inputId="wvi_impact_cat_choice",
                label="choose an impact category",
                choices = unique(wicf_impacts_net$impactCategory),
                selected="Global warming"
              )
            ),
            column(
              width=4,
              checkboxGroupInput(
                inputId="wvi_scenario_choice",
                label = "choose management scenario(s)",
                choices = unique(wicf_weight_summaries$scenario),
                selected = "actual"
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
              h3("WHERE IMPACTS COME FROM"),
              "The weight chart on the left shows the tonnage 
              of materials in the solid waste stream, as well 
              as how much of that waste was recycled or otherwise
              recovered.",
              h5(""),
              "The impact chart on the right 
              shows how life cycle impacts 
              of materials are calculated.  The net, or total, life 
              cycle impact is shown with heavy black lines.  This
              is the sum of impacts
              for the life cycle stages of production, end-of-life 
              transport, and end-of-life treatment (such as 
              landfilling or recycling).  If there 
              has been recycling or other recovery activity, 
              then the end-of-life impact may be negative, which
              lowers the net impact.",
              h5(""),
              "You can try changing the 
              management scenario to see if increasing 
              disposal or recovery will substantially change the 
              net impacts."
            ),
            mainPanel(
              width=9,
              fluidRow(
                column(
                  width=6,
                  plotOutput("wicf_weight_chart")
                ),
                column(
                  width=6,
                  plotOutput("wicf_impact_chart")
                )
              ),
              fluidRow(
                column(
                  width=4,
                  selectInput(
                    inputId="wicf_wasteshed_choice",
                    label="choose a wasteshed",
                    selected="Metro",
                    choices=unique(wicf_weights$wasteshed)
                  )
                ),
                column(
                  width=4,
                  selectInput(
                    inputId="wicf_impactCategory_choice",
                    label="choose an impact category",
                    selected="Smog",
                    choices=unique(wicf_impacts_net$impactCategory)
                  )
                ),
                column(
                  width=4,
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
        ),  # close tabPanel for weights vs. impacts display
      
      tabPanel(
        title="Impact intensities",  
        sidebarLayout(
          sidebarPanel(
            h3("IMPACT INTENSITIES"),
            width=3,
            "This chart makes it easier to evaluate impacts for 
            many different impact categories (for example, 
            global warming and water use) in a single glance. 
            The more impact a material has within an impact 
            category, the brighter the color.  For example, 
            food waste often represents a large portion of 
            water use impacts.  Each column totals to 100% of 
            the wasteshed's total impacts for that impact 
            category, under the actual scenario.",
            h5(""),
            "This chart is likely to show you that 
            materials are not at all equal!  Certain kinds 
            of impacts are likely to be concentrated among 
            one or two materials.  In addition, sometimes 
            impacts go together."
          ), # end sidebarPanel
          mainPanel(
            width=9,
            fluidRow(
              plotOutput(outputId = "hm_new_chart")
            ),
            fluidRow(
              wellPanel(
                selectInput(
                  inputId="hm_wasteshed_choice",
                  label="choose a wasteshed",
                  choices = unique(hm_13$wasteshed),
                  selected = "Metro"
                ),
                radioButtons(
                  inputId="hm_scenario_choice",
                  label="choose a management scenario",
                  choices=c("actual", "dispose_all", "optimal"),
                  selected="actual",
                  inline = TRUE
                )
              )
            )
            # ,
            # fluidRow(
            #   dataTableOutput(outputId="myTestTable")
            # ),
            # fluidRow(
            #   dataTableOutput(outputId="myTestTable2")
            # ),
            # fluidRow(
            #   plotlyOutput(outputId = "hm_chart_pctImpact")
            # )
            #,
            #tableOutput("hm_matrix")
          ) # end mainPanel
        ) #end sidebarlayout
      ) #end impact intensities page
      ), #close navbarmenu for "weights & impacts" section
    
      # ui for the Ways to reduce impacts section
      navbarMenu(
        title="Solutions",
        
        tabPanel(
          title="what if if you recycle everything? (single impact view)",
          sidebarLayout(
            sidebarPanel(
              h3("ALTERNATIVE RECOVERY RATES for OREGON WASTESHEDS"),
              width=3,
              "This page shows the results for an environmental impact 
              calculation required by Oregon law: the 'Alternative Recovery 
              Rate' (link)  ",
              h5(""),
              "As the name suggests, these recovery rates offer an 
              alternative to traditional weight-based goals 
              for recovery (recycling and composting) of solid waste,
              which are also present in Oregon state law.(link)",
              h5(""),
              "Whereas weight-based goals measures recovery as the 
              number of tons recovered divided by the total number of 
              tons in the waste stream, the alternative recovery rate 
              measures recovery by the size of impact reductions linked 
              to that recovery, divided by the total impact reductions 
              possible if recovery were optimized to reduce impacts.",
              h5(""),
              "You may notice several things.  First, the alternative 
              recovery rate may be 
              more or less than the regular weight-based recovery rate.
              But perhaps more important, high recovery rates do not 
              necessarily eliminate all the impacts associated with 
              materials.",
              h5(""),
              "To reduce impacts below the level associated
              with optimal recovery, actual reductions in the use of 
              materials are necessary.",
              h5(""),
              downloadButton(
                outputId="arr_report",
                label = "download a formatted ARR report"
                )
            ), # close sidebarpanel for "recycling and its limits"
            
            mainPanel(
              
              # summary row with Alternative Recovery Rate results
              fluidRow(
                width=12,
                h3(textOutput(outputId = "arr_screen_title")),
                h4(""),
                textOutput("arr_statement")
              ),
              fluidRow(
                width=12,
                column(
                  width=6,
                  h3("weights for three management scenarios"),
                  plotOutput("arr_weight_chart")
                ),
                column(
                  width=6,
                  h3("impacts for those same scenarios"),
                  plotOutput("arr_impact_chart")
                )
              ), # close fluidRow
              fluidRow(
                column(
                  width=6,
                  selectInput(
                    inputId="arr_wasteshed_choice",
                    label="choose a wasteshed",
                    choices = unique(arr_summary_data_1$wasteshed),
                    selected = "Metro",
                    width = "100%"
                  )
                ),
                column(
                  width=6,
                  selectInput(
                    inputId="arr_impactCategory_choice",
                    label="choose an impact category",
                    choices= unique(arr_summary_data_1$impactCategory),
                    selected="Smog",
                    width = "100%"
                  )
                )
              ) # close fluidRow
            ) # close mainpanel for "recycling and its limits (ARR)"
          ) # close sidebarlayout for "recycling and its limits (ARR)"
        ), # close tabPanel for "reycling and its limits (ARR)" page

        tabPanel(
          title = "what if you recycled everything? (normalized view)",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              h3("NORMALIZED IMPACTS"),
              "This chart explores how recycling- oriented 
              waste management scenarios affect impacts in
              multiple impact categories."
            ),
            mainPanel(
              width=9,
              fluidRow(
                width=12,
                column(
                  width=6,
                  plotOutput(outputId="normalized_chart")
                )
              ),
              fluidRow(
                width=12,
                column(
                  width=6,
                  selectInput(
                    inputId="normalized_wasteshed_choice",
                    label = "choose a wasteshed",
                    choices = unique(normalized_impacts_3$wasteshed),
                    selected = "Metro"
                  )
                ),
                column(
                  width=6,
                  checkboxGroupInput(
                    inputId="normalized_scenario_choice",
                    label = "choose scenario(s)",
                    choices = c("actual", "dispose_all", "optimal"),
                    selected = "optimal",
                    inline = TRUE
                  )
                )
              ),
              fluidRow(
                dataTableOutput(outputId="normalized_results")
              )
            )  
            ) # close sidebar layout for normalized view
        ), # close tabPanel for normalized view
        
        
                
        tabPanel(
        title="recycling vs. reduction model",
        sidebarLayout(
        sidebarPanel(
          width=3,
          h3("RECYCLING VS. REDUCTION"),
          "blah blah",
          "blah blah"
        ), # close sidebarPanel
        mainPanel(
      ) # close mainpanel
      )  # close sidebarLayout
      ),  # close the page layout

      tabPanel(
        title="strategy picker",
        sidebarLayout(
          sidebarPanel(
            width=3,
            h3("STRATEGY PICKER"),
            "This page does blah."
          ),
          mainPanel(
            width=9,
            plotOutput(outputId = "sp_chart"),
            wellPanel(
              h3("Options"),
              selectInput(
                inputId = "sp_wasteshed_choice",
                label = "choose a wasteshed",
                choices = unique(sp_data_2$wasteshed),
                selected = "Lane"
              ),
              checkboxGroupInput(
                inputId = "sp_category_choice",
                label = "choose impact categories",
                choices = unique(sp_data_2$impactCategory),
                selected = "Air PM2.5",
                inline = TRUE
              ),
              checkboxGroupInput(
                inputId = "sp_material_choice",
                label = "choose materials",
                choices = unique(sp_data_2$material),
                selected = "ScrapMetal",
                inline = TRUE
              )
            ),
            dataTableOutput(outputId = "sp_table")
          )
        ) # close sidebar layout for strategy chart page
      ) # close tabpanel for strategy chart page
      
      ), # close navbar menu for the ways to reduce impacts section
      
      # ui for the "enter your own" page
      # (this section is unusual in that it has its own tabset)
      tabPanel(
        title="Enter your own waste",
        sidebarLayout(
          sidebarPanel(
            width=3,
            h3("ENTER YOUR OWN WASTE"),
            "This set of tabs gives you the opportunity to generate 
            the same kinds of life-cycle impact results as elsewhere in 
            the Waste Impact Calculator, but using your own solid waste 
            data.",
            h5(""),
            "You need to know the weights (in short tons), end-of-life
            dispositions, and end-of-life transport distances for at 
            least one material.  Enter those for a 'baseline' scenario 
            (probably whatever the current situation is).  Then imagine 
            a different way of managing that material and enter it as the 
            'alternative' scenario.",
            h5(""),
            "The app will then calculate life cycle impacts you can view 
            in the other tabs."
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title="Enter your data",
                h3("Enter your solid waste data"),
                DTOutput("x1"),
                h3("Entry confirmation"),
                tableOutput("fe_data_entry_confirmation")
              ), # close enter your own data tab

            tabPanel(
              title="Total weights & impacts",
              column(
                width=6,
                align = "center",
                plotOutput("fe_totalWtChart"), #weight chart
                downloadButton(
                  outputId = "fe_totalWtChartDL",
                  label = "download this chart",
                  width = "100%"
                )
              ),
              column(
                width=6,
                align="center",
                plotOutput("fe_totalImpactChart"), #impact chart
                downloadButton(
                  outputId = "fe_totalImpactChartDL",
                  label = "download this chart",
                  width = "100%"
                )
              ),
              selectInput(
                inputId="fe_ImpactCategoryChoice",
                label=h3("Choose your impact category"),
                choices = unique(impact_factors$impactCategory),
                selected="Energy demand",
                width="100%"
              )
            ),
            
            tabPanel(
              title="Detailed weights & impacts",
              fluidRow(
                column(
                  width=6,
                  plotOutput("fed_detailedTonsChart")
                ),
                column(
                  width=6,
                  plotOutput("fed_detailedImpactsChart")
                )
              ),
              fluidRow(
                selectInput(
                  inputId = "fe_detailedImpactsCategoryChoice",
                  label = "choose an impact category",
                  choices = unique(impact_factors$impactCategory)
                )
              )
            ),
            
            tabPanel(
              title = "Hotspots & strategies",
              fluidRow(
                column(
                  width = 6,
                  align = "center",
                  plotOutput(outputId = "fe_normalizedComparisonChart"),
                  downloadButton(
                    outputId = "fe_normalizedComparisonChartDL",
                    label = "download this chart"
                  )
                ),
                column(
                  width=6,
                  align = "center",
                  plotOutput(outputId = "fe_heatmapBaseline")
                )
              )
            ),

            tabPanel(
              title="Download",
              fluidRow(
                width = 12,
                column(
                  width = 6,
                  wellPanel(
                    h3("Download all your entered data and results 
                       (Excel workbook format)"),
                    downloadButton(
                      outputId = "fe_fullDataDownload",
                      label = "download"
                    )
                  )
                ),
                column(
                  width=6,
                  wellPanel(
                    h3("Download a formatted report with results
                       and charts"),
                    downloadButton(
                      outputId = "fe_md_report",
                      label = 'Download html report'
                    )
                  )
                )
              ),
              fluidRow(
                  h3("Preview of detailed impact table"),
                  DTOutput("fe_detailedImpactTable")
              )
            )
          
          ) # close tabset panel for enter your own page  
          ) # close mainpanel for enter your own page
        ) # close sidebarlayout for enter your own page
      ), # close tabPanel "Enter your own"
    
    # defining ui for the documentation page
    tabPanel(
      title="Documentation",
      sidebarLayout(
        sidebarPanel(
          width=3,
          h3("DOCUMENTATION"),
          "This section provides background about the provenance of 
          this model, and the impact factors it relies on."
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              title = "Credits",
              "insert some text here -- from a modular file?"
            ),
            tabPanel(
              title = "Formal documentation",
              "insert pdfs here for the Waste Impact Calculator and 
              impact factor documents"
            )
          ) # close tabsetPanel for the documentation page
        ) # close mainpanel for documentation page
      ) # close sidebar layout for documentation page
    ) # close tabPanel for documentation page
) #close navbarpage

# end ui definition

# server
server <- function(input, output) {
  
  wrr_chart_data_1 <- reactive({
    wicf_weights %>% 
      filter(
        wasteshed==input$wvi_wasteshed_choice,
        scenario %in% input$wvi_scenario_choice,
        optVariant %in% 
          c("actual", "dispose_all", input$wvi_impact_cat_choice)
      )
  })
  
  wrr_chart_data_2 <- reactive({
    wicf_weight_summaries %>% 
      filter(
        wasteshed==input$wvi_wasteshed_choice,
        scenario %in% input$wvi_scenario_choice,
        optVariant %in% 
          c("actual", "dispose_all", input$wvi_impact_cat_choice)
      )
  })
  
  # generating output for the weight and recovery rate tab
  output$wrr_chart <- renderPlot({
  ggplot()+
    ggtitle("Weights (short tons)")+
    theme_539()+
    geom_bar(
      data = wrr_chart_data_2(),
      aes(
        x = material,
        y = tons,
        fill = scenario,
        color = scenario
      ),
      stat = "identity",
      position = "dodge",
      alpha=0.6
    )+
    # geom_text(
    #   data=wrr_chart_data_2(),
    #   aes(
    #     x=material, 
    #     y=tons,
    #     label=percent(round(wbrr, 2)),
    #     color=scenario
    #   ),
    #   hjust=-0.15
    # )+
    scale_y_continuous(name="short tons", labels=comma)+
    coord_flip()+
    scale_color_manual(values=myPal2[c(1,3,2)])+
    scale_fill_manual(values=myPal2[c(1,3,2)])+
#    scale_colour_viridis(begin=0.32, end =1, discrete=TRUE)+
#    scale_fill_viridis(begin=0.32, end=1, discrete=TRUE)+
#    scale_alpha_manual(values=c(0.25,0.75))+
    theme(
      legend.position="bottom",
      axis.title=element_text(),
      axis.title.y=element_blank(),
      axis.title.x=element_text()
    )
  })
  
  # generating output for the weight vs. impacts tab
  
  wvi_chart_data <- reactive({
    wicf_impacts_net %>%
      filter(
        impactCategory==input$wvi_impact_cat_choice,
        wasteshed == input$wvi_wasteshed_choice,
        scenario %in% input$wvi_scenario_choice,
        optVariant %in%
          c("actual", "dispose_all", input$wvi_impact_cat_choice)
      )
  })
  
  # wvi_chart_title <- reactive({
  #   paste(
  #     
  #   )
  # })
  
  output$wvi_chart <-
    renderPlot({
      ggplot()+
        ggtitle("Life cycle impacts")+
        theme_539()+
        geom_bar(
          data=wvi_chart_data(),
          aes(
            x=factor(material, levels=material_sort_order), 
            y=impact,
            fill = scenario
            ),
          alpha=0.2,
          stat="identity",
          position="dodge",
          color="black",
          size=1
        )+
        scale_fill_manual(values=myPal2[c(1,3,2)])+
        coord_flip()+
        theme(
          legend.position = "bottom",
          axis.title=element_text(),
          axis.title.y=element_blank(),
          axis.title.x=element_text()
          )
    })
  
  # generating output for the where impacts come from page
  
  wicf_weight_chart_object <- reactive({
    ggplot()+
      theme_539()+
      ggtitle("Weights and recovery rates")+
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
      geom_text(
        data=wicf_weight_summaries %>%
          filter(
            wasteshed == input$wicf_wasteshed_choice,
            scenario == input$wicf_scenario_choice,
            optVariant %in% 
              c("actual", "dispose_all", 
                input$wicf_impactCategory_choice
                )
            ),
        aes(
          x=material, 
          y=tons,
          label=percent(round(wbrr, 2))
        ),
        hjust=-0.15
      )+
      scale_fill_manual(values=myPal2)+
      scale_color_manual(values=myPal2)+
      coord_flip()+
      theme(
        panel.grid = element_blank(),
        axis.ticks = element_line()
      )
  }) # close plot definition for wicf_weight_chart_object

  output$wicf_weight_chart <- renderPlot({
    wicf_weight_chart_object()
  }) 

  output$wicf_impact_chart <- renderPlot({  
    ggplot()+
    theme_539()+
    ggtitle("Impacts")+
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
      size=1.5,
      stat="identity"
    )+
    scale_color_manual(values=myPal2)+
    scale_fill_manual(values=myPal2)+
    coord_flip()+
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_line()
    )
  }) # close definition of wicf_impact_chart
  
  # REACTIVE OBJECTS FOR THE HEATMAP PAGE
  # second try: make a plotly heatmap from input-filtered data
  
  hm_new_chart_object <- reactive({
    ggplot()+
      theme_539()+
      ggtitle("new heatmap")+
      geom_tile(
        data = 
          hm_data_2 %>%
          filter(
            wasteshed == input$hm_wasteshed_choice,
            scenario == input$hm_scenario_choice
          ) %>%
          mutate(material=factor(material, levels = material_sort_order)),
        aes(
          y=material, 
          x=impactCategory, 
          fill=pctActual*100
        )
      )+
      geom_text(
        data=
          hm_data_2 %>%
          filter(
            wasteshed == input$hm_wasteshed_choice,
            scenario == input$hm_scenario_choice
          ) %>%
          mutate(material=factor(material, levels = material_sort_order)),
        aes(
          y=material,
          x=impactCategory,
          label=round(pctActual*100,1)
        ),
        color="gray80"
      )+
      scale_fill_viridis()+
      theme(
        panel.grid = element_blank(),
        axis.ticks = element_line()
      )
  })
  
  output$hm_new_chart <- renderPlot({
    hm_new_chart_object()
  })
  
  output$myTestTable <- renderDataTable({hm_data_1})
  output$myTestTable2 <- renderDataTable({hm_data_2})
  
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
        colors = viridis( n=256, begin=0, end=1, option="viridis"),
        Rowv = FALSE
      )
    )

  
  # reactive objects for the recycling and its limits page
  
  normalized_chart_object <- reactive({
    ggplot()+
    theme_539()+
    ggtitle("Total impacts of recovery-oriented\nmanagement scenarios (as %\n of 'actual' scenario impact)")+
    geom_bar(
      data = 
        normalized_impacts_3 %>%
        filter(
          wasteshed == input$normalized_wasteshed_choice,
          scenario %in% input$normalized_scenario_choice
        ),
      aes(
        x = impactCategory,
        y = pctImpact,
        color = scenario,
        fill = scenario
      ),
      stat="identity",
      position = "dodge",
      alpha = 0.6
    )+
    geom_hline(
      yintercept = 1, 
      linetype="dotted", 
      size=2,
      color = "gray50"
    )+
    scale_y_continuous(labels=percent)+
    scale_color_manual(values=myPal2)+
    scale_fill_manual(values=myPal2)+
    coord_flip()+
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_line()
    )
  })
  
  output$normalized_chart <- 
    renderPlot({normalized_chart_object()})
  
  output$normalized_results <- 
    renderDataTable(normalized_impacts_3)
    
  # generating output objects for the 
  # recycling and its limits (ARR) page
  
  arr_weight_data <- reactive({
    arr_summary_data_2 %>% 
      filter(
        wasteshed==input$arr_wasteshed_choice &
        optVariant %in% 
          c("actual", "dispose_all", input$arr_impactCategory_choice)
      )
  })
  
  arr_weight_total <- reactive({
    sum(
      filter(arr_weight_data(), optVariant=="actual")$tons
    )
  })
  
  arr_impact_data <- reactive({
    arr_summary_data_3 %>%
      filter(
        wasteshed==input$arr_wasteshed_choice &
          optVariant %in% 
          c("actual", "dispose_all", input$arr_impactCategory_choice) &
          impactCategory==input$arr_impactCategory_choice
      )
  })
  
  output$arr_screen_title <- renderText({
    paste(
      "Alternative Recovery Rate (ARR) results for ",
      str_to_upper(input$arr_wasteshed_choice),
      " wasteshed, using ",
      str_to_lower(input$arr_impactCategory_choice), 
      " impacts.",
      sep=""
    )
    })
  
  arr_summary_data_1_current_row <- 
    reactive({
      arr_summary_data_1 %>%
      filter(
        wasteshed==input$arr_wasteshed_choice,
        impactCategory==input$arr_impactCategory_choice
      )
    })
  
  output$arr_statement <- 
    renderText({
      paste("The weight-based recovery rate is ",
            100*round(arr_summary_data_1_current_row()$wb_rr, 2),
            "%.  That recovery activity reduces life cycle ",
            str_to_lower(arr_summary_data_1_current_row()$impactCategory),
            " impacts by ",
            round(arr_summary_data_1_current_row()$curr_impact_reduction_abs, 2),
            " ",
            arr_summary_data_1_current_row()$impactUnits,
            ", or ",
            100*round(arr_summary_data_1_current_row()$curr_impact_reduction_pct, 2),
            "%, compared to a scenario where all waste is disposed.
            If all waste was recovered optimally, it would reduce 
            life cycle ",
            str_to_lower(arr_summary_data_1_current_row()$impactCategory),
            " impacts by ",
            round(arr_summary_data_1_current_row()$poss_impact_reduction_abs, 2),
            " ",
            arr_summary_data_1_current_row()$impactUnits,
            ".  ",
            "Therefore the recovery activity has reduced impacts by ",
            round(arr_summary_data_1_current_row()$curr_impact_reduction_abs, 2),
            " out of a possible ",
            round(arr_summary_data_1_current_row()$poss_impact_reduction_abs, 2),
            ", and the alternative recovery rate is ",
            100*round(arr_summary_data_1_current_row()$arr,2),
            "%.",
            "  Further reductions in impacts are not available through
            recovery, but are available through reduction in waste 
            generation.",
            sep=""
            )
    })
  
  output$arr_weight_chart <-
    renderPlot({
      ggplot()+
      theme_539()+
      geom_bar(
        data=arr_weight_data(),
        aes(
          x=scenario, y=tons, 
          color=scenario, fill=scenario, alpha=umbDisp
        ),
        stat="identity",
        position="stack"
      )+
      # geom_text(
      #   data=arr_weight_data(),
      #   aes(x=scenario, y=tons/2, label=umbDisp),
      #   color="white",
      #   size=7,
      #   hjust=0,
      #   fontfamily="bold"
      # )+
      scale_alpha_manual(values=c(1,0.5))+
      scale_color_manual(values=myPal2)+
      scale_fill_manual(values=myPal2)+
      coord_flip()+
      theme(legend.position="none")
    })
  
  output$arr_impact_chart <- renderPlot({
    ggplot()+
    theme_539()+
    geom_hline(
      data=arr_impact_data(),
      aes(yintercept=impact),
      linetype="dotted"
    )+
    geom_bar(
      data=arr_impact_data(),
      aes(
        x=scenario, y=impact, color=scenario, fill=scenario),
      stat="identity",
      position="stack"
    )+
    geom_text(
      data=arr_impact_data(),
      aes(x=scenario, y=impact/2, label=scenario),
      color="white",
      size=7,
      hjust=0.5,
      fontfamily="bold"
      )+
    coord_flip()+
    scale_fill_manual(values=myPal2)+
    scale_color_manual(values=myPal2)+
    theme(
      axis.text.y=element_blank(),
      legend.position = "none"
    )
  })
  
  # REACTIVE OBJECTS AND OUTPUTS FOR THE STRATEGY PICKER PAGE
  sp_data_to_use <- reactive({
    sp_data_2 %>%
    filter(
      wasteshed %in% input$sp_wasteshed_choice,
      impactCategory %in% input$sp_category_choice,
      material %in% input$sp_material_choice
    )
  })
  
  sp_chart_object <- reactive({
    ggplot()+
      ggtitle("Strategy chart")+
      theme_539()+
      geom_abline(aes(intercept=0, slope=1))+
      geom_abline(aes(intercept=0, slope=0.5))+
      geom_abline(aes(intercept=0, slope=0))+
      geom_circle(
        aes(x0=0, y0=0, r=0.1),
        fill="white"
      )+
      geom_text(
        data =
          data.frame(
            xxx = c(0.15, 0.3, 0.3, 0.2, 0.05),
            yyy = c(0.25, 0.21, 0.08, -0.03, 0),
            thing = 
              c("RECOVER", "RECOVER\nOR REDUCE", "REDUCE", "DISPOSE",
                "LOW\nPRI-\nORITY")
          ),
        aes(x=xxx, y=yyy, label=thing),
        hjust=0.5,
        vjust=0.5,
        size=5,
        font.family="italic",
        color = "gray50"
      )+
      geom_point(
        data = sp_data_to_use(),
        aes(
          x = totalMaterialImpactPct,
          y = potentialSavingsPct,
          color = impactCategory,
          fill = impactCategory,
          shape = material
        ),
        size = 8
      )+
      geom_text(
        data = sp_data_to_use(),
        aes(
          x = totalMaterialImpactPct,
          y = potentialSavingsPct,
          label = material,
          color = impactCategory
        ),
        size = 6,
        hjust = 1.15,
        vjust = 0.3
      )+
      coord_fixed(ratio=1)+
      expand_limits(x=0.4, y=0.3)+
      scale_color_viridis(begin = 0.32, end = 1, discrete = TRUE)+
      scale_fill_viridis(begin = 0.32, end = 1, discrete = TRUE)+
      scale_x_continuous(
        limits = c(0,NA),
        name = "Total life cycle impact assuming no recovery"
      )+
      scale_y_continuous(
#        limits = c(-0.05,NA),
#        limits = c(NA,0.5),
 #       limits = c(0,NA),
        name = "Maximum possible benefit from recovery")+
      theme(
        axis.ticks = element_line(),
        axis.title = element_text(),
        legend.position = "none",
        panel.grid = element_blank()
      )
  })
  
  output$sp_chart <- renderPlot({
    sp_chart_object()
  })
  
  output$sp_table <- renderDataTable(sp_data_to_use())

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
  
  output$fe_data_entry_confirmation <- renderTable(fe_mat_disp_combos_2())
  
  #so, mat_disp_combos_2 has the end-of-life information I need.
  #but production information isn't included.
  #so all I really need to do is double it and change the 
  #disposition to production.
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
  
  output$fe_detailedImpactTable <- 
    renderDT(
      fe_combos_with_impacts_detailed(),
      filter="top",
      rownames=F
      # ,
      # class="table table-sm"
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
      scenario=factor(scenario, levels=rev(c("baseline", "alternative")))
    )
  })
  
  # define the weight chart object
  fe_totalWtChartObject <- reactive({
    ggplot()+
      ggtitle("Weight (short tons)")+
      theme_539()+
      geom_bar(
        data=fe_combos_with_tons(),
        aes(x=scenario, y=tons, fill=disposition),
        stat="identity",
        position="stack",
        color="black"
      )+
      coord_flip()+
      # the following fill spec works but I can't control colors
      #      scale_fill_viridis(begin=0.32, end=1, discrete=TRUE)+
      # meanwhile the following manual scale works when you
      # give the color numbers as a vector WITHOUT NAMES
      scale_fill_manual(values=myPal2)+
      theme(
        axis.text.y = element_text(size=12),
        legend.position="bottom"
      )
  })
  
  output$fe_totalWtChart <- renderPlot({
    fe_totalWtChartObject()
    }
    ) 
  
# modify the following to make a weight chart download  
  output$fe_totalWtChartDL <-
    downloadHandler(
      filename = "fe_totalWtChartDL.png",
      content = function(file) {
        ggsave(
          file,
          plot = fe_totalWtChartObject(),
          device="png"
        )
      }
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
        factor(scenario, levels=rev(c("baseline", "alternative"))),
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

  # define the impact chart object
  fe_totalImpactChartObject <- reactive({
    ggplot()+
      ggtitle(fe_impact_chart_title())+
      theme_fivethirtyeight()+
      geom_bar(
        data=filter(
          fe_summed_impacts(), 
          impactCategory==input$fe_ImpactCategoryChoice
        ) %>% mutate(statistic="total impact"),
        aes(x=scenario, y=impact, color=statistic),
        alpha=0.2,
        size=2,
        stat="identity",
        position="stack"
      )+
      coord_flip()+
      theme(
        axis.text.y = element_text(size = 12),
        legend.position="bottom"
      )
  })  # close fe_totalImpactChartObject definition
  
    
  # draw the impact chart
  output$fe_totalImpactChart <- renderPlot({
    fe_totalImpactChartObject()
  }
  )  # close renderPlot
  
  output$fe_totalImpactChartDL <-
    downloadHandler(
      filename = "fe_totalImpactChartDL.png",
      content = function(file) {
        ggsave(
          file,
          plot = fe_totalImpactChartObject(),
          device="png"
        )
      }
    )
  
  # REACTIVE OBJECTS FOR THE "DETAILED WEIGHTS & IMPACTS" TAB
  # OF THE FREE ENTRY SECTION
  
  # summing tons, for tons chart
  fe_detailed_tons_data <- reactive({
    summarise(
      group_by(
        fe_mat_disp_combos_2(),
        scenario,
        material,
        disposition
      ),
      tons=sum(tons)
    ) %>% 
      ungroup() %>%
      arrange(scenario, material, disposition) %>%
      mutate(
        scenario =
          factor(scenario, levels=rev(c("baseline", "alternative")))
      )
  })
  
  # defining the chart
  fe_detailed_tons_chart_object <- reactive({
    ggplot()+
      ggtitle("Detailed weights (short tons)")+
      theme_539()+
      geom_bar(
        data = fe_detailed_tons_data(),
        aes(
          x = scenario,
          y = tons,
          fill = disposition
        ),
        stat = "identity",
        position = "stack"
      )+
      scale_fill_viridis(begin=0.32, end=1, discrete=TRUE)+
      facet_grid(material~.)+
      coord_flip()
  }) # close defining fe_detailed_tons_chart_object
  
  output$fed_detailedTonsChart <- renderPlot({
    fe_detailed_tons_chart_object()
  })
  
  
  # summing impacts, for the impacts chart total
  fe_detailed_tons_impact_total <- reactive({
    summarize(
      group_by(
        fe_combos_with_impacts_detailed(),
        scenario,
        material,
        impactCategory,
        impactUnits
      ),
      impact=sum(impact, na.rm=TRUE)
    ) %>%
    ungroup()
  })
  
  # defining title for detailed impacts chart
  fe_detailed_impacts_chart_title <- reactive({
    paste(
      # impact category name
      unique(
        filter(
          fe_summed_impacts(), 
          impactCategory==
            input$fe_detailedImpactsCategoryChoice)$impactCategory
      ),
      " (",
      # impact category units
      unique(
        filter(
          fe_summed_impacts(), 
          impactCategory==
            input$fe_detailedImpactsCategoryChoice)$impactUnits
      ),
      ")",
      sep=""
    )
  })
  
  # defining the detailed impact chart object
  fe_detailed_impacts_chart_object <- reactive({
    ggplot()+
      ggtitle(fe_detailed_impacts_chart_title())+
      theme_539()+
      geom_bar(
        data = fe_combos_with_impacts_detailed() %>%
          filter(
            impactCategory==input$fe_detailedImpactsCategoryChoice
          ),
        aes(
          x = scenario,
          y = impact,
          fill = LCstage
        ),
        stat="identity",
        position = "stack"
      )+
      geom_bar(
        data = 
          fe_detailed_tons_impact_total() %>%
          filter(
            impactCategory==input$fe_detailedImpactsCategoryChoice
          ),
        aes(x=scenario, y=impact),
        stat="identity",
        position="stack",
        color="black",
        fill = NA,
        size=2
      )+
      facet_grid(material~.)+
      coord_flip()+
      scale_fill_manual(values=myPal2)+
      theme()
  })
  
  # rendering the detailed impact plot
  output$fed_detailedImpactsChart <- renderPlot({
    fe_detailed_impacts_chart_object()
  })
  
  # REACTIVE OBJECTS FOR THE HOTSPOTS PAGE
  # and here are total impacts for the scenario
  fe_summed_material_impacts <- reactive({
    summarise(
      group_by(
        fe_combos_with_impacts_detailed(), 
        impactCategory, impactUnits, scenario, material
      ),
      impact=sum(impact)
    ) %>%
      ungroup() %>%
      arrange(impactCategory, material, scenario)
  }) # close reactive
  
  fe_hotspot_impacts <- reactive({
    left_join(
      fe_summed_material_impacts(),
      fe_summed_impacts() %>% 
        filter(scenario == "baseline") %>%
        rename(baselineMaterialImpact = impact) %>%
        select(-scenario),
      by = c("impactCategory", "impactUnits")
    ) %>%
    mutate(
      pctBaselineImpact = impact/baselineMaterialImpact,
      scenario = factor(scenario, levels = c("baseline", "alternative"))
    )
  })
  
  fe_normalizedComparisonChartObject <- reactive({
    ggplot()+
      theme_539()+
      ggtitle("'Alternative' scenario impacts\n(as % of baseline impacts)")+
      geom_bar(
        data = fe_hotspot_impacts() %>% filter(scenario=="alternative"),
        aes(
          x = impactCategory,
          y = pctBaselineImpact,
          fill = material
        ),
        stat = "identity",
        position = "stack",
        alpha=0.5
      )+
      geom_bar(
        data = 
          fe_hotspot_impacts() %>% 
          filter(scenario=="alternative") %>%
          group_by(
            impactCategory
          ) %>%
          summarise(pctBaselineImpact=sum(pctBaselineImpact)),
        aes(
          x=impactCategory,
          y=pctBaselineImpact
        ),
        stat = "identity",
        fill = NA,
        color = "black",
        size = 1.6 
      )+
      geom_hline(
        yintercept = 1,
        color = "gray50",
        size=2,
        linetype = "dotted"
        )+
      coord_flip()+
      scale_y_continuous(labels=percent)+
      scale_fill_manual(values=myPal2)+
      scale_color_manual(values=myPal2)+
#      scale_x_continuous(labels=comma)+
#      scale_fill_viridis(begin=0.32, end=1, discrete=TRUE)+
      theme(
        legend.position = "bottom",
        panel.grid = element_blank()
      )
  })
  
  output$fe_normalizedComparisonChart <- 
    renderPlot(fe_normalizedComparisonChartObject())
  
  output$fe_normalizedComparisonChartDL <-
    downloadHandler(
      filename = "fe_normalizedComparisonChartDL.png",
      content = function(file) {
        ggsave(
          file,
          plot = fe_normalizedComparisonChartObject(),
          device="png"
        )
      }
    )
  
  fe_hotspot_impacts_baseline_chart_object <- reactive({
    ggplot()+
      theme_539()+
      ggtitle("Heatmaps of material impacts\n(as % of baseline total)")+
      geom_tile(
        data=fe_hotspot_impacts(),
        aes(
          x=material, 
          y=impactCategory, 
          fill=pctBaselineImpact*100
        )
      )+
      facet_grid(.~scenario)+
      scale_fill_viridis(name="% of baseline")+
      theme(legend.position = "bottom")
  })
  
  output$fe_heatmapBaseline <-
    renderPlot(fe_hotspot_impacts_baseline_chart_object())
  
  output$testTable <- 
    renderTable(fe_summed_material_impacts())
  output$testTable2 <-
    renderTable(fe_hotspot_impacts())
    
  
  # REACTIVE OBJECTS FOR THE DOWNLOAD PAGE
  output$fe_md_report<-downloadHandler(
    filename = function() {
      paste(
        "fe_md_report_test",
        "_", 
        Sys.Date() ,
        ".html", 
        sep=""
      )
    },
    content=function(file){
      
      #create list of characteristics
      #set up parameters to pass to our Rmd document
      params <- 
        list(
          p_fe_data_entry_confirmation = 
            fe_mat_disp_combos_2(),
          p_fe_normalizedComparisonChartObject = 
            fe_normalizedComparisonChartObject()
          )
      
      rmarkdown::render(
        input = "fe_md_report.Rmd", 
        output_file=file,
        params=params,
        clean=TRUE,
        envir=new.env(parent= globalenv())
      )
    }
  )
  
  
  
  output$fe_fullDataDownload <-
    downloadHandler(
      filename = function() {
        paste("fe_fullDataDownload", ".xlsx", sep = "")
      },
      content = function(file) {
        myWorkbook <-
          createWorkbook()
        addWorksheet(wb = myWorkbook, sheetName = "total weights")
        addWorksheet(wb = myWorkbook, sheetName = "total impacts")
        writeDataTable(
          wb = myWorkbook,
          sheet = "total weights",
          x = fe_combos_with_tons()
        )
        writeDataTable(
          wb = myWorkbook,
          sheet = "total impacts",
          x = fe_summed_impacts()
        )
        addWorksheet(wb = myWorkbook, sheetName = "detailed weights")
        addWorksheet(wb = myWorkbook, sheetName = "detailed impacts")
        writeDataTable(
          wb = myWorkbook,
          sheet = "detailed weights",
          x = fe_detailed_tons_data()
        )
        writeDataTable(
          wb= myWorkbook,
          sheet = "detailed impacts",
          x = fe_combos_with_impacts_detailed()
          )
        addWorksheet(wb = myWorkbook, sheetName = "hotspot impacts")
        writeDataTable(
          wb= myWorkbook,
          sheet = "hotspot impacts",
          x = fe_hotspot_impacts()
        )
        saveWorkbook(
          wb = myWorkbook,
          file = file
        )
      }
    )
  
  } # close server

# Run the application 
shinyApp(ui = ui, server = server)