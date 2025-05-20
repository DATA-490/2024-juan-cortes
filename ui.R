

# CODE GUIDELINES ---------------------------------------------------------
# 1. Use the '#' for indentations (# vs. ##, etc.) to label your sections
# 2. When writing functions, use the styler package Addin to reformat.
# 3. Give plenty of space (1+ lines) between each function


# ---- UI ----
ui <- 
  # page_navbar() --------------------------------------------
page_navbar(
  title = "CFO Campus Office Sign-In Sheet",
  collapsible = TRUE,
  theme = "style.css",

  # ---- P1: Overview Tab ----
  ## nav_panel() --------------------------------------------
  nav_panel(
    title = "Overview",
    tags$head(
      tags$style(HTML("
      #getdata {
        background-color: #af2a30 !important;
        color: white !important;
        border: none !important;
      }
      #getdata:hover {
        background-color: #8f2226 !important;
        color: white !important;
      }
    "))
    ),
    ### sidebarLayout() --------------------------------------------
    sidebarLayout(
      #### sidebarPanel() --------------------------------------------
      sidebarPanel(
        width = 3,
        tags$div(HTML("<h3 style='color:#2B388F;text-align:center'><b>Welcome to the CHC CFO Sign-in Dashboard</b></h3>")),
        br(),
        tags$div(HTML("<h4 style='text-align:center'><b>Capstone Demonstration Version</b></h4>")),
        br(),
        tags$div(HTML("<p><b><i>This summary report captures information about walk-in student visits to the Chico State campus  
                    Student Services Center (SSC) CalFresh Outreach (CFO) office.</i></b></p>")),
        br(),
        selectInput(
          inputId = "pg1.yearRange",
          label = "Academic Year",
          choices = c(
            "2023-2024" = "2023-2024",
            "2024-2025" = "2024-2025"
          ),
          selected = "2024-2025",
          width = "100%"
        ),
        selectInput(
          inputId = "pg1.groupcountsby",
          label = "Choose Time Frame",
          choices = c(
            "Days" = "date",
            "Weeks" = "short.week",
            "Months" = "monthname",
            "Terms" = "term",
            "Academic Year" = "year"
            #"School Break" = "school_break"
          ),
          width = "100%"
        ),
        br(),
        tags$div(HTML("Partially funded by USDA SNAP, known in California as CalFresh, an equal opportunity provider and employer, and the California Department of Social Services.<br><br>")),
        img(src = "Calfresh_CHC_Logo.png", height = "100%", width = "100%")
      ), #### end sidebarPanel() --------------------------------------------
      
      #### mainPanel() -----------------------------------
      mainPanel(
        # Key Metrics Section with custom styling
        card(
          full_screen = TRUE,
          card_header(
            "CFO Office Usage Metrics",
            class = "text-white", style = "background-color: #2B388F;"
          ),
          layout_column_wrap(
            width = 1/2,
            padding = "10px",
            style = "margin-top: 10px;",
            
            # Card 1: Total Students & Today's Count
            card(
              card_header("Overall Usage", class = "bg-light"),
              layout_column_wrap(
                width = 1,
                padding = "5px",
                style = "margin-top: 5px",
                
                # Total students metric
                metric_card(
                  title = "Total Students This Year",
                  value = textOutput(outputId = "nrow_new"),
                  showcase = bsicons::bs_icon("mortarboard-fill", size = "1.5rem"),
                  description = textOutput("selected_year", inline = TRUE),
                  color = "#2B388F"
                ),
                
                # Today's count metric
                metric_card(
                  title = "Students Seen Today",
                  value = textOutput(outputId = "students_today"),
                  showcase = bsicons::bs_icon("calendar-check", size = "1.5rem"),
                  description = format(Sys.Date(), "%b %d, %Y"),
                  color = "#00944d"
                )
              )
            ),
            
            # Card 2: Average & Maximum metrics
            card(
              card_header("Performance Metrics", class = "bg-light"),
              layout_column_wrap(
                width = 1,
                padding = "5px",
                style = "margin-top: 5px",
                
                # Average attendance metric
                metric_card(
                  title = uiOutput("avg_title"),
                  value = textOutput(outputId = "avg_attendance"),
                  showcase = bsicons::bs_icon("people-fill", size = "1.5rem"),
                  description = "Based on selected time frame",
                  color = "#af2a30"
                ),
                
                # Maximum attendance metric
                metric_card(
                  title = uiOutput("max_title"),
                  value = textOutput(outputId = "max_attendance"),
                  showcase = bsicons::bs_icon("graph-up", size = "1.5rem"),
                  description = "Peak usage during selected period",
                  color = "#af2a30"
                )
              )
            )
          )
        ),
        
        # Attendance chart section
        card(
          full_screen = TRUE,
          card_header(
            "Attendance Trends",
            class = "text-white", style = "background-color: #2B388F;"
          ),
          plotOutput(outputId = "attendance_freq_plot", height = "400px")
        )
      )
      #### end mainPanel() -----------------------------------
    ) ### end sidebarLayout() --------------------------------------------
  ), ## end nav_panel() --------------------------------------------
  
  
  # ---- P2: Types of Assistance Tab ----
  ## nav_panel() (Types of Assistance) -----------------------------------
  nav_panel(
    title = "Types of Assistance",
    navset_card_tab(
      nav_panel(
        title = "Types of Assistance Analysis",
        page_sidebar(
          sidebar = sidebar(
            # Time frame selector
            radioButtons(
              inputId = "pg2.timeframeSelect",
              label = "View By:",
              choices = c(
                "Academic Year" = "year",
                "Term" = "term",
                "Month" = "month"
              ),
              selected = "term"
            ),
            
            # Conditional UI based on selection
            conditionalPanel(
              condition = "input['pg2.timeframeSelect'] == 'year'",
              selectInput(
                inputId = "pg2.yearSelect",
                label = "Select Academic Year:",
                choices = c(
                  "2023-2024",
                  "2024-2025"
                ),
                selected = "2024-2025"
              )
            ),
            
            conditionalPanel(
              condition = "input['pg2.timeframeSelect'] == 'term'",
              selectInput(
                inputId = "pg2.termSelect",
                label = "Select Term:",
                choices = c(
                  "Spring 2024",
                  "Summer 2024",
                  "Fall 2024",
                  "Spring 2025"
                ),
                selected = "Spring 2025"
              )
            ),
            
            conditionalPanel(
              condition = "input['pg2.timeframeSelect'] == 'month'",
              selectInput(
                inputId = "pg2.yearSelectMonth",
                label = "Select Academic Year:",
                choices = c(
                  "2023-2024",
                  "2024-2025"
                ),
                selected = "2024-2025"
              )
            )
          ),
          card(
            card_header(
              class = "text-white",
              style = "background-color: #2B388F;",
              "Types of Assistance"
            ),
            full_screen = TRUE,
            plotOutput(outputId = "combinedAssistancePlot", height = "600px")
          )
        )
      ),
      
      ## nav_panel() (Initial/SAR7/Recert) -----------------------------------
      nav_panel(
        title = "Initial/SAR7/Recert",
        page_sidebar(
          sidebar = sidebar(
            # Time frame selector
            radioButtons(
              inputId = "pg2.formTypeTimeframeSelect",
              label = "View By:",
              choices = c(
                "Academic Year" = "year",
                "Term" = "term",
                "Month" = "month"
              ),
              selected = "term"
            ),
            
            # Conditional UI based on selection
            conditionalPanel(
              condition = "input['pg2.formTypeTimeframeSelect'] == 'year'",
              selectInput(
                inputId = "pg2.formTypeYearSelect",
                label = "Select Academic Year:",
                choices = c(
                  "2023-2024",
                  "2024-2025"
                ),
                selected = "2024-2025"
              )
            ),
            
            conditionalPanel(
              condition = "input['pg2.formTypeTimeframeSelect'] == 'term'",
              selectInput(
                inputId = "pg2.termSelect_form_type",
                label = "Select Term:",
                choices = c(
                  "Spring 2024",
                  "Summer 2024",
                  "Fall 2024",
                  "Spring 2025"
                ),
                selected = "Spring 2025"
              )
            ),
            
            conditionalPanel(
              condition = "input['pg2.formTypeTimeframeSelect'] == 'month'",
              selectInput(
                inputId = "pg2.formTypeYearSelectMonth",
                label = "Select Academic Year:",
                choices = c(
                  "2023-2024",
                  "2024-2025"
                ),
                selected = "2024-2025"
              )
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header(
                class = "text-white",
                style = "background-color: #2B388F;",
                "Form Type Distribution"
              ),
              plotOutput("formTypeChart", height = "400px")
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "text-white",
                style = "background-color: #2B388F;",
                "Summary Table"
              ),
              tableOutput("formTypeTable")
            )
          )
        )
      ) ## end nav_panel() (Initial/SAR7/Recert) -----------------------------------
    )
  ), ## end nav_panel() (Types of Assistance) -----------------------------------
  
  # ---- P3: Visit Patterns Tab ----
  ## nav_panel (Visit Patterns) -----------------------------------
  nav_panel(
    title = "Visit Patterns",
    navset_card_tab(
      ## nav_panel (Weekly Comparison) -----------------------------------
      nav_panel(
        title = "Weekly Comparison",
        page_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "pg3.selected_month",
              label = "Select Month:",
              choices = setNames(month.abb, month.name),  # This will show full names but store abbreviations
              selected = "Apr"
            ),
            radioButtons(
              inputId = "pg3.weekly.plot_type",
              label = "Plot Type:",
              choices = c(
                "Trend Line" = "trend",
                "Bar Chart" = "bar",
                "Heatmap" = "heatmap"
              ),
              selected = "trend"
            )
          ),
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header(
                class = "text-white",
                style = "background-color: #2B388F;",
                "Weekly Comparison"
              ),
              plotOutput("weeklyPlot", height = "400px")
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "text-white",
                style = "background-color: #2B388F;",
                "Summary Table"
              ),
              tableOutput("weeklyTable")
            )
          )
        )
      ), ## end nav_panel (Weekly Comparison) -----------------------------------
      
      ## nav_panel (Visit Analysis) - Combined tab -----------------------------------
      nav_panel(
        title = "Visit Analysis",
        page_sidebar(
          sidebar = sidebar(
            # Time frame selector
            radioButtons(
              inputId = "pg3.visitTimeframeSelect",
              label = "View By:",
              choices = c(
                "Academic Year" = "year",
                "Term" = "term"
              ),
              selected = "term"
            ),
            
            # Conditional UI based on selection
            conditionalPanel(
              condition = "input['pg3.visitTimeframeSelect'] == 'year'",
              selectInput(
                inputId = "pg3.yearSelect_visits",
                label = "Select Academic Year:",
                choices = c(
                  "2023-2024",
                  "2024-2025"
                ),
                selected = "2024-2025"
              )
            ),
            
            conditionalPanel(
              condition = "input['pg3.visitTimeframeSelect'] == 'term'",
              selectInput(
                inputId = "pg3.termSelect_visits",
                label = "Select Term:",
                choices = c(
                  "Spring 2024",
                  "Summer 2024",
                  "Fall 2024",
                  "Spring 2025"
                ),
                selected = "Spring 2025"
              )
            )
          ),
          # Use layout_columns with explicit nesting for 2x2 grid
          layout_columns(
            # First row (2 columns)
            layout_columns(
              col_widths = c(6, 6),
              card(
                full_screen = TRUE,
                card_header(
                  class = "text-white",
                  style = "background-color: #2B388F;",
                  "Visit Distribution"
                ),
                plotOutput("repPlot", height = "300px")
              ),
              card(
                full_screen = TRUE,
                card_header(
                  class = "text-white",
                  style = "background-color: #2B388F;",
                  "Appointment Types"
                ),
                plotOutput("typePlot", height = "300px")
              )
            ),
            # Second row (2 columns)
            layout_columns(
              col_widths = c(6, 6),
              card(
                full_screen = TRUE,
                card_header(
                  class = "text-white",
                  style = "background-color: #2B388F;",
                  "Term Visits"
                ),
                plotOutput("termPlot", height = "300px")
              ),
              card(
                full_screen = TRUE,
                card_header(
                  class = "text-white",
                  style = "background-color: #2B388F;",
                  "Break Period Visits"
                ),
                plotOutput("breakPlot", height = "300px")
              )
            )
          )
        )
      ) ## end nav_panel (Visit Analysis) -----------------------------------
    )
  ), ## end nav_panel() (Visit Patterns) -----------------------------------

  
  # ---- P4: How Did You Hear about Us? ----
  
  ## nav_panel() (How Did You Hear about Us?) -----------------------------------
  nav_panel(
    title = "How Did You Hear about Us?",
    page_sidebar(
      ### sidebar() -----------------------------------
      sidebar = sidebar(
        # Time frame selector (similar to Visit Analysis)
        radioButtons(
          inputId = "pg4.howTimeframeSelect",
          label = "View By:",
          choices = c(
            "Academic Year" = "year",
            "Term" = "term"
          ),
          selected = "term"
        ),
        
        # Conditional UI based on selection
        conditionalPanel(
          condition = "input['pg4.howTimeframeSelect'] == 'year'",
          selectInput(
            inputId = "pg4.yearSelect_how",
            label = "Select Academic Year:",
            choices = c(
              "2023-2024",
              "2024-2025"
            ),
            selected = "2024-2025"
          )
        ),
        
        conditionalPanel(
          condition = "input['pg4.howTimeframeSelect'] == 'term'",
          selectInput(
            inputId = "pg4.termSelect_how",
            label = "Select Term:",
            choices = c(
              "Spring 2024",
              "Summer 2024",
              "Fall 2024",
              "Spring 2025"
            ),
            selected = "Spring 2025"
          )
        ),
        
        # Add checkbox to show/hide data table
        checkboxInput(
          inputId = "pg4.showDataTable",
          label = "Show Data Table",
          value = TRUE
        )
      ), ### end sidebar() -----------------------------------
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header("Response Distribution"),
          plotOutput(outputId = "hearAboutPlot", height = "400px")
        ),
        # Only show the table card when the checkbox is checked
        conditionalPanel(
          condition = "input['pg4.showDataTable'] == true",
          card(
            full_screen = TRUE,
            card_header("Data Table"),
            DTOutput(outputId = "hearAboutTable")
          )
        )
      )
    )
  ), ## end nav_panel() (How Did You Hear about Us?) -----------------------------------
  
  
  # ---- P5: Y-o-Y Comparison ----
  ## nav_panel() (Y-o-Y Comparison) -----------------------------------
  comparison_tab <- nav_panel(
    title = "Year-over-Year Comparison",
    ### layout_sidebar() -----------------------------------
    layout_sidebar(
      #### sidebar() -----------------------------------
      sidebar = sidebar(
        title = "Grouping Options",
        width = 300,
        selectInput("groupBy", "Group Data By:",
                    choices = c(
                      "Term" = "term",
                      "Month" = "monthname",
                      "Quarter" = "quarter",
                      "Week of Month" = "weekofmonth",
                      "Day of Week" = "dayofweek"
                    )),
        # Only shows when Week of Month is selected
        conditionalPanel(
          condition = "input.groupBy == 'weekofmonth_des'",
          selectInput("selected_month_comparison", "Select Month:",
                      choices = month.abb)
        ),
        
        ##### selectInput() -----------------------------------
        selectInput("aggregateBy", "Aggregate By:",
                    choices = c(
                      "Count" = "count",
                      "Unique Email Addresses" = "email_address"
                    )), ##### end selectInput() -----------------------------------
        
        ##### selectInput() -----------------------------------
        selectInput("secondaryGroupBy", "Secondary Grouping (Optional):",
                    choices = c(
                      "None" = "none",
                      "Zoom vs In-Office" = "zoom_appoint_y_n",
                      "Service Type" = "initial_sar7_or_recert",
                      "Type of Assistance" = "how_can_we_assist_you_today",
                      "School Break Status" = "school_break"
                      
                    )), ##### end selectInput() -----------------------------------
        
        checkboxInput("showTable", "Show Data Table", TRUE),
        
        downloadButton("downloadData", "Download Comparison Data")
      ), #### end sidebar() -----------------------------------
      
      # Main content
      card(
        card_header("Visual Comparison: 2024 vs 2025"),
        plotOutput("comparisonPlot", height = "400px")
      ),
      
      # Conditional panel for data table
      conditionalPanel(
        condition = "input.showTable == true",
        card(
          card_header("Data Table"),
          DTOutput("comparisonTable")
        )
      ),
      
      # Summary card
      card(
        card_header("Year-over-Year Summary"),
        textOutput("yearSummary"),
        br(),
        textOutput("percentChange")
      )
    ) ### end layout_sidebar() -----------------------------------
  ), ## end nav_panel() (Y-o-Y Comparison) -----------------------------------
)


