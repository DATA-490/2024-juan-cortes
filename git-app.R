library(shiny)
library(bslib)
library(gridlayout)
library(googlesheets4)
library(tidyverse)
library(DT)
library(sjPlot)
library(gtsummary)


# Apply ggplot theming to the entire app
theme_set(theme_bw())

# Define choices first to ensure consistency
timeframe_choices <- c(
  "Days" = "date",
  "Weeks" = "short.week",
  "Months" = "monthname",
  "Terms" = "term",
  "School Break" = "school_term"
)

# ---- UI ----
ui <- page_navbar(
  title = "CFO Campus Office Sign-In Sheet",
  collapsible = TRUE,
  theme = "style.css",
  # ---- Overview Tab ----,
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
    sidebarLayout(
      # ---- Sidebar ----
      sidebarPanel(
        width = 3,
        tags$div(HTML("<h3 style='color:#2B388F;text-align:center'><b>Welcome to the CHC CFO Sign-in Dashboard</b></h3>")),
        br(),
        tags$div(HTML("<p><b><i>This summary report captures information about walk-in student visits to the Chico State campus
                    Student Services Center (SSC) CalFresh Outreach (CFO) office.</b></i></p>")),
        br(),
        # Moved time frame selection here
        actionButton(
          inputId = "getdata",
          label = "Get new data",
          style = "margin-bottom: 20px; width: 100%;"
        ),
        selectInput(
          inputId = "yearRange",
          label = "Academic Year",
          choices = c(
            "2023-2024" = "2023",
            "2024-2025" = "2024"
          ),
          width = "100%"
        ),
        selectInput(
          inputId = "groupcountsby",
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
        # Logo
        img(src = "Calfresh_CHC_Logo.png", height = "100%", width = "100%")
      ),

      # ---- Main Panel ----
      mainPanel(
        # Overview Section with custom styling
        div(
          style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; margin-bottom: 30px; border: 1px solid #dee2e6;",
          h1("Overview", style = 'color:#2B388F; margin-top: 0;'),
          p("Students completed the Google CalFresh Outreach (CFO) Office Sign-in Sheet, which collected information about their visit history, how they learned about the SSC CFO office, the type of services they were seeking, and the name of the assistor. The report is organized into daily, weekly, monthly, and termly time periods.",
            tags$b(
              "To date, ", textOutput("nrow_new", inline = TRUE), "students have used the CFO office sign-in sheet.")
          )
        ),

        # Cards Section with custom styling
        div(
          style = "background-color: white; padding: 20px; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",

          # Single row for summary info
          layout_column_wrap(
            width = 1,
            # ---- Card: Summary Info ----
            card(
              height = "200px",
              card_header(strong("Summary Info"),
                          style = "background-color: #f8f9fa;"),
              card_body(
                layout_column_wrap(
                  width = 1/2,
                  value_box(
                    title = "Average",
                    theme = "success",
                    value = textOutput(outputId = "avg_attendance"),
                    showcase = bsicons::bs_icon("people-fill"),
                    height = "130px"  # Added fixed height
                  ),
                  value_box(
                    title = "Maximum",
                    theme = "success",
                    value = textOutput(outputId = "max_attendance"),
                    showcase = bsicons::bs_icon("graph-up"),
                    height = "130px"  # Added fixed height
                  )
                )
              )
            )
          ),

          # Bottom row with attendance plot
          layout_column_wrap(
            width = 1,
            # ---- Card: Attendance Frequency Plot ----
            card(
              card_header(strong("Attendance Frequency"),
                          style = "background-color: #f8f9fa;"),
              card_body(
                plotOutput(outputId = "attendance_freq_plot", height = "400px")
              )
            )
          )
        )
      )
    )
  ),

  # ---- Types of Assistance Tab ----
  nav_panel(
    title = "Types of Assistance",
    navset_card_underline(
      nav_panel(
        title = "Overall Types of Assistance",
        page_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "termSelect",
              label = "Select Term:",
              choices = c(
                "Spring 2024",
                "Summer 2024",
                "Fall 2024"
              ),
              selected = "Fall 2024"
            )
          ),
          layout_columns(
            plotOutput(outputId = "barChartPlot", height = "600px"),
          )
        )
      ),
      nav_panel(
        title = "Monthly Types of Assistance",
        page_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "yearSelect",
              label = "Select Academic Year:",
              choices = c(
                "2023-2024",
                "2024-2025"
              ),
              selected = "2024-2025"
            )
          ),
          layout_columns(
            plotOutput(outputId = "histogramPlot", height = "600px"),
          )
        )
      ),
    )
  ),
  
  # ---- Visit Patterns Tab ----
  nav_panel(
    title = "Visit Patterns",
    navset_card_underline(
      nav_panel(
        title = "Repeat & Types of Visit",
        page_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "termSelect_visits",
              label = "Select Term:",
              choices = c(
                "Spring 2024",
                "Summer 2024",
                "Fall 2024"
              ),
              selected = "Fall 2024"
            )
          ),
          layout_columns(
            plotOutput("repPlot", height = "600px"),
            plotOutput("typePlot", height = "600px")
          )
        )
      ),
      nav_panel(
        title = "Term & Break Visit",
        page_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "yearSelect_break",
              label = "Select Academic Year:",
              choices = c(
                "2023-2024",
                "2024-2025"
              ),
              selected = "2024-2025"
            )
          ),
          layout_columns(
            plotOutput("termPlot", height = "600px"),
            plotOutput("breakPlot", height = "600px")
          )
        )
      )
    )
  ),
  
  # ---- How Did You Hear about Us? ----
  nav_panel(
    title = "How Did You Hear about Us?",
    page_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "termSelect",
          label = "Select Term:",
          choices = c(
            "Spring 2024",
            "Summer 2024",
            "Fall 2024"
          ),
          selected = "Fall 2024"
        )
      ),
      layout_columns(
        plotOutput(outputId = "hearAboutPlot", height = "600px"),
      )
    )
  ), 
)



server <- function(input, output) {

  ## Go get new data from google
  newdata <- eventReactive(input$getdata, {
    # Load the .RData file and capture the data frame (assume the object name is 'test_data')
    load("Test_Data.RData")  # This will load the object that was saved in 'Test_Data.RData'
    
    
    return(simulated.data)
    })

  # ---- TAB: Overview ----
  # Dynamically calculate the number of rows (total number of students)
  output$nrow_new <- renderText({
    NROW(newdata())
  })

  # reactive grouping counts by selected time period
  counts.by.time <- reactive({
    cbt <- newdata() %>%
      group_by(.data[[input$groupcountsby]] ) %>%
      summarize(n=n())
    names(cbt)[1] <- "group"
    return(cbt)
  })

  # Mapping of values to display labels
  timeframe_choices <- c(
    date = "Day",
    short.week = "Week",
    monthname = "Month",
    year = "Academic Year",
    term = "Term"
  )

## ---- Value Box Outputs ----
  # reactive grouping for value box
  counts.by.time_vbox <- reactive({
    newdata() %>%
      group_by(.data[[input$groupcountsby]] ) %>%
      summarize(n=n())
  })
  
   # max attendance
  output$max_attendance <- renderText({
    data <- counts.by.time_vbox()
    max_attendance <- max(data$n, na.rm = TRUE)
    return(max_attendance)
  })
  
  # add render text for value box
  output$avg_attendance <- renderText({
    data <- counts.by.time_vbox()
    #print(data)
    avg_attendance <- round(mean(data$n, na.rm = TRUE), 0)
    return(avg_attendance)
  })

## ---- Main Plot Output ----

    # average attendance plot
  output$attendance_freq_plot <- renderPlot({
    # Ensure that counts.by.time() returns a data frame
    newdata <- counts.by.time()
    if (is.null(newdata) || !("group" %in% colnames(newdata)) || !("n" %in% colnames(newdata))) {
      return(NULL)  # Avoid errors if data is missing
    }

    # Get the display name that corresponds to the selected value
    user_timeframe <- names(which(timeframe_choices == input$groupcountsby))
    # Base plot

     attendance_plot <- ggplot(data = newdata, aes(x = group, y = n))
    # Different plot types based on timeframe
    if (input$groupcountsby == "date") {
      # Lollipop plot for days
      attendance_plot <- attendance_plot +
        geom_segment(aes(x = group, xend = group, y = 0, yend = n),
                     color = "skyblue") +
        geom_point(size = 3, color = "darkblue")
    } else {
      # Bar plot for all other timeframes
      attendance_plot <- attendance_plot +
        geom_col(fill = "skyblue", color = "darkblue", alpha = 0.7) +
        geom_text(aes(label = n), vjust = -0.5, size = 3) 
    }

    # Add common elements
    attendance_plot +
      labs(title = paste("Number of Students seen by", user_timeframe),
           x = paste(user_timeframe, "Choices"),
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })


  # ---- TAB: Types of Assistance ----
  # Separate reactive datasets for term and year views
  term_data <- reactive({
    req(input$termSelect)
    newdata() %>%
      filter(term == input$termSelect)
  })

  year_data <- reactive({
    req(input$yearSelect)
    newdata() %>%
      filter(year == input$yearSelect)
  })

  ## ----Bar Chart (tab)----
  output$barChartPlot <- renderPlot({
    req(input$termSelect)
    clean <- term_data()
    
    # Create summary data with percentages
    plot_data <- clean %>%
      drop_na(how_can_we_assist_you_today) %>%  # remove NAs
      count(how_can_we_assist_you_today) %>%
      mutate(percentage = n/sum(n) * 100)
    
    ggplot(plot_data,
           aes(x = stringr::str_wrap(how_can_we_assist_you_today, width=55),
               y = percentage,
               fill = how_can_we_assist_you_today)) +
      geom_col(position = "dodge") +
      labs(x = "",
           y = "Percentage (%)",
           title = paste("Types of Assistance -", input$termSelect)) + 
      coord_flip() +
      scale_fill_discrete(guide = "none") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 17),
        plot.title = element_text(size = 16),  # title
        axis.title = element_text(size = 14)   # axis titles
      ) +
      geom_text(aes(label = sprintf("%.1f%%", percentage)),
                position = position_dodge(width = 0.9),
                hjust = -0.2,
                size = 7) +  
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
  })
  
  ## ----Histogram Plot (tab)----
  output$histogramPlot <- renderPlot({
    req(input$yearSelect)
    clean <- year_data() 
    
    plot_data <- clean %>%
      drop_na(how_can_we_assist_you_today) %>%  # remove NAs
      group_by(monthname) %>%
      count(how_can_we_assist_you_today) %>%
      group_by(monthname) %>%
      mutate(percentage = n/sum(n) * 100)
    
    ggplot(plot_data,
           aes(x = stringr::str_wrap(how_can_we_assist_you_today, width=55),
               y = percentage,
               fill = how_can_we_assist_you_today)) +
      geom_col(position = "dodge") +
      facet_wrap(~monthname, ncol=4) +
      labs(x = "",
           y = "Percentage (%)",
           title = paste("Type of Service by Month -", input$yearSelect)) + 
      coord_flip() +
      scale_fill_discrete(guide = "none") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 15),
        strip.text = element_text(size = 14),  # facet labels
        plot.title = element_text(size = 16),  # title
        axis.title = element_text(size = 14)   # axis titles
      ) +
      geom_text(aes(label = sprintf("%.1f%%", percentage)),
                position = position_dodge(width = 0.9),
                hjust = -0.2,
                size = 7) + 
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
  })


  # ---- TAB: Visits ----
  # Reactive datasets
  visits_term_data <- reactive({
    req(input$termSelect_visits)
    newdata() %>%
      filter(term == input$termSelect_visits)
  })

  visits_year_data <- reactive({
    req(input$yearSelect_break)
    newdata() %>%
      filter(year == input$yearSelect_break)
  })
 
  ## ----Repeat & Types of Visit----
  output$repPlot <- renderPlot({
    req(input$termSelect_visits)
    clean <- visits_term_data()
    
    clean %>%
      group_by(email_address) %>%
      tally() %>%
      select(n) %>%
      plot_frq(ylim = c(0, NROW(clean)),
               geom.colors = "#2E86C1") +
      xlab("Number of visits") +
      ylab("Count") +
      ggtitle(paste("Visit Distribution -", input$termSelect_visits)) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 16)
      )

  })
  
  ## How many times were students helped in-office vs. over Zoom?
  output$typePlot <- renderPlot({
    req(input$termSelect_visits)
    clean <- visits_term_data()
    
    plot_frq(clean$zoom_appoint_y_n,
             geom.colors = "#2E86C1") + 
      xlab("Type of Appointment") +
      ylab("Count") +
      ggtitle(paste("Appointment Types -", input$termSelect_visits)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
  })
  
  ## ----Term & Break Type----
  output$termPlot <- renderPlot({
    req(input$yearSelect_break)
    clean <- visits_year_data()
    
    plot_frq(clean$term,
             geom.colors = "#2E86C1") + 
      xlab("School Term") +
      ylab("Count") +
      ggtitle(paste("Term Visits -", input$yearSelect_break)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
      
  })
  
  output$breakPlot <- renderPlot({
    req(input$yearSelect_break)
    clean <- visits_year_data()
    
    plot_frq(clean$school_break,
             geom.colors = "#2E86C1") +  
      xlab("School Break") +
      ylab("Count") +
      ggtitle(paste("Break Period Visits -", input$yearSelect_break)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
  })

  # ---- TAB: How did you hear about us (tab) ----
  how_heard <- reactive({
    req(input$termSelect)
    
    total <- term_data() %>% 
      select(how_did_you_hear_about_the_cal_fresh_office) %>% 
      separate_longer_delim(cols = how_did_you_hear_about_the_cal_fresh_office, 
                            delim = ",") %>% 
      drop_na() %>% 
      nrow() 
   
     term_data() %>%
      select(how_did_you_hear_about_the_cal_fresh_office) %>%
      separate_longer_delim(cols = how_did_you_hear_about_the_cal_fresh_office, delim = ",") %>%
      drop_na() %>%
    
      group_by(how_did_you_hear_about_the_cal_fresh_office) %>% 
      summarize(
        n = n(), 
        pct = n/total, 
        lab = paste0(n, " \n (", scales::percent(pct), ")")
      ) %>%
      arrange(desc(n)) %>% 
      slice(1:10)
  })
  
  output$hearAboutPlot <- renderPlot({
    req(input$termSelect)
    data <- how_heard()
    
    ggplot(data, 
           aes(x = reorder(how_did_you_hear_about_the_cal_fresh_office, -n), 
               y = n)) +
      geom_col(fill = "#00944d") + 
      geom_text(aes(label = lab), 
                vjust = -0.5, 
                size = 6) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(
        title = paste("How Students Heard About Us -", input$termSelect),
        x = NULL,
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16, angle = 35, hjust = 1, color = "black"),
        axis.text.y = element_text(size = 16,color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
  })

}

shinyApp(ui, server)


