server <- function(input, output, session) {
  # Initialize data loading status
  data_status <- reactiveVal("loading") # can be "loading", "success", or "error"
  
  ## Go get new data from google
  newdata <- reactive({
    if (file.exists("Test_Data.RData")) {
      load("Test_Data.RData")  # loads simulated.data into the environment
      return(simulated.data)   # return the loaded data
    } else {
      showNotification("Test_Data.RData not found.", type = "error", duration = NULL)
      return(data.frame())  # return empty data frame if file doesn't exist
    }
  })
  
  # ---- P1: Overview ----
  ## To date (count)
  output$nrow_new <- renderText({
    req(input$pg1.yearRange, filtered_data())
    nrow(filtered_data())
  })
  
  output$selected_year <- renderText({
    req(input$pg1.yearRange)
    input$pg1.yearRange
  })
  
  filtered_data <- reactive({
    req(input$pg1.yearRange, newdata())
    filtered <- newdata() %>%
      filter(year == input$pg1.yearRange)
    
    if (nrow(filtered) == 0) {
      warning("No data available for selected academic year: ", input$pg1.yearRange)
    }
    filtered
  })
  
  # Students seen today
  output$students_today <- renderText({
    req(filtered_data())
    
    # Current date
    today_date <- Sys.Date()
    
    # Filter for today's entries
    today_data <- filtered_data() %>%
      filter(date == today_date)
    
    # Return count
    count <- nrow(today_data)
    if(count == 0) {
      return("0")
    } else {
      return(as.character(count))
    }
  })
  
  counts.by.time <- reactive({
    req(input$pg1.groupcountsby, filtered_data())
    cbt <- filtered_data() %>%
      group_by(.data[[input$pg1.groupcountsby]]) %>%
      summarize(n = n())
    names(cbt)[1] <- "group"
    return(cbt)
  })
  
  pg1.counts.by.time_vbox <- reactive({
    req(input$pg1.groupcountsby, filtered_data())
    filtered_data() %>%
      group_by(.data[[input$pg1.groupcountsby]]) %>%
      summarize(n = n())
  })
  
  output$max_attendance <- renderText({
    req(pg1.counts.by.time_vbox())
    data <- pg1.counts.by.time_vbox()
    if (nrow(data) == 0) return("No data")
    max_attendance <- max(data$n, na.rm = TRUE)
    if (is.infinite(max_attendance)) return("No data")
    return(max_attendance)
  })
  
  output$avg_attendance <- renderText({
    req(pg1.counts.by.time_vbox())
    data <- pg1.counts.by.time_vbox()
    if (nrow(data) == 0) return("No data")
    avg_attendance <- round(mean(data$n, na.rm = TRUE), 0)
    if (is.infinite(avg_attendance)) return("No data")
    return(avg_attendance)
  })
  
  # Titles for Summary Information
  output$avg_title <- renderUI({
    req(input$pg1.groupcountsby)
    # Create a named vector for mapping
    time_labels <- c(
      "date" = "Days",
      "short.week" = "Weeks",
      "monthname" = "Months",
      "term" = "Terms",
      "year" = "Academic Year"
    )
    avg_choice <- time_labels[input$pg1.groupcountsby]
    # Special case for Academic Year
    if(input$pg1.groupcountsby == "year") {
      HTML("Average Number of Students for the Academic Year")
    } else {
      HTML(paste("Average Number of Students Seen in", avg_choice))
    }
  })
  
  output$max_title <- renderUI({
    req(input$pg1.groupcountsby)
    time_labels <- c(
      "date" = "Days",
      "short.week" = "Weeks",
      "monthname" = "Months",
      "term" = "Terms",
      "year" = "Academic Year"
    )
    max_choice <- time_labels[input$pg1.groupcountsby]
    if(input$pg1.groupcountsby == "year") {
      HTML("Highest Student Count for the Academic Year")
    } else {
      HTML(paste("Highest Student Count in", max_choice))
    }
  })
  
  
  # Attendance Frequency Plot
  output$nrow_new <- renderText({
    req(filtered_data())
    NROW(filtered_data())
  })
  
  output$attendance_freq_plot <- renderPlot({
    req(counts.by.time(), input$pg1.groupcountsby)
    
    newdata <- counts.by.time()
    if (is.null(newdata) || !("group" %in% colnames(newdata)) || !("n" %in% colnames(newdata))) {
      return(NULL)
    }
    
    user_timeframe <- names(which(timeframe_choices == input$pg1.groupcountsby))
    
    attendance_plot <- ggplot(data = newdata, aes(x = group, y = n))
    
    if (input$pg1.groupcountsby == "date") {
      attendance_plot <- attendance_plot +
        geom_segment(aes(x = group, xend = group, y = 0, yend = n),
                     color = "skyblue") +
        geom_point(size = 3, color = "darkblue")
    } else {
      attendance_plot <- attendance_plot +
        geom_col(fill = "skyblue", color = "darkblue", alpha = 0.7) +
        geom_text(aes(label = n), vjust = -0.5, size = 3) 
    }
    
    attendance_plot +
      labs(title = paste("Number of Students seen by", user_timeframe),
           x = paste(user_timeframe, "Choices"),
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ---- P2: Types of Assistance ----
  # Reactive data based on time frame selection
  assistance_data <- reactive({
    req(input$pg2.timeframeSelect)
    
    if (input$pg2.timeframeSelect == "year") {
      req(input$pg2.yearSelect)
      newdata() %>%
        filter(year == input$pg2.yearSelect)
    } else if (input$pg2.timeframeSelect == "term") {
      req(input$pg2.termSelect)
      newdata() %>%
        filter(term == input$pg2.termSelect)
    } else { # month view
      req(input$pg2.yearSelectMonth)
      newdata() %>%
        filter(year == input$pg2.yearSelectMonth)
    }
  })
  
  ## ----Combined Assistance Plot----
  output$combinedAssistancePlot <- renderPlot({
    req(input$pg2.timeframeSelect, assistance_data())
    clean <- assistance_data()
    
    if (input$pg2.timeframeSelect == "year") {
      # Yearly comparison - no faceting
      plot_data <- clean %>%
        drop_na(how_can_we_assist_you_today) %>%
        count(how_can_we_assist_you_today) %>%
        mutate(percentage = n/sum(n) * 100)
      
      title_text <- paste("Types of Assistance -", input$pg2.yearSelect)
      facet_var <- NULL
      
    } else if (input$pg2.timeframeSelect == "term") {
      # Term view - no faceting
      plot_data <- clean %>%
        drop_na(how_can_we_assist_you_today) %>%
        count(how_can_we_assist_you_today) %>%
        mutate(percentage = n/sum(n) * 100)
      
      title_text <- paste("Types of Assistance -", input$pg2.termSelect)
      facet_var <- NULL
      
    } else {
      # Monthly view - facet by month
      plot_data <- clean %>%
        drop_na(how_can_we_assist_you_today) %>%
        group_by(monthname) %>%
        count(how_can_we_assist_you_today) %>%
        group_by(monthname) %>%
        mutate(percentage = n/sum(n) * 100)
      
      title_text <- paste("Type of Service by Month -", input$pg2.yearSelectMonth)
      facet_var <- "monthname"
    }
    
    # Create the base plot
    p <- ggplot(plot_data,
                aes(x = stringr::str_wrap(how_can_we_assist_you_today, width=55),
                    y = percentage,
                    fill = how_can_we_assist_you_today)) +
      geom_col(position = "dodge") +
      labs(x = "",
           y = "Percentage (%)",
           title = title_text) + 
      coord_flip() +
      scale_fill_discrete(guide = "none") +
      theme_minimal() +
      geom_text(aes(label = sprintf("%.1f%%", percentage)),
                position = position_dodge(width = 0.9),
                hjust = -0.2,
                size = 7) +  
      scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
    
    # Apply faceting if needed
    if (!is.null(facet_var)) {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)), ncol=4) +
        theme(
          axis.text.x = element_text(color = "black", size = 12),
          axis.text.y = element_text(color = "black", size = 15),
          strip.text = element_text(size = 14),
          plot.title = element_text(size = 16),
          axis.title = element_text(size = 14)
        )
    } else {
      p <- p + theme(
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 17),
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 14)
      )
    }
    
    return(p)
  })
  
  ## ----Initial/SAR7/Recert Analysis----
  # Reactive data based on time frame selection
  form_type_data <- reactive({
    req(input$pg2.formTypeTimeframeSelect)
    
    if (input$pg2.formTypeTimeframeSelect == "year") {
      req(input$pg2.formTypeYearSelect)
      newdata() %>%
        filter(year == input$pg2.formTypeYearSelect) %>%
        filter(!is.na(initial_sar7_or_recert)) %>%
        filter(initial_sar7_or_recert != "Not Specified")
    } else if (input$pg2.formTypeTimeframeSelect == "term") {
      req(input$pg2.termSelect_form_type)
      newdata() %>%
        filter(term == input$pg2.termSelect_form_type) %>%
        filter(!is.na(initial_sar7_or_recert)) %>%
        filter(initial_sar7_or_recert != "Not Specified")
    } else { # month view
      req(input$pg2.formTypeYearSelectMonth)
      newdata() %>%
        filter(year == input$pg2.formTypeYearSelectMonth) %>%
        filter(!is.na(initial_sar7_or_recert)) %>%
        filter(initial_sar7_or_recert != "Not Specified")
    }
  })
  
  # Chart for form types
  output$formTypeChart <- renderPlot({
    req(form_type_data(), input$pg2.formTypeTimeframeSelect)
    
    if (input$pg2.formTypeTimeframeSelect == "month") {
      # Monthly breakdown with facets
      plot_data <- form_type_data() %>%
        group_by(monthname, initial_sar7_or_recert) %>%
        summarize(n = n(), .groups = "drop") %>%
        group_by(monthname) %>%
        mutate(percentage = n/sum(n) * 100)
      
      title_text <- paste("Initial/SAR7/Recert Distribution by Month -", 
                          if(input$pg2.formTypeTimeframeSelect == "year") input$pg2.formTypeYearSelect 
                          else input$pg2.formTypeYearSelectMonth)
      
      p <- ggplot(plot_data,
                  aes(x = initial_sar7_or_recert,
                      y = percentage,
                      fill = initial_sar7_or_recert)) +
        geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.2) +
        facet_wrap(~monthname, ncol = 3) +
        labs(title = title_text,
             x = "Form Type",
             y = "Percentage (%)") +
        scale_fill_manual(values = c(
          "Initial" = "#C0392B",
          "SAR7" = "#27AE60",
          "Recert" = "#3498DB"
        ), guide = "none") +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10, color = "black"),
          plot.title = element_text(size = 14, color = "black"),
          strip.text = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 12, color = "black")
        ) +
        geom_text(aes(label = sprintf("%.1f%%", percentage)),
                  vjust = -0.5,
                  size = 3,
                  color = "black") +
        scale_y_continuous(limits = function(x) c(0, max(x) * 1.1))
      
    } else {
      # Year or Term view (no faceting)
      plot_data <- form_type_data() %>%
        count(initial_sar7_or_recert) %>%
        mutate(percentage = n/sum(n) * 100)
      
      # Set title based on selected time frame
      if (input$pg2.formTypeTimeframeSelect == "year") {
        title_text <- paste("Initial/SAR7/Recert Distribution -", input$pg2.formTypeYearSelect)
      } else {
        title_text <- paste("Initial/SAR7/Recert Distribution -", input$pg2.termSelect_form_type)
      }
      
      p <- ggplot(plot_data,
                  aes(x = reorder(initial_sar7_or_recert, -percentage),
                      y = percentage,
                      fill = initial_sar7_or_recert)) +
        geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.2) +
        labs(title = title_text,
             x = "Form Type",
             y = "Percentage (%)") +
        scale_fill_manual(values = c(
          "Initial" = "#C0392B",
          "SAR7" = "#27AE60",
          "Recert" = "#3498DB"
        ), guide = "none") +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(size = 12, color = "black", hjust = 0.5),
          axis.text.y = element_text(size = 12, color = "black"),
          plot.title = element_text(size = 16, color = "black"),
          axis.title = element_text(size = 14, color = "black")
        ) +
        geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", percentage, n)),
                  vjust = -0.5,
                  size = 5,
                  color = "black") +
        scale_y_continuous(limits = function(x) c(0, max(x) * 1.1))
    }
    
    return(p)
  })
  
  # Table for form types
  output$formTypeTable <- renderTable({
    req(form_type_data(), input$pg2.formTypeTimeframeSelect)
    
    if (input$pg2.formTypeTimeframeSelect == "month") {
      # For monthly view, summarize across all months
      data <- form_type_data() %>%
        count(initial_sar7_or_recert) %>%
        mutate(
          Percentage = sprintf("%.1f%%", n/sum(n) * 100),
          `Form Type` = initial_sar7_or_recert,
          Count = n
        ) %>%
        select(`Form Type`, Count, Percentage)
      
    } else {
      # For year or term view
      data <- form_type_data() %>%
        count(initial_sar7_or_recert) %>%
        mutate(
          Percentage = sprintf("%.1f%%", n/sum(n) * 100),
          `Form Type` = initial_sar7_or_recert,
          Count = n
        ) %>%
        select(`Form Type`, Count, Percentage)
    }
    
    return(data)
  }, striped = TRUE, bordered = TRUE, align = 'c')
  
  # ---- P3: Visits ----
  # Unified reactive dataset for visit analysis
  visit_analysis_data <- reactive({
    req(input$pg3.visitTimeframeSelect)
    
    if (input$pg3.visitTimeframeSelect == "year") {
      req(input$pg3.yearSelect_visits)
      newdata() %>%
        filter(year == input$pg3.yearSelect_visits)
    } else { # term view
      req(input$pg3.termSelect_visits)
      newdata() %>%
        filter(term == input$pg3.termSelect_visits)
    }
  })
  
  ## ----Weekly Comparison----
  # Weekly comparison outputs
  output$weeklyPlot <- renderPlot({
    req(input$pg3.selected_month)
    
    if (input$pg3.weekly.plot_type == "trend") {
      # Pre-calculate counts using weekofmonth instead of actual dates
      data <- newdata() %>%
        filter(monthname == input$pg3.selected_month) %>%
        group_by(weekofmonth, year) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(is_current = ifelse(year == "2024-2025", 1, 0.5))
      
      if (nrow(data) == 0) return(NULL)
      
      ggplot(data, aes(x = factor(weekofmonth), y = count, color = factor(year), group = year)) +
        geom_line(aes(alpha = is_current), linewidth = 1.5) + 
        geom_point(aes(alpha = is_current), size = 3) +
        scale_color_manual(
          values = c("2023-2024" = "#2E86C1", "2024-2025" = "#00944d"),
          name = "Academic Year"
        ) +
        scale_alpha_identity() +
        labs(title = paste("Weekly Trend for", month.name[match(input$pg3.selected_month, month.abb)]),
             subtitle = "Current year (2024-2025) shown with full opacity",
             x = "Week of Month",
             y = "Number of Visits") +
        theme_minimal() +
        theme(
          legend.position = "top",
          axis.text.x = element_text(size = 15),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, "cm")   
        )
      
    } else if (input$pg3.weekly.plot_type == "bar") {
      data <- newdata() %>%
        filter(monthname == input$pg3.selected_month) %>%
        group_by(weekofmonth, year) %>%
        summarise(count = n(), .groups = 'drop')
      
      ggplot(data, aes(x = factor(weekofmonth), y = count, fill = factor(year))) +
        geom_bar(stat = "identity", 
                 position = "dodge", 
                 color = "black",        
                 linewidth = 0.2,        # Make the outline thicker
                 alpha = 0.9) +          
        scale_fill_manual(
          values = c("2023-2024" = "#2E86C1", "2024-2025" = "#00944d"),
          name = "Academic Year"
        ) +
        labs(title = paste("Year Comparison for", month.name[match(input$pg3.selected_month, month.abb)]),
             x = "Week of Month",
             y = "Number of Visits") +
        theme_minimal() +
        theme(
          legend.position = "top",
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "gray90"), # Lighter grid lines
          panel.grid.minor = element_line(color = "gray95"),  # Lighter grid lines
          axis.text.x = element_text(size = 15),  # Increase x-axis numbers size
          axis.text.y = element_text(size = 15),  # Increase y-axis numbers size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14),  # Increase y-axis 
          legend.title = element_text(size = 13),    # Makes the legend title larger
          legend.text = element_text(size = 11),     # Makes the legend labels larger
          legend.key.size = unit(1, "cm")   
        )
      
    } else if (input$pg3.weekly.plot_type == "heatmap") {
      data <- newdata() %>%
        filter(monthname == input$pg3.selected_month) %>%
        group_by(year, weekofmonth) %>%
        summarise(visits = n(), .groups = 'drop')
      
      ggplot(data, aes(x = weekofmonth, y = factor(year), fill = visits)) +
        geom_tile() +
        scale_fill_gradient(low = "#E8F4F8", high = "#2E86C1") +
        labs(title = paste("Visit Heatmap for", month.name[match(input$pg3.selected_month, month.abb)]),  # This will use the full month name
             x = "Week of Month",
             y = "Year",
             fill = "Number of Visits") +
        theme_minimal() +
        theme(
          legend.position = "right",
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(size = 15),  # Increase x-axis numbers size
          axis.text.y = element_text(size = 15),  # Increase y-axis numbers size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)  # Increase y-axis 
        )
    }
  })
  
  
  # Weekly comparison table
  output$weeklyTable <- renderTable({
    req(input$pg3.selected_month)
    # Create base frame with just the weeks
    base_table <- data.frame(
      Week = 1:5
    )
    # Get the actual data if it exists
    actual_data <- newdata() %>%
      filter(monthname == input$pg3.selected_month) %>%
      group_by(year, weekofmonth) %>%
      summarise(Visits = n(), .groups = 'drop') %>%
      pivot_wider(
        names_from = year, 
        values_from = Visits,
        names_prefix = "Year "
      ) %>%
      mutate(Week = as.integer(weekofmonth)) %>%
      select(Week, starts_with("Year"))
    
# Combine base table with actual data
  base_table %>%
    left_join(actual_data, by = "Week") %>%
    arrange(Week)
}, 
digits = 0,
striped = TRUE,
bordered = TRUE,
align = 'c')
  
  ## ----Visit Analysis Outputs----
  # Visit Distribution plot
  output$repPlot <- renderPlot({
    req(visit_analysis_data())
    clean <- visit_analysis_data()
    
    title_text <- if(input$pg3.visitTimeframeSelect == "year") {
      paste("Visit Distribution -", input$pg3.yearSelect_visits)
    } else {
      paste("Visit Distribution -", input$pg3.termSelect_visits)
    }
    
    clean %>%
      group_by(email_address) %>%
      tally() %>%
      select(n) %>%
      plot_frq(ylim = c(0, NROW(clean)),
               geom.colors = "#00944d") +
      
      xlab("Number of visits") +
      ylab("Count") +
      ggtitle(title_text) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 14)
      ) +
      scale_color_manual(values = "black") 
  })
  
  # Appointment Types plot
  output$typePlot <- renderPlot({
    req(visit_analysis_data())
    clean <- visit_analysis_data()
    
    title_text <- if(input$pg3.visitTimeframeSelect == "year") {
      paste("Appointment Types -", input$pg3.yearSelect_visits)
    } else {
      paste("Appointment Types -", input$pg3.termSelect_visits)
    }
    
    plot_frq(clean$zoom_appoint_y_n,
             geom.colors = "#00944d") + 
      xlab("Type of Appointment") +
      ylab("Count") +
      ggtitle(title_text) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
  })
  
  # Term Visits plot
  output$termPlot <- renderPlot({
    req(visit_analysis_data())
    clean <- visit_analysis_data()
    
    title_text <- if(input$pg3.visitTimeframeSelect == "year") {
      paste("Term Visits -", input$pg3.yearSelect_visits)
    } else {
      paste("Term Visits -", input$pg3.termSelect_visits)
    }
    
    plot_frq(clean$term,
             geom.colors = "#00944d") + 
      xlab("School Term") +
      ylab("Count") +
      ggtitle(title_text) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
  })
  
  # Break Period Visits plot
  output$breakPlot <- renderPlot({
    req(visit_analysis_data())
    clean <- visit_analysis_data()
    
    title_text <- if(input$pg3.visitTimeframeSelect == "year") {
      paste("Break Period Visits -", input$pg3.yearSelect_visits)
    } else {
      paste("Break Period Visits -", input$pg3.termSelect_visits)
    }
    
    plot_frq(clean$school_break,
             geom.colors = "#00944d") +  
      xlab("School Break") +
      ylab("Count") +
      ggtitle(title_text) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 15),
        axis.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
  })
  
  # ---- P4: How did you hear about us (tab) ----
  # Updated reactive dataset for "How did you hear" that handles both year and term views
  how_did_data <- reactive({
    if (input$pg4.howTimeframeSelect == "year") {
      req(input$pg4.yearSelect_how)
      # Filter by academic year
      newdata() %>%
        filter(year == input$pg4.yearSelect_how)
    } else {
      req(input$pg4.termSelect_how)
      # Filter by selected term - changed to single term selection
      newdata() %>%
        filter(term == input$pg4.termSelect_how)
    }
  })
  
  # Create a reactive for getting consistent factor levels across all terms
  all_responses <- reactive({
    how_did_data() %>% 
      select(how_did_you_hear_about_the_cal_fresh_office) %>% 
      separate_longer_delim(cols = how_did_you_hear_about_the_cal_fresh_office, 
                            delim = ",") %>% 
      mutate(how_did_you_hear_about_the_cal_fresh_office = trimws(how_did_you_hear_about_the_cal_fresh_office)) %>%
      filter(!is.na(how_did_you_hear_about_the_cal_fresh_office) & 
               how_did_you_hear_about_the_cal_fresh_office != "") %>%
      group_by(how_did_you_hear_about_the_cal_fresh_office) %>%
      summarize(n = n()) %>%
      arrange(desc(n)) %>%
      slice(1:8) %>%
      pull(how_did_you_hear_about_the_cal_fresh_office)
  })
  
  how_heard <- reactive({
    # First, clean and split the data
    split_data <- how_did_data() %>% 
      select(how_did_you_hear_about_the_cal_fresh_office) %>% 
      separate_longer_delim(cols = how_did_you_hear_about_the_cal_fresh_office, 
                            delim = ",") %>% 
      mutate(how_did_you_hear_about_the_cal_fresh_office = trimws(how_did_you_hear_about_the_cal_fresh_office)) %>%
      filter(!is.na(how_did_you_hear_about_the_cal_fresh_office) & 
               how_did_you_hear_about_the_cal_fresh_office != "") %>%
      filter(how_did_you_hear_about_the_cal_fresh_office %in% all_responses())
    
    # Calculate total number of respondents (not responses)
    total_respondents <- how_did_data() %>% 
      filter(!is.na(how_did_you_hear_about_the_cal_fresh_office)) %>%
      nrow()
    
    # Calculate frequencies and percentages
    split_data %>%
      group_by(how_did_you_hear_about_the_cal_fresh_office) %>% 
      summarize(
        n = n(),
        pct = n/total_respondents,
        lab = paste0(n, " (", scales::percent(pct, accuracy = 0.1), ")")
      ) %>%
      mutate(how_did_you_hear_about_the_cal_fresh_office = 
               factor(how_did_you_hear_about_the_cal_fresh_office, 
                      levels = all_responses()))
  })
  
  # How did you hear about us (plot)
  output$hearAboutPlot <- renderPlot({
    data <- how_heard()
    
    # Create title based on view mode
    if (input$pg4.howTimeframeSelect == "year") {
      subtitle <- paste("Academic Year:", input$pg4.yearSelect_how)
    } else {
      subtitle <- paste("Term:", input$pg4.termSelect_how)
    }
    
    ggplot(data, 
           aes(x = reorder(how_did_you_hear_about_the_cal_fresh_office, n),
               y = n,
               fill = n)) +  # Using n for continuous fill scale
      geom_bar(stat = "identity",
               color = "black",
               size = .2,
               alpha = 0.8) + 
      geom_text(aes(label = lab), 
                hjust = -0.1,  # Adjusted for horizontal bars 
                size = 4) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +  # More expansion for text labels
      scale_fill_continuous(high = "#00944d", low = "#B3E6CF") +  # Grayscale palette
      labs(
        title = "How Students Heard About Us",
        subtitle = paste0(subtitle, "\nNote: Percentages may sum to more than 100% as students could select multiple options"),
        x = NULL,
        y = "Number of Responses"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12, color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10, color = "grey40")
      ) + 
      guides(fill = "none") +  # Remove legend
      coord_flip()  # Flip coordinates for horizontal bars
  })
  
  # Data table for "How did you hear about us"
  output$hearAboutTable <- renderDT({
    req(input$pg4.showDataTable == TRUE) # Only render when checkbox is checked
    
    data <- how_heard() %>%
      # Maintain the same order as the plot (already ordered by count desc)
      arrange(desc(n)) %>%
      select(
        Source = how_did_you_hear_about_the_cal_fresh_office,
        Count = n,
        Percentage = pct
      )
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20),
        scrollX = TRUE,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        order = list(list(1, 'desc')), # Order by Count (column 1) in descending order
        columnDefs = list(list(
          targets = 0, # Source column
          orderable = FALSE # Disable ordering to preserve our factor order
        ))
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'cell-border stripe'
    ) %>%
      formatPercentage('Percentage', digits = 1)
  })
  
  
  # ---- P5: Y-o-Y Comparison ----
  # Create aggregated data for comparison
  comparison_data <- reactive({
    req(input$groupBy)
    
    # Get the data from your existing reactive
    data <- newdata()
    
    # Add academic year column for easier comparison
    # data <- data %>%
    #   mutate(academic_year = case_when(
    #     year(timestamp) == 2024 ~ "2023-2024",
    #     year(timestamp) == 2025 ~ "2024-2025",
    #     TRUE ~ "Other"
    #   ))
    
    # Add filter for month when week of month is selected
    if (input$groupBy == "weekofmonth_des" && !is.null(input$selected_month_comparison)) {
      data <- data %>% filter(monthname == input$selected_month_comparison)
    }
    
    # Handle secondary grouping if selected
    if (input$secondaryGroupBy != "none") {
      group_vars <- c(input$groupBy, input$secondaryGroupBy, "academic_year")
    } else {
      group_vars <- c(input$groupBy, "academic_year")
    }
    
    # Perform aggregation based on user selection
    if (input$aggregateBy == "count") {
      agg_data <- data %>%
        group_by(across(all_of(group_vars))) %>%
        summarize(value = n(), .groups = "drop")
    } else if (input$aggregateBy == "email_address") {
      agg_data <- data %>%
        group_by(across(all_of(group_vars))) %>%
        summarize(value = n_distinct(email_address), .groups = "drop")
    }
    
    # Create a comprehensive dataset with years side by side for comparison
    if (input$secondaryGroupBy != "none") {
      result <- agg_data %>%
        pivot_wider(
          id_cols = c(input$groupBy, input$secondaryGroupBy),
          names_from = academic_year,
          values_from = value
        ) %>%
        mutate(
          # Calculate year-over-year change
          YoY_Change = `2024-2025` - `2023-2024`,
          YoY_Percent = ifelse(is.na(`2023-2024`) | `2023-2024` == 0, 
                               NA, 
                               round((`2024-2025` - `2023-2024`) / `2023-2024` * 100, 1))
        )
    } else {
      result <- agg_data %>%
        pivot_wider(
          id_cols = input$groupBy,
          names_from = academic_year,
          values_from = value
        ) %>%
        mutate(
          # Calculate year-over-year change
          YoY_Change = `2024-2025` - `2023-2024`,
          YoY_Percent = ifelse(is.na(`2023-2024`) | `2023-2024` == 0, 
                               NA, 
                               round((`2024-2025` - `2023-2024`) / `2023-2024` * 100, 1))
        )
    }
    
    # Replace NAs with 0s for plotting
    result <- result %>%
      mutate(across(c(`2023-2024`, `2024-2025`), ~ifelse(is.na(.), 0, .)))
    
    return(result)
  })
  
  # Generate the comparison plot
  output$comparisonPlot <- renderPlot({
    req(comparison_data())
    
    plot_data <- comparison_data()
    
    # Define custom colors
    custom_colors <- c("2023-2024" = "gray", "2024-2025" = "#00944d")
    
    # Reshape data for plotting
    if (input$secondaryGroupBy != "none") {
      plot_long <- plot_data %>%
        pivot_longer(
          cols = c(`2023-2024`, `2024-2025`),
          names_to = "Year",
          values_to = "Count"
        ) %>%
        filter(!is.na(!!sym(input$secondaryGroupBy)))  # Remove NA
      
      p <- ggplot(plot_long, aes_string(x = input$groupBy, y = "Count", fill = "Year")) +
        geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.2) +
        facet_wrap(as.formula(paste("~", input$secondaryGroupBy)), scales = "free_y") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = custom_colors) +
        labs(title = paste("Comparison by", input$groupBy, "and", input$secondaryGroupBy),
             y = if(input$aggregateBy == "count") "Number of Sign-ins" else "Number of Unique Emails",
             fill = "Academic Year")
    } else {
      plot_long <- plot_data %>%
        pivot_longer(
          cols = c(`2023-2024`, `2024-2025`),
          names_to = "Year",
          values_to = "Count"
        )
      
      p <- ggplot(plot_long, aes_string(x = input$groupBy, y = "Count", fill = "Year")) +
        geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.2) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = custom_colors) +
        labs(title = paste("Comparison by", input$groupBy),
             y = if(input$aggregateBy == "count") "Number of Sign-ins" else "Number of Unique Emails",
             fill = "Academic Year")
    }
    
    p
  }, height = "auto", width = "auto")
  
  # Generate the data table
  output$comparisonTable <- renderDT({
    req(comparison_data())
    
    # Format the table with appropriate columns and styling
    datatable(comparison_data(),
              options = list(pageLength = 10, 
                             scrollX = TRUE,
                             dom = 'Blfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf')),
              rownames = FALSE) %>%
      formatStyle(
        c('YoY_Change', 'YoY_Percent'),
        color = styleInterval(0, c('red', 'green'))
      )
  })
  
  # Generate summary text
  output$yearSummary <- renderText({
    req(comparison_data())
    
    data <- comparison_data()
    
    # Calculate total sign-ins for each year
    total_2024 <- sum(data$`2023-2024`, na.rm = TRUE)
    total_2025 <- sum(data$`2024-2025`, na.rm = TRUE)
    
    paste0("Total for 2023-2024: ", total_2024, ", Total for 2024-2025: ", total_2025, 
           " (Difference: ", total_2025 - total_2024, ")")
  })
  
  output$percentChange <- renderText({
    req(comparison_data())
    
    data <- comparison_data()
    
    # Calculate total sign-ins for each year and percent change
    total_2024 <- sum(data$`2023-2024`, na.rm = TRUE)
    total_2025 <- sum(data$`2024-2025`, na.rm = TRUE)
    
    if (total_2024 > 0) {
      percent_change <- round((total_2025 - total_2024) / total_2024 * 100, 1)
      
      if (percent_change > 0) {
        paste0("Year-over-year increase: ", percent_change, "%")
      } else if (percent_change < 0) {
        paste0("Year-over-year decrease: ", abs(percent_change), "%")
      } else {
        "No year-over-year change (0%)"
      }
    } else {
      "Cannot calculate percentage change (no data for 2023-2024)"
    }
  })
  
  # Download handler for the comparison data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("year-comparison-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(comparison_data(), file, row.names = FALSE)
    }
  )
  
}