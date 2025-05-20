# global.R

# INSTALL DEPENDENCIES ----------------------------------------------------

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

# Authorize connection to google sheets
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

# UI HELPER FUNCTION -----------------------------------

metric_card <- function(title, value, showcase = NULL, description = NULL, color = "#5e81ac") {
  div(
    class = "p-3 border rounded",
    style = paste0("border-left: 4px solid ", color, " !important;"),
    div(
      class = "d-flex justify-content-between align-items-center",
      div(
        style = "font-size: 1rem; font-weight: 500; color: #495057;",
        title
      ),
      if (!is.null(showcase)) {
        div(
          style = paste0("color: ", color, ";"),
          showcase
        )
      }
    ),
    div(
      style = "font-size: 1.75rem; font-weight: 700; margin-top: 0.5rem; margin-bottom: 0.25rem;",
      value
    ),
    if (!is.null(description)) {
      div(
        style = "font-size: 0.875rem; color: #6c757d;",
        description
      )
    }
  )
}

# Apply ggplot theming to the entire app
theme_set(theme_bw())

# Mapping of values to display labels
timeframe_choices <- c(
  date = "Day",
  short.week = "Week",
  monthname = "Month",
  year = "Academic Year",
  term = "Term"
)

# DATA TRANSFORMATION AND NEW VARIABLES -----------------------------------

# Import data
raw1 <- read_sheet("https://docs.google.com/spreadsheets/d/1hIPP1JIoGmAocGMUoJaFA0YfsvW6teYDN6az7kYgOZY", sheet = "(OLD) AY 2023-2024") %>%
  mutate(location = "Main")

raw2 <- read_sheet("https://docs.google.com/spreadsheets/d/1hIPP1JIoGmAocGMUoJaFA0YfsvW6teYDN6az7kYgOZY", sheet = "MAIN OFFICE AY 2024-2025") %>%
  mutate(location = "Main")

raw3 <- read_sheet("https://docs.google.com/spreadsheets/d/1hIPP1JIoGmAocGMUoJaFA0YfsvW6teYDN6az7kYgOZY", sheet = "OUTREACH AY 2024-2025") %>%
  mutate(location = "Temp")

# combine sheets
raw <- bind_rows(raw1, raw2, raw3)

raw <- raw %>%
  janitor::clean_names() %>%
  select(-c(first_name,last_name,phone_number,
            do_you_want_to_receive_optional_emails_from_our_office_about_cal_fresh_topics)) %>%
  mutate(date= as.Date(timestamp),
         dayofweek= wday(timestamp, label = TRUE, week_start = 1),
         monthname= month(timestamp, label = TRUE),
         monthnumber= month(timestamp),
         week = floor_date(timestamp, unit = "week", week_start= 1),
         short.week = format(week, "%m/%d"),
         time12h= format(timestamp,"%I"),
         time24h= format(timestamp,"%H"),
         weekofmonth = mday(timestamp - 1) %/% 7 + 1,
         weekofmonth_des = paste0(monthname, " Week ", weekofmonth),
         email_address = tolower(email_address),
         zoom_appoint_y_n = ifelse(zoom_appoint_y_n %in% c("Y", "Yes"), "Zoom", "In Office"),
         term = case_when( #earliest timestamp (2/8/2024 12:32:18)
           #timestamp >= as.Date("2023-08-21") & timestamp <= as.Date("2023-12-31") ~ "Fall 2023",
           #timestamp >= as.Date("2024-01-01") & timestamp <= as.Date("2024-01-21") ~ "Winter 2024",
           timestamp >= as.Date("2024-02-08") & timestamp <= as.Date("2024-05-31") ~ "Spring 2024",
           timestamp >= as.Date("2024-06-01") & timestamp <= as.Date("2024-08-25") ~ "Summer 2024",
           timestamp >= as.Date("2024-08-26") & timestamp <= as.Date("2024-12-31") ~ "Fall 2024",
           timestamp >= as.Date("2025-01-01") & timestamp <= as.Date("2025-01-20") ~ "Winter 2025",
           timestamp >= as.Date("2025-01-21") & timestamp <= as.Date("2025-05-31") ~ "Spring 2025",
           TRUE ~ "Unknown"),
         school_break = case_when(
           date >= as.Date("2024-03-18") & date <= as.Date("2024-03-22") ~ "Spring 2024 Break",
           date >= as.Date("2024-11-25") & date <= as.Date("2024-11-29") ~ "Fall 2024 Break",
           date >= as.Date("2025-03-21") & date <= as.Date("2025-03-21") ~ "Spring 2025 Break",
           TRUE ~ "In School Term"),
         year = case_when(
           timestamp >= as.Date("2024-02-08") & timestamp <= as.Date("2024-05-31") ~ "2023-2024",
           timestamp >= as.Date("2024-06-01") & timestamp <= as.Date("2025-05-31") ~ "2024-2025",
           TRUE ~ "Unknown"),
         quarter = case_when(
           quarter(timestamp) == 1 ~ "Q1",
           quarter(timestamp) == 2 ~ "Q2",
           quarter(timestamp) == 3 ~ "Q3",
           quarter(timestamp) == 4 ~ "Q4",
           TRUE ~ "Unknown"
         ),
         initial_sar7_or_recert = case_when(
           str_detect(tolower(initial_sar7_or_recert), "initial") ~ "Initial",
           str_detect(tolower(initial_sar7_or_recert), "sar7") ~ "SAR7",
           str_detect(tolower(initial_sar7_or_recert), "recert") ~ "Recert",
           TRUE ~ "Other"),
         how_can_we_assist_you_today = ifelse(how_can_we_assist_you_today == "Yes", "Other", how_can_we_assist_you_today)) %>%
  filter(!is.na(monthname))
