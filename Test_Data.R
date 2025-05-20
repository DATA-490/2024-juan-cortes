library(bslib)
library(gridlayout)
library(googlesheets4)
library(tidyverse)
library(DT)
library(sjPlot)
library(gtsummary)
library(rsconnect)

## Go get new data from google
  # Import data
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
           weekofmonth= mday(timestamp - 1) %/% 7 + 1,
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
             TRUE ~ "in_school_term"),
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
           academic_year = case_when(
             year(timestamp) == 2024 ~ "2023-2024",
             year(timestamp) == 2025 ~ "2024-2025",
             TRUE ~ "Other"
           ),
           initial_sar7_or_recert = case_when(
             str_detect(tolower(initial_sar7_or_recert), "initial") ~ "Initial",
             str_detect(tolower(initial_sar7_or_recert), "sar7") ~ "SAR7",
             str_detect(tolower(initial_sar7_or_recert), "recert") ~ "Recert",
             TRUE ~ "Other"),
           how_can_we_assist_you_today = ifelse(how_can_we_assist_you_today == "Yes", "Other", how_can_we_assist_you_today)) %>%
    filter(!is.na(monthname)) |> mutate()


  how_heard <- raw %>% 
    select(how_did_you_hear_about_the_cal_fresh_office) %>% 
    separate_longer_delim(cols = how_did_you_hear_about_the_cal_fresh_office, 
                          delim = ",") %>% 
    na.omit() %>% 
    group_by(how_did_you_hear_about_the_cal_fresh_office) %>% 
    summarize(n=n(), pct = n/NROW(raw), 
              lab = paste0(n, " \n (", scales::percent(pct), ")")) %>% # number of visits, not number of students 
    arrange(desc(n)) %>% 
    slice(1:10)
  
## 
n <- 1200

possible.ids <- expand.grid(LETTERS, letters) |> mutate(id=paste0(Var1, Var2, "@csuchico.edu"))

simulated.data <- data.frame(
  date = sample(raw$timestamp, n, replace=TRUE),
  dayofweek = sample(raw$dayofweek, n, replace=TRUE),
  monthname = sample(raw$monthname, n, replace=TRUE),
  zoom_appoint_y_n = sample(raw$zoom_appoint_y_n, n, replace=TRUE),
  email_address = sample(possible.ids$id, n, replace=TRUE),
  week = sample(raw$week, n, replace=TRUE),
  short.week = sample(raw$short.week, n, replace=TRUE),
  weekofmonth = sample(raw$weekofmonth, n, replace=TRUE),
  year = sample(raw$year, n, replace=TRUE),
  term = sample(raw$term, n, replace=TRUE),
  school_break = sample(raw$school_break, n, replace=TRUE),
  how_did_you_hear_about_the_cal_fresh_office = sample(raw$how_did_you_hear_about_the_cal_fresh_office, n, replace=TRUE),
  how_can_we_assist_you_today = sample(raw$how_can_we_assist_you_today, n, replace=TRUE),
  quarter = sample(raw$quarter, n, replace=TRUE),
  initial_sar7_or_recert = sample(raw$initial_sar7_or_recert, n, replace=TRUE),
  academic_year = sample(raw$academic_year, n, replace=TRUE)
  
  ) %>% 
  mutate(date)

save(simulated.data, file = "Test_Data.RData")
