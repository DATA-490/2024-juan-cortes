# global.R

# INSTALL DEPENDENCIES ----------------------------------------------------

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

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

