# dependencies.R

# LIST OF REQUIRED PACKAGES -----------------------------------------------
required_packages <- c(
  "shiny",
  "bslib",
  "gridlayout",
  "googlesheets4",
  "tidyverse",
  "DT",
  "sjPlot",
  "gtsummary",
  "lubridate",
  "sjmisc",
  "reactable"
)

# Install missing packages ------------------------------------------------
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# Load packages -----------------------------------------------------------
library(shiny)
library(bslib)
library(gridlayout)
library(googlesheets4)
library(tidyverse)
library(DT)
library(sjPlot)
library(gtsummary)
library(lubridate)
library(sjmisc)
library(reactable)
