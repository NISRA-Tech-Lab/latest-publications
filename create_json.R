# Code explanation
# This script orchestrates the execution of two source scripts (latest_NISRA_pubs_from_gov_uk.R and upcoming_NISRA_pubs_from_gov_uk.R) to generate a JSON file containing information about both recently published and upcoming publications from the Northern Ireland Statistics and Research Agency (NISRA). Here's what it does:
# 
# 1. Load Libraries
# Loads R libraries for:
#   Parsing XML and HTML (xml2, rvest).
# Data manipulation (dplyr).
# Working with dates (lubridate).
# Converting data to JSON (jsonlite).
# 
# 2. Initialize Output List
# Creates an empty list (output_list) with:
#   name: A description of the calendar.
# modified: The current timestamp in ISO 8601 format.
# entries: An empty collection that will hold data about NISRA publications.
# 
# 3. Source External Scripts
# latest_NISRA_pubs_from_gov_uk.R:
#   
#   Processes the RSS feed for recently published statistics.
# Extracts and appends details (e.g., titles, summaries, URLs, and publication dates) to output_list$entries.
# upcoming_NISRA_pubs_from_gov_uk.R:
#   
#   Processes the RSS feed for upcoming statistics.
# Extracts and appends similar details for upcoming publications to output_list$entries.
# 
# 4. Convert to JSON
# Converts the output_list into a JSON format:
#   Uses auto_unbox = TRUE to simplify the JSON structure.
# Applies prettify() for better readability.
# 
# 5. Save as File
# Writes the formatted JSON data to a file named nisra_release_calendar.json.

library(xml2)  # For parsing XML and HTML documents
library(rvest) # For web scraping and extracting data from HTML documents
library(jsonlite) # For Converting data to JSON
library(dplyr)     # For data manipulation and transformation
library(lubridate) # For working with date-time data
library(textutils)
library(stringr)

# List that will be converted to json at the end
output_list <- list(name = "nisra release calendar",
                    modified = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ"),
                    entries = c(),
                    cancelled = c())

org_names <- list(
  "Department for Communities (Northern Ireland)" = "DfC",
  "Department for Infrastructure (Northern Ireland)" = "DfI",
  "Department for the Economy (Northern Ireland)" = "DfE",
  "Department of Agriculture, Environment and Rural Affairs (Northern Ireland)" = "DAERA",
  "Department of Education (Northern Ireland)" = "DE",
  "Department of Finance (Northern Ireland)" = "DoF",
  "Department of Health (Northern Ireland)" = "DoH",
  "Department of Justice (Northern Ireland)" = "DoJ",
  "HSC Business Services Organisation (Northern Ireland)" = "BSO",
  "Invest Northern Ireland" = "INI",
  "Legal Services Agency (Northern Ireland)" = "LSA",
  "Northern Ireland Policing Board" = "NIPB",
  "Northern Ireland Courts and Tribunals Service" = "NICTS",
  "Northern Ireland Executive" = "NIE",
  "Northern Ireland Prison Service" = "NIPS",
  "Northern Ireland Statistics and Research Agency" = "NISRA",
  "Office of the Police Ombudsman for Northern Ireland" = "OPONI",
  "Police Service of Northern Ireland" = "PSNI",
  "Probation Board for Northern Ireland" = "PBNI",
  "Public Prosecution Service for Northern Ireland" = "PPS",
  "The Executive Office (Northern Ireland)" = "TEO",
  "Youth Justice Agency of Northern Ireland" = "YJA"
)

release_types <- list(
  "Research" = "R",
  "Official Statistics" = "OS",
  "Official statistics" = "OS",
  "Accredited official statistics" = "AOS"
)

source("latest_NISRA_pubs_from_gov_uk.R")
source("upcoming_NISRA_pubs_from_gov_uk.R")
source("research_NISRA_pubs_from_gov_uk.R")
source("cancelled_NISRA_pubs_from_gov_uk.R")

toJSON(output_list, auto_unbox = TRUE) %>%
  prettify() %>%
  writeLines("nisra_release_calendar.json")