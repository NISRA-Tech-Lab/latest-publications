# Code Explanation:
###   Library Loading:
      #   xml2: Used for parsing XML and HTML documents.
      # rvest: A package for web scraping and extracting data from HTML.
      # dplyr: Provides functions for data manipulation.
      # lubridate: Facilitates date-time manipulation and processing.

# Data Frame Initialisation:
#   An empty data frame, pub_info, is created to store publication titles and their associated metadata.

# Outer Loop:
#   Loops through the first 5 pages of the RSS feed for upcoming statistics publications.

# RSS Feed URL Construction:
#   Constructs the URL for each page using paste0 to include pagination.

# Fetching RSS Feed:
#   Reads the RSS feed and extracts the entry nodes containing publication details.

# Inner Loop:
#   For each publication entry, it extracts the title and link.
# Reads the linked publication page and extracts the metadata using appropriate class attributes.

# Data Frame Population:
#   The extracted publication title and metadata are appended to pub_info.

# Data Processing:
#   The metadata is processed to extract the release day, month, and year.
# The month is converted from a name to a numeric value.
# The release date is constructed, and metadata is updated to indicate if the publication is delayed.

# Sorting:
#   The pub_info data frame is sorted by the release_date.

# HTML Structure Initialisation:
#   Initialises the structure for the HTML output with a DOCTYPE declaration and basic HTML tags.

# Generating HTML List Items:
#   Loops through the pub_info data frame and constructs list items for each publication with its title and metadata.

# Finalising HTML Output:
#   Closes the unordered list and HTML tags.

# Writing to File:
#   Writes the constructed HTML content to a file named upcoming_publications.html.




# Load necessary libraries
library(xml2)      # For parsing XML and HTML documents
library(rvest)     # For web scraping and extracting data from HTML documents
library(dplyr)     # For data manipulation and transformation
library(lubridate) # For working with date-time data
library(jsonlite)

# Initialise an empty data frame to store publication titles and metadata
pub_info <- data.frame() # Ensure character columns are not converted to factors

# Loop through the first 5 pages of the RSS feed for upcoming statistics
for (i in 1:5) {
  
  # Construct the RSS feed URL for the specified page number
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=upcoming_statistics&organisations%5B%5D=northern-ireland-statistics-and-research-agency&page=", i)
  
  # Read the RSS feed HTML content
  rss_feed <- read_html(rss_url)
  
  # Extract all publication entries from the RSS feed
  publications <- html_nodes(rss_feed, "entry")
  
  # Loop through each publication entry
  for (j in 1:length(publications)) {
    
    # Extract the publication title
    pub_title <- html_text(html_nodes(publications[j], "title"))
    
    # Extract the link to the detailed government publication page
    gov_uk_link <- html_attr(html_nodes(publications[j], "link"), "href")
    
    # Read the content of the government publication page
    gov_uk_page <- read_html(gov_uk_link)
    
    # Extract relevant metadata from the publication page
    meta_data <- html_text(html_nodes(gov_uk_page, "dd"))[html_attr(html_nodes(gov_uk_page, "dd"), "class") == "gem-c-metadata__definition"]
    
    # Remove cancelled publications and only extract meta data item with publication date in it
    meta_data <- meta_data[grepl("confirmed|provisional|delayed", meta_data)]
    
    updated <- html_text(html_nodes(publications[j], "updated")) %>% 
      sub("\\+00:00", "Z", .) %>% 
      sub("\\+01:00", "Z", .)
    
    id <- sub(".*/", "", html_text(html_nodes(publications[j], "id")))
    
    # Append the extracted title and metadata to the pub_info data frame
    if (length(meta_data > 0)) {
      pub_info <- pub_info %>%
        bind_rows(data.frame(pub_title = pub_title,
                             id = id,
                             meta_data = trimws(meta_data),
                             updated = updated,
                             summary = trimws(html_text(html_nodes(publications[j], "summary"))),
                             stringsAsFactors = FALSE)) # Avoid factors in data frame
    }
  }
}

# Process the pub_info data frame to extract release dates and modify metadata
pub_info <- pub_info %>%
  mutate(
    status = sub("\\)", "", sub(".*\\(", "", meta_data)),
    release_day = as.numeric(substr(meta_data, 1, 2)), # Extract day as numeric
    release_month = case_when(
      is.na(release_day) ~ sub(" .*", "", meta_data), # Extract month name if release_day is NA
      TRUE ~ sub("^\\s*(\\S+\\s+\\S+).*", "\\1", meta_data) %>% sub(".* ", "", .) # Otherwise extract month from metadata
    ),
    release_year = case_when(
      grepl(year(today()), meta_data) ~ year(today()),  # Current year if it matches
      grepl(year(today()) + 1, meta_data) ~ year(today()) + 1,  # Next year if it matches
      grepl(year(today()) + 2, meta_data) ~ year(today()) + 2   # Two years ahead if it matches
    ),
    release_month_numeric = match(release_month, month.name), # Convert month name to numeric
    release_day_fixed = case_when(
      is.na(release_day) & release_month_numeric %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31,  # Handle months with 31 days
      is.na(release_day) & release_month_numeric %in% c(4, 6, 9, 11) ~ 30,  # Handle months with 30 days
      is.na(release_day) & release_month_numeric == 2 & release_year %% 4 == 0 ~ 29, # Handle Leap years
      is.na(release_day) & release_month_numeric == 2 ~ 28,  # Handle February
      TRUE ~ release_day # Default to original release_day
    )) %>% 
  filter(release_month %in% month.name) %>% # Remove strings that don't contain a date
  mutate(
    release_date = as.Date(paste(release_year, release_month_numeric, release_day_fixed, sep = "-")), # Construct date
    status = case_when(
      release_date < today() ~ paste(meta_data, "(delayed)"),  # Mark as delayed if release date is past
      TRUE ~ status  # Otherwise keep original metadata
    ),
    release_date = format(release_date, format = "%Y-%m-%dT%09:30:00Z")
  ) %>%
  arrange(release_date) # Sort data frame by release date


output_list <- list(name = "upcoming nisra publications",
                    modified = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ"),
                    entries = c())

# Loop through each row of the processed pub_info data frame
for (i in 1:nrow(pub_info)) {
  
  
  output_list$entries[[length(output_list$entries) + 1]] <-
    list(id = pub_info$id[i],
         title = pub_info$pub_title[i],
         summary = pub_info$summary[i],
         release_date = pub_info$release_date[i],
         updated = pub_info$updated[i])
  
}

# Write out to a json file named 'upcoming_publications.json'
toJSON(output_list, auto_unbox = TRUE) %>% 
  prettify() %>% 
  writeLines("upcoming_publications.json")
