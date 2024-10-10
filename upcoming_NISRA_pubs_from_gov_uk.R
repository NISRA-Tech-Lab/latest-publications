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

# Initialise an empty data frame to store publication titles and metadata
pub_info <- data.frame(pub_title = character(),
                       meta_data = character(),
                       stringsAsFactors = FALSE) # Ensure character columns are not converted to factors

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
    meta_data <- html_text(html_nodes(gov_uk_page, "dd"))[html_attr(html_nodes(gov_uk_page, "dd"), "class") == "app-c-important-metadata__definition"]
    
    # Append the extracted title and metadata to the pub_info data frame
    pub_info <- pub_info %>%
      rbind(data.frame(pub_title = pub_title,
                       meta_data = meta_data,
                       stringsAsFactors = FALSE)) # Avoid factors in data frame
  }
}

# Process the pub_info data frame to extract release dates and modify metadata
pub_info <- pub_info %>%
  mutate(
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
      is.na(release_day) & release_month_numeric == 2 ~ 28,  # Handle February
      TRUE ~ release_day # Default to original release_day
    ),
    release_date = as.Date(paste(release_year, release_month_numeric, release_day_fixed, sep = "-")), # Construct date
    meta_data = case_when(
      release_date < today() ~ paste(meta_data, "(delayed)"),  # Mark as delayed if release date is past
      TRUE ~ meta_data  # Otherwise keep original metadata
    )
  ) %>%
  arrange(release_date) # Sort data frame by release date

# Initialise the HTML structure for the output
output_html <- c('<!DOCTYPE html>',
                 '<html lang="en">',
                 '<body>',
                 '<ul>')  # Start of an unordered list

# Loop through each row of the processed pub_info data frame
for (i in 1:nrow(pub_info)) {
  
  # Construct the HTML list item for each publication
  output_html <- c(output_html,
                   paste0("<li>", pub_info$pub_title[i],
                          "<div><strong>Release date:</strong> ",
                          pub_info$meta_data[i],
                          "</div></li>"))  # HTML structure for each publication
}

# Finalise the HTML output by closing the unordered list and body tags
output_html <- c(output_html,
                 '</ul>',          # End of unordered list
                 '</body>',       # End of body
                 '</html>')      # End of HTML document

# Write the output HTML to a file named 'upcoming_publications.html'
writeLines(output_html, "upcoming_publications.html")
