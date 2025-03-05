# Code Explanation:
# This R script is designed to scrape publication details from a statistics RSS feed on the UK government's website, process the information, and construct a structured JSON-like output. Here's a breakdown of the steps:
#   
# 1. Loop Through RSS Feed Pages
# The script loops through the first all pages of the RSS feed until it returns a page with no entries.
# For each page:
#   Constructs the URL dynamically using the page number (i).
# Fetches the RSS feed content.
# 
# 2. Extract Publication Entries
# Retrieves all <entry> nodes from the RSS feed, where each node corresponds to a publication.
# Loops through each entry:
#   Extracts the URL of the publication page.
# 
# 3. Process Publication Page
# For each publication page:
#   
#   Extract Paragraph (<p>) Metadata:
#   
#   Searches for <p> tags in the publication page to find links or metadata.
# Looks specifically for tags with the class "gem-c-attachment__metadata":
#   If found and contains an HTTP link, sets it as the pub_link.
# Defaults to the main publication page URL if no specific link is found.
# Extract Date Metadata:
#   
#   Retrieves <dd> tags and searches for text containing month names.
# Converts this text into a standardized ISO 8601 date format (YYYY-MM-DDT09:30:00Z).
# 
# 4. Extract RSS Entry Metadata
# From the RSS entry:
#   
#   Extracts:
#   Title: The title of the publication.
# Summary: A brief summary of the publication.
# Updated Timestamp: The last updated date in the RSS feed, formatted as ISO 8601.
# ID: A unique identifier derived from the <id> tag.
# 
# 5. Build Output List
# Constructs a JSON-like list (output_list$entries):
#   Each entry contains:
#   id: Unique identifier.
# title: Publication title.
# summary: Publication summary.
# url: Either a specific metadata link or the main publication URL.
# release_date: Extracted or formatted release date.
# updated: Last updated timestamp.
# Adds each processed publication as an entry in output_list$entries.

library(xml2)  # For parsing XML and HTML documents
library(rvest) # For web scraping and extracting data from HTML documents
library(jsonlite) # For Converting data to JSON
library(dplyr)     # For data manipulation and transformation

# List that will be converted to json at the end
output_list <- list(name = "nisra release calendar",
                    modified = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ"),
                    entries = c())


# Loop through all pages of the RSS feed
i = 1
has_pubs = TRUE
while (has_pubs == TRUE) {
  
  # Construct the RSS feed URL for the specific page of statistics
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=statistics_published&organisations%5B%5D=northern-ireland-statistics-and-research-agency&order=updated-newest&page=", i)
  
  # Read the RSS feed HTML content
  rss_feed <- read_html(rss_url)
  
  # Extract the 'entry' nodes, which contain publication details
  publications <- html_nodes(rss_feed, "entry")
  if (length(publications) == 0) {
    has_pubs <- FALSE
    break
  }
  
  i <- i + 1
  
  # Loop through each publication entry
  for (j in 1:length(publications)) {
    
    # Extract the link to the government publication page
    gov_uk_link <- html_attr(html_nodes(publications[j], "link"), "href")
    
    # Read the content of the government publication page
    gov_uk_page <- read_html(gov_uk_link)
    
    # Extract all <p> tags from the publication page
    p_tags <- html_nodes(gov_uk_page, "p")
    
    # Initialise pub_link variable
    pub_link <- gov_uk_link  # Default link if no specific link is found
    
    # Loop through each <p> tag to find specific metadata
    for (k in 1:length(p_tags)) {
      
      # Get the class attribute of the <p> tag
      class <- html_attr(p_tags[k], "class")
      
      # Check for the specific class that contains metadata
      if (class == "gem-c-attachment__metadata" & !is.na(class)) {
        # If the <p> contains an HTTP link, use that
        if (grepl("http", html_text(p_tags[k]))) {
          pub_link <- html_text(p_tags[k])
          break  # Exit the loop once a valid link is found
        }
      }
    }
    
    # Loop through dd tags to find specific metadata
    dd_tags <- html_nodes(gov_uk_page, "dd")
    
    # Look for where a month name appears in dd tag and re-format date
    for (k in 1:length(dd_tags)) {
      if (grepl(paste(month.name, collapse = "|"), html_text(dd_tags[k]))) {
        release_date <- html_text(dd_tags[k]) %>% 
          as.Date(., "%d %B %Y") %>% 
          format(., format = "%Y-%m-%dT09:30:00Z")
      }
    }
    
    # Loop through span tags to find specific metadata for release type
    span_tags <- html_nodes(gov_uk_page, "span")
    
    for (k in 1:length(span_tags)) {
      
      # Get the class attribute of the <span> tag
      class <- html_attr(span_tags[k], "class")
      
      if (grepl("govuk-caption-xl", class) & !is.na(class)) {
        release_type <- trimws(html_text(span_tags[k]))
        break # Exit the loop once a valid class is found
      }
    }
    
    # Extract the updated date from the publication entry
    updated <- html_text(html_nodes(publications[j], "updated")) %>% 
      sub("\\+00:00", "Z", .) %>% 
      sub("\\+01:00", "Z", .)
    
    id <- sub(".*/", "", html_text(html_nodes(publications[j], "id")))
    
    # Create list for json output
    output_list$entries[[length(output_list$entries) + 1]] <-
      list(id = id,
           title = html_text(html_nodes(publications[j], "title")),
           summary = html_text(html_nodes(publications[j], "summary")),
           url = pub_link,
           release_date = release_date,
           updated = updated,
           release_type = release_type)
    
  }
}

toJSON(output_list, auto_unbox = TRUE) %>%
  prettify() %>%
  writeLines("nisra_release_historic.json")

