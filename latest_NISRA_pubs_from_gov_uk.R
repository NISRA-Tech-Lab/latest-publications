# Code Explanation:
#   Libraries Loaded:
#   
#   xml2: Provides functions for parsing XML and HTML documents.
# rvest: A package for web scraping and extracting data from HTML documents.
# HTML Structure:
#   
#   The code constructs an HTML document with a list of publications, beginning with the document type declaration and HTML tags.
# Looping Through Pages:
#   
#   The outer loop (for (i in 1:5)) iterates through the first five pages of the RSS feed.
# Fetching RSS Feed:
#   
#   The RSS URL is constructed with pagination, and the content is read and parsed.
# Extracting Publications:
#   
#   Each publication's details are extracted by iterating through the entry nodes.
# Metadata Extraction:
# 
# For each publication, the link to the detailed page is retrieved, and the code looks for metadata within <p> tags to get publication links.
# Date and Time Formatting:
# 
# The code extracts the publication's update time and formats it into a human-readable date and time format.
# HTML Output Construction:
#   
#   The publication titles and their respective links, along with the formatted date and time, are appended to the output HTML structure.
# Writing to File:
#   
#   Finally, the constructed HTML output is saved to a file named latest_publications.html.
# 
# Load necessary libraries

library(xml2)  # For parsing XML and HTML documents
library(rvest) # For web scraping and extracting data from HTML documents

# Initialise the HTML output
output_html <- c('<!DOCTYPE html>',
                 '<html lang="en">',
                 '<body>',
                 '<ul>')  # Start of an unordered list in HTML

# Loop through the first 5 pages of the RSS feed
for (i in 1:5) {
  
  # Construct the RSS feed URL for the specific page of statistics
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=statistics_published&organisations%5B%5D=northern-ireland-statistics-and-research-agency&order=updated-newest&page=", i)
  
  # Read the RSS feed HTML content
  rss_feed <- read_html(rss_url)
  
  # Extract the 'entry' nodes, which contain publication details
  publications <- html_nodes(rss_feed, "entry")
  
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
    
    # Construct the HTML list item with the publication title and link
    final_link <- paste0('<li><a href = "', pub_link, '">', html_text(html_nodes(publications[j], "title")),'</a>')
    
    # Extract the updated date from the publication entry
    updated <- html_text(html_nodes(publications[j], "updated"))
    
    # Format the date for display
    date <- paste(substr(updated, 9, 10), month.name[as.numeric(substr(updated, 6, 7))], substr(updated, 1, 4))
    
    # Extract and format the time from the updated field
    hour <- as.numeric(substr(updated, 12, 13))
    minute <- substr(updated, 15, 16)
    
    # Format the time to am/pm notation
    time <- if (hour < 12) {
      paste0(hour, ":", minute, "am")
    } else {
      paste0(hour, ":", minute, "pm")
    }
    
    # Create the metadata string containing the release date and publication information
    meta_data <- paste('<div><strong>Release date:</strong>', date, time, '| Published </div></li>')
    
    # Append the constructed list item and metadata to the output HTML
    output_html <- c(output_html, final_link, meta_data)
    
  }
}

# Finalise the HTML output by closing the unordered list and body tags
output_html <- c(output_html,
                 '</ul>',
                 '</body>',
                 '</html>')

# Write the output HTML to a file named 'latest_publications.html'
writeLines(output_html, "latest_publications.html")
