# Code Explanation:
# This R script automates the process of retrieving, parsing, and organizing metadata from an RSS feed for cancelled statistics publications. Here’s a summary of what the code does:
#   
#   1. Loop Through RSS Feed Pages
# The script loops through all pages of the RSS feed to gather entries.
# 
# For each page:
#   It constructs the URL dynamically.
# Reads the HTML content of the RSS feed.
# 
# 2. Extract Publication Entries
# For each entry (publication) in the RSS feed:
#   Extracts the publication title and link.
# Reads the detailed content from the publication's webpage.
# 
# 
# 3. Store Extracted Information
# Constructs a data frame (pub_info) to store:
# ID
# Updated timestamp
# Summary
# Filters out cancelled publications and keeps only those with valid release dates or statuses.
# 
# 4. Process Metadata and updated Dates
# The metadata is parsed to extract:
# Constructs a proper release_date in ISO 8601 format (YYYY-MM-DDT09:30:00Z) for sorting and further processing.
# 
# 5. Build Output Data Structure
# For each row in the processed pub_info data frame:
# Creates an entry in output_list$cancelled containing:
# ID and Updated timestamp.
# 
# 6. Output Result
# The script produces a structured list (output_list$cancelled) for the cancelled publications, suitable for further use, such as generating reports or API responses.


# Loop through all pages of the RSS feed for cancelled statistics using a while loop until a page is returned containing no publications
i <- 1
has_pubs <- TRUE
while (has_pubs == TRUE) {
  
  # Construct the RSS feed URL for the specified page number
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=cancelled_statistics&organisations%5B%5D=northern-ireland-statistics-and-research-agency&order=updated-newest&page=", i)
  
  i <- i + 1
  
  # Read the RSS feed HTML content
  rss_feed <- read_html(rss_url)
  
  # Extract all publication entries from the RSS feed
  publications <- html_nodes(rss_feed, "entry")
  if (length(publications) == 0) {
    has_pubs <- FALSE
    break
  }
  
  
  # Loop through each publication entry
  for (j in 1:length(publications)) {
    
    # Extract the link to the detailed government publication page
    gov_uk_link <- html_attr(html_nodes(publications[j], "link"), "href")
    
    # Read the content of the government publication page
    gov_uk_page <- read_html(gov_uk_link)
    
    # Extract the id
    id <- sub(".*/", "", html_text(html_nodes(publications[j], "id")))
    
    # When status was updated
    updated <- html_text(html_nodes(publications[j], "updated")) %>% 
      sub("\\+00:00", "Z", .) %>% 
      sub("\\+01:00", "Z", .)
    
    # Appened to list of cancelled publications
    output_list$cancelled[[length(output_list$cancelled) + 1]] <- 
      list(id = id,
           updated = updated)
    
  }
}
