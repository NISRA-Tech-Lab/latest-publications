# Code Explanation:
# This R script automates the process of retrieving, parsing, and organizing metadata from an RSS feed for upcoming statistics published by the Northern Ireland Statistics and Research Agency. Here’s a summary of what the code does:
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
# 3. Parse Metadata
# From the detailed publication webpage:
# Extracts metadata values, focusing on publication statuses (e.g., "confirmed," "provisional," or "delayed").
# Processes the updated date and the publication id.
# 
# 4. Store Extracted Information
# Constructs a data frame (pub_info) to store:
# Title
# ID
# Metadata
# Updated timestamp
# Summary
# Filters out cancelled publications and keeps only those with valid release dates or statuses.
# 
# 5. Process Metadata and Release Dates
# The metadata is parsed to extract:
# Release Day, Month, and Year:
# Handles edge cases like missing day values, leap years, and non-date strings.
# Status:
# If the release date is in the past, marks the status as delayed.
# Constructs a proper release_date in ISO 8601 format (YYYY-MM-DDT09:30:00Z) for sorting and further processing.
# 
# 6. Build Output Data Structure
# For each row in the processed pub_info data frame:
# Creates an entry in output_list$entries containing:
# ID, Title, Summary, Release Date, and Updated timestamp.
# Entries are sorted by release_date.
# 
# 7. Output Result
# The script produces a structured list (output_list$entries) with metadata and release information for the upcoming publications, suitable for further use, such as generating reports or API responses.

# Initialise pub_info data_frame
pub_info <- data.frame()

# Loop through all pages of the RSS feed for upcoming statistics using a while loop until a page is returned containing no publications
i <- 1
has_pubs <- TRUE
while (has_pubs == TRUE) {
  
  # Construct the RSS feed URL for the specified page number
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=upcoming_statistics&organisations%5B%5D=northern-ireland-statistics-and-research-agency&page=", i)
  
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
    
    # Append the extracted title and metadata to the pub_info data frame
    if (length(meta_data > 0)) {
      pub_info <- pub_info %>%
        bind_rows(data.frame(pub_title = pub_title,
                             id = id,
                             meta_data = trimws(meta_data),
                             updated = updated,
                             summary = trimws(html_text(html_nodes(publications[j], "summary"))),
                             release_type = release_type,
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
    release_date = format(release_date, format = "%Y-%m-%dT09:30:00Z")
  ) %>%
  arrange(release_date) # Sort data frame by release date

# Loop through each row of the processed pub_info data frame
for (i in 1:nrow(pub_info)) {
  
  output_list$entries[[length(output_list$entries) + 1]] <-
    list(id = pub_info$id[i],
         title = pub_info$pub_title[i],
         summary = paste0("Status: ", pub_info$status[i], ". ", HTMLdecode(pub_info$summary[i])),
         release_date = pub_info$release_date[i],
         # updated = pub_info$updated[i],
         updated = format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ"),
         release_type = pub_info$release_type[i],
         status = pub_info$status[i])
  
}
