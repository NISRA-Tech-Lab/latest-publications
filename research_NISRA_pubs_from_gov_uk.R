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
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=research&organisations%5B%5D=northern-ireland-statistics-and-research-agency&order=updated-newest&page=", i)
  
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
        display_date <- html_text(dd_tags[k])
        release_date <- display_date %>% 
          as.Date(., "%d %B %Y") %>% 
          format(., format = "%Y-%m-%dT09:30:00Z")
        break
      }
    }
    
    a_tags <- html_nodes(gov_uk_page, "a")
    
    organisations <- c()
    
    # Lookup table: full names as names, codes as values
    org_lookup <- c(
      "Department for Communities" = "DfC",
      "Department for Infrastructure" = "DfI",
      "Department for the Economy" = "DfE",
      "Environment and Rural Affairs" = "DAERA",
      "Department of Education" = "DE",
      "Department of Finance" = "DoF",
      "Department of Health" = "DoH",
      "Department of Justice" = "DoJ",
      "HSC Business Services Organisation" = "BSO",
      "Invest Northern Ireland" = "INI",
      "Legal Services Agency" = "LSA",
      "Northern Ireland Policing Board" = "NIPB",
      "Northern Ireland Courts and Tribunals Service" = "NICTS",
      "Northern Ireland Executive" = "NIE",
      "Northern Ireland Prison Service" = "NIPS",
      "Northern Ireland Statistics and Research Agency" = "NISRA",
      "Office of the Police Ombudsman for Northern Ireland" = "OPONI",
      "Police Service of Northern Ireland" = "PSNI",
      "Probation Board for Northern Ireland" = "PBNI",
      "Public Prosecution Service for Northern Ireland" = "PPS",
      "The Executive Office" = "TEO",
      "Youth Justice Agency of Northern Ireland" = "YJA"
    )
    
    for (l in 1:length(a_tags)) {
      
      # Get the class attribute of the <a> tag
      class <- html_attr(a_tags[l], "class")
      
      href <- html_attr(a_tags[l], "href")
      
      if (grepl("govuk-link", class) & !is.na(class) & grepl("organisations/", href) & !grepl("Northern Ireland Statistics and Research Agency", html_text(a_tags[l]))) {
        org_name <- html_text(a_tags[l]) %>%
          gsub("\\(Northern Ireland\\)", "", .) %>% 
          trimws()
        
        organisations <- c(organisations, org_name)
        
        organisation <- paste(organisations, collapse = ", ") 
        
        
        if (grepl(",", organisation)) {
          parts <- strsplit(organisation, ",\\s*")[[1]]  # Split on comma and optional space
          
          
          # Identify parts
          non_dept <- parts[!grepl("department", parts, ignore.case = TRUE)]
          dept <- parts[grepl("department", parts, ignore.case = TRUE)]
          
          
          # Choose based on presence in org_lookup
          if (length(non_dept) > 0 && any(non_dept %in% names(org_lookup))) {
            organisation <- non_dept[non_dept %in% names(org_lookup)][1]  # pick first match
          } else if (length(dept) > 0) {
            organisation <- dept[1]  # fallback to department part
          } else {
            organisation <- parts[1]  # fallback to first part if nothing matches
          }
        }
        
        # Replace full name with code if it exists in the lookup
        if (length(organisation) == 0) {
          organisation <- "NISRA"
        }
        if (length(organisation) > 1) {
          organisation <- if (organisation[1] %in% names(org_lookup)) unname(org_lookup[organisation[1]]) else organisation[1]
        } else {
          organisation <- if (organisation %in% names(org_lookup)) unname(org_lookup[organisation]) else organisation
        }
        
      }
    }
    
    
    # Appened to list of cancelled publications
    output_list$entries[[length(output_list$entries) + 1]] <- 
      list(id = id,
           title = html_text(html_nodes(publications[j], "title")),
           summary = HTMLdecode(html_text(html_nodes(publications[j], "summary"))),
           url = pub_link,
           release_date = release_date,
           display_date = display_date,
           org = organisation,
           updated = updated,
           type = "R")
    
  }
}
