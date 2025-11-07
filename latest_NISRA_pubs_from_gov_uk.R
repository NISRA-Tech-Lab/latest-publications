# Code Explanation:
# This R script is designed to scrape publication details from a statistics RSS feed on the UK government's website, process the information, and construct a structured JSON-like output. Here's a breakdown of the steps:
#   
# 1. Loop Through RSS Feed Pages
# The script loops through the first 5 pages of the RSS feed.
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
    
    # Loop through dd tags to find specific metadata
    dd_tags <- html_nodes(gov_uk_page, "dd")
    
    # Look for where a month name appears in dd tag and re-format date
    for (k in 1:length(dd_tags)) {
      if (grepl(paste(month.name, collapse = "|"), html_text(dd_tags[k]))) {
        display_date <- trimws(html_text(dd_tags[k]))
        release_date <- display_date %>% 
          as.Date(., "%d %B %Y") %>% 
          format(., format = "%Y-%m-%dT09:30:00Z")
        break
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
    
    type_lookup <- c(
      "Research" = "R",
      "Official Statistics" = "OS",
      "Official statistics" = "OS",
      "Accredited official statistics" = "AOS"
    )
    
    release_type <- if (release_type %in% names(type_lookup)) unname(type_lookup[release_type]) else release_type
    
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
    
    # Extract the updated date from the publication entry
    updated <- html_text(html_nodes(publications[j], "updated")) %>% 
      sub("\\+00:00", "Z", .) %>% 
      sub("\\+01:00", "Z", .)
    
    id <- sub(".*/", "", html_text(html_nodes(publications[j], "id")))
    
    # Create list for json output
    output_list$entries[[length(output_list$entries) + 1]] <-
      list(id = id,
           title = html_text(html_nodes(publications[j], "title")),
           summary = HTMLdecode(html_text(html_nodes(publications[j], "summary"))),
           url = pub_link,
           release_date = release_date,
           display_date = display_date,
           org = organisation,
           updated = updated,
           type = release_type)
    
  }
}

