library(xml2)
library(rvest)

output_html <- c('<!DOCTYPE html>',
                 '<html lang="en">',
                 '<body>',
                 '<ul>')

for (i in 1:5) {
  
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=statistics_published&organisations%5B%5D=northern-ireland-statistics-and-research-agency&order=updated-newest&page=", i)
  
  rss_feed <- read_html(rss_url)
  
  publications <- html_nodes(rss_feed, "entry")
  
  for (j in 1:length(publications)) {
    
    gov_uk_link <- html_attr(html_nodes(publications[j], "link"), "href")
    
    gov_uk_page <- read_html(gov_uk_link)
    
    p_tags <- html_nodes(gov_uk_page, "p")
    
    for (k in 1:length(p_tags)) {
      
      class <- html_attr(p_tags[k], "class")
      
      if (class == "gem-c-attachment__metadata" & !is.na(class)) {
        if (grepl("http", html_text(p_tags[k]))) {
          pub_link <- html_text(p_tags[k])
          break
        } else {
          pub_link <- gov_uk_link
        }
        
      }
      
    }
    
    final_link <- paste0('<li><a href = "', pub_link, '">', html_text(html_nodes(publications[j], "title")),'</a>')
    
    updated <- html_text(html_nodes(publications[j], "updated"))
    
    date <- paste(substr(updated, 9, 10), month.name[as.numeric(substr(updated, 6, 7))], substr(updated, 1, 4))
    
    hour <- as.numeric(substr(updated, 12, 13))
    minute <- substr(updated, 15, 16)
    
    time <- if (hour < 12) {
      paste0(hour, ":", minute, "am")
    } else {
      paste0(hour, ":", minute, "pm")
    }
    
    meta_data <- paste('<div><strong>Release date:</strong>', date, time, '| Published </div></li>')
    
    output_html <- c(output_html, final_link, meta_data)
    
  }
  
}

output_html <- c(output_html,
                 '</ul>',
                 '</body>',
                 '</hmtl')

writeLines(output_html, "latest_publications.html")
