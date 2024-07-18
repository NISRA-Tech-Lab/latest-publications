library(xml2)
library(rvest)

output_html <- c('<!DOCTYPE html>',
                 '<html lang="en">',
                 '<body>',
                 '<ul>')

for (i in 1:5) {
  
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=upcoming_statistics&organisations%5B%5D=northern-ireland-statistics-and-research-agency&page=", i)
  
  rss_feed <- read_html(rss_url)
  
  publications <- html_nodes(rss_feed, "entry")
  
  
  for (j in 1:length(publications)) {
    
    pub_title <- paste0("<li>", html_text(html_nodes(publications[j], "title")))
    
    gov_uk_link <- html_attr(html_nodes(publications[j], "link"), "href")
    
    gov_uk_page <- read_html(gov_uk_link)
    
    meta_data <- paste0("<div><strong>Release date:</strong> ",
                        html_text(html_nodes(gov_uk_page, "dd"))[html_attr(html_nodes(gov_uk_page, "dd"), "class") == "app-c-important-metadata__definition"],
                        "</div></li>")
    
    output_html <- c(output_html, pub_title, meta_data)
    
  }
  
}

output_html <- c(output_html,
                 '</ul>',
                 '</body>',
                 '</hmtl')

writeLines(output_html, "upcoming_publications.html")
