library(xml2)
library(rvest)
library(dplyr)
library(lubridate)


pub_info <- data.frame(pub_title = character(),
                       meta_data = character())


for (i in 1:5) {
  
  rss_url <- paste0("https://www.gov.uk/search/research-and-statistics.atom?content_store_document_type=upcoming_statistics&organisations%5B%5D=northern-ireland-statistics-and-research-agency&page=", i)
  
  rss_feed <- read_html(rss_url)
  
  publications <- html_nodes(rss_feed, "entry")
  
  
  for (j in 1:length(publications)) {
    
    pub_title <- html_text(html_nodes(publications[j], "title"))
    
    gov_uk_link <- html_attr(html_nodes(publications[j], "link"), "href")
    
    gov_uk_page <- read_html(gov_uk_link)
    
    meta_data <- html_text(html_nodes(gov_uk_page, "dd"))[html_attr(html_nodes(gov_uk_page, "dd"), "class") == "app-c-important-metadata__definition"]
    
    pub_info <- pub_info %>%
      rbind(data.frame(pub_title = pub_title,
                       meta_data = meta_data))
    
  }
  
}

pub_info <- pub_info %>%
  mutate(release_day = as.numeric(substr(meta_data, 1, 2)),
         release_month = case_when(is.na(release_day) ~ sub(" .*", "", meta_data),
                                   TRUE ~ sub("^\\s*(\\S+\\s+\\S+).*", "\\1", meta_data) %>%
                                     sub(".* ", "", .)),
         release_year = case_when(grepl(year(today()), meta_data) ~ year(today()),
                                  grepl(year(today()) + 1, meta_data) ~ year(today()) + 1,
                                  grepl(year(today()) + 2, meta_data) ~ year(today()) + 2),
         release_month_numeric = match(release_month, month.name),
         release_day_fixed = case_when(is.na(release_day) & release_month_numeric %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31,
                                       is.na(release_day) & release_month_numeric %in% c(4, 6, 9, 11) ~ 30,
                                       is.na(release_day) & release_month_numeric == 2 ~ 28,
                                       TRUE ~ release_day),
         release_date = as.Date(paste(release_year, release_month_numeric, release_day_fixed, sep = "-")),
         meta_data = case_when(release_date < today() ~ paste(meta_data, "(delayed)"),
                               TRUE ~ meta_data)) %>%
  arrange(release_date)

output_html <- c('<!DOCTYPE html>',
                 '<html lang="en">',
                 '<body>',
                 '<ul>')

for (i in 1:nrow(pub_info)) {
  
  output_html <- c(output_html,
                   paste0("<li>", pub_info$pub_title[i],
                    "<div><strong>Release date:</strong> ",
                    pub_info$meta_data[i],
                    "</div></li>"))
  
}

output_html <- c(output_html,
                 '</ul>',
                 '</body>',
                 '</hmtl')

writeLines(output_html, "upcoming_publications.html")
