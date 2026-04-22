upcoming_research <- read.csv("upcoming-research.csv") %>% 
  mutate(url = case_when(url == "" ~ NA,
                         TRUE ~ url))


for (i in 1:nrow(upcoming_research)) {
  
  entry <- upcoming_research[i,]
  
  id <- entry$title %>% 
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", "") %>%
    str_replace_all(" +", "-")
  
  new_output_index <- length(output_list$entries) + 1

  
  output_list$entries[[new_output_index]] <-
    list(id = id,
         title = entry$title,
         summary = entry$summary,
         release_date = entry$release_date,
         display_date = paste0(entry$display_date, " (", entry$status, ")"),
         updated = entry$updated,
         org = entry$org,
         type = "R",
         status = entry$status)
  
  if (!is.na(entry$url)) {
    output_list$entries[[new_output_index]]$url <- entry$url
  }
  
}
