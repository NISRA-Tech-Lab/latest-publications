upcoming_research <- read.csv("upcoming-research.csv")

for (i in 1:nrow(upcoming_research)) {
  
  entry <- upcoming_research[i,]
  
  id <- entry$title %>% 
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", "") %>%
    str_replace_all(" +", "-")

  
  output_list$entries[[length(output_list$entries) + 1]] <-
    list(id = id,
         title = entry$title,
         summary = entry$summary,
         release_date = entry$release_date,
         display_date = entry$display_date,
         updated = entry$updated,
         org = entry$org,
         type = "R",
         status = entry$status)
}
