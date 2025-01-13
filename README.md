# NISRA Release Calendar

This project contains scripts to scrape, process, and generate a JSON file (`nisra_release_calendar.json`) containing details about recently published and upcoming statistical publications by the Northern Ireland Statistics and Research Agency (NISRA).

## **Scripts Overview**
1. **`latest_NISRA_pubs_from_gov_uk.R`**
   - Scrapes the RSS feed for recently published statistics on gov.uk.
   - Extracts metadata (titles, summaries, URLs, and publication dates) for recent NISRA publications.

2. **`upcoming_NISRA_pubs_from_gov_uk.R`**
   - Scrapes the RSS feed for upcoming NISRA publications.
   - Extracts metadata about upcoming releases, including their release dates and statuses.

3. **`create_json.R`**
   - Orchestrates the execution of the above scripts.
   - Combines data into a JSON file (`nisra_release_calendar.json`).
   - **Instruction**: Open and run `create_json.R` to generate or update the JSON file.

## **Automation with GitHub Actions**
This project includes a GitHub Actions workflow file named `update.yml`. The workflow:
- Runs on pushes to the `main` branch and on a schedule (weekdays at 09:30 and 17:30 UTC).
- Automatically:
  - Sets up the R environment.
  - Installs required R packages.
  - Executes the `create_json.R` script to scrape and update the JSON file.
  - Commits and pushes the updated file to the repository.

## **How to Use**
1. Ensure all dependencies (`xml2`, `rvest`, `dplyr`, `lubridate`, `jsonlite`) are installed in your R environment.
2. Run the `create_json.R` script to generate the `nisra_release_calendar.json` file.
3. Optionally, rely on GitHub Actions to automate updates via the `update.yml` workflow.

This setup ensures a regularly updated and accurate release calendar for NISRA publications.
