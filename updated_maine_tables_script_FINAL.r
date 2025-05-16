# Install and load necessary packages
library(dplyr)
library(htmlwidgets)
library(DT)
library(here)
library(htmltools)

## Set working directory
here::here()

# Read population estimates CSV file into R
me_towns <- read.csv("Inputs/PEP_towns_2024.csv", header=TRUE, stringsAsFactors=FALSE)

# First, let's clean up and prepare the me_towns dataset
prepare_town_data <- function(data) {
  # Define columns to remove and rename
  remove_cols <- c("GEOID", "MCDName", "countyFIPS", "AREALAND_SQMI")
  rename_cols <- c(
    "MCDNameSHORT" = "Town",
    "CountyName" = "County",
    "POPESTIMATE" = "2024 Population",
    "PopDen" = "Population density per square mile (2024)",
    "annual_prc_chg" = "Percent Change (2023-2024)",
    "annual_chg" = "Numeric Change (2023-2024)",
    "cuml_prc_chg" = "Cumulative Percent Change (2020-2024)",
    "cuml_chg" = "Cumulative Numeric Change (2020-2024)"
  )
  
  # Drop specified columns
  data <- data[, !(names(data) %in% remove_cols)]
  
  # Rename specified columns
  for (old_name in names(rename_cols)) {
    if (old_name %in% names(data)) {
      names(data)[names(data) == old_name] <- rename_cols[old_name]
    }
  }
  
  return(data)
}

# Clean the data first
me_towns_clean <- prepare_town_data(me_towns)

# Add code to ensure percent values are properly displayed
# If annual_prc_chg, cuml_prc_chg are stored as decimals like 0.0012, convert them to percentages
if ("annual_prc_chg" %in% names(me_towns)) {
  me_towns_clean <- me_towns_clean %>%
    mutate(`Percent Change (2023-2024)` = `Percent Change (2023-2024)` * 100,
           `Cumulative Percent Change (2020-2024)` = `Cumulative Percent Change (2020-2024)` * 100)
}

# 1. Create the percent change table with proper ordering
# Get top 10 towns by percent change
top_10_percent <- me_towns_clean %>%
  arrange(desc(`Percent Change (2023-2024)`)) %>%
  head(10) %>%
  mutate(Category = "Largest Increases") %>%
  # Add a sorting helper column that preserves order within category
  mutate(SortOrder = row_number())

# Get bottom 10 towns by percent change
bottom_10_percent <- me_towns_clean %>%
  arrange(`Percent Change (2023-2024)`) %>%
  head(10) %>%
  mutate(Category = "Largest Declines") %>%
  # Add a sorting helper column with higher values to place them at the bottom
  mutate(SortOrder = row_number() + 100)

# Combine top and bottom towns
percent_change_data <- bind_rows(top_10_percent, bottom_10_percent) %>%
  # Sort by SortOrder to ensure top towns first, then bottom towns
  arrange(SortOrder) %>%
  # Remove the SortOrder column as it's no longer needed
  select(Category, Town, County, `2024 Population`, `Numeric Change (2023-2024)`, `Percent Change (2023-2024)`)

# Create the percent change table - this is Figure 1
percent_change_table <- datatable(
  percent_change_data,
  # caption = "Figure 1: Top and Bottom 10 Towns by Percent Change, 2023-2024",
  caption = htmltools::tags$caption(
    style = "font-size: 12px; font-weight: italic; text-align: left;", "Data Source: U.S. Census Bureau, Population Estimates Program, Vintage 2024 provided by the Maine Office of the State Economist"
  ),
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    dom = 'rt<"bottom"Bp>',
    buttons = list(
      list(extend = 'copy', text = 'Copy'),
      list(extend = 'excel', text = 'Excel', 
           filename = 'Figure_1_Top_Bottom_Towns_by_Percent_Change_2023-2024',
           title = 'Figure 1: Top and Bottom 10 Towns by Percent Change, 2023-2024'),
      list(extend = 'pdf', text = 'PDF', 
           filename = 'Figure_1_Top_Bottom_Towns_by_Percent_Change_2023-2024',
           title = 'Figure 1: Top and Bottom 10 Towns by Percent Change, 2023-2024')
    ),
    pageLength = 20,
    ordering = FALSE  # Disable user sorting to preserve our custom order
  )
)

# Apply formatting separately to avoid errors
percent_change_table <- percent_change_table %>%
  formatCurrency(columns = "2024 Population", currency = "", digits = 0, interval = 3, mark = ",") %>%
  formatCurrency(columns = "Numeric Change (2023-2024)", currency = "", digits = 0, interval = 3, mark = ",") %>%
  formatRound(columns = "Percent Change (2023-2024)", digits = 1) %>%
  # Add color coding based on Category
  formatStyle(
    columns = 'Category',
    target = 'row',
    backgroundColor = styleEqual(
      c("Largest Increases", "Largest Declines"),
      c("#d4edda", "#f8d7da")  # Green for increases, red for declines
    )
  )

# 2. Create the numeric change table with proper ordering
# Get top 10 towns by numeric change
top_10_numeric <- me_towns_clean %>%
  arrange(desc(`Numeric Change (2023-2024)`)) %>%
  head(10) %>%
  mutate(Category = "Largest Increases") %>%
  # Add a sorting helper column that preserves order within category
  mutate(SortOrder = row_number())

# Get bottom 10 towns by numeric change
bottom_10_numeric <- me_towns_clean %>%
  arrange(`Numeric Change (2023-2024)`) %>%
  head(10) %>%
  mutate(Category = "Largest Declines") %>%
  # Add a sorting helper column with higher values to place them at the bottom
  mutate(SortOrder = row_number() + 100)

# Combine top and bottom towns
numeric_change_data <- bind_rows(top_10_numeric, bottom_10_numeric) %>%
  # Sort by SortOrder to ensure top towns first, then bottom towns
  arrange(SortOrder) %>%
  # Remove the SortOrder column as it's no longer needed
  select(Category, Town, County, `2024 Population`, `Numeric Change (2023-2024)`, `Percent Change (2023-2024)`)

# Create the numeric change table - this is Figure 2
numeric_change_table <- datatable(
  numeric_change_data,
  # caption = "Figure 2: Top and Bottom 10 Towns by Numeric Change, 2023-2024",
  caption = htmltools::tags$caption(
    style = "font-size: 12px; font-weight: italic; text-align: left;", "Data Source: U.S. Census Bureau, Population Estimates Program, Vintage 2024 provided by the Maine Office of the State Economist"
  ),
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    dom = 'rt<"bottom"Bp>',
    buttons = list(
      list(extend = 'copy', text = 'Copy'),
      list(extend = 'excel', text = 'Excel', 
           filename = 'Figure_2_Top_Bottom_Towns_by_Numeric_Change_2023-2024',
           title = 'Figure 2: Top and Bottom 10 Towns by Numeric Change, 2023-2024'),
      list(extend = 'pdf', text = 'PDF', 
           filename = 'Figure_2_Top_Bottom_Towns_by_Numeric_Change_2023-2024',
           title = 'Figure 2: Top and Bottom 10 Towns by Numeric Change, 2023-2024')
    ),
    pageLength = 20,
    ordering = FALSE  # Disable user sorting to preserve our custom order
  )
)

# Apply formatting separately to avoid errors
numeric_change_table <- numeric_change_table %>%
  formatCurrency(columns = "2024 Population", currency = "", digits = 0, interval = 3, mark = ",") %>%
  formatCurrency(columns = "Numeric Change (2023-2024)", currency = "", digits = 0, interval = 3, mark = ",") %>%
  formatRound(columns = "Percent Change (2023-2024)", digits = 1) %>%
  # Add color coding based on Category
  formatStyle(
    columns = 'Category',
    target = 'row',
    backgroundColor = styleEqual(
      c("Largest Increases", "Largest Declines"),
      c("#d4edda", "#f8d7da")  # Green for increases, red for declines
    )
  )

# 3. Create the population table with ranking - this is Figure 4
population_data <- me_towns_clean %>%
  arrange(desc(`2024 Population`)) %>%
  head(10) %>%
  mutate(Rank = row_number()) %>%
  # Include all the requested columns
  select(Town, County, `2024 Population`, `Numeric Change (2023-2024)`, `Percent Change (2023-2024)`, 
         `Population density per square mile (2024)`)

# Create the population table
population_table <- datatable(
  population_data,
  # caption = "Figure 4: Largest Cities and Towns, 2024",
  caption = htmltools::tags$caption(
    style = "font-size: 12px; font-weight: italic; text-align: left;", "Data Source: U.S. Census Bureau, Population Estimates Program, Vintage 2024 provided by the Maine Office of the State Economist"
  ),
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    dom = 'rt<"bottom"Bp>',
    buttons = list(
      list(extend = 'copy', text = 'Copy'),
      list(extend = 'excel', text = 'Excel', 
           filename = 'Figure_4_Largest_Cities_and_Towns_2024',
           title = 'Figure 4: Largest Cities and Towns, 2024'),
      list(extend = 'pdf', text = 'PDF', 
           filename = 'Figure_4_Largest_Cities_and_Towns_2024',
           title = 'Figure 4: Largest Cities and Towns, 2024')
    ),
    pageLength = 10
  )
)

# Apply formatting separately to avoid errors
population_table <- population_table %>%
  formatCurrency(columns = "2024 Population", currency = "", digits = 0, interval = 3, mark = ",") %>%
  formatCurrency(columns = "Numeric Change (2023-2024)", currency = "", digits = 0, interval = 3, mark = ",") %>%
  formatRound(columns = "Percent Change (2023-2024)", digits = 1) %>%
  formatRound(columns = "Population density per square mile (2024)", digits = 1, interval = 3, mark = ",")

# #Add visual bar for population
# population_table <- population_table %>%
#   formatStyle(
#     columns = '2024 Population',
#     background = styleColorBar(
#       population_data$`2024 Population`,
#       'lightgreen'
#     ),
#     backgroundSize = '100% 90%',
#     backgroundRepeat = 'no-repeat',
#     backgroundPosition = 'center'
#   )

# Create CSS for larger caption
caption_css <- htmltools::tags$style("
  .dataTables_wrapper .dataTables_caption {
    font-size: 18px;
    font-weight: bold;
    padding: 10px 0;
    text-align: center;
  }
")

# Create directory if it doesn't exist
if (!dir.exists("Outputs")) {
  dir.create("Outputs")
}

# Save all tables - using the most direct and basic approach possible
# First make sure we're using the right packages explicitly
library(htmlwidgets)
library(DT)

# Save each table individually without any extra wrappers
# Simple approach with minimal dependencies
saveWidget(numeric_change_table, "Outputs/Figure_2_numeric_change_table.html", selfcontained = TRUE)
saveWidget(percent_change_table, "Outputs/Figure_1_percent_change_table.html", selfcontained = TRUE) 
saveWidget(population_table, "Outputs/Figure_4_population_table.html", selfcontained = TRUE)

# Alternative low-level approach if htmlwidgets::saveWidget still fails
# This directly prints the widget HTML to a file
write_widget <- function(widget, file) {
  html_content <- htmlwidgets::saveWidget(widget, tempfile(), selfcontained = FALSE)
  html_text <- readLines(html_content)
  writeLines(html_text, file)
  message("Saved to ", file)
}

# Try this if the above fails
tryCatch({
  saveWidget(numeric_change_table, "Figure_2_numeric_change_table.html", selfcontained = TRUE)
}, error = function(e) {
  message("Fallback method being used due to error: ", e$message)
  write_widget(numeric_change_table, "Outputs/Figure_2_numeric_change_table.html")
  write_widget(percent_change_table, "Figure_2_numeric_change_table.html")
  write_widget(population_table, "Outputs/Figure_4_population_table.html")
})

# Print the tables to view in R
percent_change_table  # Figure 1
numeric_change_table  # Figure 2
population_table      # Figure 4



# Save tables as images (add this at the end of your script)
if (!requireNamespace("webshot2", quietly = TRUE)) {
  install.packages("webshot2")
}
library(webshot2)

# Make sure PhantomJS is installed
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}

# Save each table as a PNG image
webshot2::webshot(
  url = "Outputs/Figure_1_percent_change_table.html",
  file = "Outputs/Figure_1_percent_change_table.png",
  zoom = 2
)

webshot2::webshot(
  url = "Outputs/Figure_2_numeric_change_table.html",
  file = "Outputs/Figure_2_population_change_table.png",
  zoom = 2
)

webshot2::webshot(
  url = "Outputs/Figure_4_population_table.html",
  file = "Outputs/Figure_4_population_table.png",
  zoom = 2
)
