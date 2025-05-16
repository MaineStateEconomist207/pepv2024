# Install and load necessary packages
library(dplyr)
library(htmlwidgets)
library(DT)
library(here)
library(htmltools)

## Set working directory
here::here()

# Read population estimates CSV file into R
me_towns <- read.csv("Inputs/geocorr2023.csv", header=TRUE, stringsAsFactors=FALSE)

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
# If percent values are stored as decimals like 0.0012, convert them to actual percentages
if ("Percent Change (2023-2024)" %in% names(me_towns_clean)) {
  # Check if values appear to be in decimal form (less than 0.1)
  if (mean(abs(me_towns_clean$`Percent Change`), na.rm = TRUE) < 0.1) {
    me_towns_clean$`Percent Change (2023-2024)` <- me_towns_clean$`Percent Change (2023-2024)` * 100
  }
  
  if ("Cumulative Percent Change (2020-2024)" %in% names(me_towns_clean)) {
    if (mean(abs(me_towns_clean$`Cumulative Percent Change (2020-2024)`), na.rm = TRUE) < 0.1) {
      me_towns_clean$`Cumulative Percent Change (2020-2024)` <- me_towns_clean$`Cumulative Percent Change (2020-2024)` * 100
    }
  }
}

# Create the all-towns table with alphabetical order and wider Town column
all_towns_table <- datatable(
  me_towns_clean %>% arrange(Town),  # Sort by Town name A to Z
  # caption = "Maine Cities and Towns Population Estimates, 2024",
  caption = htmltools::tags$caption(
    style = "font-size: 12px; font-weight: italic; text-align: left;", "Data Source: U.S. Census Bureau, Population Estimates Program, Vintage 2024 provided by the Maine Office of the State Economist"
  ),
  rownames = FALSE,
  filter = 'top',  # Add search bar at the top
  extensions = 'Buttons',
  options = list(
    dom = '<"top"f>rt<"bottom"Bp>',  # Search at top, buttons/pagination at bottom
    buttons = list(
      list(
        extend = 'copy',
        text = 'Copy'
      ),
      list(
        extend = 'excel',
        text = 'Excel',
        filename = 'Maine_Cities_Towns_Population_Estimates_2024',
        title = 'Maine Cities and Towns Population Estimates, 2024'
      ),
      list(
        extend = 'pdf',
        text = 'PDF',
        filename = 'Maine_Cities_Towns_Population_Estimates_2024',
        title = 'Maine Cities and Towns Population Estimates, 2024'
      )
    ),
    pageLength = 20,  # Display 20 rows by default
    order = list(list(0, 'asc')),  # Initial sort by Town column (assuming it's the first column)
    columnDefs = list(
      list(
        targets = 0,  # Town column (assuming it's the first column)
        width = '180px'  # Make town column wider
      )
    )
  )
)

# Apply formatting separately to avoid chaining issues
all_towns_table <- all_towns_table %>%
  # Format population estimate with commas
  formatCurrency(
    columns = "2024 Population",
    currency = "",
    digits = 0,
    interval = 3,
    mark = ","
  ) 

all_towns_table <- all_towns_table %>%
  # Format numeric change with commas
  formatCurrency(
    columns = "Numeric Change (2023-2024)",
    currency = "",
    digits = 0,
    interval = 3, 
    mark = ","
  )

all_towns_table <- all_towns_table %>%
  # Format percent change values
  formatRound(
    columns = "Percent Change (2023-2024)",
    digits = 1
  )

# Format additional columns if they exist
if("Cumulative Numeric Change (2020-2024)" %in% colnames(me_towns_clean)) {
  all_towns_table <- all_towns_table %>%
    formatCurrency(
      columns = "Cumulative Numeric Change (2020-2024)",
      currency = "",
      digits = 0,
      interval = 3,
      mark = ","
    )
}

if("Cumulative Percent Change (2020-2024)" %in% colnames(me_towns_clean)) {
  all_towns_table <- all_towns_table %>%
    formatRound(
      columns = "Cumulative Percent Change (2020-2024)",
      digits = 1
    )
}

if("Population density per square mile (2024)" %in% colnames(me_towns_clean)) {
  all_towns_table <- all_towns_table %>%
    formatRound(
      columns = "Population density per square mile (2024)",
      digits = 1,
      interval = 3,
      mark = ","
    )
}

# Create CSS for larger caption and better column spacing
caption_css <- htmltools::tags$style("
  .dataTables_wrapper .dataTables_caption {
    font-size: 18px;
    font-weight: bold;
    padding: 10px 0;
    text-align: center;
  }

  /* Additional styling for better readability */
  # table.dataTable th, table.dataTable td {
  #   padding: 8px 10px; /* More padding for all cells */
  # }
  # 
  # /* Make town column text wrap properly */
  # table.dataTable td:first-child {
  #   white-space: normal !important;
  #   word-wrap: break-word;
  # }
")

# Create directory if it doesn't exist
dir.create("Outputs", showWarnings = FALSE)

# Combine with CSS for saving
all_towns_widget <- htmltools::tagList(caption_css, all_towns_table)

# Save as self-contained HTML with robust error handling
tryCatch({
  # Try to save with the standard approach
  htmlwidgets::saveWidget(
    widget = all_towns_widget,
    file = "Outputs/maine_all_towns.html",
    selfcontained = TRUE,
    title = "Maine Cities and Towns Population Estimates, 2024"
  )
  cat("Successfully saved self-contained HTML table!\n")
}, error = function(e) {
  cat("First save attempt failed:", e$message, "\n")
  cat("Trying alternative approach...\n")
  
  # Method 2: Try saving with minimal options without CSS
  tryCatch({
    htmlwidgets::saveWidget(
      widget = all_towns_table, # Just the table without CSS
      file = "Outputs/maine_all_towns.html",
      selfcontained = TRUE,
      title = "Maine Cities and Towns Population Estimates, 2024"
    )
    cat("Successfully saved using simplified approach!\n")
  }, error = function(e2) {
    cat("Second save attempt failed:", e2$message, "\n")
    cat("Trying final approach...\n")
    
    # Method 3: First save without selfcontained, then try to make selfcontained
    temp_file <- tempfile(fileext = ".html")
    temp_lib <- tempfile()
    dir.create(temp_lib)
    
    htmlwidgets::saveWidget(
      widget = all_towns_table,
      file = temp_file,
      selfcontained = FALSE,
      libdir = temp_lib
    )
    
    # Now try to make it self-contained
    tryCatch({
      # Use separate tool to make selfcontained if available
      if (requireNamespace("htmltools", quietly = TRUE)) {
        html_content <- htmltools::includeHTML(temp_file)
        html_text <- as.character(html_content)
        writeLines(html_text, "Outputs/maine_all_towns.html")
        cat("Created self-contained HTML using htmltools approach!\n")
      } else {
        # Simple copy of non-selfcontained and warn user
        file.copy(temp_file, "Outputs/maine_all_towns.html", overwrite = TRUE)
        file.copy(temp_lib, "Outputs", recursive = TRUE)
        cat("WARNING: Could not create self-contained HTML.\n")
        cat("Created non-selfcontained version with dependencies in Outputs/lib\n")
      }
    }, error = function(e3) {
      cat("All approaches failed. Saving basic version.\n")
      file.copy(temp_file, "Outputs/maine_all_towns.html", overwrite = TRUE)
      file.copy(temp_lib, "Outputs", recursive = TRUE)
    })
  })
})

# Display in R
all_towns_table