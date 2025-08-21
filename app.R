# SCM Salary Analysis Shiny App - Database Version
# Interactive dashboard for Supply Chain Management salary data from MySQL database

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(scales)
library(RMySQL)
library(DBI)

# Database configuration
DB_CONFIG <- list(
  host = Sys.getenv("MYSQL_HOST"),
  dbname = Sys.getenv("MYSQL_DATABASE"),
  username = Sys.getenv("MYSQL_USERNAME"),
  password = Sys.getenv("MYSQL_PASSWORD")
)

# Check database configuration
check_db_config <- function() {
  required_vars <- c("MYSQL_HOST", "MYSQL_DATABASE", "MYSQL_USERNAME", "MYSQL_PASSWORD")
  missing_vars <- required_vars[sapply(required_vars, function(x) Sys.getenv(x) == "")]
  
  if(length(missing_vars) > 0) {
    stop(paste("Missing required environment variables:", paste(missing_vars, collapse = ", ")))
  }
  
  return(TRUE)
}

# Simple connection test function
test_db_connection_simple <- function() {
  tryCatch({
    conn <- get_db_connection()
    on.exit(dbDisconnect(conn))
    
    # Test basic query
    result <- dbGetQuery(conn, "SELECT 1 as test_value")
    
    # Test table access
    tables <- dbListTables(conn)
    
    return(list(
      success = TRUE,
      tables = tables,
      table_count = length(tables)
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error = e$message
    ))
  })
}

# Create simple database connection with MariaDB compatibility
get_db_connection <- function() {
  check_db_config()
  
  tryCatch({
    conn <- dbConnect(
      MySQL(),
      host = DB_CONFIG$host,
      dbname = DB_CONFIG$dbname,
      username = DB_CONFIG$username,
      password = DB_CONFIG$password
    )
    
    # Test the connection with a simple query
    test_result <- dbGetQuery(conn, "SELECT 1 as test_value")
    if(test_result$test_value != 1) {
      stop("Connection test failed")
    }
    
    return(conn)
  }, error = function(e) {
    stop(paste("Failed to create database connection:", e$message))
  })
}

# Database query functions with direct connections
get_available_years <- function() {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- "SELECT DISTINCT data_year FROM scm_salary_data ORDER BY data_year DESC"
  tryCatch({
    result <- dbGetQuery(conn, query)
    return(result$data_year)
  }, error = function(e) {
    stop(paste("Error getting available years:", e$message))
  })
}

get_occupation_categories <- function() {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- "SELECT DISTINCT occupation_category FROM occupation_definitions WHERE is_active = TRUE"
  tryCatch({
    result <- dbGetQuery(conn, query)
    return(result$occupation_category)
  }, error = function(e) {
    stop(paste("Error getting occupation categories:", e$message))
  })
}

get_scm_data_from_db <- function(year, occupation_set = "both") {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  # Build the occupation filter
  if (occupation_set == "both") {
    occupation_filter <- ""
  } else {
    occupation_filter <- paste0("AND od.occupation_category = '", occupation_set, "'")
  }
  
  query <- paste0("
    SELECT 
      sd.occupation_code,
      od.occupation_name,
      od.occupation_category,
      od.occupation_level,
      od.scm_function,
      sd.employment,
      sd.median_wage,
      sd.mean_wage,
      sd.median_hourly,
      sd.mean_hourly,
      sd.wage_ratio,
      sd.wage_distribution,
      sd.data_available,
      sd.updated_date
    FROM scm_salary_data sd
    JOIN occupation_definitions od ON sd.occupation_code = od.occupation_code
    WHERE sd.data_year = ", year, "
      AND od.is_active = TRUE
      ", occupation_filter, "
    ORDER BY sd.median_wage DESC
  ")
  
  tryCatch({
    result <- dbGetQuery(conn, query)
    
    # Convert data types and add calculated fields
    result <- result %>%
      mutate(
        employment = as.numeric(employment),
        median_wage = as.numeric(median_wage),
        mean_wage = as.numeric(mean_wage),
        median_hourly = as.numeric(median_hourly),
        mean_hourly = as.numeric(mean_hourly),
        wage_ratio = as.numeric(wage_ratio),
        data_available = as.logical(data_available),
        # Add occupation level if not in database
        occupation_level = case_when(
          !is.na(occupation_level) ~ occupation_level,
          str_detect(occupation_code, "^11-") ~ "Management",
          str_detect(occupation_code, "^13-1081|^13-1023|^13-1022|^13-1199") ~ "Core SCM Professional",
          str_detect(occupation_code, "^13-1111|^15-2031|^17-2112") ~ "SCM-Adjacent Analytical",
          str_detect(occupation_code, "^43-|^53-") ~ "Operational/Support",
          TRUE ~ "Other"
        ),
        # Add SCM function if not in database
        scm_function = case_when(
          !is.na(scm_function) ~ scm_function,
          str_detect(occupation_code, "^11-3061|^13-1023|^13-1022") ~ "Procurement & Sourcing",
          str_detect(occupation_code, "^11-3071|^43-5011|^43-5071|^53-1047") ~ "Transportation & Logistics",
          str_detect(occupation_code, "^13-1081") ~ "Supply Chain Planning",
          str_detect(occupation_code, "^43-5061") ~ "Production Planning",
          str_detect(occupation_code, "^13-1199") ~ "Supply Chain Analysis",
          str_detect(occupation_code, "^13-1111|^15-2031|^17-2112") ~ "Process Optimization",
          str_detect(occupation_code, "^11-9199") ~ "General Operations",
          TRUE ~ "Other SCM Functions"
        )
      )
    
    return(result)
  }, error = function(e) {
    stop(paste("Error getting SCM data:", e$message))
  })
}

get_refresh_log <- function(limit = 10) {
  conn <- get_db_connection()
  on.exit(dbDisconnect(conn))
  
  query <- paste0("
    SELECT 
      data_year,
      occupation_set,
      occupations_requested,
      occupations_successful,
      refresh_status,
      refresh_date,
      refresh_duration_seconds
    FROM data_refresh_log 
    ORDER BY refresh_date DESC 
    LIMIT ", limit)
  
  tryCatch({
    result <- dbGetQuery(conn, query)
    return(result)
  }, error = function(e) {
    stop(paste("Error getting refresh log:", e$message))
  })
}

# Remove pool creation - no longer needed
# db_pool <- create_db_pool()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "SCM Salary Analysis Dashboard - Database Edition"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Detailed Analysis", tabName = "detailed", icon = icon("table")),
      menuItem("Comparisons", tabName = "comparisons", icon = icon("chart-bar")),
      menuItem("Data Export", tabName = "export", icon = icon("download")),
      menuItem("Data Status", tabName = "status", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Analysis Controls", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3,
                           uiOutput("year_selector")
                    ),
                    column(3,
                           uiOutput("occupation_set_selector")
                    ),
                    column(3,
                           br(),
                           actionButton("analyze_btn", "Load Data", 
                                        class = "btn-primary", style = "margin-top: 5px;")
                    ),
                    column(3,
                           br(),
                           downloadButton("download_report", "Download Report", 
                                          class = "btn-success", style = "margin-top: 5px;")
                    )
                  )
                )
              ),
              
              fluidRow(
                valueBoxOutput("total_employment"),
                valueBoxOutput("median_wage"),
                valueBoxOutput("occupations_analyzed")
              ),
              
              fluidRow(
                box(
                  title = "Salary Distribution by Occupation Level", 
                  status = "primary", solidHeader = TRUE, width = 8,
                  plotlyOutput("salary_by_level_plot")
                ),
                box(
                  title = "Employment Distribution", 
                  status = "info", solidHeader = TRUE, width = 4,
                  plotlyOutput("employment_pie")
                )
              ),
              
              fluidRow(
                box(
                  title = "Top 10 Highest Paying SCM Occupations",
                  status = "success", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("top_occupations_table")
                )
              )
      ),
      
      # Detailed Analysis Tab
      tabItem(tabName = "detailed",
              fluidRow(
                box(
                  title = "Detailed Occupation Data", 
                  status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("detailed_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Salary vs Employment Scatter Plot",
                  status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("salary_employment_scatter")
                ),
                box(
                  title = "Wage Distribution Analysis",
                  status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("wage_distribution_plot")
                )
              )
      ),
      
      # Comparisons Tab
      tabItem(tabName = "comparisons",
              fluidRow(
                box(
                  title = "Compare Occupations", 
                  status = "primary", solidHeader = TRUE, width = 4,
                  uiOutput("comparison_selector"),
                  br(),
                  actionButton("compare_btn", "Compare Selected", class = "btn-info")
                ),
                box(
                  title = "Comparison Results",
                  status = "success", solidHeader = TRUE, width = 8,
                  DT::dataTableOutput("comparison_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Salary Comparison Chart",
                  status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("comparison_chart")
                )
              )
      ),
      
      # Data Export Tab
      tabItem(tabName = "export",
              fluidRow(
                box(
                  title = "Export Options", 
                  status = "primary", solidHeader = TRUE, width = 12,
                  h4("Available Downloads:"),
                  br(),
                  fluidRow(
                    column(4,
                           h5("Complete Analysis Data"),
                           p("Full dataset with all calculated fields"),
                           downloadButton("download_full", "Download CSV", class = "btn-primary")
                    ),
                    column(4,
                           h5("Summary by Level"),
                           p("Aggregated data by occupation level"),
                           downloadButton("download_summary", "Download CSV", class = "btn-success")
                    ),
                    column(4,
                           h5("Custom Report"),
                           p("Formatted report with insights"),
                           downloadButton("download_custom", "Download Report", class = "btn-info")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Data Preview", 
                  status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("export_preview")
                )
              )
      ),
      
      # Data Status Tab
      tabItem(tabName = "status",
              fluidRow(
                box(
                  title = "Database Connection Status", 
                  status = "primary", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("db_status")
                ),
                box(
                  title = "Available Data Years", 
                  status = "info", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("available_years_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Recent Data Refreshes", 
                  status = "success", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("refresh_log_table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    scm_data = NULL,
    analysis_complete = FALSE,
    available_years = NULL,
    available_categories = NULL
  )
  
  # Initialize available years and categories
  observe({
    tryCatch({
      values$available_years <- get_available_years()
      values$available_categories <- get_occupation_categories()
    }, error = function(e) {
      showNotification(paste("Database connection error:", e$message), type = "error")
    })
  })
  
  # Dynamic UI for year selection
  output$year_selector <- renderUI({
    if(is.null(values$available_years)) {
      numericInput("analysis_year", "Analysis Year:", value = 2024, min = 2015, max = 2024)
    } else {
      selectInput("analysis_year", "Analysis Year:", 
                  choices = setNames(values$available_years, values$available_years),
                  selected = max(values$available_years))
    }
  })
  
  # Dynamic UI for occupation set selection
  output$occupation_set_selector <- renderUI({
    if(is.null(values$available_categories)) {
      selectInput("occupation_set", "Occupation Set:",
                  choices = list("Core SCM Only" = "core", "Extended SCM" = "extended", "Both Core & Extended" = "both"),
                  selected = "core")
    } else {
      choices <- list("Both Core & Extended" = "both")
      for(cat in values$available_categories) {
        choices[[paste(str_to_title(cat), "SCM")]] <- cat
      }
      selectInput("occupation_set", "Occupation Set:", choices = choices, selected = "both")
    }
  })
  
  # Load data when button is clicked
  observeEvent(input$analyze_btn, {
    req(input$analysis_year, input$occupation_set)
    
    # Show progress
    progress <- Progress$new(session)
    progress$set(message = "Loading data from database...", value = 0.5)
    on.exit(progress$close())
    
    tryCatch({
      # Load data from database
      raw_data <- get_scm_data_from_db(input$analysis_year, input$occupation_set)
      
      if(nrow(raw_data) == 0) {
        showNotification(paste("No data found for year", input$analysis_year, "and occupation set", input$occupation_set), 
                         type = "warning")
        return()
      }
      
      # Filter for available data and arrange
      processed_data <- raw_data %>%
        filter(data_available == TRUE) %>%
        arrange(desc(median_wage))
      
      values$scm_data <- processed_data
      values$analysis_complete <- TRUE
      
      showNotification(paste("Loaded", nrow(processed_data), "occupations with data!"), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Database status
  output$db_status <- renderText({
    tryCatch({
      # Test database connection
      conn <- get_db_connection()
      on.exit(dbDisconnect(conn))
      
      test_query <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM occupation_definitions")
      occupation_count <- test_query$count
      
      # Get data summary
      summary_query <- dbGetQuery(conn, "
        SELECT 
          COUNT(DISTINCT data_year) as years_available,
          COUNT(*) as total_records,
          SUM(CASE WHEN data_available = TRUE THEN 1 ELSE 0 END) as records_with_data
        FROM scm_salary_data
      ")
      
      paste(
        "✓ Database connection: SUCCESS",
        paste("✓ Occupation definitions:", occupation_count),
        paste("✓ Data years available:", summary_query$years_available),
        paste("✓ Total salary records:", summary_query$total_records),
        paste("✓ Records with data:", summary_query$records_with_data),
        paste("✓ Database:", DB_CONFIG$dbname, "@", DB_CONFIG$host),
        sep = "\n"
      )
    }, error = function(e) {
      paste("✗ Database connection: FAILED", paste("Error:", e$message), sep = "\n")
    })
  })
  
  # Available years table
  output$available_years_table <- DT::renderDataTable({
    tryCatch({
      conn <- get_db_connection()
      on.exit(dbDisconnect(conn))
      
      query <- "
        SELECT 
          data_year,
          COUNT(*) as total_occupations,
          SUM(CASE WHEN data_available = TRUE THEN 1 ELSE 0 END) as with_data,
          MAX(updated_date) as last_updated
        FROM scm_salary_data 
        GROUP BY data_year 
        ORDER BY data_year DESC
      "
      result <- dbGetQuery(conn, query)
      
      result %>%
        mutate(
          coverage_pct = round(100 * with_data / total_occupations, 1),
          last_updated = as.Date(last_updated)
        ) %>%
        select(data_year, total_occupations, with_data, coverage_pct, last_updated) %>%
        setNames(c("Year", "Total Occupations", "With Data", "Coverage %", "Last Updated"))
      
    }, error = function(e) {
      data.frame(Error = paste("Failed to load data:", e$message))
    })
  }, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  
  # Refresh log table
  output$refresh_log_table <- DT::renderDataTable({
    tryCatch({
      log_data <- get_refresh_log(db_pool, 20)
      
      if(nrow(log_data) > 0) {
        log_data %>%
          mutate(
            success_rate = round(100 * occupations_successful / occupations_requested, 1),
            duration_min = round(refresh_duration_seconds / 60, 2),
            refresh_date = as.POSIXct(refresh_date)
          ) %>%
          select(refresh_date, data_year, occupation_set, occupations_requested, 
                 occupations_successful, success_rate, refresh_status, duration_min) %>%
          setNames(c("Refresh Date", "Year", "Occupation Set", "Requested", 
                     "Successful", "Success %", "Status", "Duration (min)"))
      } else {
        data.frame(Message = "No refresh log data found")
      }
      
    }, error = function(e) {
      data.frame(Error = paste("Failed to load refresh log:", e$message))
    })
  }, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  
  # Value boxes (same logic as original, but using database data)
  output$total_employment <- renderValueBox({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      valueBox(
        value = "Click 'Load Data'",
        subtitle = "Total Employment",
        icon = icon("users"),
        color = "blue"
      )
    } else {
      total_emp <- sum(values$scm_data$employment, na.rm = TRUE)
      
      valueBox(
        value = scales::comma(total_emp),
        subtitle = "Total Employment",
        icon = icon("users"),
        color = "blue"
      )
    }
  })
  
  output$median_wage <- renderValueBox({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      valueBox(
        value = "Click 'Load Data'",
        subtitle = "Weighted Median Wage",
        icon = icon("dollar-sign"),
        color = "green"
      )
    } else {
      if(nrow(values$scm_data) > 0) {
        weighted_median <- weighted.mean(values$scm_data$median_wage, values$scm_data$employment, na.rm = TRUE)
        
        valueBox(
          value = scales::dollar(weighted_median, accuracy = 1),
          subtitle = "Weighted Median Wage",
          icon = icon("dollar-sign"),
          color = "green"
        )
      } else {
        valueBox(
          value = "No Data",
          subtitle = "Weighted Median Wage",
          icon = icon("dollar-sign"),
          color = "red"
        )
      }
    }
  })
  
  output$occupations_analyzed <- renderValueBox({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      valueBox(
        value = "Click 'Load Data'",
        subtitle = "Occupations with Data",
        icon = icon("chart-bar"),
        color = "yellow"
      )
    } else {
      data_count <- nrow(values$scm_data)
      
      valueBox(
        value = data_count,
        subtitle = "Occupations with Data",
        icon = icon("chart-bar"),
        color = "yellow"
      )
    }
  })
  
  # All the visualization outputs (same as original app)
  output$salary_by_level_plot <- renderPlotly({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(plot_ly() %>% add_annotations(text = "Load data to see visualization", showarrow = FALSE))
    }
    
    if(nrow(values$scm_data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    p <- values$scm_data %>%
      plot_ly(x = ~occupation_level, y = ~median_wage, type = "box",
              text = ~paste("Occupation:", occupation_name, 
                            "<br>Median Wage:", scales::dollar(median_wage),
                            "<br>Employment:", scales::comma(employment)),
              hovertemplate = "%{text}<extra></extra>") %>%
      layout(title = "Median Wage Distribution by Occupation Level",
             xaxis = list(title = "Occupation Level"),
             yaxis = list(title = "Median Wage ($)", tickformat = "$,.0f"))
    
    p
  })
  
  output$employment_pie <- renderPlotly({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(plot_ly() %>% add_annotations(text = "Load data to see visualization", showarrow = FALSE))
    }
    
    if(nrow(values$scm_data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    pie_data <- values$scm_data %>%
      group_by(occupation_level) %>%
      summarise(total_employment = sum(employment, na.rm = TRUE), .groups = 'drop')
    
    p <- pie_data %>%
      plot_ly(labels = ~occupation_level, values = ~total_employment, type = "pie",
              textinfo = "label+percent",
              hovertemplate = "%{label}<br>Employment: %{value:,}<extra></extra>") %>%
      layout(title = "Employment Distribution by Level")
    
    p
  })
  
  output$top_occupations_table <- DT::renderDataTable({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(data.frame(Message = "Load data to see results"))
    }
    
    display_data <- values$scm_data %>%
      head(10) %>%
      select(occupation_name, occupation_level, employment, median_wage, mean_wage, wage_distribution) %>%
      mutate(
        employment = scales::comma(employment),
        median_wage = scales::dollar(median_wage),
        mean_wage = scales::dollar(mean_wage)
      )
    
    names(display_data) <- c("Occupation", "Level", "Employment", "Median Wage", "Mean Wage", "Distribution")
    
    DT::datatable(display_data, 
                  options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE)
  })
  
  output$detailed_table <- DT::renderDataTable({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(data.frame(Message = "Load data to see results"))
    }
    
    display_data <- values$scm_data %>%
      select(occupation_code, occupation_name, occupation_level, scm_function, 
             employment, median_wage, mean_wage, median_hourly, wage_distribution) %>%
      mutate(
        employment = scales::comma(employment),
        median_wage = scales::dollar(median_wage),
        mean_wage = scales::dollar(mean_wage),
        median_hourly = scales::dollar(median_hourly, accuracy = 0.01)
      )
    
    names(display_data) <- c("Code", "Occupation", "Level", "Function", "Employment", 
                             "Median Wage", "Mean Wage", "Median Hourly", "Distribution")
    
    DT::datatable(display_data, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Comparison functionality
  output$comparison_selector <- renderUI({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(p("Load data first to enable comparisons"))
    }
    
    choices <- setNames(values$scm_data$occupation_code, values$scm_data$occupation_name)
    
    selectInput("occupations_to_compare", "Select Occupations to Compare:",
                choices = choices,
                multiple = TRUE,
                selected = head(names(choices), 3))
  })
  
  comparison_data <- eventReactive(input$compare_btn, {
    req(input$occupations_to_compare)
    
    values$scm_data %>%
      filter(occupation_code %in% input$occupations_to_compare) %>%
      arrange(desc(median_wage))
  })
  
  output$comparison_table <- DT::renderDataTable({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(data.frame(Message = "Load data first"))
    }
    
    if(input$compare_btn == 0) {
      return(data.frame(Message = "Select occupations and click Compare"))
    }
    
    comp_data <- comparison_data() %>%
      select(occupation_name, occupation_level, employment, median_wage, mean_wage) %>%
      mutate(
        employment = scales::comma(employment),
        median_wage = scales::dollar(median_wage),
        mean_wage = scales::dollar(mean_wage)
      )
    
    names(comp_data) <- c("Occupation", "Level", "Employment", "Median Wage", "Mean Wage")
    
    DT::datatable(comp_data, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$comparison_chart <- renderPlotly({
    if(is.null(values$scm_data) || !values$analysis_complete || input$compare_btn == 0) {
      return(plot_ly() %>% add_annotations(text = "Select occupations to compare", showarrow = FALSE))
    }
    
    comp_data <- comparison_data()
    
    p <- comp_data %>%
      plot_ly(x = ~reorder(occupation_name, median_wage), y = ~median_wage, 
              type = "bar", name = "Median Wage",
              text = ~paste("Employment:", scales::comma(employment)),
              hovertemplate = "%{y:$,.0f}<br>%{text}<extra></extra>") %>%
      layout(title = "Salary Comparison",
             xaxis = list(title = ""),
             yaxis = list(title = "Median Wage ($)", tickformat = "$,.0f")) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # Additional plots
  output$salary_employment_scatter <- renderPlotly({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(plot_ly() %>% add_annotations(text = "Load data to see visualization", showarrow = FALSE))
    }
    
    p <- values$scm_data %>%
      plot_ly(x = ~employment, y = ~median_wage, color = ~occupation_level,
              text = ~occupation_name,
              hovertemplate = "%{text}<br>Employment: %{x:,}<br>Median Wage: %{y:$,.0f}<extra></extra>") %>%
      add_markers(size = ~median_wage, sizes = c(10, 30)) %>%
      layout(title = "Employment vs Median Wage by Occupation Level",
             xaxis = list(title = "Employment", type = "log"),
             yaxis = list(title = "Median Wage ($)", tickformat = "$,.0f"))
    
    p
  })
  
  output$wage_distribution_plot <- renderPlotly({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(plot_ly() %>% add_annotations(text = "Load data to see visualization", showarrow = FALSE))
    }
    
    p <- values$scm_data %>%
      plot_ly(x = ~wage_distribution, type = "histogram",
              text = ~paste("Count:", ..count..),
              hovertemplate = "%{text}<extra></extra>") %>%
      layout(title = "Distribution of Wage Patterns",
             xaxis = list(title = "Wage Distribution Pattern"),
             yaxis = list(title = "Number of Occupations"))
    
    p
  })
  
  # Download handlers
  output$download_full <- downloadHandler(
    filename = function() {
      paste0("scm_salary_analysis_", input$analysis_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(!is.null(values$scm_data)) {
        write_csv(values$scm_data, file)
      }
    }
  )
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("scm_level_summary_", input$analysis_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(!is.null(values$scm_data)) {
        summary_data <- values$scm_data %>%
          group_by(occupation_level) %>%
          summarise(
            occupations_count = n(),
            total_employment = sum(employment, na.rm = TRUE),
            median_wage_avg = mean(median_wage, na.rm = TRUE),
            median_wage_min = min(median_wage, na.rm = TRUE),
            median_wage_max = max(median_wage, na.rm = TRUE),
            .groups = 'drop'
          )
        write_csv(summary_data, file)
      }
    }
  )
  
  output$download_custom <- downloadHandler(
    filename = function() {
      paste0("scm_custom_report_", input$analysis_year, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      if(!is.null(values$scm_data)) {
        # Create a simple HTML report
        report_content <- paste0(
          "<html><head><title>SCM Salary Report - ", input$analysis_year, "</title></head><body>",
          "<h1>Supply Chain Management Salary Analysis Report</h1>",
          "<h2>Year: ", input$analysis_year, "</h2>",
          "<h2>Generated: ", Sys.Date(), "</h2>",
          "<h3>Summary Statistics</h3>",
          "<p>Total Occupations Analyzed: ", nrow(values$scm_data), "</p>",
          "<p>Total Employment: ", scales::comma(sum(values$scm_data$employment, na.rm = TRUE)), "</p>",
          "<p>Weighted Average Median Wage: ", scales::dollar(weighted.mean(values$scm_data$median_wage, values$scm_data$employment, na.rm = TRUE)), "</p>",
          "</body></html>"
        )
        writeLines(report_content, file)
      }
    }
  )
  
  output$export_preview <- DT::renderDataTable({
    if(is.null(values$scm_data) || !values$analysis_complete) {
      return(data.frame(Message = "Load data to see preview"))
    }
    
    preview_data <- values$scm_data %>%
      head(20) %>%
      select(occupation_name, occupation_level, employment, median_wage, mean_wage) %>%
      mutate(
        employment = scales::comma(employment),
        median_wage = scales::dollar(median_wage),
        mean_wage = scales::dollar(mean_wage)
      )
    
    names(preview_data) <- c("Occupation", "Level", "Employment", "Median Wage", "Mean Wage")
    
    DT::datatable(preview_data, 
                  options = list(pageLength = 20, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Clean up database pool on session end
  session$onSessionEnded(function() {
    if(exists("db_pool")) {
      poolClose(db_pool)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)