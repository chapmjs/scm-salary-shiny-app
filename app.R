# SCM Salary Analysis Shiny App
# Interactive dashboard for Supply Chain Management salary data from BLS

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(blsAPI)
library(tidyverse)
library(jsonlite)
library(scales)

# Load API key from environment variable
# Set BLS_KEY environment variable in Posit Connect Cloud
if(Sys.getenv("BLS_KEY") == "") {
  warning("BLS API key not found. Please set BLS_KEY environment variable in Posit Connect Cloud.")
}

# Define Supply Chain Management occupations
scm_occupations <- list(
  # Core SCM Management
  "11-3061" = "Purchasing Managers",
  "11-3071" = "Transportation, Storage, and Distribution Managers", 
  "11-9199" = "Managers, All Other (includes Operations Managers)",
  
  # Core SCM Professional/Analytical
  "13-1081" = "Logisticians",
  "13-1023" = "Purchasing Agents, Except Wholesale, Retail, and Farm Products",
  "13-1022" = "Wholesale and Retail Buyers, Except Farm Products",
  "13-1199" = "Business Operations Specialists, All Other (includes Supply Chain Analysts)",
  
  # SCM-Adjacent Analytical Roles
  "13-1111" = "Management Analysts (often work on supply chain optimization)",
  "15-2031" = "Operations Research Analysts",
  "17-2112" = "Industrial Engineers",
  
  # Core SCM Operational/Support
  "43-5011" = "Cargo and Freight Agents",
  "43-5061" = "Production, Planning, and Expediting Clerks",
  "43-5071" = "Shipping, Receiving, and Traffic Clerks",
  "53-1047" = "Traffic Technicians"
)

extended_scm_occupations <- list(
  "13-1021" = "Buyers and Purchasing Agents, Farm Products",
  "43-5021" = "Couriers and Messengers", 
  "43-5052" = "Postal Service Mail Carriers",
  "53-7064" = "Packers and Packagers, Hand",
  "53-7065" = "Stockers and Order Fillers"
)

# Core functions from original script
construct_series_ids <- function(occupation_code) {
  clean_code <- sprintf("%06s", gsub("-", "", occupation_code))
  base_id <- paste0("OEUN0000000000000", clean_code)
  series_ids <- paste0(base_id, c("01", "04", "13"))
  names(series_ids) <- c("employment", "mean_wage", "median_wage")
  return(series_ids)
}

get_occupation_data <- function(occupation_code, year, max_retries = 3) {
  series_ids <- construct_series_ids(occupation_code)
  
  payload <- list(
    'seriesid' = as.vector(series_ids),
    'startyear' = as.character(year),
    'endyear' = as.character(year),
    'registrationKey' = Sys.getenv("BLS_KEY")
  )
  
  for(attempt in 1:max_retries) {
    tryCatch({
      response <- blsAPI(payload, api_version = 2)
      json_data <- fromJSON(response)
      
      if(json_data$status == "REQUEST_SUCCEEDED") {
        return(json_data)
      } else {
        if(attempt == max_retries) return(NULL)
      }
    }, error = function(e) {
      if(attempt == max_retries) return(NULL)
      Sys.sleep(1)
    })
  }
  return(NULL)
}

process_occupation_data <- function(api_response, occupation_code, occupation_name) {
  if(is.null(api_response) || is.null(api_response$Results) || is.null(api_response$Results$series)) {
    return(data.frame(
      occupation_code = occupation_code,
      occupation_name = occupation_name,
      employment = NA,
      median_wage = NA,
      mean_wage = NA,
      data_available = FALSE
    ))
  }
  
  series_df <- api_response$Results$series
  
  if(!is.data.frame(series_df) || nrow(series_df) == 0) {
    return(data.frame(
      occupation_code = occupation_code,
      occupation_name = occupation_name,
      employment = NA,
      median_wage = NA,
      mean_wage = NA,
      data_available = FALSE
    ))
  }
  
  results <- list(employment = NA, median_wage = NA, mean_wage = NA)
  
  for(i in 1:nrow(series_df)) {
    tryCatch({
      series_id <- series_df$seriesID[i]
      
      if(!"data" %in% names(series_df) || !is.list(series_df$data)) {
        next
      }
      
      series_data <- series_df$data[[i]]
      
      if(is.null(series_data) || !is.data.frame(series_data) || nrow(series_data) == 0 || !"value" %in% names(series_data)) {
        next
      }
      
      raw_value <- series_data$value[1]
      if(is.na(raw_value) || raw_value == "" || raw_value == "-") {
        next
      }
      
      value <- as.numeric(raw_value)
      if(is.na(value)) {
        next
      }
      
      if(grepl("01$", series_id)) {
        results$employment <- value
      } else if(grepl("04$", series_id)) {
        results$mean_wage <- value
      } else if(grepl("13$", series_id)) {
        results$median_wage <- value
      }
      
    }, error = function(e) {
      # Continue with next series
    })
  }
  
  return(data.frame(
    occupation_code = occupation_code,
    occupation_name = occupation_name,
    employment = results$employment,
    median_wage = results$median_wage,
    mean_wage = results$mean_wage,
    data_available = !all(is.na(c(results$employment, results$median_wage, results$mean_wage)))
  ))
}

analyze_all_occupations <- function(occupations_list, year, progress = NULL) {
  all_results <- list()
  
  for(i in seq_along(occupations_list)) {
    code <- names(occupations_list)[i]
    name <- occupations_list[[code]]
    
    if(!is.null(progress)) {
      progress$inc(1/length(occupations_list), detail = paste("Analyzing:", name))
    }
    
    if(i > 1) Sys.sleep(0.5)
    
    raw_data <- get_occupation_data(code, year)
    processed_data <- process_occupation_data(raw_data, code, name)
    
    all_results[[i]] <- processed_data
  }
  
  final_results <- do.call(rbind, all_results)
  return(final_results)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "SCM Salary Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Detailed Analysis", tabName = "detailed", icon = icon("table")),
      menuItem("Comparisons", tabName = "comparisons", icon = icon("chart-bar")),
      menuItem("Data Export", tabName = "export", icon = icon("download"))
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
                numericInput("analysis_year", "Analysis Year:", 
                           value = 2024, min = 2015, max = 2024)
              ),
              column(3,
                selectInput("occupation_set", "Occupation Set:",
                           choices = list(
                             "Core SCM Only" = "core",
                             "Extended SCM" = "extended",
                             "Both Core & Extended" = "both"
                           ),
                           selected = "core")
              ),
              column(3,
                br(),
                actionButton("analyze_btn", "Run Analysis", 
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
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    scm_data = NULL,
    analysis_complete = FALSE
  )
  
  # Run analysis when button is clicked
  observeEvent(input$analyze_btn, {
    if(Sys.getenv("BLS_KEY") == "") {
      showNotification("BLS API key not found. Please set BLS_KEY environment variable.", 
                      type = "error", duration = 10)
      return()
    }
    
    # Show progress
    progress <- Progress$new(session)
    progress$set(message = "Fetching data from BLS API...", value = 0)
    on.exit(progress$close())
    
    # Determine which occupations to analyze
    occupations_to_use <- switch(input$occupation_set,
      "core" = scm_occupations,
      "extended" = extended_scm_occupations,
      "both" = c(scm_occupations, extended_scm_occupations)
    )
    
    # Analyze occupations
    raw_data <- analyze_all_occupations(occupations_to_use, input$analysis_year, progress)
    
    # Add calculated fields
    processed_data <- raw_data %>%
      mutate(
        median_hourly = median_wage / 2080,
        mean_hourly = mean_wage / 2080,
        wage_ratio = mean_wage / median_wage,
        wage_distribution = case_when(
          wage_ratio > 1.15 ~ "Right-skewed (high earners)",
          wage_ratio < 0.85 ~ "Left-skewed (compressed)",
          TRUE ~ "Relatively symmetric"
        ),
        occupation_level = case_when(
          str_detect(occupation_code, "^11-") ~ "Management",
          str_detect(occupation_code, "^13-1081|^13-1023|^13-1022|^13-1199") ~ "Core SCM Professional",
          str_detect(occupation_code, "^13-1111|^15-2031|^17-2112") ~ "SCM-Adjacent Analytical",
          str_detect(occupation_code, "^43-|^53-") ~ "Operational/Support",
          TRUE ~ "Other"
        ),
        scm_function = case_when(
          str_detect(occupation_code, "^11-3061|^13-1023|^13-1022") ~ "Procurement & Sourcing",
          str_detect(occupation_code, "^11-3071|^43-5011|^43-5071|^53-1047") ~ "Transportation & Logistics",
          str_detect(occupation_code, "^13-1081") ~ "Supply Chain Planning",
          str_detect(occupation_code, "^43-5061") ~ "Production Planning",
          str_detect(occupation_code, "^13-1199") ~ "Supply Chain Analysis",
          str_detect(occupation_code, "^13-1111|^15-2031|^17-2112") ~ "Process Optimization",
          str_detect(occupation_code, "^11-9199") ~ "General Operations",
          TRUE ~ "Other SCM Functions"
        )
      ) %>%
      arrange(desc(median_wage))
    
    values$scm_data <- processed_data
    values$analysis_complete <- TRUE
    
    showNotification("Analysis complete!", type = "success")
  })
  
  # Value boxes
  output$total_employment <- renderValueBox({
    if(is.null(values$scm_data)) {
      valueBox(
        value = "Click 'Run Analysis'",
        subtitle = "Total Employment",
        icon = icon("users"),
        color = "blue"
      )
    } else {
      available_data <- values$scm_data %>% filter(data_available == TRUE)
      total_emp <- sum(available_data$employment, na.rm = TRUE)
      
      valueBox(
        value = scales::comma(total_emp),
        subtitle = "Total Employment",
        icon = icon("users"),
        color = "blue"
      )
    }
  })
  
  output$median_wage <- renderValueBox({
    if(is.null(values$scm_data)) {
      valueBox(
        value = "Click 'Run Analysis'",
        subtitle = "Weighted Median Wage",
        icon = icon("dollar-sign"),
        color = "green"
      )
    } else {
      available_data <- values$scm_data %>% filter(data_available == TRUE)
      if(nrow(available_data) > 0) {
        weighted_median <- weighted.mean(available_data$median_wage, available_data$employment, na.rm = TRUE)
        
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
    if(is.null(values$scm_data)) {
      valueBox(
        value = "Click 'Run Analysis'",
        subtitle = "Occupations with Data",
        icon = icon("chart-bar"),
        color = "yellow"
      )
    } else {
      data_count <- sum(values$scm_data$data_available)
      total_count <- nrow(values$scm_data)
      
      valueBox(
        value = paste(data_count, "/", total_count),
        subtitle = "Occupations with Data",
        icon = icon("chart-bar"),
        color = "yellow"
      )
    }
  })
  
  # Salary by level plot
  output$salary_by_level_plot <- renderPlotly({
    if(is.null(values$scm_data)) {
      return(plot_ly() %>% add_annotations(text = "Run analysis to see data", showarrow = FALSE))
    }
    
    available_data <- values$scm_data %>% filter(data_available == TRUE)
    
    if(nrow(available_data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    p <- available_data %>%
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
  
  # Employment pie chart
  output$employment_pie <- renderPlotly({
    if(is.null(values$scm_data)) {
      return(plot_ly() %>% add_annotations(text = "Run analysis to see data", showarrow = FALSE))
    }
    
    available_data <- values$scm_data %>% filter(data_available == TRUE)
    
    if(nrow(available_data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data available", showarrow = FALSE))
    }
    
    pie_data <- available_data %>%
      group_by(occupation_level) %>%
      summarise(total_employment = sum(employment, na.rm = TRUE), .groups = 'drop')
    
    p <- pie_data %>%
      plot_ly(labels = ~occupation_level, values = ~total_employment, type = "pie",
              textinfo = "label+percent",
              hovertemplate = "%{label}<br>Employment: %{value:,}<extra></extra>") %>%
      layout(title = "Employment Distribution by Level")
    
    p
  })
  
  # Top occupations table
  output$top_occupations_table <- DT::renderDataTable({
    if(is.null(values$scm_data)) {
      return(data.frame(Message = "Run analysis to see data"))
    }
    
    available_data <- values$scm_data %>% 
      filter(data_available == TRUE) %>%
      arrange(desc(median_wage)) %>%
      head(10) %>%
      select(occupation_name, occupation_level, employment, median_wage, mean_wage, wage_distribution) %>%
      mutate(
        employment = scales::comma(employment),
        median_wage = scales::dollar(median_wage),
        mean_wage = scales::dollar(mean_wage)
      )
    
    names(available_data) <- c("Occupation", "Level", "Employment", "Median Wage", "Mean Wage", "Distribution")
    
    DT::datatable(available_data, 
                  options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE)
  })
  
  # Detailed table
  output$detailed_table <- DT::renderDataTable({
    if(is.null(values$scm_data)) {
      return(data.frame(Message = "Run analysis to see data"))
    }
    
    display_data <- values$scm_data %>%
      filter(data_available == TRUE) %>%
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
  
  # Comparison selector
  output$comparison_selector <- renderUI({
    if(is.null(values$scm_data)) {
      return(p("Run analysis first to enable comparisons"))
    }
    
    available_data <- values$scm_data %>% filter(data_available == TRUE)
    choices <- setNames(available_data$occupation_code, available_data$occupation_name)
    
    selectInput("occupations_to_compare", "Select Occupations to Compare:",
                choices = choices,
                multiple = TRUE,
                selected = head(names(choices), 3))
  })
  
  # Comparison table
  comparison_data <- eventReactive(input$compare_btn, {
    req(input$occupations_to_compare)
    
    values$scm_data %>%
      filter(occupation_code %in% input$occupations_to_compare, data_available == TRUE) %>%
      arrange(desc(median_wage))
  })
  
  output$comparison_table <- DT::renderDataTable({
    if(is.null(values$scm_data)) {
      return(data.frame(Message = "Run analysis first"))
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
  
  # Additional plots and download handlers would go here...
  # (I'll include a few key ones)
  
  # Comparison chart
  output$comparison_chart <- renderPlotly({
    if(is.null(values$scm_data) || input$compare_btn == 0) {
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
  
  # Download handlers
  output$download_full <- downloadHandler(
    filename = function() {
      paste0("scm_salary_analysis_", input$analysis_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(values$scm_data, file)
    }
  )
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("scm_level_summary_", input$analysis_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(!is.null(values$scm_data)) {
        summary_data <- values$scm_data %>%
          filter(data_available == TRUE) %>%
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
}

# Run the app
shinyApp(ui = ui, server = server)
