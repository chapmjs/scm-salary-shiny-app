---
title: "README"
output: html_document
---

# SCM Salary Analysis Shiny App

Interactive dashboard for analyzing Supply Chain Management salaries using BLS data.

## Dependencies
- **CRAN packages**: shiny, shinydashboard, DT, plotly, tidyverse, jsonlite, scales
- **GitHub packages**: blsAPI (mikeasilva/blsAPI)

## Local Development Setup
1. Clone repository
2. Install dependencies:
   ```r
   # Install CRAN packages
   install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                      "tidyverse", "jsonlite", "scales", "devtools"))
   
   # Install GitHub packages
   devtools::install_github("mikeasilva/blsAPI")