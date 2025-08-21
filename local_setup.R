# Install all CRAN packages
install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                   "tidyverse", "jsonlite", "scales", "devtools", "rsconnect"))

# Install blsAPI from GitHub
devtools::install_github("mikeasilva/blsAPI")

# Generate manifest.json with ALL dependencies
rsconnect::writeManifest()

# to install packages on shiny server
# sudo su - -c "R -e \"install.packages(c('shiny', 'shinydashboard', 'DT', 'plotly', 'tidyverse', 'jsonlite', 'scales', 'devtools', 'rsconnect'), repos='https://cran.rstudio.com/')\""
# sudo su - -c "R -e \"devtools::install_github('mikeasilva/blsAPI')\""
