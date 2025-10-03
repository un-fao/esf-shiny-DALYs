# user_profile.R
# ----------------------
# Set up environment to ensure portability and reproducibility
# ----------------------

# Load 'here' package early for path management
if (!require("here")) {
  install.packages("here")
}
library(here)

# List of required packages
required_packages <- c(
  "tidyverse",
  "tidylog",
  "data.table",
  "readxl",
  "DescTools",
  "writexl",
  "here",
  "rmarkdown",
  "dplyr",
  "readr",
  "zip",
  "gt",
  "plotly",
  "openxlsx",
  "httr",
  "RCurl",
  "shiny",
  "shinydashboard",
  "DT",
  "ggplot2"
)


# Install missing packages
installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
}

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Global options
options(stringsAsFactors = FALSE)
options(scipen = 999)


message("Environment setup completed")
