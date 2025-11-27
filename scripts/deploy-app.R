# Deploying the shiny app to shinyapps.io

install.package("rsconnect")

# For FAO internal server
rsconnect::deployApp(
  account = "foodandagricultureorganization",
  server = "shinyapps.io",
  appPrimaryDoc = "scripts/shiny.r",
  appFiles = c(
    "scripts/shiny.r",
    "data/XLSX/dalys_values.xlsx",
    "data/XLSX/risk_factors.xlsx"
  ),
  appName = "fao-food-safety-risk-categorization-model" #"esf-shiny-DALYs"
)
