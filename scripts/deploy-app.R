# Deploying the shiny app to shinyapps.io

install.package("rsconnect")
# install.package("openssl@1.4.5")


rsconnect::deployApp(account = "foodandagricultureorganization",server = "shinyapps.io",
appPrimaryDoc = "scripts/shiny.r",
appFiles = c("scripts/shiny.r","data/dalys_values.csv","data/risk_factors.csv"), 
appName = "esf-shiny-DALYs")


# For FAO internal server


rsconnect::deployApp(account = "luc.warde@fao.org",server = "https://rstudio.fao.org",
appPrimaryDoc = "scripts/shiny.r",
appFiles = c("scripts/shiny.r","data/dalys_values.csv","data/risk_factors.csv"), 
appName = "esf-shiny-DALYs")


# Possible servers:

https://rstudio.fao.org 
https://rstudio.qa..fao.org

shinyapps.io  
