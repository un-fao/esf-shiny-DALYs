# Script to create CSV database from Excel settings sheet
# This creates a settings database for the Food Safety Risk Assessment Shiny App

library(readxl)

# Read the DALYs Calculations sheet (3rd sheet)
excel_file <- "data/Quessy et al, 2025_Supplementary material H_example.xlsx"
dalys_data <- read_excel(excel_file, sheet = "DALYs Calculations")

# Extract DALYs values by food group and hazard type (rows 3-13)
dalys_matrix <- dalys_data[2:12, ]
colnames(dalys_matrix) <- c("food_group", "grow", "survive", "decrease", "kill")

# Extract risk factor weights
# Inherent factors (rows 18-19)
inherent_factors <- data.frame(
  factor_type = "inherent",
  factor_id = c("vulnerable_distribution", "vulnerable_food"),
  factor_description = c(
    "Direct distribution to a vulnerable population (e.g. distribution in hospital, elderly facilities, baby care unit, ...)",
    "Food made for a vulnerable population (such as Baby Food)"
  ),
  weight = c(3, 2)
)

# Mitigation factors (rows 22-29)
mitigation_factors <- data.frame(
  factor_type = "mitigation",
  factor_id = c(
    "certification",
    "training",
    "export_recognition",
    "potable_water",
    "sanitary_facilities",
    "handwashing_facilities",
    "electrical_power",
    "cooling_equipment"
  ),
  factor_description = c(
    "Certification of the business food safety control system: Accreditation of the food safety control system (e.g. HACCP, ISO 22000, ...) by a reputable body",
    "An adequate Food Safety Training program (e.g. PAHO Food Handlers Manual) for employees (manipulating/processing food) is in place",
    "Recognition of the business by a relevant authority to export products to other countries",
    "Access to potable water, as demonstrated by annual on site test results or by municipal authorities",
    "Presence of and access by the personnel of functional and adequately maintained sanitary facilities (e.g. toilets)",
    "Access by the personnel to functional and adequately maintained hand washing facilities",
    "Presence and continuous access to electrical power",
    "Presence and continuous access of sufficient cooling equipments according to the expected need (e.g. adequate cooling system if the product needs to be cool)"
  ),
  weight = rep(2, 8)
)

# Compliance factors (rows 32-35)
compliance_factors <- data.frame(
  factor_type = "compliance",
  factor_id = c(
    "current_nonconformity",
    "past_nonconformity",
    "enforcement_history",
    "outbreak_linked"
  ),
  factor_description = c(
    "Inspection results. Presence of at least one important non-conformity in the current visit directly linked to food safety",
    "Inspection results. Presence of at least one important non-conformity in the last visit directly linked to food safety",
    "History of enforcement actions in the past 5 years: was this business targeted by an enforcement action such as permit suspension or temporary cessation of activities by inspection authorities",
    "Was this business linked in the past 3 years to a documented food safety related outbreak?"
  ),
  weight = c(3, 4, 5, 2)
)

# Combine all risk factors
risk_factors <- rbind(inherent_factors, mitigation_factors, compliance_factors)

# Add country column
risk_factors$country <- "Zimbabwe"
dalys_matrix$country <- "Zimbabwe"

# Reorder columns to have country first
risk_factors <- risk_factors[, c(
  "country",
  "factor_type",
  "factor_id",
  "factor_description",
  "weight"
)]
dalys_matrix <- dalys_matrix[, c(
  "country",
  "food_group",
  "grow",
  "survive",
  "decrease",
  "kill"
)]

# Save to CSV files
write.csv(dalys_matrix, "data/dalys_values.csv", row.names = FALSE)
write.csv(risk_factors, "data/risk_factors.csv", row.names = FALSE)

cat("CSV files created successfully!\n")
cat("- dalys_values.csv: Contains DALYs values by food group and hazard type\n")
cat("- risk_factors.csv: Contains all risk factors with their weights\n")

# Display preview
cat("\n=== Preview of dalys_values.csv ===\n")
print(head(dalys_matrix))

cat("\n=== Preview of risk_factors.csv ===\n")
print(head(risk_factors, 10))
