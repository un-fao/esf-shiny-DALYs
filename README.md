Food Safety Risk Assessment Shiny Application

Implements an interactive web application for food safety risk assessment based on
Quessy et al. (2025) methodology. The tool calculates DALYs (Disability-Adjusted
Life Years) and risk scores for food businesses.

Features:
- Country-specific risk assessment with expandable database
- Excel-style product quantity input table for 11 food groups
- Four pathogen hazard type categories (grow, survive, decrease, kill)
- Comprehensive risk factor questionnaire:
  * Inherent factors (vulnerable population distribution)
  * Mitigation factors (HACCP, training, facilities)
  * Compliance factors (inspection results, enforcement history)
- Interactive visualizations and detailed reporting
- Mandatory field validation before submission

Project structure:
- data/: CSV databases and original Excel template
- scripts/: R scripts for data preparation and Shiny app
- README.md: Comprehensive documentation

Technical stack:
- R Shiny for web application
- ggplot2 for visualizations
- DT for interactive tables
- dplyr for data manipulation

Based on Zimbabwe example from Quessy et al. (2025) Supplementary Material H.
Designed for food safety inspectors and regulatory agencies.
