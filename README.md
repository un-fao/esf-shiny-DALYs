
# FAO Food Safety Risk Categorization Model

This repository contains an R Shiny application developed by the **Food and Agriculture Organization of the United Nations (FAO)**. The tool implements a food safety risk assessment model designed to calculate risk scores for Food Business Operators (FBOs), particularly in low- and middle-income countries.

## 📖 Project Context & Overview

This Shiny App is a key component of a wider FAO project focused on **risk categorization**. It is designed to support competent authorities in developing risk categorization-based inspection plans.

The tool works in tandem with other resources to provide a standardized, data-driven approach to food safety:

* **Wider Ecosystem:** The application will be hosted on a dedicated FAO website, currently in development, similar to the [GHP Toolbox](https://www.fao.org/good-hygiene-practices-haccp-toolbox/en) and [JECFA Toolbox](https://www.fao.org/jefca-toolbox-veterinary-drugs-assessment/en).
* **Resource Kit:** The app is referenced within a series of PDF modules that collectively form a **resource kit** for competent authorities.

### Methodology

The application is based on the methodology described in **Quessy et al. (2025)**. The model estimates the burden of foodborne disease, expressed in **Disability-Adjusted Life Years (DALYs)**, associated with a business's annual production and adjusts this score based on specific risk factors.

The core calculation logic follows this progression:

1.  **DALY Calculation:** Annual production volumes (kg) are multiplied by country-specific DALY rates for specific food groups and hazard categories.
2.  **Inherent Risk:** The base DALY score is multiplied by inherent risk factors (e.g., distributing to vulnerable populations).
3.  **Mitigated Risk:** The score is divided by mitigation factors (e.g., having a HACCP certification or potable water).
4.  **Final Risk Score:** The score is multiplied by compliance history factors (e.g., past non-conformities).

$$\text{Final Risk} = \left( \frac{\text{Total DALYs} \times \text{Inherent Multiplier}}{\text{Mitigation Multiplier}} \right) \times \text{Compliance Multiplier}$$

## ✨ Features

* **Country Specificity:** Loads country-specific DALY rates for precise calculation.
* **Production Volume Input:** Allows users to input annual production volumes (kg) across 10 food groups (e.g., Beef, Dairy, Vegetables) and 4 hazard categories:
    * *Grow:* Pathogens can grow (e.g., raw meat).
    * *Survive:* Pathogens survive but don't grow (e.g., raw produce).
    * *Decrease:* Treated to reduce pathogens (e.g., fermented).
    * *Kill:* Treated to eliminate pathogens (e.g., canned/cooked).
* **Risk Factor Assessment:** Interactive checklist for:
    * *Inherent Factors* (e.g., vulnerable consumers).
    * *Mitigation Factors* (e.g., training, sanitation, electricity).
    * *Compliance Factors* (e.g., inspection history).
* **Visualization:** Interactive bar charts and progression graphs showing how the risk score evolves from the base DALYs to the final score.

## 📂 Repository Structure

```text
esf-shiny-DALYs/
├── data/
│   ├── XLSX/
│   │   ├── dalys_values.xlsx   # DALY rates per country/food/hazard
│   │   └── risk_factors.xlsx   # Weights for risk factors
│   └── CSV/                    # Archive/Alternative formats
├── scripts/
│   ├── shiny.r                 # Main application code (UI & Server)
│   ├── deploy-app.R            # Script for deploying to shinyapps.io
│   └── data-inputs.r           # Utility to generate CSVs from raw Excel data
└── README.md                   # Project documentation
````

## 🚀 Installation and Usage

### Prerequisites

You need **R** and **RStudio** installed on your machine.

### Required Packages

Run the following code in R to install the necessary dependencies:

```r
install.packages(c("shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "readxl", "rsconnect"))
```

### Running the App Locally

1.  Clone this repository.
2.  Open `scripts/shiny.r` in RStudio.
3.  Click the **"Run App"** button in RStudio, or run the following command in the R console:

<!-- end list -->

```r
shiny::runApp('scripts/shiny.r')
```

*Note: Ensure your working directory is set to the project root folder so the app can locate the `data/` directory correctly.*

## 📝 How to Use

1.  **Select Country:** Choose the country of operation from the dropdown menu. This loads the specific epidemiological data for that region.
2.  **Input Quantities:** Enter the annual production volume (in kg) for the relevant food groups in the table rows corresponding to the process type (Grow, Survive, Decrease, Kill).
3.  **Risk Factors:** Answer "Yes" or "No" to the questions regarding:
      * **Inherent Risk:** Target audience and food nature.
      * **Mitigation:** Safety measures currently in place.
      * **Compliance:** Past inspection results and outbreak history.
4.  **Calculate:** Click the **"Calculate Risk Score"** button.
5.  **View Results:** The app will display the calculated Inherent, Mitigated, and Final Risk scores, along with visualizations of the risk progression.

## 🌐 Live Deployment

The application is deployed and accessible via shinyapps.io at:
[https://foodandagricultureorganization.shinyapps.io/fao-food-safety-risk-categorization-model/](https://foodandagricultureorganization.shinyapps.io/fao-food-safety-risk-categorization-model/)

## 📄 License and Credits

**Copyright © FAO 2025**

Food and Agriculture Organization of the United Nations.

  * **Contact:** food-quality@fao.org

-----

