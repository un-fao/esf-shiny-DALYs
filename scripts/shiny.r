# Food Safety Risk Assessment Shiny App
# Based on Quessy et al 2025 methodology

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

# Load settings from CSV files
dalys_values <- read.csv("data/dalys_values.csv", stringsAsFactors = FALSE)
risk_factors <- read.csv("data/risk_factors.csv", stringsAsFactors = FALSE)

# Get list of available countries
available_countries <- unique(dalys_values$country)

# Define food groups
food_groups <- c(
  "Lamb",
  "Beef",
  "Pork",
  "Poultry",
  "Eggs",
  "Dairy",
  "Fish and shellfish",
  "Fruit and vegetables",
  "Beverages",
  "Grains",
  "Other foods"
)

# Define hazard types (short labels for table)
hazard_types <- list(
  grow = "Product where pathogens can grow (e.g. meat, cheese)",
  survive = "Product where pathogens can survive (e.g. fruit, vegetable)",
  decrease = "Product treated to decrease pathogens (e.g. fermented)",
  kill = "Product treated to kill all pathogens (e.g. canned, cooked RTE)"
)

# UI
ui <- fluidPage(
  titlePanel("Food Safety Risk Assessment Tool"),

  tags$head(
    tags$style(HTML(
      "
      .btn-validate { 
        background-color: #337ab7; 
        color: white; 
        font-size: 18px; 
        padding: 10px 30px;
        margin-top: 20px;
      }
      .section-header {
        background-color: #f5f5f5;
        padding: 10px;
        margin-top: 20px;
        margin-bottom: 10px;
        font-weight: bold;
        border-left: 4px solid #337ab7;
      }
      .error-message {
        color: red;
        font-weight: bold;
        margin-top: 10px;
      }
      .food-table {
        margin-top: 20px;
      }
      .food-table table {
        width: 100%;
        border-collapse: collapse;
      }
      .food-table th {
        background-color: #337ab7;
        color: white;
        padding: 10px;
        text-align: center;
        border: 1px solid #ddd;
      }
      .food-table td {
        padding: 5px;
        text-align: center;
        border: 1px solid #ddd;
      }
      .food-table input[type='number'] {
        width: 80px;
        padding: 5px;
        text-align: center;
      }
      .hazard-label {
        background-color: #f5f5f5;
        padding: 8px;
        font-weight: bold;
        text-align: left;
      }
    "
    ))
  ),

  uiOutput("mainContent")
)

# Server
server <- function(input, output, session) {
  # Reactive value to track current page
  current_page <- reactiveVal("questionnaire")

  # Reactive value for selected country
  selected_country <- reactiveVal(available_countries[1])

  # Store validation errors
  validation_errors <- reactiveVal(NULL)

  # Render main content based on current page
  output$mainContent <- renderUI({
    if (current_page() == "questionnaire") {
      questionnaireUI()
    } else {
      resultsUI()
    }
  })

  # Questionnaire UI
  questionnaireUI <- function() {
    fluidPage(
      h3("Food Safety Questionnaire"),

      # Country selection
      fluidRow(
        column(
          4,
          selectInput(
            "country_select",
            label = h4("Select Country:"),
            choices = available_countries,
            selected = selected_country()
          )
        ),
        column(
          8,
          div(
            style = "padding-top: 30px;",
            p("Please complete all required fields before proceeding.")
          )
        )
      ),

      hr(),

      # Product quantities section - Excel-style table
      div(class = "section-header", "Product Type (in kg)"),

      div(
        class = "food-table",
        tags$table(
          # Header row with food groups
          tags$tr(
            tags$th("Product Type", style = "text-align: left; width: 400px;"),
            lapply(food_groups, function(fg) {
              tags$th(fg)
            })
          ),

          # Row 1: Products where pathogens can grow
          tags$tr(
            tags$td(
              class = "hazard-label",
              "A product where pathogenic micro-organisms can grow (e.g. meat, cheese) (kg)"
            ),
            lapply(food_groups, function(fg) {
              tags$td(
                numericInput(
                  paste0("qty_grow_", gsub(" ", "_", fg)),
                  label = NULL,
                  value = 0,
                  min = 0,
                  step = 0.1,
                  width = "80px"
                )
              )
            })
          ),

          # Row 2: Products where pathogens can survive
          tags$tr(
            tags$td(
              class = "hazard-label",
              "A product where pathogenic micro-organism can survive (e.g. fruit, vegetable) (kg)"
            ),
            lapply(food_groups, function(fg) {
              tags$td(
                numericInput(
                  paste0("qty_survive_", gsub(" ", "_", fg)),
                  label = NULL,
                  value = 0,
                  min = 0,
                  step = 0.1,
                  width = "80px"
                )
              )
            })
          ),

          # Row 3: Products treated to decrease pathogens
          tags$tr(
            tags$td(
              class = "hazard-label",
              "A product that received treatment to decrease (e.g. fermented sausage) the pathogenic microbial load (kg)"
            ),
            lapply(food_groups, function(fg) {
              tags$td(
                numericInput(
                  paste0("qty_decrease_", gsub(" ", "_", fg)),
                  label = NULL,
                  value = 0,
                  min = 0,
                  step = 0.1,
                  width = "80px"
                )
              )
            })
          ),

          # Row 4: Products treated to kill all pathogens
          tags$tr(
            tags$td(
              class = "hazard-label",
              "A product that received a treatment that kill (e.g. canned food, cooked RTE) all pathogenic microorganisms (kg)"
            ),
            lapply(food_groups, function(fg) {
              tags$td(
                numericInput(
                  paste0("qty_kill_", gsub(" ", "_", fg)),
                  label = NULL,
                  value = 0,
                  min = 0,
                  step = 0.1,
                  width = "80px"
                )
              )
            })
          )
        )
      ),

      # Inherent Risk Factors
      div(class = "section-header", "Inherent Risk Factors"),
      lapply(
        1:nrow(risk_factors[risk_factors$factor_type == "inherent", ]),
        function(i) {
          factor <- risk_factors[risk_factors$factor_type == "inherent", ][i, ]
          selectInput(
            paste0("inherent_", factor$factor_id),
            label = factor$factor_description,
            choices = c("Select..." = "", "Yes" = "yes", "No" = "no"),
            selected = ""
          )
        }
      ),

      # Mitigation Factors
      div(class = "section-header", "Mitigation Factors"),
      p("(When present, these factors reduce the risk)"),
      lapply(
        1:nrow(risk_factors[risk_factors$factor_type == "mitigation", ]),
        function(i) {
          factor <- risk_factors[risk_factors$factor_type == "mitigation", ][
            i,
          ]
          selectInput(
            paste0("mitigation_", factor$factor_id),
            label = factor$factor_description,
            choices = c("Select..." = "", "Yes" = "yes", "No" = "no"),
            selected = ""
          )
        }
      ),

      # Compliance Factors
      div(class = "section-header", "Compliance Factors"),
      lapply(
        1:nrow(risk_factors[risk_factors$factor_type == "compliance", ]),
        function(i) {
          factor <- risk_factors[risk_factors$factor_type == "compliance", ][
            i,
          ]
          selectInput(
            paste0("compliance_", factor$factor_id),
            label = factor$factor_description,
            choices = c("Select..." = "", "Yes" = "yes", "No" = "no"),
            selected = ""
          )
        }
      ),

      # Error message display
      uiOutput("errorMessages"),

      # Validate button
      actionButton(
        "validateBtn",
        "Calculate Risk Assessment",
        class = "btn-validate"
      )
    )
  }

  # Results UI
  resultsUI <- function() {
    results <- calculateResults()

    fluidPage(
      h3(paste("Risk Assessment Results -", selected_country())),

      actionButton("backBtn", "← Back to Questionnaire"),

      hr(),

      # Summary boxes
      fluidRow(
        column(
          4,
          div(
            style = "background-color: #d9edf7; padding: 20px; border-radius: 5px; text-align: center;",
            h4("Total DALYs"),
            h2(
              style = "color: #31708f;",
              format(results$total_dalys, scientific = FALSE, digits = 6)
            )
          )
        ),
        column(
          4,
          div(
            style = "background-color: #fcf8e3; padding: 20px; border-radius: 5px; text-align: center;",
            h4("Risk Factor Score"),
            h2(style = "color: #8a6d3b;", results$risk_factor_score)
          )
        ),
        column(
          4,
          div(
            style = "background-color: #f2dede; padding: 20px; border-radius: 5px; text-align: center;",
            h4("Final Risk Level"),
            h2(
              style = "color: #a94442;",
              format(results$final_risk, scientific = FALSE, digits = 6)
            )
          )
        )
      ),

      hr(),

      # Visualizations
      fluidRow(
        column(
          6,
          h4("DALYs by Food Group"),
          plotOutput("dalysPlot", height = "400px")
        ),
        column(
          6,
          h4("Risk Factors Contribution"),
          plotOutput("riskFactorsPlot", height = "400px")
        )
      ),

      hr(),

      # Detailed tables
      h4("Detailed DALYs Breakdown by Food Group"),
      DTOutput("dalysTable"),

      br(),

      h4("Risk Factors Summary"),
      DTOutput("riskFactorsTable")
    )
  }

  # Validation function
  validateInputs <- function() {
    errors <- c()

    # Check all select inputs
    all_factors <- risk_factors$factor_id
    all_types <- risk_factors$factor_type

    for (i in 1:nrow(risk_factors)) {
      input_id <- paste0(all_types[i], "_", all_factors[i])
      if (is.null(input[[input_id]]) || input[[input_id]] == "") {
        errors <- c(
          errors,
          paste("Please answer:", risk_factors$factor_description[i])
        )
      }
    }

    return(errors)
  }

  # Display error messages
  output$errorMessages <- renderUI({
    errors <- validation_errors()
    if (!is.null(errors) && length(errors) > 0) {
      div(
        class = "error-message",
        h4("Please complete the following fields:"),
        tags$ul(
          lapply(errors, function(e) tags$li(e))
        )
      )
    }
  })

  # Validate button click
  observeEvent(input$validateBtn, {
    errors <- validateInputs()

    if (length(errors) > 0) {
      validation_errors(errors)
    } else {
      validation_errors(NULL)
      current_page("results")
    }
  })

  # Back button click
  observeEvent(input$backBtn, {
    current_page("questionnaire")
  })

  # Update selected country when changed
  observeEvent(input$country_select, {
    selected_country(input$country_select)
  })

  # Calculate results
  calculateResults <- function() {
    # Get country-specific data
    country <- selected_country()
    country_dalys <- dalys_values[dalys_values$country == country, ]
    country_risk_factors <- risk_factors[risk_factors$country == country, ]
    # Calculate DALYs for each food group
    dalys_by_group <- data.frame(
      food_group = character(),
      hazard_type = character(),
      quantity = numeric(),
      daly_rate = numeric(),
      total_dalys = numeric(),
      stringsAsFactors = FALSE
    )

    total_dalys <- 0

    for (fg in food_groups) {
      fg_clean <- gsub(" ", "_", fg)
      fg_match <- country_dalys$food_group[grepl(
        fg,
        country_dalys$food_group,
        ignore.case = TRUE
      )][1]

      for (hz in names(hazard_types)) {
        input_id <- paste0("qty_", hz, "_", fg_clean)
        qty <- input[[input_id]]

        if (!is.null(qty) && qty > 0) {
          daly_rate <- country_dalys[country_dalys$food_group == fg_match, hz]
          dalys <- qty * daly_rate
          total_dalys <- total_dalys + dalys

          dalys_by_group <- rbind(
            dalys_by_group,
            data.frame(
              food_group = fg,
              hazard_type = hazard_types[[hz]],
              quantity = qty,
              daly_rate = daly_rate,
              total_dalys = dalys
            )
          )
        }
      }
    }

    # Calculate risk factor score
    risk_score <- 0
    risk_summary <- data.frame(
      factor_type = character(),
      factor = character(),
      present = character(),
      weight = numeric(),
      stringsAsFactors = FALSE
    )

    for (i in 1:nrow(country_risk_factors)) {
      input_id <- paste0(
        country_risk_factors$factor_type[i],
        "_",
        country_risk_factors$factor_id[i]
      )
      response <- input[[input_id]]

      if (!is.null(response) && response == "yes") {
        weight <- country_risk_factors$weight[i]
        if (country_risk_factors$factor_type[i] == "mitigation") {
          risk_score <- risk_score - weight # Mitigation reduces risk
        } else {
          risk_score <- risk_score + weight # Inherent and compliance increase risk
        }

        risk_summary <- rbind(
          risk_summary,
          data.frame(
            factor_type = country_risk_factors$factor_type[i],
            factor = country_risk_factors$factor_description[i],
            present = "Yes",
            weight = weight
          )
        )
      }
    }

    # Calculate final risk
    final_risk <- total_dalys * (1 + risk_score / 10)

    list(
      total_dalys = total_dalys,
      risk_factor_score = risk_score,
      final_risk = final_risk,
      dalys_by_group = dalys_by_group,
      risk_summary = risk_summary
    )
  }

  # DALYs plot
  output$dalysPlot <- renderPlot({
    results <- calculateResults()

    if (nrow(results$dalys_by_group) > 0) {
      dalys_summary <- results$dalys_by_group %>%
        group_by(food_group) %>%
        summarise(total = sum(total_dalys)) %>%
        arrange(desc(total))

      ggplot(
        dalys_summary,
        aes(x = reorder(food_group, total), y = total, fill = food_group)
      ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "Food Group", y = "Total DALYs", title = "") +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_fill_brewer(palette = "Set3")
    }
  })

  # Risk factors plot
  output$riskFactorsPlot <- renderPlot({
    results <- calculateResults()

    if (nrow(results$risk_summary) > 0) {
      risk_plot_data <- results$risk_summary %>%
        mutate(
          impact = ifelse(factor_type == "mitigation", -weight, weight),
          factor_short = substr(factor, 1, 50)
        )

      ggplot(
        risk_plot_data,
        aes(x = reorder(factor_short, impact), y = impact, fill = factor_type)
      ) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "", y = "Risk Impact", title = "") +
        theme_minimal() +
        scale_fill_manual(
          values = c(
            "inherent" = "#d9534f",
            "mitigation" = "#5cb85c",
            "compliance" = "#f0ad4e"
          ),
          labels = c("Inherent", "Mitigation", "Compliance")
        ) +
        theme(legend.position = "bottom", legend.title = element_blank())
    }
  })

  # DALYs table
  output$dalysTable <- renderDT({
    results <- calculateResults()
    if (nrow(results$dalys_by_group) > 0) {
      datatable(
        results$dalys_by_group,
        options = list(pageLength = 10),
        rownames = FALSE
      ) %>%
        formatRound(
          columns = c("quantity", "daly_rate", "total_dalys"),
          digits = 6
        )
    }
  })

  # Risk factors table
  output$riskFactorsTable <- renderDT({
    results <- calculateResults()
    if (nrow(results$risk_summary) > 0) {
      datatable(
        results$risk_summary,
        options = list(pageLength = 10),
        rownames = FALSE
      )
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
