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

  # Store user inputs (persistent across page changes)
  user_inputs <- reactiveValues(
    quantities = list(),
    risk_factors = list()
  )

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
    # Create table rows for product quantities
    table_rows <- list(
      # Header row
      tags$tr(
        tags$th("Product Type", style = "text-align: left; width: 400px;"),
        lapply(food_groups, function(fg) tags$th(fg))
      ),

      # Row 1: grow
      tags$tr(
        tags$td(
          class = "hazard-label",
          "A product where pathogenic micro-organisms can grow (e.g. meat, cheese) (kg)"
        ),
        lapply(food_groups, function(fg) {
          input_id <- paste0("qty_grow_", gsub(" ", "_", fg))
          tags$td(
            numericInput(
              input_id,
              label = NULL,
              value = if (!is.null(user_inputs$quantities[[input_id]])) {
                user_inputs$quantities[[input_id]]
              } else {
                0
              },
              min = 0,
              step = 0.1,
              width = "80px"
            )
          )
        })
      ),

      # Row 2: survive
      tags$tr(
        tags$td(
          class = "hazard-label",
          "A product where pathogenic micro-organism can survive (e.g. fruit, vegetable) (kg)"
        ),
        lapply(food_groups, function(fg) {
          input_id <- paste0("qty_survive_", gsub(" ", "_", fg))
          tags$td(
            numericInput(
              input_id,
              label = NULL,
              value = if (!is.null(user_inputs$quantities[[input_id]])) {
                user_inputs$quantities[[input_id]]
              } else {
                0
              },
              min = 0,
              step = 0.1,
              width = "80px"
            )
          )
        })
      ),

      # Row 3: decrease
      tags$tr(
        tags$td(
          class = "hazard-label",
          "A product that received treatment to decrease (e.g. fermented sausage) the pathogenic microbial load (kg)"
        ),
        lapply(food_groups, function(fg) {
          input_id <- paste0("qty_decrease_", gsub(" ", "_", fg))
          tags$td(
            numericInput(
              input_id,
              label = NULL,
              value = if (!is.null(user_inputs$quantities[[input_id]])) {
                user_inputs$quantities[[input_id]]
              } else {
                0
              },
              min = 0,
              step = 0.1,
              width = "80px"
            )
          )
        })
      ),

      # Row 4: kill
      tags$tr(
        tags$td(
          class = "hazard-label",
          "A product that received a treatment that kill (e.g. canned food, cooked RTE) all pathogenic microorganisms (kg)"
        ),
        lapply(food_groups, function(fg) {
          input_id <- paste0("qty_kill_", gsub(" ", "_", fg))
          tags$td(
            numericInput(
              input_id,
              label = NULL,
              value = if (!is.null(user_inputs$quantities[[input_id]])) {
                user_inputs$quantities[[input_id]]
              } else {
                0
              },
              min = 0,
              step = 0.1,
              width = "80px"
            )
          )
        })
      )
    )

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

      div(class = "food-table", do.call(tags$table, table_rows)),

      # Inherent Risk Factors
      div(class = "section-header", "Inherent Risk Factors"),
      lapply(
        1:nrow(risk_factors[risk_factors$factor_type == "inherent", ]),
        function(i) {
          factor <- risk_factors[risk_factors$factor_type == "inherent", ][i, ]
          input_id <- paste0("inherent_", factor$factor_id)
          selectInput(
            input_id,
            label = factor$factor_description,
            choices = c("Select..." = "", "Yes" = "yes", "No" = "no"),
            selected = if (!is.null(user_inputs$risk_factors[[input_id]])) {
              user_inputs$risk_factors[[input_id]]
            } else {
              ""
            }
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
          input_id <- paste0("mitigation_", factor$factor_id)
          selectInput(
            input_id,
            label = factor$factor_description,
            choices = c("Select..." = "", "Yes" = "yes", "No" = "no"),
            selected = if (!is.null(user_inputs$risk_factors[[input_id]])) {
              user_inputs$risk_factors[[input_id]]
            } else {
              ""
            }
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
          input_id <- paste0("compliance_", factor$factor_id)
          selectInput(
            input_id,
            label = factor$factor_description,
            choices = c("Select..." = "", "Yes" = "yes", "No" = "no"),
            selected = if (!is.null(user_inputs$risk_factors[[input_id]])) {
              user_inputs$risk_factors[[input_id]]
            } else {
              ""
            }
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

      # Section 1: Final Risk Results (MOVED TO TOP)
      fluidRow(
        column(
          4,
          div(
            style = "background-color: #d9edf7; padding: 20px; border-radius: 5px; text-align: center;",
            h4("Inherent Risk"),
            h2(
              style = "color: #31708f;",
              format(results$inherent_risk, scientific = FALSE, digits = 8)
            )
          )
        ),
        column(
          4,
          div(
            style = "background-color: #fcf8e3; padding: 20px; border-radius: 5px; text-align: center;",
            h4("Mitigated Risk"),
            h2(
              style = "color: #8a6d3b;",
              format(results$mitigated_risk, scientific = FALSE, digits = 8)
            )
          )
        ),
        column(
          4,
          div(
            style = "background-color: #f2dede; padding: 20px; border-radius: 5px; text-align: center;",
            h4("Final Risk"),
            h2(
              style = "color: #a94442;",
              format(results$final_risk, scientific = FALSE, digits = 8)
            )
          )
        )
      ),

      hr(),

      # Section 2: Product Type and DALYs
      h4("Product Type"),
      DTOutput("productTypeTable"),

      hr(),

      # Section 3: Risk Factor Clusters
      h4("Risk Factor Cluster"),
      DTOutput("riskClusterTable"),

      hr(),

      # Section 4: Visualizations
      h4("Visualizations"),
      fluidRow(
        column(6, plotOutput("dalysBarPlot", height = "400px")),
        column(6, plotOutput("riskMultiplierPlot", height = "400px"))
      ),

      br(),

      fluidRow(
        column(12, plotOutput("riskProgressionPlot", height = "300px"))
      )
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
      # Save all inputs before moving to results page
      saveInputs()
      validation_errors(NULL)
      current_page("results")
    }
  })

  # Save user inputs to persistent storage
  saveInputs <- function() {
    # Save quantities
    for (fg in food_groups) {
      fg_clean <- gsub(" ", "_", fg)
      for (hz in names(hazard_types)) {
        input_id <- paste0("qty_", hz, "_", fg_clean)
        if (!is.null(input[[input_id]])) {
          user_inputs$quantities[[input_id]] <- input[[input_id]]
        }
      }
    }

    # Save risk factors
    for (i in 1:nrow(risk_factors)) {
      input_id <- paste0(
        risk_factors$factor_type[i],
        "_",
        risk_factors$factor_id[i]
      )
      if (!is.null(input[[input_id]])) {
        user_inputs$risk_factors[[input_id]] <- input[[input_id]]
      }
    }
  }

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

        if (!is.null(qty) && !is.na(qty) && qty > 0) {
          daly_rate <- country_dalys[country_dalys$food_group == fg_match, hz]

          # Check if daly_rate is valid
          if (length(daly_rate) > 0 && !is.na(daly_rate)) {
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
    }

    # Calculate risk factor score using MULTIPLICATION (Excel method)
    # If factor is NOT selected: multiply by 1 (no effect)
    # If factor IS selected: multiply by the weight

    inherent_multiplier <- 1
    mitigation_multiplier <- 1
    compliance_multiplier <- 1

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
      weight <- country_risk_factors$weight[i]

      # If the answer is "yes" (marked with x in Excel), use the weight
      # If the answer is "no" or blank, use 1 (no effect on multiplication)
      if (!is.null(response) && response == "yes") {
        if (country_risk_factors$factor_type[i] == "inherent") {
          inherent_multiplier <- inherent_multiplier * weight
        } else if (country_risk_factors$factor_type[i] == "mitigation") {
          mitigation_multiplier <- mitigation_multiplier * weight
        } else if (country_risk_factors$factor_type[i] == "compliance") {
          compliance_multiplier <- compliance_multiplier * weight
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

    # Calculate final risk using Excel methodology (MULTIPLICATION)
    # Inherent Risk = Total DALYs × Inherent_multiplier
    inherent_risk <- total_dalys * inherent_multiplier

    # Mitigated Risk = Total DALYs × Inherent_multiplier / Mitigation_multiplier
    mitigated_risk <- (total_dalys * inherent_multiplier) /
      mitigation_multiplier

    # Final Risk = Total DALYs × Inherent_multiplier / Mitigation_multiplier × Compliance_multiplier
    final_risk <- (total_dalys * inherent_multiplier / mitigation_multiplier) *
      compliance_multiplier

    list(
      total_dalys = total_dalys,
      inherent_multiplier = inherent_multiplier,
      mitigation_multiplier = mitigation_multiplier,
      compliance_multiplier = compliance_multiplier,
      inherent_risk = inherent_risk,
      mitigated_risk = mitigated_risk,
      final_risk = final_risk,
      dalys_by_group = dalys_by_group,
      risk_summary = risk_summary,
      risk_clusters = data.frame(
        cluster = c(
          "Inherent Factors",
          "Mitigation Factors",
          "Compliance Factors"
        ),
        combined_weight = c(
          inherent_multiplier,
          mitigation_multiplier,
          compliance_multiplier
        )
      )
    )
  }

  # DALYs Bar Plot
  output$dalysBarPlot <- renderPlot({
    results <- calculateResults()

    if (!is.null(results$dalys_by_group) && nrow(results$dalys_by_group) > 0) {
      dalys_summary <- results$dalys_by_group %>%
        group_by(food_group) %>%
        summarise(total = sum(total_dalys, na.rm = TRUE)) %>%
        arrange(desc(total)) %>%
        filter(total > 0)

      if (nrow(dalys_summary) > 0) {
        ggplot(
          dalys_summary,
          aes(x = reorder(food_group, total), y = total, fill = food_group)
        ) +
          geom_bar(stat = "identity", show.legend = FALSE) +
          coord_flip() +
          labs(
            title = "DALYs Contribution by Food Group",
            x = "Food Group",
            y = "DALYs"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y = element_text(size = 11)
          ) +
          scale_fill_manual(
            values = c(
              "#b3d9e6",
              "#c2e0b3",
              "#f5d6a8",
              "#e8c4d4",
              "#d4c8e0",
              "#c8e0d8",
              "#f0dcc8",
              "#e0d8c8",
              "#d8e8e0",
              "#e0e0d8",
              "#d8d8e0"
            )
          ) +
          geom_text(
            aes(label = format(total, scientific = FALSE, digits = 6)),
            hjust = -0.1,
            size = 3.5
          )
      } else {
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No product quantities entered",
            size = 6
          ) +
          theme_void()
      }
    } else {
      ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "No product quantities entered",
          size = 6
        ) +
        theme_void()
    }
  })

  # Risk Multiplier Plot
  output$riskMultiplierPlot <- renderPlot({
    results <- calculateResults()

    multiplier_data <- data.frame(
      cluster = c("Inherent", "Mitigation", "Compliance"),
      multiplier = c(
        results$inherent_multiplier,
        results$mitigation_multiplier,
        results$compliance_multiplier
      ),
      color = c("#a8c7d9", "#d9c89e", "#d9a8a8")
    )

    ggplot(multiplier_data, aes(x = cluster, y = multiplier, fill = cluster)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = multiplier_data$color) +
      labs(
        title = "Risk Factor Multipliers",
        x = "Risk Factor Cluster",
        y = "Multiplier Value"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 12)
      ) +
      geom_text(
        aes(label = format(multiplier, digits = 4)),
        vjust = -0.5,
        size = 5,
        fontface = "bold",
        color = "#333333"
      ) +
      ylim(0, max(multiplier_data$multiplier) * 1.15)
  })

  # Risk Progression Plot
  output$riskProgressionPlot <- renderPlot({
    results <- calculateResults()

    progression_data <- data.frame(
      stage = factor(
        c("Total DALYs", "Inherent Risk", "Mitigated Risk", "Final Risk"),
        levels = c(
          "Total DALYs",
          "Inherent Risk",
          "Mitigated Risk",
          "Final Risk"
        )
      ),
      value = c(
        results$total_dalys,
        results$inherent_risk,
        results$mitigated_risk,
        results$final_risk
      ),
      color = c("#cccccc", "#a8c7d9", "#d9c89e", "#d9a8a8")
    )

    ggplot(progression_data, aes(x = stage, y = value, fill = stage)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = progression_data$color) +
      labs(
        title = "Risk Calculation Progression",
        x = "",
        y = "Risk Value (DALYs)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, size = 11)
      ) +
      geom_text(
        aes(label = format(value, scientific = FALSE, digits = 6)),
        vjust = -0.5,
        size = 4,
        fontface = "bold",
        color = "#333333"
      ) +
      ylim(0, max(progression_data$value) * 1.2)
  })

  # Product Type table
  output$productTypeTable <- renderDT({
    results <- calculateResults()
    if (!is.null(results$dalys_by_group) && nrow(results$dalys_by_group) > 0) {
      # Aggregate by food group
      product_summary <- results$dalys_by_group %>%
        group_by(food_group) %>%
        summarise(DALYs = sum(total_dalys, na.rm = TRUE)) %>%
        arrange(desc(DALYs))

      # Add total row
      total_row <- data.frame(
        food_group = "TOTAL",
        DALYs = sum(product_summary$DALYs, na.rm = TRUE)
      )
      product_summary <- rbind(product_summary, total_row)

      colnames(product_summary) <- c("Product Type", "DALYs")

      datatable(
        product_summary,
        options = list(pageLength = 15, dom = 't'),
        rownames = FALSE
      ) %>%
        formatRound(columns = "DALYs", digits = 8)
    } else {
      empty_df <- data.frame(
        `Product Type` = "No data",
        DALYs = 0,
        check.names = FALSE
      )
      datatable(empty_df, options = list(dom = 't'), rownames = FALSE)
    }
  })

  # Risk Cluster table
  output$riskClusterTable <- renderDT({
    results <- calculateResults()

    cluster_data <- results$risk_clusters
    colnames(cluster_data) <- c("Risk Factor Cluster", "Combined Weight")

    datatable(
      cluster_data,
      options = list(pageLength = 3, dom = 't'),
      rownames = FALSE
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
