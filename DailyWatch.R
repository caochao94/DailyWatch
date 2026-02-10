# ==============================================================================
# DailyWatch | Clinical Wearable Analytics
# Author: Chao Cao (chao_cao@dfci.harvard.edu)
# ==============================================================================

# --- 0. Global Configuration ---
options(shiny.maxRequestSize = 100 * 1024^2) # 100MB Upload Limit

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(bslib)
library(nlme)
library(emmeans)
library(tidyr)
library(factoextra)
library(sjPlot)
library(readxl)
library(haven)
library(cluster)
library(nnet) 

# --- 1. DFCI Brand Colors & Themes ---
DFCI_NAVY     <- "#003B71"
DFCI_RED      <- "#E31837"
DFCI_BLUE     <- "#00AEEF"
DFCI_PALETTE <- c(DFCI_NAVY, DFCI_RED, DFCI_BLUE, "#373A36", "#F2A900", "#63666A")

my_theme <- bs_theme(
  version = 5,
  bootswatch = "pulse",
  primary = DFCI_NAVY,
  secondary = DFCI_RED,
  success = DFCI_BLUE,
  base_font = font_google("Inter")
)

# --- 2. UI Section ---
ui <- page_sidebar(
  theme = my_theme,
  title = "DailyWatch | Clinical Wearable Analytics",
  
  header = tags$head(
    tags$style(HTML(paste0("
      :root { --bs-primary: ", DFCI_NAVY, " !important; --bs-secondary: ", DFCI_RED, " !important; }
      .navbar { background-color: ", DFCI_NAVY, " !important; }
      .btn-primary { background-color: ", DFCI_NAVY, " !important; border-color: ", DFCI_NAVY, " !important; }
      .btn-secondary { background-color: ", DFCI_RED, " !important; border-color: ", DFCI_RED, " !important; }
      .nav-pills .nav-link.active { background-color: ", DFCI_NAVY, " !important; }
      .card-header { background-color: ", DFCI_NAVY, " !important; color: white !important; font-weight: 600; }
    ")))
  ),
  
  sidebar = sidebar(
    title = "Model Configuration",
    width = 350,
    bg = "#F8F9FA",
    
    accordion(
      accordion_panel("1. Data Upload", icon = icon("cloud-upload-alt"), 
                      fileInput("file", NULL, accept = c(".csv", ".xlsx", ".xls", ".sas7bdat", ".sav", ".dta")), 
                      uiOutput("var_selector")),
      
      accordion_panel("2. Relative Day Range", icon = icon("ruler-horizontal"), 
                      numericInput("hard_min", "Min:", value = -7), 
                      numericInput("hard_max", "Max:", value = 60)),
      
      accordion_panel("3. View Mode", icon = icon("users"), 
                      radioButtons("view_type", NULL, choices = c("Group Analysis" = "all", "Specific Participant" = "single")), 
                      conditionalPanel(condition = "input.view_type == 'single'", 
                                       selectInput("selected_id", "Choose Participant:", choices = NULL))),
      
      accordion_panel("4. Trajectory Analysis", icon = icon("chart-line"), 
                      radioButtons("trend_method", "Model Type:", choices = c("Raw Average" = "raw", "Mixed-Effect Model" = "lmm")), 
                      selectInput("bin_size", "Smoothing (Average Every):", choices = c("1 Day" = 1, "3 Days" = 3, "7 Days" = 7, "30 Days" = 30), selected = 1), 
                      selectInput("traj_group", "Group Trajectory By:", choices = c("None")), 
                      uiOutput("dynamic_slider_ui"), tags$hr(), 
                      tags$b("Y-Axis Range"), uiOutput("y_axis_range_ui"), tags$hr(), 
                      checkboxInput("show_individual", "Show Individual Lines", value = FALSE)),
      
      accordion_panel("5. Prediction Modeling", icon = icon("microscope"), 
                      uiOutput("manual_predictor_ui"), 
                      actionButton("run_prediction", "Execute Modeling", class = "btn-primary w-100 mt-2")),
      
      accordion_panel("6. Cluster Analysis", icon = icon("project-diagram"), 
                      conditionalPanel(condition = "input.view_type == 'all'", 
                                       numericInput("clusters", "Clusters (k):", value = 3, min = 2, max = 6), 
                                       actionButton("run_cluster", "Execute Clustering", class = "btn-primary w-100"))),
      
      accordion_panel("7. Cluster Determinants", icon = icon("dna"),
                      uiOutput("cluster_predictor_ui"),
                      actionButton("run_multinom", "Execute Modeling", class = "btn-primary w-100 mt-2"))
    ),
    
    hr(),
    downloadButton("downloadData", "Export Processed Data", class = "btn-primary w-100"),
    downloadButton("downloadModel", "Export Raw Model Results", class = "btn-secondary w-100 mt-2"),
    
    tags$div(
      style = "margin-top: 20px; font-size: 11px; color: #7f8c8d; text-align: center;",
      tags$hr(),
      tags$p(tags$b("Contact Author:"), tags$a(href="mailto:chao_cao@dfci.harvard.edu", "Chao Cao", style="color: #003B71; text-decoration: none;")),
      "© 2026 | Version 1.2.0", tags$br(), 
      "Dr. Chao Cao,", tags$br(), 
      "Dr. Kathryn Schmitz,", tags$br(), 
      "Dr. Jennifer Ligibel"
    )
  ),
  
  navset_card_pill(
    id = "main_tabs",
    
    nav_panel(
      title = "About DailyWatch", 
      icon = icon("info-circle"), 
      card(
        card_header("DailyWatch Methodology"), 
        layout_column_wrap(
          width = 1/2, 
          card(
            card_header("Wearable Data Ecosystem"), 
            tags$p("DailyWatch is a tool designed to analyze daily tracking data from diverse wearable technologies:"), 
            tags$ul(
              tags$li(tags$b("Research Accelerometry:"), " Daily summary data (ActiGraph)."), 
              tags$li(tags$b("Consumer Wearables:"), " Daily commercial APIs (Garmin, Fitbit)."), 
              tags$li(tags$b("Digital Biomarkers:"), " Steps, MVPA, sleep efficiency, heart rate and etc.")
            )
          ), 
          card(
            card_header("Statistical Framework"), 
            tags$p("Wearable data is uniquely challenging due to 'Missing at Random' observations. DailyWatch incorporates:"), 
            tags$ul(
              tags$li(tags$b("Linear Mixed-Effects Models (LMMs):"), " To provide unbiased estimates of change over time and identify clinical and behavioral predictors of longitudinal patterns."), 
              tags$li(tags$b("K-means cluster analysis:"), " To cluster distinct participant phenotypes using overall means and temporal trend slopes "), 
              tags$li(tags$b("Multinomial Logistic Regression:"), " To identify baseline clinical and behavioral factors that determine participant membership within specific clusters.")
            )
          )
        ),
        
        card(card_header("Understanding Prediction Models"),
             tags$p("Mixed-effects regression outputs estimate how selected factors influence the digital biomarker over time:"),
             tags$ul(
               tags$li(tags$b("Baseline (Intercept):"), " The expected digital biomarker value at Day 0 for a reference participant."),
               tags$li(tags$b("Time (Relative Day):"), " The average daily rate of change (slope) across the cohort."),
               tags$li(tags$b("Continuous Predictors (e.g., BMI):"), " The constant increase or decrease in digital biomarker per unit change in the predictor."),
               tags$li(tags$b("Categorical Predictors:"), " The estimated difference in the digital biomarker compared to the designated 'Reference Group'.")
             ))
      )
    ),
    
    nav_panel(title = "Trajectory Plot", value = "trajectory_tab", icon = icon("chart-area"), 
              card(full_screen = TRUE, plotOutput("trajectoryPlot", height = "800px"))),
    
    nav_panel(title = "Trajectory Estimates", icon = icon("table"), 
              card(card_header("Statistical Estimates of Activity Over Time"), uiOutput("beautifulMarginalTable"))),
    
    nav_panel(title = "Prediction Model", value = "predictive_tab", icon = icon("calculator"), 
              layout_column_wrap(width = 1, card(card_header("Fixed Effects Forest Plot"), plotOutput("predPlot")), 
                                 card(card_header("Mixed-Effect Model Summary"), uiOutput("beautifulTableOutput")))),
    
    nav_panel(title = "Cluster Analysis", value = "clustering_tab", icon = icon("braille"), 
              layout_column_wrap(width = 1/2,
                                 card(card_header("Optimal Cluster Count (Elbow Method)"), plotOutput("optimalKPlot", height = "400px")),
                                 card(card_header("Cluster Map (2D Projection)"), plotOutput("pcaPlot", height = "400px"))),
              layout_column_wrap(width = 1/2,
                                 card(card_header("Silhouette Validation Plot"), plotOutput("silPlot", height = "400px")),
                                 card(card_header("Mean Trajectory by Cluster"), plotOutput("clusterTrajPlot", height = "500px")))),
    
    nav_panel(title = "Cluster Determinants", value = "multinom_tab", icon = icon("vials"),
              layout_column_wrap(width = 1,
                                 card(card_header("Multinomial Relative Risk Ratios (Relative to Cluster 1)"), plotOutput("multinomPlot")),
                                 card(card_header("Multinomial Model Summary (RRR)"), uiOutput("beautifulMultinomTable")))),
    
    nav_panel(title = "Data Preview", icon = icon("table"), card(tableOutput("rawData")))
  )
)

# --- 3. Server Section ---
server <- function(input, output, session) {
  
  # --- 3.1. Auto-Tab Switching ---
  observeEvent(raw_df(), { nav_select("main_tabs", "trajectory_tab") })
  observeEvent(input$run_cluster, { nav_select("main_tabs", "clustering_tab") })
  observeEvent(input$run_prediction, { nav_select("main_tabs", "predictive_tab") })
  observeEvent(input$run_multinom, { nav_select("main_tabs", "multinom_tab") })
  
  # --- 3.2. Data Engine ---
  raw_df <- reactive({
    req(input$file); ext <- tools::file_ext(input$file$name)
    tryCatch({
      if (ext == "csv") read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      else if (ext %in% c("xlsx", "xls")) read_excel(input$file$datapath)
      else if (ext == "sas7bdat") read_sas(input$file$datapath)
      else if (ext == "sav") read_spss(input$file$datapath)
      else if (ext == "dta") read_dta(input$file$datapath)
      else validate("Invalid file type.")
    }, error = function(e) validate(paste("Error reading file:", e$message)))
  })
  
  output$var_selector <- renderUI({
    df <- raw_df(); req(df); cols <- names(df)
    tagList(
      selectInput("id_var", "ID:", choices = c("", cols)), 
      selectInput("date_var", "Device Date:", choices = c("", cols)), 
      selectInput("step_var", "Digital Biomarker:", choices = c("", cols)), 
      selectInput("treat_var", "Treatment/Base Date:", choices = c("", cols))
    )
  })
  
  observe({ 
    df <- raw_df(); req(df); 
    updateSelectInput(session, "traj_group", choices = c("None", names(df))) 
  })
  
  processed_base <- reactive({
    req(input$id_var != "", input$date_var != "", input$step_var != "", input$treat_var != "")
    convert_date <- function(x) { 
      if(is.numeric(x) || is.Date(x) || inherits(x, "POSIXt")) as.Date(x) 
      else as.Date(lubridate::parse_date_time(x, orders = c("dmy", "ymd", "mdy", "T"))) 
    }
    raw_df() %>%
      mutate(
        ID = as.character(.data[[input$id_var]]), 
        Steps = as.numeric(.data[[input$step_var]]), 
        Date = convert_date(.data[[input$date_var]]), 
        Treatment_Date = convert_date(.data[[input$treat_var]])
      ) %>%
      filter(!is.na(Date), !is.na(Treatment_Date), !is.na(Steps)) %>%
      mutate(Relative_Day = as.numeric(difftime(Date, Treatment_Date, units = "days"))) %>%
      filter(Relative_Day >= input$hard_min, Relative_Day <= input$hard_max)
  })
  
  processed_filtered <- reactive({
    req(input$day_range)
    daily <- processed_base() %>% 
      filter(Relative_Day >= input$day_range[1], Relative_Day <= input$day_range[2]) %>% 
      mutate(Bin_Day = floor(Relative_Day / as.numeric(input$bin_size)) * as.numeric(input$bin_size)) %>% 
      group_by(ID, Bin_Day) %>% 
      summarise(Steps = mean(Steps, na.rm = TRUE), .groups = "drop") %>% 
      rename(Relative_Day = Bin_Day)
    
    raw_df() %>% 
      rename(ID = !!sym(input$id_var)) %>% 
      group_by(ID) %>% 
      summarise(across(everything(), first), .groups = "drop") %>% 
      right_join(daily, by = "ID")
  })
  
  output$dynamic_slider_ui <- renderUI({ 
    data <- processed_base(); req(nrow(data) > 0); 
    days <- data$Relative_Day; 
    sliderInput("day_range", "Zoom to Specific Range:", min = floor(min(days)), max = ceiling(max(days)), value = c(-7, 14), step = 1) 
  })
  
  output$y_axis_range_ui <- renderUI({ 
    data <- processed_filtered(); req(nrow(data) > 0); 
    curr_max <- max(data$Steps, na.rm = TRUE); 
    sliderInput("y_range", NULL, min = 0, max = 15000, value = c(0, min(15000, curr_max * 1.1)), step = 500) 
  })
  
  # --- 3.3. Statistical Analysis ---
  trajectory_lmm_obj <- reactive({
    req(input$trend_method == "lmm"); data <- processed_filtered(); req(nrow(data) > 5)
    data$DayFact <- relevel(as.factor(data$Relative_Day), ref = "0")
    if (input$traj_group != "None") {
      lme(as.formula(paste("Steps ~ DayFact *", input$traj_group)), random = ~ 1 | ID, data = data, na.action = na.omit)
    } else {
      lme(Steps ~ DayFact, random = ~ 1 | ID, data = data, na.action = na.omit)
    }
  })
  
  marginal_means <- reactive({
    model <- trajectory_lmm_obj(); req(model)
    if (input$traj_group != "None") {
      as.data.frame(emmeans(model, specs = c("DayFact", input$traj_group))) %>% mutate(Relative_Day = as.numeric(as.character(DayFact)))
    } else {
      as.data.frame(emmeans(model, ~ DayFact)) %>% mutate(Relative_Day = as.numeric(as.character(DayFact)))
    }
  })
  
  output$trajectoryPlot <- renderPlot({
    df <- processed_filtered(); req(nrow(df) > 0); 
    sample_info <- paste0("Total Sample: N = ", n_distinct(df$ID), " | ", nrow(df), " Observations")
    p <- ggplot(df, aes(x = Relative_Day, y = Steps)) + 
      geom_vline(xintercept = 0, linetype = "dashed", color = DFCI_RED, linewidth = 1.2) + 
      theme_minimal(base_size = 18) + labs(x = "Relative Day", y = "Steps", subtitle = sample_info) + 
      theme(panel.grid = element_blank(), axis.line = element_line(color = "black"))
    if (input$traj_group != "None") {
      group_counts <- df %>% group_by(!!sym(input$traj_group)) %>% summarise(n = n_distinct(ID), .groups = "drop") %>% mutate(new_label = paste0(.data[[input$traj_group]], " (N=", n, ")"))
      labels_map <- setNames(group_counts$new_label, group_counts[[input$traj_group]])
      df[[input$traj_group]] <- as.factor(df[[input$traj_group]]); p <- p + aes(color = .data[[input$traj_group]], fill = .data[[input$traj_group]])
      if (input$view_type == "single") p <- p + geom_line(data = filter(df, ID == input$selected_id), linewidth = 2.2) else {
        if (input$show_individual) p <- p + geom_line(aes(group = ID), alpha = 0.15); 
        if (input$trend_method == "raw") p <- p + stat_summary(fun = mean, geom = "line", linewidth = 2.5) else { 
          emm <- marginal_means(); if (!is.null(emm)) { 
            p <- p + geom_ribbon(data = emm, aes(x = Relative_Day, y = emmean, ymin = lower.CL, ymax = upper.CL, fill = .data[[input$traj_group]]), alpha = 0.2, inherit.aes = FALSE) + 
              geom_line(data = emm, aes(x = Relative_Day, y = emmean, color = .data[[input$traj_group]]), linewidth = 2.5, inherit.aes = FALSE) 
          } 
        }
      }
      p <- p + scale_color_manual(values = DFCI_PALETTE, labels = labels_map) + scale_fill_manual(values = DFCI_PALETTE, labels = labels_map)
    } else {
      if (input$view_type == "single") p <- p + geom_line(data = filter(df, ID == input$selected_id), color = DFCI_BLUE, linewidth = 2.2) else {
        if (input$show_individual) p <- p + geom_line(aes(group = ID), alpha = 0.1, color = "grey60"); 
        if (input$trend_method == "raw") p <- p + stat_summary(fun = mean, geom = "line", linewidth = 2.5, color = DFCI_NAVY) else { 
          emm <- marginal_means(); if (!is.null(emm)) { 
            p <- p + geom_ribbon(data = emm, aes(x = Relative_Day, y = emmean, ymin = lower.CL, ymax = upper.CL), fill = DFCI_BLUE, alpha = 0.3, inherit.aes = FALSE) + 
              geom_line(data = emm, aes(x = Relative_Day, y = emmean), color = DFCI_NAVY, linewidth = 2.8, inherit.aes = FALSE) 
          } 
        }
      }
    }
    if(!is.null(input$y_range)) p <- p + coord_cartesian(ylim = input$y_range); p
  })
  
  output$beautifulMarginalTable <- renderUI({
    res <- marginal_means(); req(res); 
    display_df <- res %>% mutate(Conf_Int = paste0("[", round(lower.CL, 2), ", ", round(upper.CL, 2), "]")) %>% select(any_of(c("Relative_Day", input$traj_group, "emmean", "SE", "Conf_Int"))); 
    header_names <- c("Relative Day", if(input$traj_group != "None") "Group", "Activity Estimate", "SE", "95% Confidence Interval")
    build_row <- function(row) { tags$tr(lapply(row, function(cell) tags$td(style = "padding:12px; border-bottom:1px solid #eee; text-align:center;", cell))) }
    tags$div(style = "overflow-x: auto; padding: 10px;", tags$table(style = "width:100%; border-collapse: collapse; font-family: 'Inter', sans-serif; font-size: 14px;", tags$thead(tags$tr(style = paste0("background-color: ", DFCI_NAVY, "; color: white;"), lapply(header_names, function(h) tags$th(style = "padding:15px; text-align:center; font-weight: 600;", h)))), tags$tbody(apply(display_df, 1, build_row))))
  })
  
  prediction_model <- eventReactive(input$run_prediction, {
    data <- as.data.frame(processed_filtered())
    if(!is.null(input$cat_preds)) data <- data %>% mutate(across(all_of(input$cat_preds), as.factor))
    pred_terms <- c(input$cont_preds, input$cat_preds)
    form_str <- if(length(pred_terms) > 0) paste("Steps ~ Relative_Day +", paste0("`", pred_terms, "`", collapse = " + ")) else "Steps ~ Relative_Day"
    lme(as.formula(form_str), random = ~ 1 | ID, data = data, na.action = na.omit)
  })
  
  output$beautifulTableOutput <- renderUI({
    model <- prediction_model(); req(model); results <- as.data.frame(summary(model)$tTable); results$Term <- rownames(results); get_stars <- function(p) if(p < 0.001) return("***") else if(p < 0.01) return("**") else if(p < 0.05) return("*") else return(""); results$Sig <- sapply(results$`p-value`, get_stars)
    build_row <- function(term, row_data, is_ref = FALSE) { if(is_ref) { tags$tr(tags$td(style = "padding:8px; color:grey; padding-left:25px;", term), tags$td(colspan = 4, style = "padding:8px; color:grey; text-align:center; font-style:italic;", "Reference Group")) } else { tags$tr(tags$td(style = "padding:8px; border-bottom:1px solid #eee; padding-left:25px;", term), tags$td(style = "padding:8px; border-bottom:1px solid #eee; text-align:center;", sprintf("%.2f", row_data$Value)), tags$td(style = "padding:8px; border-bottom:1px solid #eee; text-align:center;", sprintf("%.2f", row_data$Std.Error)), tags$td(style = "padding:8px; border-bottom:1px solid #eee; text-align:center;", sprintf("%.4f", row_data$`p-value`)), tags$td(style = "padding:8px; border-bottom:1px solid #eee; text-align:center;", row_data$Sig)) } }
    table_content <- list(); table_content[[length(table_content)+1]] <- tags$tr(style = "background-color:#f1f1f1; font-weight:bold;", tags$td(colspan = 5, "Longitudinal Time Trend")); table_content[[length(table_content)+1]] <- build_row("Baseline (Intercept)", results["(Intercept)",]); if("Relative_Day" %in% results$Term) table_content[[length(table_content)+1]] <- build_row("Time (Relative Day)", results["Relative_Day",]); if(length(input$cont_preds) > 0) { table_content[[length(table_content)+1]] <- tags$tr(style = "background-color:#f1f1f1; font-weight:bold;", tags$td(colspan = 5, "Continuous Variables")); for(p in input$cont_preds) if(p %in% results$Term) table_content[[length(table_content)+1]] <- build_row(p, results[p,]) }
    if(length(input$cat_preds) > 0) { table_content[[length(table_content)+1]] <- tags$tr(style = "background-color:#f1f1f1; font-weight:bold;", tags$td(colspan = 5, "Categorical Variables (w/ Reference)")); data_context <- as.data.frame(processed_filtered()); for(p in input$cat_preds) { lvls <- levels(as.factor(data_context[[p]])); table_content[[length(table_content)+1]] <- build_row(paste0(p, ": ", lvls[1]), NULL, is_ref = TRUE); for(l in lvls[-1]) { term_name <- paste0(p, l); if(term_name %in% results$Term) table_content[[length(table_content)+1]] <- build_row(paste0("↳ ", l), results[term_name,]) } } }
    tags$div(tags$table(style = "width:100%; border-collapse:collapse; font-size:14px;", tags$thead(tags$tr(style = paste0("background-color:", DFCI_NAVY, "; color:white;"), tags$th("Predictor", style="padding:10px;"), tags$th("Estimate", style="text-align:center;"), tags$th("Std.Error", style="text-align:center;"), tags$th("p-value", style="text-align:center;"), tags$th("Sig", style="text-align:center;"))), tags$tbody(table_content)), tags$div(style = "margin-top:15px; font-weight:bold; border-top: 2px solid #eee; padding-top:10px;", sprintf("Model Stats | Observations: %d | Participants: %d", summary(model)$dims$N, summary(model)$dims$ngrps[1])))
  })
  
  # --- 3.4. Clustering Engine ---
  cluster_results <- eventReactive(input$run_cluster, {
    df <- processed_filtered(); req(nrow(df) > 0)
    features <- df %>% group_by(ID) %>% summarise(Mean_Steps = mean(Steps, na.rm = TRUE), Trend_Slope = ifelse(n() > 1, coef(lm(Steps ~ Relative_Day))[2], 0), .groups = "drop") %>% filter(!is.na(Mean_Steps), !is.na(Trend_Slope))
    dat_scale <- scale(features[, -1]); rownames(dat_scale) <- features$ID
    km <- kmeans(dat_scale, centers = input$clusters, nstart = 50)
    sil <- silhouette(km$cluster, dist(dat_scale))
    features$Cluster <- as.factor(km$cluster); counts <- features %>% group_by(Cluster) %>% summarise(n = n())
    features <- features %>% left_join(counts, by = "Cluster") %>% mutate(Cluster_Label = paste0("Cluster ", Cluster, " (N=", n, ")"))
    list(model = km, data = dat_scale, features = features, sil = sil)
  })
  
  output$optimalKPlot <- renderPlot({
    df <- processed_filtered(); req(nrow(df) > 0)
    features <- df %>% group_by(ID) %>% summarise(Mean_Steps = mean(Steps, na.rm = TRUE), Trend_Slope = ifelse(n() > 1, coef(lm(Steps ~ Relative_Day))[2], 0), .groups = "drop") %>% filter(!is.na(Mean_Steps), !is.na(Trend_Slope))
    dat_scale <- scale(features[, -1])
    
    fviz_nbclust(dat_scale, kmeans, method = "wss", k.max = 6) +
      geom_vline(xintercept = input$clusters, linetype = "dashed", color = DFCI_RED, linewidth = 1) +
      labs(title = "Elbow Method: Optimal Clusters", subtitle = "Current selection marked in red") +
      theme_minimal() + theme(panel.grid = element_blank(), axis.line = element_line(color = "black"))
  })
  
  output$pcaPlot <- renderPlot({
    res <- cluster_results(); req(res)
    fviz_cluster(res$model, data = res$data, geom = "point", ellipse.type = "convex", ggtheme = theme_minimal()) + scale_color_manual(values = DFCI_PALETTE) + labs(title = NULL) + theme(panel.grid = element_blank(), axis.line = element_line(color = "black"))
  })
  
  output$silPlot <- renderPlot({
    res <- cluster_results(); req(res)
    fviz_silhouette(res$sil, palette = DFCI_PALETTE, ggtheme = theme_minimal()) + labs(title = NULL) + theme(panel.grid = element_blank())
  })
  
  output$clusterTrajPlot <- renderPlot({
    res <- cluster_results(); req(res); plot_df <- processed_filtered() %>% inner_join(res$features %>% select(ID, Cluster_Label), by = "ID")
    ggplot(plot_df, aes(x = Relative_Day, y = Steps, color = Cluster_Label, fill = Cluster_Label)) +
      stat_summary(fun = mean, geom = "line", linewidth = 2) + stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA) +
      theme_minimal(base_size = 16) + labs(y = "Digital Biomarker", x = "Relative Day", color = "Group", fill = "Group") +
      scale_color_manual(values = DFCI_PALETTE) + scale_fill_manual(values = DFCI_PALETTE) + theme(panel.grid = element_blank(), axis.line = element_line(color = "black"))
  })
  
  # --- 3.5. Multinomial Cluster Determinants ---
  multinom_model <- eventReactive(input$run_multinom, {
    res <- cluster_results(); req(res)
    participant_data <- raw_df() %>% rename(ID = !!sym(input$id_var)) %>% group_by(ID) %>% summarise(across(everything(), first), .groups = "drop") %>% inner_join(res$features %>% select(ID, Cluster), by = "ID")
    if(!is.null(input$multinom_cat_preds)) participant_data <- participant_data %>% mutate(across(all_of(input$multinom_cat_preds), as.factor))
    pred_terms <- c(input$multinom_cont_preds, input$multinom_cat_preds)
    req(length(pred_terms) > 0)
    form_str <- paste("Cluster ~", paste0("`", pred_terms, "`", collapse = " + "))
    multinom(as.formula(form_str), data = participant_data, trace = FALSE)
  })
  
  output$beautifulMultinomTable <- renderUI({
    model <- multinom_model(); req(model)
    sum_m <- summary(model)
    
    # Handle dimensionality for k=2 vs k>2
    if (is.vector(sum_m$coefficients)) {
      coeffs <- t(as.matrix(sum_m$coefficients))
      ses    <- t(as.matrix(sum_m$standard.errors))
      rownames(coeffs) <- "Cluster 2"
    } else {
      coeffs <- sum_m$coefficients
      ses    <- sum_m$standard.errors
      rownames(coeffs) <- paste("Cluster", rownames(coeffs))
    }
    
    # Calculate RRR, CI, and P-values
    rrr    <- exp(coeffs)
    low_ci <- exp(coeffs - 1.96 * ses)
    upp_ci <- exp(coeffs + 1.96 * ses)
    z      <- coeffs / ses
    p_vals <- (1 - pnorm(abs(z), 0, 1)) * 2
    
    # Prepare data for vertical display
    var_names <- colnames(rrr)
    cluster_names <- rownames(rrr)
    header_names <- c("Predictor", cluster_names)
    
    build_row <- function(var_idx) {
      var_name <- var_names[var_idx]
      cluster_cells <- lapply(1:nrow(rrr), function(c_idx) {
        p_val <- p_vals[c_idx, var_idx]
        stars <- if(p_val < 0.001) "***" else if(p_val < 0.01) "**" else if(p_val < 0.05) "*" else ""
        estimate_text <- sprintf("%.3f %s", rrr[c_idx, var_idx], stars)
        ci_text <- sprintf("(%.3f, %.3f)", low_ci[c_idx, var_idx], upp_ci[c_idx, var_idx])
        tags$td(style = "padding:12px; border-bottom:1px solid #eee; text-align:center;",
                tags$div(tags$b(estimate_text)),
                tags$div(style = "font-size: 11px; color: #666;", ci_text))
      })
      tags$tr(tags$td(style = "padding:12px; border-bottom:1px solid #eee; font-weight:600; background-color: #f9f9f9;", var_name), cluster_cells)
    }
    
    tags$div(style = "overflow-x: auto; padding: 10px;", 
             tags$table(style = "width:100%; border-collapse: collapse; font-family: 'Inter', sans-serif; font-size: 14px;", 
                        tags$thead(tags$tr(style = paste0("background-color: ", DFCI_NAVY, "; color: white;"), 
                                           lapply(header_names, function(h) tags$th(style = "padding:15px; text-align:center; font-weight: 600;", h)))), 
                        tags$tbody(lapply(1:length(var_names), build_row))))
  })
  
  output$multinomPlot <- renderPlot({
    model <- multinom_model(); req(model)
    plot_model(model, type = "est", transform = "exp", show.values = TRUE, colors = "Set1") + 
      geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
      theme_minimal(base_size = 14) + labs(title = NULL, y = "Relative Risk Ratios (RRR)") +
      theme(panel.grid = element_blank(), 
            axis.line = element_line(color = "black"),
            strip.background = element_rect(fill = DFCI_NAVY),
            strip.text = element_text(color = "white", face = "bold"))
  })
  
  # --- Helpers & UI Selectors ---
  output$predPlot <- renderPlot({ 
    req(prediction_model())
    plot_model(prediction_model(), type = "est", show.values = TRUE, show.p = TRUE, p.style = "asterisk", vline.color = "grey80", colors = c(DFCI_RED, DFCI_NAVY)) + 
      theme_minimal(base_size = 16) + labs(y = NULL, title = NULL) + theme(panel.grid = element_blank(), axis.line = element_line(color = "black")) 
  })
  
  output$rawData <- renderTable({ head(processed_filtered(), 50) })
  
  output$manual_predictor_ui <- renderUI({ 
    req(raw_df()); all_vars <- names(raw_df())
    tagList(selectizeInput("cont_preds", "Continuous Variables:", choices = all_vars, multiple = TRUE), 
            selectizeInput("cat_preds", "Categorical Variables:", choices = all_vars, multiple = TRUE)) 
  })
  
  output$cluster_predictor_ui <- renderUI({
    req(raw_df()); all_vars <- names(raw_df())
    tagList(selectizeInput("multinom_cont_preds", "Continuous Variables:", choices = all_vars, multiple = TRUE),
            selectizeInput("multinom_cat_preds", "Categorical Variables:", choices = all_vars, multiple = TRUE))
  })
  
  observe({ req(processed_base()); updateSelectInput(session, "selected_id", choices = unique(processed_base()$ID)) })
  
  # --- 3.6. Export Handlers ---
  output$downloadData <- downloadHandler(
    filename = function() { "wearable_processed_data.csv" }, 
    content = function(file) { write.csv(processed_filtered(), file, row.names = FALSE) }
  )
  
  output$downloadModel <- downloadHandler(
    filename = function() { paste0("DailyWatch_Raw_Stats_", Sys.Date(), ".zip") },
    content = function(file) {
      temp_dir <- tempdir(); files_to_zip <- c()
      if (!is.null(marginal_means())) { write.csv(marginal_means(), file.path(temp_dir, "trajectory_estimates.csv"), row.names = FALSE); files_to_zip <- c(files_to_zip, file.path(temp_dir, "trajectory_estimates.csv")) }
      traj_raw <- try(as.data.frame(summary(trajectory_lmm_obj())$tTable), silent = TRUE)
      if (!inherits(traj_raw, "try-error")) { traj_raw$Term <- rownames(traj_raw); write.csv(traj_raw, file.path(temp_dir, "raw_lmm_day_vs_day0.csv"), row.names = FALSE); files_to_zip <- c(files_to_zip, file.path(temp_dir, "raw_lmm_day_vs_day0.csv")) }
      pred_raw <- try(as.data.frame(summary(prediction_model())$tTable), silent = TRUE)
      if (!inherits(pred_raw, "try-error")) { pred_raw$Term <- rownames(pred_raw); write.csv(pred_raw, file.path(temp_dir, "raw_prediction_model_results.csv"), row.names = FALSE); files_to_zip <- c(files_to_zip, file.path(temp_dir, "raw_prediction_model_results.csv")) }
      clust_res <- try(cluster_results()$features, silent = TRUE)
      if (!inherits(clust_res, "try-error")) { write.csv(clust_res, file.path(temp_dir, "participant_clusters.csv"), row.names = FALSE); files_to_zip <- c(files_to_zip, file.path(temp_dir, "participant_clusters.csv")) }
      mn_res <- try(summary(multinom_model()), silent = TRUE)
      if (!inherits(mn_res, "try-error")) {
        mn_rrr <- as.data.frame(exp(mn_res$coefficients))
        write.csv(mn_rrr, file.path(temp_dir, "multinomial_relative_risk_ratios.csv"), row.names = TRUE)
        files_to_zip <- c(files_to_zip, file.path(temp_dir, "multinomial_relative_risk_ratios.csv"))
      }
      zip(file, files = files_to_zip, extras = "-j")
    }
  )
}

# --- 4. Run App ---
shinyApp(ui, server)