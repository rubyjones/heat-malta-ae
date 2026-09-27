# run line by line until 'shiny starts here' 
# then you can click run app
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(dlnm)
library(MASS)
library(lubridate)
library(readxl)

# read in data
data_raw <- read_excel("heatwave_data_combined.xlsx")

# select variables
data_raw <- data_raw %>%
  dplyr::select(
    date,
    total_admissions,
    ae_attendances,
    temp_max,
    temp_max_feels_like,
    temp_min,
    dew_point,
    humidity_highest,
    exact_relative_hum,
    hsi_eqn,
    temp_difference,
    total_deaths,
    res_deaths,
    nonres_deaths
  )

# convert variables
data_raw <- data_raw %>%
  dplyr::mutate(
    date = as.Date(date),
    total_admissions = as.numeric(total_admissions),
    ae_attendances = as.numeric(ae_attendances),
    temp_max = as.numeric(temp_max),
    temp_max_feels_like = as.numeric(temp_max_feels_like),
    temp_min = as.numeric(temp_min),
    dew_point = as.numeric(dew_point),
    humidity_highest = as.numeric(humidity_highest),
    exact_relative_hum = as.numeric(exact_relative_hum),
    hsi_eqn = as.numeric(hsi_eqn),
    temp_difference = as.numeric(temp_difference),
    total_deaths = as.numeric(total_deaths),
    res_deaths = as.numeric(res_deaths),
    nonres_deaths = as.numeric(nonres_deaths)
  )

# restrict to july and august only
data_raw <- data_raw %>%
  filter(month(date) %in% c(7, 8))

# create complete dataset
# removes missing weather data - all of 2023 - 62 rows, too many
data_complete <- data_raw %>% 
  filter(!is.na(temp_max),
         !is.na(temp_max_feels_like),
         !is.na(dew_point),
         !is.na(humidity_highest),
         !is.na(temp_min),
         !is.na(exact_relative_hum),
         !is.na(hsi_eqn),
         !is.na(temp_difference)
  )

# add covariates
data_complete <- data_complete %>%
  mutate(
    year = factor(year(date)),
    dow = weekdays(date)
  )

# distributed lags
library(dlnm)
cb_ae <- crossbasis(data_complete$temp_max, lag = 7,
                    argvar = list(fun="ns", df=3),
                    arglag = list(fun="ns", df=3),
                    groups = factor(data_complete$year))

dlnm_ae <- glm.nb(ae_attendances ~ cb_ae + factor(year) + factor(dow), data = data_complete)

cp_ae <- crosspred(
  cb_ae,
  dlnm_ae,
  from = min(data_complete$temp_max, na.rm = TRUE),
  to   = max(data_complete$temp_max, na.rm = TRUE),
  by   = 0.5,
  cen  = median(data_complete$temp_max, na.rm = TRUE)
)


# shiny starts here
# needs dlnm_ae, cb_ae, data_complete
required_objects <- c("dlnm_ae", "cb_ae", "data_complete")

missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]

if (length(missing_objects) > 0) {
  stop(
    paste(
      "The following objects are missing:",
      paste(missing_objects, collapse = ", "),
      "\nLoad your modelling objects before running app.R."
    )
  )
}

beta_hat  <- coef(dlnm_ae)
Sigma_hat <- vcov(dlnm_ae)
theta_hat <- dlnm_ae$theta

argvar_used <- attr(cb_ae, "argvar")
arglag_used <- attr(cb_ae, "arglag")
maxlag      <- attr(cb_ae, "lag")[2]

train_years <- dlnm_ae$xlevels[["factor(year)"]]
train_dows  <- dlnm_ae$xlevels[["factor(dow)"]]

data_complete <- data_complete %>%
  mutate(date = as.Date(date))

historical_daily_mean <- data_complete %>%
  mutate(month_day = format(date, "%m-%d")) %>%
  group_by(month_day) %>%
  summarise(
    historical_mean = mean(ae_attendances, na.rm = TRUE),
    n_years         = sum(!is.na(ae_attendances)),
    .groups         = "drop"
  )

max_temp_val <- max(data_complete$temp_max, na.rm = TRUE)
min_temp_val <- min(data_complete$temp_max, na.rm = TRUE)

# get august 2025 values
aug_2025_start <- as.Date("2025-08-01")
aug_2025_dates_fcst <- seq.Date(aug_2025_start, length.out = 7, by = "day")
aug_2025_dates_hist <- seq.Date(aug_2025_start - 7, length.out = 7, by = "day")

aug_2025_fcst_temps <- data_complete %>%
  filter(date %in% aug_2025_dates_fcst) %>%
  arrange(date) %>%
  pull(temp_max)

aug_2025_hist_temps <- data_complete %>%
  filter(date %in% aug_2025_dates_hist) %>%
  arrange(date) %>%
  pull(temp_max)

# build crossbasis
build_forecast_crossbasis <- function(history_temp, forecast_temp) {
  stopifnot(length(history_temp) == maxlag)
  
  combined_temp <- c(history_temp, forecast_temp)
  
  cb <- crossbasis(
    combined_temp,
    lag = maxlag,
    argvar = argvar_used,
    arglag = arglag_used
  )
  
  cb[(length(history_temp) + 1):length(combined_temp), , drop = FALSE]
}

# forecast function
run_probabilistic_forecast <- function(history_temp, forecast_temp, forecast_dates, temp_sd, n_sims = 1000) {
  n_days <- length(forecast_dates)
  
  forecast_years <- unique(format(forecast_dates, "%Y"))
  if (length(forecast_years) > 1) {
    stop("Forecast period cannot span more than one calendar year.")
  }
  
  forecast_year <- forecast_years[1]
  year_label    <- if (forecast_year %in% train_years) forecast_year else max(train_years)
  dow_forecast  <- weekdays(forecast_dates)
  
  beta_draws <- MASS::mvrnorm(n = n_sims, mu = beta_hat, Sigma = Sigma_hat)
  sim_daily  <- matrix(NA_real_, nrow = n_sims, ncol = n_days)
  
  for (s in seq_len(n_sims)) {
    temp_sim <- forecast_temp + rnorm(n_days, mean = 0, sd = temp_sd)
    cb_sim   <- build_forecast_crossbasis(history_temp = history_temp, forecast_temp = temp_sim)
    
    n_coef      <- length(beta_hat)
    X_s         <- matrix(0, nrow = n_days, ncol = n_coef)
    colnames(X_s) <- names(beta_hat)
    
    X_s[, "(Intercept)"] <- 1
    
    year_coef <- paste0("factor(year)", year_label)
    if (year_coef %in% colnames(X_s)) {
      X_s[, year_coef] <- 1
    } else {
      fallback_year <- max(train_years)
      fallback_coef <- paste0("factor(year)", fallback_year)
      if (fallback_coef %in% colnames(X_s)) X_s[, fallback_coef] <- 1
    }
    
    dow_coef_names <- grep("^factor\\(dow\\)", colnames(X_s), value = TRUE)
    for (i in seq_len(n_days)) {
      dow_name <- paste0("factor(dow)", dow_forecast[i])
      if (dow_name %in% dow_coef_names) X_s[i, dow_name] <- 1
    }
    
    cb_coef_names <- grep("^cb_ae", names(beta_hat), value = TRUE)
    if (length(cb_coef_names) != ncol(cb_sim)) {
      cb_coef_names <- names(beta_hat)[seq_len(ncol(cb_sim)) + 1]
    }
    
    X_s[, cb_coef_names] <- as.matrix(cb_sim)
    
    eta_s <- as.numeric(X_s %*% beta_draws[s, ])
    mu_s  <- exp(eta_s)
    sim_daily[s, ] <- rnbinom(n_days, size = theta_hat, mu = mu_s)
  }
  
  sim_weekly_total <- rowSums(sim_daily)
  
  baseline <- data.frame(date = forecast_dates) %>%
    mutate(month_day = format(date, "%m-%d")) %>%
    left_join(historical_daily_mean, by = "month_day")
  
  daily_summary <- data.frame(
    date                = forecast_dates,
    expected_admissions = colMeans(sim_daily),
    lower_PI            = apply(sim_daily, 2, quantile, probs = 0.05),
    upper_PI            = apply(sim_daily, 2, quantile, probs = 0.95)
  ) %>%
    left_join(baseline %>% dplyr::select(date, historical_mean), by = "date") %>%
    mutate(expected_excess = expected_admissions - historical_mean)
  
  list(
    daily            = daily_summary,
    sim_daily        = sim_daily,
    sim_weekly_total = sim_weekly_total,
    year_used        = year_label
  )
}


# user interface
ui <- dashboardPage(
  
  dashboardHeader(title = "Malta Heat & A&E Forecast"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forecast", tabName = "forecast", icon = icon("temperature-high")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f5f6f8; }
        .small-box h3 { font-size: 26px; }
        .box { border-radius: 8px; }
        .table { font-size: 13px; }
        .forecast-title { font-weight: 600; font-size: 18px; }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "forecast",
        fluidRow(
          
          # left input panel
          box(
            title = "Forecast settings",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            
            # scenario dropdown
            selectInput(
              "temp_scenario",
              "Temperature Preset Scenario",
              choices = c(
                "Custom" = "custom",
                "Reasonable Worst Case" = "worst_case",
                "Historical Minimum" = "hist_min",
                "August 2025 Actual" = "august_2025"
              ),
              selected = "custom"
            ),
            
            dateInput(
              "forecast_start",
              "Forecast start date",
              value = as.Date("2026-08-01"),
              format = "dd/mm/yyyy"
            ),
            
            tags$hr(),
            
            h4("Previous 7 observed temperatures", class = "forecast-title"),
            
            fluidRow(
              lapply(1:7, function(i) {
                column(
                  width = 6,
                  numericInput(
                    paste0("history_", i),
                    paste0("Day ", i),
                    value = 30,
                    min = 0,
                    max = 50,
                    step = 0.1
                  )
                )
              })
            ),
            
            tags$hr(),
            
            h4("Next 7 forecast temperatures", class = "forecast-title"),
            
            fluidRow(
              lapply(1:7, function(i) {
                column(
                  width = 6,
                  numericInput(
                    paste0("forecast_", i),
                    paste0("Day ", i),
                    value = 30,
                    min = 0,
                    max = 50,
                    step = 0.1
                  )
                )
              })
            ),
            
            tags$hr(),
            
            numericInput("daily_excess_threshold", "Meaningful daily excess", value = 20, min = 1, step = 5),
            numericInput("weekly_excess_threshold", "Meaningful weekly excess", value = 100, min = 1, step = 10),
            numericInput("n_sims", "Number of simulations", value = 1000, min = 500, max = 5000, step = 500),
            
            actionButton("run_forecast", "Run probabilistic forecast", class = "btn-primary btn-block")
          ),
          
          # right side box
          column(
            width = 8,
            fluidRow(
              valueBoxOutput("weekly_excess_box", width = 4),
              valueBoxOutput("weekly_probability_box", width = 4),
              valueBoxOutput("heat_days_box", width = 4)
            ),
            
            fluidRow(
              box(
                title = "Daily forecast",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                plotOutput("forecast_plot", height = "360px")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Probability of meaningful excess",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            plotOutput("probability_plot", height = "330px")
          ),
          
          box(
            title = "Expected excess above historical baseline",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("excess_plot", height = "330px")
          )
        ),
        
        fluidRow(
          box(
            title = "Probabilistic daily forecast",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("forecast_table")
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About this forecast",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            p(strong("Model:"), " Negative binomial distributed lag non-linear model (DLNM)."),
            p(strong("Temperature effects:"), " Modelled across a 7-day lag period using the fitted cross-basis."),
            p(strong("Uncertainty:"), " The forecast incorporates uncertainty in both the fitted model coefficients and future temperature values, followed by negative-binomial simulation of realised A&E attendances."),
            p(strong("Prediction interval:"), " The 90% prediction interval represents the range containing approximately 90% of simulated future attendance counts."),
            p(strong("Historical baseline:"), " Expected excess is calculated relative to the historical mean attendance for the same calendar date."),
            p(strong("Year effect:"), " Where the forecast year is not present in the fitted model, the most recent available training year is used as an explicit stand-in.")
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
 
  # autofill temps based on scenario selected
  observeEvent(input$temp_scenario, {
    scenario <- input$temp_scenario
    
    if (scenario == "custom") return()
    
    # define target historical and forecast vectors according to scenario
    if (scenario == "worst_case") {
      hist_vals <- rep(round(max_temp_val, 1), 7)
      fcst_vals <- rep(round(max_temp_val, 1), 7)
    } else if (scenario == "hist_min") {
      hist_vals <- rep(round(min_temp_val, 1), 7)
      fcst_vals <- rep(round(min_temp_val, 1), 7)
    } else if (scenario == "august_2025") {
      hist_vals <- round(aug_2025_hist_temps, 1)
      fcst_vals <- round(aug_2025_fcst_temps, 1)
      
      # update start date automatically
      updateDateInput(session, "forecast_start", value = as.Date("2025-08-01"))
    }
    
    # update the 7 history inputs
    for (i in 1:7) {
      updateNumericInput(session, paste0("history_", i), value = hist_vals[i])
    }
    
    # update the 7 forecast inputs
    for (i in 1:7) {
      updateNumericInput(session, paste0("forecast_", i), value = fcst_vals[i])
    }
  })
  
  
  # run forecast
  forecast_result <- eventReactive(input$run_forecast, {
    history_temp <- sapply(1:7, function(i) input[[paste0("history_", i)]])
    forecast_temp <- sapply(1:7, function(i) input[[paste0("forecast_", i)]])
    
    forecast_dates <- seq.Date(
      from = input$forecast_start,
      by = "day",
      length.out = 7
    )
    
    temp_sd <- c(1, 1, 1, 2, 2, 2, 3)
    
    run_probabilistic_forecast(
      history_temp   = history_temp,
      forecast_temp  = forecast_temp,
      forecast_dates = forecast_dates,
      temp_sd        = temp_sd,
      n_sims         = input$n_sims
    )
  }, ignoreNULL = FALSE)
  
  
  daily_data <- reactive({
    req(forecast_result())
    forecast_result()$daily
  })
  
  weekly_expected_excess <- reactive({
    d <- daily_data()
    sum(d$expected_excess, na.rm = TRUE)
  })
  
  weekly_excess_probability <- reactive({
    result <- forecast_result()
    d <- result$daily
    baseline_total <- sum(d$historical_mean, na.rm = TRUE)
    threshold <- input$weekly_excess_threshold
    
    mean(result$sim_weekly_total > baseline_total + threshold)
  })
  
  
  # outputs
  output$weekly_excess_box <- renderValueBox({
    valueBox(
      value = round(weekly_expected_excess()),
      subtitle = "Expected weekly excess",
      icon = icon("arrow-up"),
      color = ifelse(weekly_expected_excess() > 0, "yellow", "green")
    )
  })
  
  output$weekly_probability_box <- renderValueBox({
    probability <- weekly_excess_probability()
    valueBox(
      value = paste0(round(probability * 100), "%"),
      subtitle = paste0("Probability of >", input$weekly_excess_threshold, " excess attendances"),
      icon = icon("chart-line"),
      color = ifelse(probability >= 0.5, "red", ifelse(probability >= 0.2, "yellow", "green"))
    )
  })
  
  output$heat_days_box <- renderValueBox({
    d <- daily_data()
    n_heat <- sum(d$expected_excess >= input$daily_excess_threshold, na.rm = TRUE)
    
    valueBox(
      value = n_heat,
      subtitle = paste0("Days with ≥", input$daily_excess_threshold, " expected excess"),
      icon = icon("temperature-high"),
      color = ifelse(n_heat >= 3, "red", "yellow")
    )
  })
  
  output$forecast_plot <- renderPlot({
    d <- daily_data()
    ggplot(d, aes(x = date)) +
      geom_ribbon(aes(ymin = lower_PI, ymax = upper_PI), alpha = 0.20) +
      geom_line(aes(y = expected_admissions), linewidth = 1.2) +
      geom_line(aes(y = historical_mean), linetype = "dashed", linewidth = 0.9) +
      geom_point(aes(y = expected_admissions), size = 2.5) +
      labs(
        x = NULL, y = "A&E attendances",
        title = "Probabilistic 7-day A&E forecast",
        subtitle = "Solid line = expected attendances; dashed line = historical calendar-date mean; shaded area = 90% prediction interval"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none")
  })
  
  output$probability_plot <- renderPlot({
    result <- forecast_result()
    d <- result$daily
    
    probabilities <- sapply(seq_len(nrow(d)), function(i) {
      mean(result$sim_daily[, i] > d$historical_mean[i] + input$daily_excess_threshold)
    })
    
    plot_data <- data.frame(date = d$date, probability = probabilities)
    
    ggplot(plot_data, aes(x = date, y = probability * 100)) +
      geom_col(alpha = 0.75) +
      geom_hline(yintercept = 50, linetype = "dashed") +
      labs(
        x = NULL, y = "Probability (%)",
        title = paste0("Probability of >", input$daily_excess_threshold, " excess attendances")
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$excess_plot <- renderPlot({
    d <- daily_data()
    ggplot(d, aes(x = date, y = expected_excess)) +
      geom_col(aes(alpha = abs(expected_excess))) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        x = NULL, y = "Expected excess attendances",
        title = "Expected excess above historical baseline"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "none")
  })
  
  output$forecast_table <- DT::renderDataTable({
    d <- daily_data()
    
    probability <- sapply(seq_len(nrow(d)), function(i) {
      mean(forecast_result()$sim_daily[, i] > d$historical_mean[i] + input$daily_excess_threshold)
    })
    
    table_data <- d %>%
      mutate(
        `Expected attendances`  = round(expected_admissions, 0),
        `90% PI`                = paste0(round(lower_PI), "–", round(upper_PI)),
        `Historical mean`       = round(historical_mean, 0),
        `Expected excess`       = round(expected_excess, 0),
        `P(excess > threshold)` = paste0(round(probability * 100), "%")
      ) %>%
      dplyr::select(
        Date = date,
        `Expected attendances`,
        `90% PI`,
        `Historical mean`,
        `Expected excess`,
        `P(excess > threshold)`
      )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      options = list(
        pageLength = 7, lengthChange = FALSE, searching = FALSE, scrollX = TRUE, dom = "tip"
      )
    )
  })
}

# run app
shinyApp(ui = ui, server = server)
