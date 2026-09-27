# load libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(MASS)
library(mice)
library(mgcv)
library(zoo)
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

# check missing data
colSums(is.na(data_raw))
data_raw[!complete.cases(data_raw), ]

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


# exploratory plots 
# histograms
ggplot(data_complete, aes(temp_max)) + geom_histogram()
ggplot(data_complete, aes(ae_attendances)) + geom_histogram()

# scatter plots
ggplot(data_complete, aes(temp_max, ae_attendances)) +
  geom_point()
ggplot(data_complete, aes(temp_max, total_deaths)) +
  geom_point()

# facet plots
ggplot(data_complete, aes(x = date, y = ae_attendances)) +
  geom_line() +
  facet_wrap(~ year, nrow = 2, scales = "free_x") + 
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b"
  ) +
  theme_minimal() + 
  labs(
    title = "",
    x = "Month",
    y = "A&E Attendances"
  )

ggplot(data_complete, aes(x = date, y = ae_attendances)) +
  geom_line() +
  facet_wrap(~year, nrow = 2, scales = "free_x") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  theme_minimal() +
  labs(
    x = "Month",
    y = "A&E attendances"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 13)
  )



ggplot(data_complete, aes(x = date, y = total_deaths)) +
  geom_line() +
  facet_wrap(~ year, nrow = 2, scales = "free_x") + 
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%b"
  ) +
  theme_minimal() + 
  labs(
    title = "",
    x = "Month",
    y = "Total Deaths"
  )

# box plots
ggplot(data_complete, aes(x = year, y = temp_max, fill = year)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "", x = "", y = "Temperature (°C)")

ggplot(data_complete, aes(x = year, y = ae_attendances, fill = year)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "A&E Attendances", x = "", y = "Attendances")

ggplot(data_complete, aes(x = year, y = total_deaths, fill = year)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Mortality Counts", x = "", y = "Total Deaths")

ggplot(data_complete, aes(x = dow, y = ae_attendances, fill = dow)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "A&E Attendances", x = "", y = "Attendances")

ggplot(data_complete, aes(x = dow, y = ae_attendances)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Day of the week",
    y = "Daily A&E attendances"
  ) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )
ggplot(data_complete, aes(x = dow, y = total_deaths, fill = dow)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Mortality", x = "", y = "Deaths")

# explore data
summary(data_complete)
colSums(is.na(data_complete))
View(data_complete)

# removed code for total admissions

# A&E ATTENDANCES !!!!!!
# simple poisson
poisson_ae_temp <- glm(
  ae_attendances ~ temp_max +
    factor(year) +
    factor(dow),
  family = poisson(),
  data = data_complete
)
summary(poisson_ae_temp)
exp(coef(poisson_ae_temp))

disp_ratio <- deviance(poisson_ae_temp)/ df.residual(poisson_ae_temp)
disp_ratio

# simple model
nb_ae_temp <- glm.nb(
  ae_attendances ~ temp_max + factor(year) +
    factor(dow),
  data = data_complete
)
summary(nb_ae_temp)
exp(coef(nb_ae_temp))
exp(5 * coef(nb_ae_temp)["temp_max"]) # per 5 degrees effect

# AGE STRATIFICATION 
age_strat <- read_excel("age_stratified.xlsx")

age_strat <- age_strat %>%
  dplyr::select(
    date,
    zero_four,
    five_eighteen,
    nineteen_fiftyfive,
    fiftysix_seventyfive,
    seventysix_plus,
    total)
    
age_strat <- age_strat %>%
  dplyr::mutate(
    date = as.Date(date),
    zero_four = as.numeric(zero_four),
    five_eighteen = as.numeric(five_eighteen),
    nineteen_fiftyfive = as.numeric(nineteen_fiftyfive),
    fiftysix_seventyfive = as.numeric(fiftysix_seventyfive),
    seventysix_plus = as.numeric(seventysix_plus),
    total = as.numeric(total)
  )

data_complete <- left_join(
  data_complete,
  age_strat,
  by = "date"
)

age_data <- data_complete %>%
  filter(year == "2025")

age_zero_four <- glm.nb(zero_four ~ temp_max + factor(dow) , data = data_complete)
age_five_eighteen <- glm.nb(five_eighteen ~ temp_max + factor(dow), data = age_data)
age_nineteen_fiftyfive <- glm.nb(nineteen_fiftyfive ~ temp_max + factor(dow), data = age_data)
age_fiftysix_seventyfive <- glm.nb(fiftysix_seventyfive ~ temp_max + factor(dow), data = age_data)
age_seventysix_plus <- glm.nb(seventysix_plus ~ temp_max + factor(dow) , data = age_data)

age_models <- list(
  "0-4" = age_zero_four,
  "5-18" = age_five_eighteen,
  "19-55" = age_nineteen_fiftyfive,
  "56-75" = age_fiftysix_seventyfive,
  "76+" = age_seventysix_plus
)

age_comparison <- data.frame(
  Age_Group = names(age_models),
  Estimate = sapply(age_models, function(x) coef(x)["temp_max"]),
  P_value = sapply(age_models, function(x) summary(x)$coefficients["temp_max", "Pr(>|z|)"]),
  AIC = sapply(age_models, AIC)
)

age_comparison
age_comparison <- age_comparison %>%
  mutate(
    Percent_Increase_per_1C = (exp(Estimate)-1)*100,
    Percent_Increase_per_5C = (exp(Estimate * 5) - 1) * 100
  )

age_comparison

age_comparison <- age_comparison %>%
  mutate(
    lower_CI = sapply(age_models, function(x) {
      confint(x)["temp_max",1]
    }),
    upper_CI = sapply(age_models, function(x) {
      confint(x)["temp_max",2]
    })
  ) %>%
  mutate(
    lower_percent = (exp(lower_CI)-1)*100,
    upper_percent = (exp(upper_CI)-1)*100,
    lower_percent_5C = (exp(lower_CI * 5) - 1) * 100,
    upper_percent_5C = (exp(upper_CI * 5) - 1) * 100
  )

age_comparison
ggplot(age_comparison, 
       aes(x = Age_Group, y = Percent_Increase_per_5C)) +
  geom_col() +
  ylab("Percentage increase in A&E attendances per 5°C increase") +
  xlab("Age group") +
  theme_minimal()


data_complete <- data_complete %>%
  mutate(covid_status = ifelse(year %in% c(2020, 2021), "COVID", "Non-COVID"))

data_complete$predict_ae_simple <- predict(nb_ae_temp, type = "response")

ggplot(data_complete, aes(x = ae_attendances, y = predict_ae_simple, colour = factor(year))) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "solid", size = 1) +
  theme_minimal() +
  labs(
    title = "Observed vs Predicted A&E Attendances",
    x = "Observed A&E Attendances",
    y = "Predicted A&E Attendances",
    colour = "year")

# multivariate model - using temp difference
nb_ae_multi <- glm.nb(
  ae_attendances ~
    temp_difference + exact_relative_hum + dew_point +
    factor(year) +
    factor(dow),
  data = data_complete
)
summary(nb_ae_multi)
exp(coef(nb_ae_multi))

# diurnal temperature range
summary(glm.nb(
  ae_attendances ~ temp_difference +
    factor(year) +
    factor(dow),
  data = data_complete
))

# Relative humidity
summary(glm.nb(
  ae_attendances ~ exact_relative_hum +
    factor(year) +
    factor(dow),
  data = data_complete
))

# Dew point
summary(glm.nb(
  ae_attendances ~ dew_point +
    factor(year) +
    factor(dow),
  data = data_complete
))

# extreme heat model on ae attendances
# top 10% hottest days
top_10 <- quantile(data_complete$temp_max, 0.90, na.rm = TRUE)
top_10 
data_complete <- data_complete %>%
  mutate(extreme_heat = ifelse(temp_max >= top_10, 1, 0))

heat_model_nb <- glm.nb(
  ae_attendances ~ extreme_heat +
    factor(year) + factor(dow),
  data = data_complete
)
summary(heat_model_nb)
exp(coef(heat_model_nb))

# extend regression to include lags
data_complete <- data_complete %>% 
  arrange(year, date) %>%
  group_by(year) %>%
  mutate(
    lag1 = dplyr:: lag(temp_max,1),
    lag2 = dplyr:: lag(temp_max, 2)
  ) %>%
  ungroup()

lag1_model_ae <- glm.nb(
  ae_attendances ~ lag1 +
    factor(year) + factor(dow),
  data = data_complete
)
summary(lag1_model_ae)

lag2_model_ae <- glm.nb(
  ae_attendances ~ lag2 +
    factor(year) + factor(dow),
  data = data_complete
)
summary(lag2_model_ae)

# MOVING AVERAGE: T1 + T2 + T3 / 3
data_complete <- data_complete %>%
  mutate(
    MA3 = (
      temp_max +
        lag1 +
        lag2
    ) / 3
  )

MA_3 <- gam(
  ae_attendances ~
    s(MA3) +
    factor(year) +
    factor(dow),
  family = nb(),
  data = data_complete
)
summary(MA_3)

# TEMP LOAD
temps <- pmax(data_complete$temp_max - 36, 0)
data_complete$temp_load7 <-
  rollapply(
    temps,
    width = 7,
    FUN = sum,
    align = "right",
    fill = NA
  )


tempload_7 <- gam(
  ae_attendances ~
    s(temp_load7) +
    factor(year) +
    factor(dow),
  family = nb(),
  data = data_complete
)
summary(tempload_7)

# HOT RUN: nday>Tref
data_complete$hot_run <- 0

for(i in 2:nrow(data_complete)){
  
  if(data_complete$temp_max[i] > 36){
    
    data_complete$hot_run[i] <-
      data_complete$hot_run[i-1] + 1
    
  } else {
    
    data_complete$hot_run[i] <- 0
    
  }
  
}

hotrun_35 <- gam(
  ae_attendances ~
    s(hot_run, k=3) +
    factor(year) +
    factor(dow),
  family = nb(),
  data = data_complete
)
summary(hotrun_35)
gam.check(hotrun_35)
table(data_complete$hot_run)

AIC = c(AIC(MA_3), AIC(tempload_7), AIC(hotrun_35))
AIC

# distributed lags
library(dlnm)
#vignette("dlnmOverview")
cb_ae <- crossbasis(data_complete$temp_max, lag = 7,
                 argvar = list(fun="ns", df=3),
                 arglag = list(fun="ns", df=3),
                 groups = factor(data_complete$year))

dlnm_ae <- glm.nb(ae_attendances ~ cb_ae + factor(year) + factor(dow), data = data_complete)
summary(dlnm_ae)

dlnm_ae_null <- glm.nb(
  ae_attendances ~ factor(year) + factor(dow),
  data = data_complete
)

anova(dlnm_ae_null, dlnm_ae, test = "LRT")

cp_ae <- crosspred(
  cb_ae,
  dlnm_ae,
  from = min(data_complete$temp_max, na.rm = TRUE),
  to   = max(data_complete$temp_max, na.rm = TRUE),
  by   = 0.5,
  cen  = median(data_complete$temp_max, na.rm = TRUE)
)

cp_ae$matRRfit
cp_ae$matRRlow
cp_ae$matRRhigh
cp_ae$allRRfit
cp_ae$allRRlow
cp_ae$allRRhigh

quantile(data_complete$temp_max,
         probs = c(0.25, 0.50, 0.90),
         na.rm = TRUE)

cp_percentiles <- crosspred(
  cb_ae,
  dlnm_ae,
  at = c(30.8, 32.2, 35.8),
  cen = 32.2
)

cp_percentiles$allRRfit
cp_percentiles$allRRlow
cp_percentiles$allRRhigh

max(cp_ae$allRRfit)
cp_ae$predvar[which.max(cp_ae$allRRfit)]

i <- which.max(cp_ae$allRRfit)

c(
  temperature = cp_ae$predvar[i],
  RR = cp_ae$allRRfit[i],
  lower = cp_ae$allRRlow[i],
  upper = cp_ae$allRRhigh[i]
)

apply(cp_ae$matRRfit, 2, max)
apply(cp_ae$matRRfit, 2, function(x) cp_ae$predvar[which.max(x)])

i40 <- which(cp_ae$predvar == 40)

c(
  RR = cp_ae$allRRfit[i40],
  lower = cp_ae$allRRlow[i40],
  upper = cp_ae$allRRhigh[i40]
)

par(mfrow = c(2,2),
    mar = c(4,4,2,1))

#3d plot
d3 <- plot(
  cp_ae,
  xlab = "\nMaximum Temperature",
  ylab = "\nLag (days)     ",
  zlab = "\nRelative Risk",
  main = "Temperature–Lag Association with A&E Attendances",
  theta = 205,
  ltheta = 170,
  phi = 35,
  shade = 0.4
)

# Increase outer plot margins so labels aren't cut off
par(mar = c(4, 4, 4, 4) + 0.1, mgp = c(2.5, 0.8, 0))

d3 <- plot(
  cp_ae,
  ptype = "3d",
  xlab = "\n\nMaximum Temperature (°C)",
  ylab = "\n\nLag (days)",
  zlab = "\n\nRelative Risk",
  main = "Temperature–Lag Association with A&E Attendances",
  theta = 205,
  phi = 25,           # Lowered angle slightly to reduce vertical compression
  expand = 0.6,       # Stretches/balances z-axis relative to x/y
  ltheta = 170,
  shade = 0.3,
  ticktype = "detailed" # Displays proper numbers on ticks instead of simple marks
)

# 1. Expand plot margins so labels and ticks don't get cut off at the border
par(mar = c(5, 5, 4, 5) + 0.1)

# 2. Render plot with explicit tick spacing
d3 <- plot(
  cp_ae,
  ptype = "3d",
  
  # Multi-line spacing pushes titles away from tick values
  xlab = "\n\n\nMaximum Temperature (°C)",
  ylab = "\n\n\nLag (days)",
  zlab = "\n\n\nRelative Risk",
  main = "Temperature–Lag Association with A&E Attendances",
  
  # Camera & Projection
  theta = 215,          # Rotated slightly to create clearer separation on the Z-axis line
  phi = 25,            # Lower angle opens up space along the floor lines
  expand = 0.65,       # Expands vertical height to fix squishing
  ltheta = 170,
  shade = 0.35,
  
  # Tick Formatting
  ticktype = "detailed", # Draws numbered ticks
  nticks = 5            # Reduces tick density so numbers don't overlap lines or each other
)
#reference at 32
lines(trans3d(x=32.2,y=0:7,z=cp_ae$matRRfit[as.character(32),],
             pmat=d3),lwd=2, col = "red")
lines(trans3d(x=36,y=0:7,z=cp_ae$matRRfit[as.character(36),],
              pmat=d3),lwd=2, col = "red")
lines(trans3d(x=cp_ae$predvar,y=2,z=cp_ae$matRRfit[,"lag2"],
              pmat=d3),lwd=2, col="red")

#lag response at 36 deg
plot(
  cp_ae,
  "slices",
  var = 31, 
  xlab = "Lag (Days)", 
  ylab = "Relative Risk",
     main = "Lag-Response at 40.5°C",
     col = "red",
      lwd =2)

# Lag-specific predictions at selected temperatures
cp_lag_selected <- crosspred(
  cb_ae,
  dlnm_ae,
  at = c(30.8, 32.2, 34.0, 35.8, 40.6),
  cen = 32.2
)

# T25 = 30.8°C
plot(
  cp_lag_selected,
  "slices",
  var = 30.8,
  xlab = "Lag (Days)",
  ylab = "Relative Risk",
  main = "Lag-Response at T25 (30.8°C)",
  lwd = 2
)

# T50 = 32.2°C
plot(
  cp_lag_selected,
  "slices",
  var = 32.2,
  xlab = "Lag (Days)",
  ylab = "Relative Risk",
  main = "Lag-Response at T50 (32.2°C)",
  lwd = 2
)

# T75 = 34.0°C
plot(
  cp_lag_selected,
  "slices",
  var = 34.0,
  xlab = "Lag (Days)",
  ylab = "Relative Risk",
  main = "Lag-Response at T75 (34.0°C)",
  lwd = 2
)

# T90 = 35.8°C
plot(
  cp_lag_selected,
  "slices",
  var = 35.8,
  xlab = "Lag (Days)",
  ylab = "Relative Risk",
  main = "Lag-Response at T90 (35.8°C)",
  lwd = 2
)

# Maximum observed temperature = 40.6°C
plot(
  cp_lag_selected,
  "slices",
  var = 40.6,
  xlab = "Lag (Days)",
  ylab = "Relative Risk",
  main = "Lag-Response at 40.6°C",
  lwd = 2
)

plot(
  cp_lag_selected,
  "slices",
  var = 30.8,
  xlab = "Lag (Days)",
  ylab = "Relative Risk",
  main = "Lag-Response at Selected Temperatures",
  lwd = 2
)

lines(cp_lag_selected, "slices", var = 32.2, lwd = 2, lty = 2)
lines(cp_lag_selected, "slices", var = 34.0, lwd = 2, lty = 3)
lines(cp_lag_selected, "slices", var = 35.8, lwd = 2, lty = 4)
lines(cp_lag_selected, "slices", var = 40.6, lwd = 2, lty = 5)

legend(
  "topright",
  legend = c(
    "T25 (30.8°C)",
    "T50 (32.2°C)",
    "T75 (34.0°C)",
    "T90 (35.8°C)",
    "40.6°C"
  ),
  lty = 1:5,
  lwd = 2,
  bty = "n"
)

par(mfrow = c(2, 2))

plot(
  cp_lag_selected, "slices",
  var = 30.8,
  xlab = "Lag (Days)", ylab = "Relative Risk",
  main = "T25 (30.8°C)",
  col = "blue", lwd = 2
)

plot(
  cp_lag_selected, "slices",
  var = 34.0,
  xlab = "Lag (Days)", ylab = "Relative Risk",
  main = "T75 (34.0°C)",
  col = "blue", lwd = 2
)

plot(
  cp_lag_selected, "slices",
  var = 35.8,
  xlab = "Lag (Days)", ylab = "Relative Risk",
  main = "T90 (35.8°C)",
  col = "red", lwd = 2
)

plot(
  cp_lag_selected, "slices",
  var = 40.6,
  xlab = "Lag (Days)", ylab = "Relative Risk",
  main = "40.6°C",
  col = "red", lwd = 2
)

par(mfrow = c(1, 1))



# Plot first line (30.8°C)
plot(
  cp_lag_selected,
  "slices",
  var = 30.8,
  xlab = "Lag (Days)",
  ylab = "Relative Risk",
  main = "Lag-Response at Selected Temperatures",
  col = cols[1],
  lty = ltys[1],
  lwd = lwds[1]
)

# Remaining temperature slices
lines(cp_lag_selected, "slices", var = 32.2, col = cols[2], lty = ltys[2], lwd = lwds[2]) # Reference
lines(cp_lag_selected, "slices", var = 34.0, col = cols[3], lty = ltys[3], lwd = lwds[3])
lines(cp_lag_selected, "slices", var = 35.8, col = cols[4], lty = ltys[4], lwd = lwds[4])
lines(cp_lag_selected, "slices", var = 40.6, col = cols[5], lty = ltys[5], lwd = lwds[5])

# Legend
legend(
  "topright",
  legend = c(
    "T25 (30.8°C)",
    "T50 (32.2°C - Ref)",
    "T75 (34.0°C)",
    "T90 (35.8°C)",
    "40.6°C"
  ),
  col = cols,
  lty = ltys,
  lwd = lwds,
  bty = "n",
  cex = 0.9
)

library(ggplot2)

# Create data for the five selected temperatures
lag_plot_data <- do.call(rbind, lapply(
  1:length(cp_lag_selected$predvar),
  function(i) {
    
    data.frame(
      temperature = cp_lag_selected$predvar[i],
      lag = 0:7,
      RR = cp_lag_selected$matRRfit[i, ],
      lower = cp_lag_selected$matRRlow[i, ],
      upper = cp_lag_selected$matRRhigh[i, ]
    )
  }
))

# Temperature labels
lag_plot_data$temp_label <- factor(
  lag_plot_data$temperature,
  levels = c(30.8, 32.2, 34.0, 35.8, 40.6),
  labels = c(
    "T25 (30.8°C)",
    "T50 (32.2°C)",
    "T75 (34.0°C)",
    "T90 (35.8°C)",
    "40.6°C"
  )
)

# Faceted lag-response plot
ggplot(
  lag_plot_data,
  aes(x = lag, y = RR)
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ temp_label, ncol = 2) +
  scale_x_continuous(
    breaks = 0:7
  ) +
  labs(
    x = "Lag (days)",
    y = "Relative Risk (RR)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 12, face = "bold")
  )

ggplot(
  lag_plot_data,
  aes(x = lag, y = RR)
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ temp_label, ncol = 2) +
  scale_x_continuous(
    breaks = 0:7
  ) +
  scale_y_continuous(
    limits = c(0.97, 1.04),
    breaks = seq(0.97, 1.04, 0.01)
  ) +
  labs(
    x = "Lag (days)",
    y = "Relative Risk (RR)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 12, face = "bold")
  )
#temp response at lag 2
plot(
  cp_ae,
  "slices",
  lag = 2,
  xlab = "Maximum temperature (°C)",
  ylab = "Relative Risk",
  main = "temp response at lag 2",
  col = "red",
  lwd = 2
)

# lag 0
plot(
  cp_ae,
  "slices",
  lag = 0,
  xlab = "Maximum temperature (°C)",
  ylab = "Relative Risk",
  main = "temp response at lag 0",
  col = "orange",
  lwd = 2
)

#overall cumulative assoc
plot(
  cp_ae, 
  "overall", 
     xlab = "Maximum Temperature (°C)", 
     ylab = "Relative Risk",
     main = "Cumulative Temperature–Response Association Across 7 Days",
     col = "darkgreen",
  lwd = 2
  )

# Overall cumulative association
plot(
  cp_ae,
  "overall",
  xlab = "Daily maximum temperature (°C)",
  ylab = "Relative risk",
  main = "",
  col = "darkgreen",
  lwd = 2
)

cp_slices <- crosspred(
  cb_ae,
  dlnm_ae,
  at = c(30.8, 35.8, 40.5),
  cen = 32.2
)
cp_slices$matRRfit
cp_slices$matRRlow
cp_slices$matRRhigh
for (i in 1:length(cp_slices$predvar)) {
  
  j <- which.max(cp_slices$matRRfit[i, ])
  
  cat(
    "Temperature:", cp_slices$predvar[i],
    "| Lag:", j - 1,
    "| RR:", round(cp_slices$matRRfit[i, j], 3),
    "| 95% CI:",
    round(cp_slices$matRRlow[i, j], 3), "-",
    round(cp_slices$matRRhigh[i, j], 3),
    "\n"
  )
}

# For each temperature, find the lag with the highest RR
max_lag_each_temp <- apply(
  cp_ae$matRRfit,
  1,
  which.max
) - 1

# Count how often each lag is the maximum
table(max_lag_each_temp)
data.frame(
  temperature = cp_ae$predvar,
  max_lag = max_lag_each_temp
)

sum(data_complete$temp_max == 40.6, na.rm = TRUE)

quantile(
  data_complete$temp_max,
  probs = 0.75,
  na.rm = TRUE
)
cp_selected <- crosspred(
  cb_ae,
  dlnm_ae,
  at = c(34, 40.6),
  cen = 32.2
)
cp_selected$matRRfit
cp_selected$matRRlow
cp_selected$matRRhigh
for (temp in c(34, 40.6)) {
  
  i <- which(cp_selected$predvar == temp)
  j <- which.max(cp_selected$matRRfit[i, ])
  
  cat(
    "Temperature:", temp,
    "| Highest lag:", j - 1,
    "| RR:", round(cp_selected$matRRfit[i, j], 3),
    "| 95% CI:",
    round(cp_selected$matRRlow[i, j], 3), "-",
    round(cp_selected$matRRhigh[i, j], 3),
    "\n"
  )
}
# Cumulative RR
cp_selected$allRRfit

# Lower 95% CI
cp_selected$allRRlow

# Upper 95% CI
cp_selected$allRRhigh


cp_lag <- crosspred(
  cb_ae,
  dlnm_ae,
  at = c(30.8, 34, 35.8, 40.6),
  cen = 32.2
)
temps_interest <- c(30.8, 34, 35.8, 40.6)

for (temp in temps_interest) {
  
  i <- which(cp_lag$predvar == temp)
  
  j_max <- which.max(cp_lag$matRRfit[i, ])
  j_min <- which.min(cp_lag$matRRfit[i, ])
  
  cat(
    "\nTemperature:", temp,
    "\n  Maximum RR: lag", j_max - 1,
    "| RR =", round(cp_lag$matRRfit[i, j_max], 3),
    "| 95% CI:", round(cp_lag$matRRlow[i, j_max], 3),
    "-", round(cp_lag$matRRhigh[i, j_max], 3),
    
    "\n  Minimum RR: lag", j_min - 1,
    "| RR =", round(cp_lag$matRRfit[i, j_min], 3),
    "| 95% CI:", round(cp_lag$matRRlow[i, j_min], 3),
    "-", round(cp_lag$matRRhigh[i, j_min], 3),
    
    "\n  Lag-specific RR range:",
    round(min(cp_lag$matRRfit[i, ]), 3),
    "-",
    round(max(cp_lag$matRRfit[i, ]), 3),
    "\n"
  )
}
  
temps_interest <- c(30.8, 34, 35.8, 40.6)

for (temp in temps_interest) {
  
  i <- which(cp_lag$predvar == temp)
  
  rr <- cp_lag$matRRfit[i, c("lag0", "lag1", "lag2", "lag3")]
  
  cat("\nTemperature:", temp, "\n")
  print(round(rr, 3))
}
for (temp in temps_interest) {
  
  i <- which(cp_lag$predvar == temp)
  
  rr <- cp_lag$matRRfit[i, c("lag0", "lag1", "lag2", "lag3")]
  
  cat(
    "\nTemperature:", temp,
    "| All RR > 1:", all(rr > 1),
    "\n"
  )
}

for (temp in temps_interest) {
  
  i <- which(cp_lag$predvar == temp)
  
  results <- data.frame(
    lag = 0:3,
    RR = cp_lag$matRRfit[i, c("lag0", "lag1", "lag2", "lag3")],
    lower95 = cp_lag$matRRlow[i, c("lag0", "lag1", "lag2", "lag3")],
    upper95 = cp_lag$matRRhigh[i, c("lag0", "lag1", "lag2", "lag3")]
  )
  
  cat("\nTemperature:", temp, "\n")
  print(round(results, 3))
}
library(ggplot2)

# predict based on scenarios
august_2025 <- c(
  29.7, 31.1, 32.8, 30.5, 30.1, 30.5, 31.9,  
  32.6, 32.3, 34.5, 33.3, 33.6, 33.2, 33.4, 
  33.5, 31.2, 29.8, 31.7, 31.3, 29.8, 31.6, 
  34.1, 31.7, 31.9, 31.4, 29.6, 31.5, 30.5, 30.2, 30.8, 31.8 
)


total_days <- length(august_2025)

timeline_dows <- rep(c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"), length.out = total_days)

forecast_df <- data.frame(
  t = 1:total_days,
  temp_max = august_2025,
  dow = timeline_dows,
  year = "2025" 
)


# running scenarios using dlnm!!
# scenario 1: august 2025
cb_forecast <- crossbasis(forecast_df$temp_max, lag = 7, xvec = data_complete$temp_max,
                          argvar = attributes(cb_ae)$argvar, 
                          arglag = attributes(cb_ae)$arglag)

forecast_df$cb_ae <- cb_forecast
forecast_df$year  <- factor(forecast_df$year, levels = levels(factor(data_complete$year)))
forecast_df$dow   <- factor(forecast_df$dow, levels = levels(factor(data_complete$dow)))

forecast_df$predict_ae <- round(predict(dlnm_ae, newdata = forecast_df, type = "response"))
pred <- predict(dlnm_ae,
                newdata = forecast_df,
                type = "response",
                se.fit = TRUE)

str(pred)
sum(is.na(pred$fit))
pred$fit

# scenario 2: baseline (min observed)
dataset_baseline_min <- min(data_complete$temp_max, na.rm = TRUE)
baseline_scenario <- data.frame(
  t = 1:total_days,
  temp_max = rep(dataset_baseline_min, total_days),
  dow = timeline_dows,
  year = "2025" 
)


cb_baseline  <- crossbasis(baseline_scenario$temp_max, lag = 7, xvec = data_complete$temp_max,
                           argvar = attributes(cb_ae)$argvar, 
                           arglag = attributes(cb_ae)$arglag)

baseline_scenario$cb_ae <- cb_baseline
baseline_scenario$year  <- factor(baseline_scenario$year, levels = levels(factor(data_complete$year)))
baseline_scenario$dow   <- factor(baseline_scenario$dow, levels = levels(factor(data_complete$dow)))

baseline_scenario$predict_ae <- round(predict(dlnm_ae, newdata = baseline_scenario, type = "response"))

baseline_forecast_table <- baseline_scenario %>%
  dplyr::select(t, dow, year, temp_max, predict_ae) %>%
  rename(
    "Day" = t,
    "Day of Week" = dow,
    "Year" = year,
    "Forecasted Temp (°C)" = temp_max,
    "Predicted A&E Attendances" = predict_ae
  )

print(baseline_forecast_table, row.names = FALSE)

# scenario 3: hot (max observed)
dataset_hot_max <- max(data_complete$temp_max, na.rm = TRUE)
hot_scenario <- data.frame(
  t = 1:total_days,
  temp_max = rep(dataset_hot_max, total_days), 
  dow = timeline_dows,
  year = "2025" 
)


cb_hot  <- crossbasis(hot_scenario$temp_max, lag = 7, xvec = data_complete$temp_max,
                           argvar = attributes(cb_ae)$argvar, 
                           arglag = attributes(cb_ae)$arglag)

hot_scenario$cb_ae <- cb_hot
hot_scenario$year  <- factor(hot_scenario$year, levels = levels(factor(data_complete$year)))
hot_scenario$dow   <- factor(hot_scenario$dow, levels = levels(factor(data_complete$dow)))

hot_scenario$predict_ae <- round(predict(dlnm_ae, newdata = hot_scenario, type = "response"))

hot_forecast_table <- hot_scenario %>%
  dplyr::select(t, dow, year, temp_max, predict_ae) %>%
  rename(
    "Day" = t,
    "Day of Week" = dow,
    "Year" = year,
    "Forecasted Temp (°C)" = temp_max,
    "Predicted A&E Attendances" = predict_ae
  )

print(hot_forecast_table, row.names = FALSE)


historical_augusts <- data_complete[format(as.Date(data_complete$date), "%m") == "08", ]
calculated_aug_mean <- mean(historical_augusts$temp_max, na.rm = TRUE)
calculated_aug_mean

# scenario 4: typical heatwave
typical_heatwave <- c(
  29.7, 31.1, 32.8, 30.5, 30.1, 30.5, 31.9,  
  32.6, 32.3, 34.5, 
  37.9, 38.6, 38.9, 37.6, # days 11, 12, 13, 14 (heatwave event)
  33.5, 31.2, 29.8, 31.7, 31.3, 29.8, 31.6,  
  34.1, 31.7, 31.9, 31.4, 29.6, 31.5, 30.5, 30.2, 30.8, 31.8 
)

typical_df <- data.frame(
  t = 1:total_days,
  temp_max = typical_heatwave,
  dow = timeline_dows,
  year = "2025" 
)

cb_typical <- crossbasis(typical_df$temp_max, lag = 7, xvec = data_complete$temp_max,
                         argvar = attributes(cb_ae)$argvar, 
                         arglag = attributes(cb_ae)$arglag)

typical_df$cb_ae <- cb_typical
typical_df$year  <- factor(typical_df$year, levels = levels(factor(data_complete$year)))
typical_df$dow   <- factor(typical_df$dow, levels = levels(factor(data_complete$dow)))

pred_typical <- predict(dlnm_ae, newdata = typical_df, type = "link", se.fit = TRUE)
fit_typ <- as.numeric(pred_typical$fit)
se_typ  <- as.numeric(pred_typical$se.fit)

typical_df$predict_ae <- round(exp(fit_typ))
typical_df$lower      <- round(exp(fit_typ - (1.96 * se_typ)))
typical_df$upper      <- round(exp(fit_typ + (1.96 * se_typ)))

# scenario 5: prolonged heatwave
prolonged_heatwave <- c(
  29.7, 31.1, 32.8, 30.5, 30.1, 30.5, 31.9,  
  32.6, 32.3, 34.5, 
  37.7, 38.9, 40.2, 40.5, 40.6, 40.4, 39.6, 39.2, 38.3, 37.5, # days 11-20 (10 day heatwave)
  31.6, 34.1, 31.7, 31.9, 31.4, 29.6, 31.5, 30.5, 30.2, 30.8, 31.8 
)

prolonged_df <- data.frame(
  t = 1:total_days,
  temp_max = prolonged_heatwave,
  dow = timeline_dows,
  year = "2025" 
)

cb_prolonged <- crossbasis(prolonged_df$temp_max, lag = 7, xvec = data_complete$temp_max,
                           argvar = attributes(cb_ae)$argvar, 
                           arglag = attributes(cb_ae)$arglag)

prolonged_df$cb_ae <- cb_prolonged
prolonged_df$year  <- factor(prolonged_df$year, levels = levels(factor(data_complete$year)))
prolonged_df$dow   <- factor(prolonged_df$dow, levels = levels(factor(data_complete$dow)))

pred_prolonged <- predict(dlnm_ae, newdata = prolonged_df, type = "link", se.fit = TRUE)
fit_pro <- as.numeric(pred_prolonged$fit)
se_pro  <- as.numeric(pred_prolonged$se.fit)

prolonged_df$predict_ae <- round(exp(fit_pro))
prolonged_df$lower      <- round(exp(fit_pro - (1.96 * se_pro)))
prolonged_df$upper      <- round(exp(fit_pro + (1.96 * se_pro)))

# scenario 6: year 2100 max - high emissions, + 3.7 to typical heatwave
future_temps_max <- prolonged_heatwave + 3.7
future_temps_df <- data.frame(
  t = 1:total_days,
  temp_max = future_temps_max,
  dow = timeline_dows,
  year = "2025" 
)

cb_future <- crossbasis(future_temps_df$temp_max, lag = 7, xvec = data_complete$temp_max,
                           argvar = attributes(cb_ae)$argvar, 
                           arglag = attributes(cb_ae)$arglag)

future_temps_df$cb_ae <- cb_future
future_temps_df$year  <- factor(future_temps_df$year, levels = levels(factor(data_complete$year)))
future_temps_df$dow   <- factor(future_temps_df$dow, levels = levels(factor(data_complete$dow)))

pred_future <- predict(dlnm_ae, newdata = future_temps_df, type = "link", se.fit = TRUE)
fit_fut <- as.numeric(pred_future$fit)
se_fut  <- as.numeric(pred_future$se.fit)

future_temps_df$predict_ae <- round(exp(fit_fut))
future_temps_df$lower      <- round(exp(fit_fut - (1.96 * se_fut)))
future_temps_df$upper      <- round(exp(fit_fut + (1.96 * se_fut)))
future_temps_table <- future_temps_df %>%
  dplyr::select(t, dow, year, temp_max, predict_ae) %>%
  rename(
    "Day" = t,
    "Day of Week" = dow,
    "Year" = year,
    "Forecasted Temp (°C)" = temp_max,
    "Predicted A&E Attendances" = predict_ae
  )

print(future_temps_table, row.names = FALSE)


# plotting 
plot_range <- 8:total_days
day_labels <- paste0("Day ", plot_range, " (", timeline_dows[plot_range], ")")


all_points <- c(baseline_scenario$predict_ae[plot_range], future_temps_df$predict_ae[plot_range])
min_y <- min(all_points, na.rm = TRUE) - 10
max_y <- max(all_points, na.rm = TRUE) + 30 # Leaves clean margin space for the legend

par(mar = c(7, 4.1, 4.1, 2.1))

plot(forecast_df$t[plot_range], forecast_df$predict_ae[plot_range], 
     type = "n", ylim = c(min_y, max_y), xlab = "", ylab = "Predicted A&E Attendances", 
     main = "August Heatwave Scenario Projections", xaxt = "n")

rect(xleft = 11, ybottom = min_y - 20, xright = 14, ytop = max_y + 20, 
     col = adjustcolor("gold", alpha.f = 0.15), border = NA) 
rect(xleft = 11, ybottom = min_y - 20, xright = 20, ytop = max_y + 20, 
     col = adjustcolor("darkorange", alpha.f = 0.08), border = NA) 
abline(v = c(11, 20), col = "gray60", lty = 2)

lines(baseline_scenario$t[plot_range], baseline_scenario$predict_ae[plot_range], col = "blue", lwd = 2, lty = 3)
lines(hot_scenario$t[plot_range],      hot_scenario$predict_ae[plot_range],      col = "red", lwd = 2, lty = 3)
lines(forecast_df$t[plot_range],   forecast_df$predict_ae[plot_range],   col = "darkgreen", lwd = 2)
lines(typical_df$t[plot_range],    typical_df$predict_ae[plot_range],    col = "gold", lwd = 2)
lines(prolonged_df$t[plot_range],  prolonged_df$predict_ae[plot_range],  col = "darkorange", lwd = 2)
lines(future_temps_df$t[plot_range], future_temps_df$predict_ae[plot_range], col = "purple", lwd = 2)

axis(side = 1, at = plot_range, labels = FALSE)
text(x = plot_range, y = par("usr")[3] - 2, labels = day_labels, 
     srt = 45, adj = c(1, 1), xpd = TRUE, cex = 0.65)

legend("topright", 
       legend = c(
         "Baseline (Minimum Observed)",
         "Hot (Maximum Observed)",
         "August 2025",
         "Typical Heatwave (4 Day Block)",
         "Prolonged Heatwave (10 Day Block)",
         "Future-Adjusted Heatwave (+3.7°C)",
         "Active Heatwave Window"
       ), 
       col = c("blue", "red", "darkgreen", "gold", "darkorange", "purple", adjustcolor("darkorange", alpha.f = 0.2)), 
       lty = c(3, 3, 1, 1,  1, 1, NA), 
       lwd = c(2, 2, 2.5, 2.5, 2.5, 3, NA),
       pch = c(NA, NA, NA, NA, NA, NA, 15), # Adds a solid square block icon for the shaded region row
       pt.cex = c(NA, NA, NA, NA, NA, NA, 2),
       bty = "n", cex = 0.75)

# neater facet plotting ae
par(mfrow = c(3, 1), mar = c(1.5, 4.5, 1.0, 1), oma = c(8, 0, 3, 0))

plot_range <- 8:total_days
day_labels <- paste0("Day ", plot_range, " (", timeline_dows[plot_range], ")")

# ------------------------------------------
# PANEL 1: Typical Heatwave
# ------------------------------------------
ylim_a <- c(350, max(typical_df$upper[plot_range], na.rm = TRUE) + 10)

plot(forecast_df$t[plot_range], forecast_df$predict_ae[plot_range], type = "n", 
     ylim = ylim_a, xlab = "", ylab = "Attendances", 
     main = "Scenario A: Typical Heatwave (4-Day Block)", xaxt = "n", las = 1)

# Heatwave Window Highlight
rect(xleft = 11, ybottom = par("usr")[3], xright = 14, ytop = par("usr")[4], 
     col = adjustcolor("gold", alpha.f = 0.10), border = NA)
abline(v = c(11, 14), col = "gray70", lty = 2)

# DLNM Confidence Interval Shading
polygon(c(typical_df$t[plot_range], rev(typical_df$t[plot_range])),
        c(typical_df$lower[plot_range], rev(typical_df$upper[plot_range])),
        col = adjustcolor("gold", alpha.f = 0.25), border = NA)

# Lines
lines(baseline_scenario$t[plot_range], baseline_scenario$predict_ae[plot_range], col = "blue", lwd = 1.5, lty = 3)
lines(hot_scenario$t[plot_range],      hot_scenario$predict_ae[plot_range],      col = "red", lwd = 1.5, lty = 3)
lines(forecast_df$t[plot_range],       forecast_df$predict_ae[plot_range],       col = "darkgreen", lwd = 1.5)
lines(typical_df$t[plot_range],        typical_df$predict_ae[plot_range],        col = "gold", lwd = 3)


# ------------------------------------------
# PANEL 2: Prolonged Heatwave
# ------------------------------------------
ylim_b <- c(350, max(prolonged_df$upper[plot_range], na.rm = TRUE) + 10)

plot(forecast_df$t[plot_range], forecast_df$predict_ae[plot_range], type = "n", 
     ylim = ylim_b, xlab = "", ylab = "Attendances", 
     main = "Scenario B: Prolonged Heatwave (10-Day Block)", xaxt = "n", las = 1)

rect(xleft = 11, ybottom = par("usr")[3], xright = 20, ytop = par("usr")[4], 
     col = adjustcolor("darkorange", alpha.f = 0.06), border = NA)
abline(v = c(11, 20), col = "gray70", lty = 2)

# DLNM Confidence Interval Shading
polygon(c(prolonged_df$t[plot_range], rev(prolonged_df$t[plot_range])),
        c(prolonged_df$lower[plot_range], rev(prolonged_df$upper[plot_range])),
        col = adjustcolor("darkorange", alpha.f = 0.20), border = NA)

# Lines
lines(baseline_scenario$t[plot_range], baseline_scenario$predict_ae[plot_range], col = "blue", lwd = 1.5, lty = 3)
lines(hot_scenario$t[plot_range],      hot_scenario$predict_ae[plot_range],      col = "red", lwd = 1.5, lty = 3)
lines(forecast_df$t[plot_range],       forecast_df$predict_ae[plot_range],       col = "darkgreen", lwd = 1.5)
lines(prolonged_df$t[plot_range],      prolonged_df$predict_ae[plot_range],      col = "darkorange", lwd = 3)


# ------------------------------------------
# PANEL 3: Future Heatwave (+3.7°C)
# ------------------------------------------
ylim_c <- c(350, 580)

plot(forecast_df$t[plot_range], forecast_df$predict_ae[plot_range], type = "n", 
     ylim = ylim_c, xlab = "", ylab = "Attendances", 
     main = "Scenario C: Future Prolonged Heatwave (+3.7°C)", xaxt = "n", las = 1)

rect(xleft = 11, ybottom = par("usr")[3], xright = 20, ytop = par("usr")[4], 
     col = adjustcolor("purple", alpha.f = 0.05), border = NA)
abline(v = c(11, 20), col = "gray70", lty = 2)

# DLNM Confidence Interval Shading
polygon(c(future_temps_df$t[plot_range], rev(future_temps_df$t[plot_range])),
        c(future_temps_df$lower[plot_range], rev(future_temps_df$upper[plot_range])),
        col = adjustcolor("purple", alpha.f = 0.15), border = NA)

# Lines
lines(baseline_scenario$t[plot_range], baseline_scenario$predict_ae[plot_range], col = "blue", lwd = 1.5, lty = 3)
lines(hot_scenario$t[plot_range],      hot_scenario$predict_ae[plot_range],      col = "red", lwd = 1.5, lty = 3)
lines(forecast_df$t[plot_range],       forecast_df$predict_ae[plot_range],       col = "darkgreen", lwd = 1.5)
lines(future_temps_df$t[plot_range],   future_temps_df$predict_ae[plot_range],   col = "purple", lwd = 3)


# ==========================================
# COMMON X-AXIS (VERTICAL LABELS)
# ==========================================
# Native vertical rotation (las = 2) aligned cleanly under the plots
axis(side = 1, at = plot_range, labels = day_labels, las = 2, cex.axis = 0.75)

mtext("August Heatwave Scenario Projections (DLNM)", outer = TRUE, side = 3, line = 0.8, font = 2, cex = 1.2)


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)

# Explicitly define the 0 to 1 coordinate system
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlim = c(0, 1), ylim = c(0, 1))

legend("bottom", 
       legend = c("Min Historical", "Max Historical", "August 2025", "Active Scenario Trend", "Heatwave Window"), 
       col = c("blue", "red", "darkgreen", "black", adjustcolor("gray60", alpha.f = 0.2)), 
       lty = c(3, 3, 1, 1, NA), 
       lwd = c(1.5, 1.5, 1.5, 3, NA), 
       pch = c(NA, NA, NA, NA, 15),
       pt.cex = 1.1, 
       bty = "n", 
       horiz = TRUE,
       xpd = TRUE,
       cex = 0.75,         
       x.intersp = 0.3,   # Pulls lines/boxes closer to text
       # Identical tight width constraints:
       text.width = c(0.08, 0.15, 0.12, 0.14, 0.10) 
)

# Maximum predicted attendance during each heatwave period
typical_max <- typical_df %>%
  filter(t >= 11, t <= 14) %>%
  slice_max(predict_ae, n = 1)

prolonged_max <- prolonged_df %>%
  filter(t >= 11, t <= 20) %>%
  slice_max(predict_ae, n = 1)

future_max <- future_temps_df %>%
  filter(t >= 11, t <= 20) %>%
  slice_max(predict_ae, n = 1)

typical_max
prolonged_max
future_max

typical_df %>%
  filter(t >= 11, t <= 14) %>%
  summarise(
    mean_predicted = mean(predict_ae),
    min_predicted = min(predict_ae),
    max_predicted = max(predict_ae)
  )

prolonged_df %>%
  filter(t >= 11, t <= 20) %>%
  summarise(
    mean_predicted = mean(predict_ae),
    min_predicted = min(predict_ae),
    max_predicted = max(predict_ae)
  )

future_temps_df %>%
  filter(t >= 11, t <= 20) %>%
  summarise(
    mean_predicted = mean(predict_ae),
    min_predicted = min(predict_ae),
    max_predicted = max(predict_ae)
  )

baseline_scenario %>%
  filter(t >= 11, t <= 20) %>%
  summarise(
    mean_predicted = mean(predict_ae),
    min_predicted = min(predict_ae),
    max_predicted = max(predict_ae)
  )

hot_scenario %>%
  filter(t >= 11, t <= 20) %>%
  summarise(
    mean_predicted = mean(predict_ae),
    min_predicted = min(predict_ae),
    max_predicted = max(predict_ae)
  )

forecast_df %>%
  filter(t >= 11, t <= 20) %>%
  summarise(
    mean_predicted = mean(predict_ae),
    min_predicted = min(predict_ae),
    max_predicted = max(predict_ae)
  )

# excess
excess_df <- data.frame(
  t = hotrio$t,nario$dow,
  year = hot_scenario$year,
  temp_max = hot_scenario$temp_max,
  predict_ae = hot_scenario$predict_ae - baseline_scenario$predict_ae
)

excess_forecast_table <- excess_df %>%
  dplyr::select(t, dow, year, temp_max, predict_ae) %>%
  rename(
    "Day" = t,
    "Day of Week" = dow,
    "Year" = year,
    "Forecasted Temp (°C)" = temp_max,
    "Predicted A&E Attendances" = predict_ae
  )

print(excess_forecast_table, row.names = FALSE)


min_y_ex <- min(excess_df$predict_ae[plot_range], na.rm = TRUE) - 2
max_y_ex <- max(excess_df$predict_ae[plot_range], na.rm = TRUE) + 2

par(mar = c(7, 4.1, 4.1, 2.1))

plot(excess_df$t[plot_range], excess_df$predict_ae[plot_range], type = "l", col = "purple", lwd = 2,
     ylim = c(min_y_ex, max_y_ex), xlab = "", ylab = "Predicted A&E Attendances", 
     main = "August Excess Projections (Hot - Baseline)", xaxt = "n")

axis(side = 1, at = plot_range, labels = FALSE)
text(x = plot_range, 
     y = par("usr")[3] - (par("usr")[4] - par("usr")[3]) * 0.02, 
     labels = day_labels, 
     srt = 45, 
     adj = c(1, 1), 
     xpd = TRUE,           
     cex = 0.65)



# LSTM 
#install.packages("keras3")
library(keras3)
#install_keras()

train_df <- data_complete %>% filter(as.numeric(as.character(year)) <= 2023)
val_df   <- data_complete %>% filter(as.numeric(as.character(year)) >= 2024)

day_train <- model.matrix(~ dow - 1, data = train_df)
day_val   <- model.matrix(~ dow - 1, data = val_df)

ad_mean   <- mean(train_df$ae_attendances, na.rm = TRUE)
ad_sd     <- sd(train_df$ae_attendances, na.rm = TRUE)
if (ad_sd == 0) ad_sd <- 1

maxT_mean <- mean(train_df$temp_max, na.rm = TRUE)
maxT_sd   <- sd(train_df$temp_max, na.rm = TRUE)
if (maxT_sd == 0) maxT_sd <- 1

train_ad_sc   <- (train_df$ae_attendances - ad_mean) / ad_sd
train_maxT_sc <- (train_df$temp_max       - maxT_mean) / maxT_sd

val_ad_sc     <- (val_df$ae_attendances   - ad_mean) / ad_sd
val_maxT_sc   <- (val_df$temp_max         - maxT_mean) / maxT_sd

x_train_mat <- cbind(
  ae_attendances = train_ad_sc,
  temp_max       = train_maxT_sc,
  day_train
)

x_val_mat <- cbind(
  ae_attendances = val_ad_sc,
  temp_max       = val_maxT_sc,
  day_val
)

create_sequences <- function(x, y, timesteps = 7) {
  n <- nrow(x)
  n_features <- ncol(x)
  
  X <- array(NA_real_, dim = c(n - timesteps, timesteps, n_features))
  Y <- numeric(n - timesteps)
  
  for (i in 1:(n - timesteps)) {
    X[i, , ] <- as.matrix(x[i:(i + timesteps - 1), ])
    Y[i] <- y[i + timesteps]
  }
  
  list(X = X, Y = Y)
}

seq_train <- create_sequences(x = x_train_mat, y = train_ad_sc, timesteps = 7)
seq_val   <- create_sequences(x = x_val_mat,   y = val_ad_sc,   timesteps = 7)

x_train <- seq_train$X
y_train <- seq_train$Y
x_val   <- seq_val$X
y_val   <- seq_val$Y

n_features <- dim(x_train)[3]

model <- keras_model_sequential(input_shape = c(7, n_features)) |>
  layer_lstm(units = 32, return_sequences = TRUE) |> 
  layer_dropout(rate = 0.1) |>
  layer_lstm(units = 16) |> 
  layer_dropout(rate = 0.1) |>
  layer_dense(units = 1)

model |> compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mean_squared_error",
  metrics = c("mean_absolute_error")
)

early_stop <- callback_early_stopping(
  monitor = "val_loss", 
  patience = 10,         # stops if validation loss doesn't improve for 10 straight epochs
  restore_best_weights = TRUE
)


history <- model |> fit(
  x_train, y_train,
  epochs = 80,          
  batch_size = 16,
  validation_data = list(x_val, y_val),
  callbacks = list(early_stop), 
  verbose = 1
)

pred_sc <- model |> predict(x_val)
pred    <- as.numeric(pred_sc) * ad_sd + ad_mean
actual  <- y_val * ad_sd + ad_mean

rmse <- sqrt(mean((pred - actual)^2))
r2   <- 1 - sum((pred - actual)^2) / sum((actual - mean(actual))^2)

print(paste("Validation RMSE:", round(rmse, 2)))
print(paste("Validation R-Squared:", round(r2, 4)))

par(mar = c(5, 5, 4, 2) + 0.1)

plot(actual, type = "l", col = "blue", lwd = 2,
     ylab = "A&E Attendances", 
     xlab = "Validation Days (2024 and 2025)", 
     main = "LSTM Model",
     las = 1) 

lines(pred, col = "red", lwd = 2)
legend("topleft", 
       legend = c("Actual", "Predicted"), 
       col = c("blue", "red"), 
       lty = 1, lwd = 2,
       bty = "n") #

# running lstm for scenarios
aug_2025_data <- data_complete %>% 
  filter(format(as.Date(date), "%m") == "08" & as.numeric(as.character(year)) == 2025)

if(nrow(aug_2025_data) != 31) {
  stop("Ensure data_complete contains all 31 days of August 2025.")
}

dow_factor <- factor(timeline_dows, levels = levels(factor(data_complete$dow)))
dow_matrix <- model.matrix(~ dow_factor - 1)

aug_ad_sc <- (aug_2025_data$ae_attendances - ad_mean) / ad_sd

predict_lstm_scenario <- function(temp_vector) {
  temp_sc <- (temp_vector - maxT_mean) / maxT_sd
  

  sc_matrix <- cbind(
    ae_attendances = aug_ad_sc,
    temp_max       = temp_sc,
    dow_matrix
  )
  
  sc_seq <- create_sequences(x = sc_matrix, y = aug_ad_sc, timesteps = 7)
  
  pred_sc <- model %>% predict(sc_seq$X)
  

  pred_real <- as.numeric(pred_sc) * ad_sd + ad_mean
  return(pred_real)
}

lstm_normal     <- predict_lstm_scenario(august_2025)
lstm_baseline   <- predict_lstm_scenario(rep(dataset_baseline_min, total_days))
lstm_hot        <- predict_lstm_scenario(rep(dataset_hot_max, total_days))
lstm_typical    <- predict_lstm_scenario(typical_heatwave)
lstm_prolonged  <- predict_lstm_scenario(prolonged_heatwave)
lstm_future     <- predict_lstm_scenario(future_temps_max)


par(mfrow = c(3, 1), mar = c(1.5, 4.5, 1.0, 1), oma = c(7, 0, 3.5, 0))

plot_range <- 8:total_days
day_labels <- paste0("Day ", plot_range, " (", timeline_dows[plot_range], ")")

#panel 1
ylim_a <- c(350, max(lstm_typical, na.rm = TRUE) + 10)

plot(plot_range, lstm_normal, type = "n", 
     ylim = ylim_a, xlab = "", ylab = "Attendances", 
     main = "Scenario A: Typical Heatwave (4-Day Block)", xaxt = "n", las = 1)

rect(xleft = 11, ybottom = par("usr")[3], xright = 14, ytop = par("usr")[4], 
     col = adjustcolor("gold", alpha.f = 0.10), border = NA)
abline(v = c(11, 14), col = "gray70", lty = 2)

lines(plot_range, lstm_baseline, col = "blue", lwd = 1.5, lty = 3)
lines(plot_range, lstm_hot, col = "red", lwd = 1.5, lty = 3)
lines(plot_range, lstm_normal, col = "darkgreen", lwd = 1.5)
lines(plot_range, lstm_typical, col = "gold", lwd = 3)


# panel 2
ylim_b <- c(350, max(lstm_prolonged, na.rm = TRUE) + 10)

plot(plot_range, lstm_normal, type = "n", 
     ylim = ylim_b, xlab = "", ylab = "Attendances", 
     main = "Scenario B: Prolonged Heatwave (10-Day Block)", xaxt = "n", las = 1)

rect(xleft = 11, ybottom = par("usr")[3], xright = 20, ytop = par("usr")[4], 
     col = adjustcolor("darkorange", alpha.f = 0.06), border = NA)
abline(v = c(11, 20), col = "gray70", lty = 2)

lines(plot_range, lstm_baseline, col = "blue", lwd = 1.5, lty = 3)
lines(plot_range, lstm_hot, col = "red", lwd = 1.5, lty = 3)
lines(plot_range, lstm_normal, col = "darkgreen", lwd = 1.5)
lines(plot_range, lstm_prolonged, col = "darkorange", lwd = 3)


#panel 3
ylim_c <- c(350, 460)

plot(plot_range, lstm_normal, type = "n", 
     ylim = ylim_c, xlab = "", ylab = "Attendances", 
     main = "Scenario C: Future Prolonged Heatwave (+3.7°C)", xaxt = "n", las = 1)

rect(xleft = 11, ybottom = par("usr")[3], xright = 20, ytop = par("usr")[4], 
     col = adjustcolor("purple", alpha.f = 0.05), border = NA)
abline(v = c(11, 20), col = "gray70", lty = 2)

lines(plot_range, lstm_baseline, col = "blue", lwd = 1.5, lty = 3)
lines(plot_range, lstm_hot, col = "red", lwd = 1.5, lty = 3)
lines(plot_range, lstm_normal, col = "darkgreen", lwd = 1.5)
lines(plot_range, lstm_future, col = "purple", lwd = 3)

axis(side = 1, at = plot_range, labels = day_labels, las = 2, cex.axis = 0.75)

mtext("August Heatwave Scenario Projections (LSTM)", outer = TRUE, side = 3, line = 1, font = 2, cex = 1.2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)

plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlim = c(0, 1), ylim = c(0, 1))

legend("bottom", 
       legend = c("Min Baseline", "Max Historical Baseline", "Normal August 2025", "Active Scenario Trend", "Heatwave Window"), 
       col = c("blue", "red", "darkgreen", "black", adjustcolor("gray60", alpha.f = 0.2)), 
       lty = c(3, 3, 1, 1, NA), 
       lwd = c(1.5, 1.5, 1.5, 3, NA), 
       pch = c(NA, NA, NA, NA, 15),
       pt.cex = 1.1, 
       bty = "n", 
       horiz = TRUE,
       xpd = TRUE,
       cex = 0.75,         
       x.intersp = 0.3,  
       text.width = c(0.08, 0.15, 0.12, 0.14, 0.10) 
)

# checking LSTM for additive vs consecutive
rounded_temps <- round(data_complete$temp_max)
temp_counts <- table(rounded_temps)
print(temp_counts)

temp_base <- 30.8 # 25th percentile 30.8
temp_base

temp_spike <- 35.8 # 90th percentile 35.8
temp_spike

flat_baseline <- rep(temp_base, total_days)
spike_day13 <- flat_baseline
spike_day13[13] <- temp_spike

spike_day14 <- flat_baseline
spike_day14[14] <- temp_spike

consecutive_spikes <- flat_baseline
consecutive_spikes[13:14] <- temp_spike

pred_baseline    <- predict_lstm_scenario(flat_baseline)
pred_spike13     <- predict_lstm_scenario(spike_day13)
pred_spike14     <- predict_lstm_scenario(spike_day14)
pred_consecutive <- predict_lstm_scenario(consecutive_spikes)

excess_13 <- pred_spike13 - pred_baseline
excess_14 <- pred_spike14 - pred_baseline

theoretical_additive <- excess_13 + excess_14
actual_consecutive <- pred_consecutive - pred_baseline

comparison_table <- data.frame(
  Day = day_labels,
  Baseline_Pred = pred_baseline[plot_range],
  Spike_13_Pred = pred_spike13[plot_range],
  Spike_14_Pred = pred_spike14[plot_range],
  Consecutive_Pred = pred_consecutive[plot_range],
  Excess_Day_13_Only = excess_13[plot_range],
  Excess_Day_14_Only = excess_14[plot_range],
  Theoretical_Sum = theoretical_additive[plot_range],
  Actual_Consecutive = actual_consecutive[plot_range],
  Difference = actual_consecutive[plot_range] - theoretical_additive[plot_range]
)

print(comparison_table, row.names = FALSE)

cat("3-day theoretical additive effect:",
    sum(theoretical_additive[13:15]), "\n")

cat("3-day consecutive effect:",
    sum(actual_consecutive[13:15]), "\n")

cat("3-day difference:",
    sum(actual_consecutive[13:15]) -
      sum(theoretical_additive[13:15]), "\n")

cat("Sum of daily differences:",
    sum(
      actual_consecutive[13:15] -
        theoretical_additive[13:15]
    ), "\n")



# XGBOOST
library(xgboost)
library(SHAPforxgboost)
library(shapviz)

xgdata <- data_complete %>% 
  arrange(year, date) %>%
  group_by(year) %>%
  mutate(
    lag1 = dplyr::lag(temp_max, 1),
    lag2 = dplyr::lag(temp_max, 2)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag1) & !is.na(lag2))

dow_mat <- model.matrix(~ dow - 1, data = xgdata) 
feature_cols <- c("temp_max", "lag1", "lag2")
X_all <- cbind(as.matrix(xgdata[, feature_cols]), dow_mat)
y_all <- xgdata$ae_attendances

# Match LSTM setup
train_mask <- as.numeric(as.character(xgdata$year)) <= 2023
test_mask  <- as.numeric(as.character(xgdata$year)) >= 2024

X_train <- X_all[train_mask, ]
y_train <- y_all[train_mask]

X_test  <- X_all[test_mask, ]
y_test  <- y_all[test_mask]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# Standard XGBoost (max_depth = 3 allows temperature & lag interactions)
model_standard <- xgb.train(
  params = list(
    objective = "count:poisson", 
    max_depth = 3, 
    learning_rate = 0.1
  ),
  data    = dtrain,
  nrounds = 100
)

# Additive XGBoost (max_depth = 1 prevents feature interactions)
model_additive <- xgb.train(
  params = list(
    objective = "count:poisson", 
    max_depth = 1, 
    learning_rate = 0.1
  ),
  data    = dtrain,
  nrounds = 100
)


calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

pred_standard <- predict(model_standard, dtest)
pred_additive <- predict(model_additive, dtest)

rmse_standard <- calculate_rmse(y_test, pred_standard)
rmse_additive <- calculate_rmse(y_test, pred_additive)

error_table <- data.frame(
  `Model Configuration`  = c("Standard Model (max_depth = 3)", "Additive Model (max_depth = 1)"),
  `Interactions Allowed` = c("Yes", "No"),
  `Test RMSE (2024-2025)` = c(round(rmse_standard, 2), round(rmse_additive, 2)),
  check.names = FALSE
)

cat("--- Error Comparison ---\n")
print(error_table)

library(xgboost)
# Extract feature importance
importance_matrix <- xgb.importance(model = 
                                  model_standard)
print(importance_matrix)
xgb.plot.importance(importance_matrix)

#shap 
shp <- shapviz(model_standard, X_pred = X_test)

sv_importance(shp, kind = "beeswarm")
sv_dependence(shp, v = "temp_max", color_var = "lag1")

# SHAP Interactions Matrix
shp_interaction <- shapviz(
  model_standard, 
  X_pred = X_test, 
  X = as.data.frame(X_test), 
  interactions = TRUE          
)

sv_interaction(shp_interaction, kind = "bar")
sv_dependence(shp_interaction, v = "temp_max", color_var = "lag1", interactions = TRUE)

# XGBoost actual vs predicted plot
par(mar = c(5, 5, 4, 2) + 0.1)

# Predictions from interaction-capable XGBoost model
pred_xgb <- predict(model_standard, dtest)
actual_xgb <- y_test

# Performance
rmse_xgb <- sqrt(mean((pred_xgb - actual_xgb)^2))
r2_xgb <- 1 - sum((actual_xgb - pred_xgb)^2) /
  sum((actual_xgb - mean(actual_xgb))^2)

print(paste("XGBoost Validation RMSE:", round(rmse_xgb, 2)))
print(paste("XGBoost Validation R-Squared:", round(r2_xgb, 4)))

# Actual vs predicted plot
plot(
  actual_xgb,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "A&E Attendances",
  xlab = "Validation Days (2024 and 2025)",
  main = "XGBoost Model",
  las = 1
)

lines(
  pred_xgb,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)
# 2-day spikes
temp_base  <- 30.8 # ~25th percentile
temp_spike <- 35.8 # ~90th percentile

flat_baseline <- rep(temp_base, total_days)

spike_day13 <- flat_baseline
spike_day13[13] <- temp_spike

spike_day14 <- flat_baseline
spike_day14[14] <- temp_spike

consecutive_spikes <- flat_baseline
consecutive_spikes[13:14] <- temp_spike

predict_xgb_scenario <- function(temp_seq, template_df = aug_2025_data, model = model_standard) {
  df_sim <- template_df %>%
    mutate(
      temp_max = temp_seq,
      lag1 = lag(temp_max, default = first(template_df$lag1)),
      lag2 = lag(temp_max, default = first(template_df$lag1))
    )
  dow_mat <- model.matrix(~ dow - 1, data = df_sim)
  X_sc <- cbind(as.matrix(df_sim[, c("temp_max", "lag1", "lag2")]), dow_mat)
  predict(model, xgb.DMatrix(data = X_sc))
}

pred_xgb_baseline    <- predict_xgb_scenario(flat_baseline)
pred_xgb_spike13     <- predict_xgb_scenario(spike_day13)
pred_xgb_spike14     <- predict_xgb_scenario(spike_day14)
pred_xgb_consecutive <- predict_xgb_scenario(consecutive_spikes)

# Calculate excess attendances above baseline
excess_xgb_13 <- pred_xgb_spike13 - pred_xgb_baseline
excess_xgb_14 <- pred_xgb_spike14 - pred_xgb_baseline

xgb_theoretical_additive <- excess_xgb_13 + excess_xgb_14
xgb_actual_consecutive   <- pred_xgb_consecutive - pred_xgb_baseline

comparison_table_xgb <- data.frame(
  Day                 = day_labels,
  Baseline_Pred       = pred_xgb_baseline[plot_range],
  Spike_13_Pred       = pred_xgb_spike13[plot_range],
  Spike_14_Pred       = pred_xgb_spike14[plot_range],
  Consecutive_Pred    = pred_xgb_consecutive[plot_range],
  Excess_Day_13_Only  = excess_xgb_13[plot_range],
  Excess_Day_14_Only  = excess_xgb_14[plot_range],
  Theoretical_Sum     = xgb_theoretical_additive[plot_range],
  Actual_Consecutive  = xgb_actual_consecutive[plot_range],
  Difference          = xgb_actual_consecutive[plot_range] -
    xgb_theoretical_additive[plot_range]
)

print(comparison_table_xgb, row.names = FALSE)

cat("3-day theoretical additive effect:",
    sum(xgb_theoretical_additive[13:15]), "\n")

cat("3-day consecutive effect:",
    sum(xgb_actual_consecutive[13:15]), "\n")

cat("3-day difference:",
    sum(xgb_actual_consecutive[13:15]) -
      sum(xgb_theoretical_additive[13:15]), "\n")

cat("Sum of daily differences:",
    sum(
      xgb_actual_consecutive[13:15] -
        xgb_theoretical_additive[13:15]
    ), "\n")

# tensor product gam
library(mgcv)

gam_interaction <- gam(
  ae_attendances ~ 
    te(temp_max, lag1, k = c(5, 5)) + # <--- The 3D interaction term (k limits complexity)
    factor(year) + 
    factor(dow),
  family = nb(), 
  data = data_complete
)  

summary(gam_interaction)

gam_additive <- gam(
  ae_attendances ~ 
    s(temp_max, k = 5) + 
    s(lag1, k = 5) + 
    factor(year) + 
    factor(dow),
  family = nb(),
  data = data_complete
)
summary(gam_additive)
AIC(gam_additive, gam_interaction)

library(mgcv)
library(dplyr)

gam_data <- data_complete %>%
  arrange(year, date) %>%
  group_by(year) %>%
  mutate(
    lag1 = lag(temp_max, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag1))

gam_train <- gam_data %>%
  filter(as.numeric(as.character(year)) <= 2023)

gam_test <- gam_data %>%
  filter(as.numeric(as.character(year)) >= 2024)

gam_interaction_val <- gam(
  ae_attendances ~
    te(temp_max, lag1, k = c(5, 5)) +
    factor(dow),
  family = nb(),
  data = gam_train,
  method = "REML"
)

summary(gam_interaction_val)

pred_gam <- predict(
  gam_interaction_val,
  newdata = gam_test,
  type = "response"
)

actual_gam <- gam_test$ae_attendances

rmse_gam <- sqrt(
  mean((pred_gam - actual_gam)^2)
)

mae_gam <- mean(
  abs(pred_gam - actual_gam)
)

r2_gam <- 1 -
  sum((actual_gam - pred_gam)^2) /
  sum((actual_gam - mean(actual_gam))^2)

mean_signed_error_gam <- mean(
  pred_gam - actual_gam
)

cat("Tensor GAM validation results (2024–2025)\n")
cat("------------------------------------------\n")
cat("RMSE:", round(rmse_gam, 2), "\n")
cat("MAE:", round(mae_gam, 2), "\n")
cat("R-squared:", round(r2_gam, 4), "\n")
cat("Mean signed error:", round(mean_signed_error_gam, 2), "\n")


plot(
  actual_gam,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "A&E Attendances",
  xlab = "Validation Days (2024 and 2025)",
  main = "Tensor GAM",
  las = 1
)

lines(
  pred_gam,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

# 2-day spike
temp_base  <- 30.8 # 25th percentile 30.8
temp_spike <- 35.8 # 90th percentile 35.8

total_days <- 31

flat_baseline <- rep(temp_base, total_days)

spike_day13 <- flat_baseline
spike_day13[13] <- temp_spike

spike_day14 <- flat_baseline
spike_day14[14] <- temp_spike

consecutive_spikes <- flat_baseline
consecutive_spikes[13:14] <- temp_spike

predict_gam_scenario <- function(temp_seq, template_df = aug_2025_data, model = gam_interaction) {
  df_sim <- template_df %>%
    mutate(temp_max = temp_seq) %>%
    mutate(lag1 = lag(temp_max, default = first(template_df$lag1)))
  
  predict(model, newdata = df_sim, type = "response")
}

pred_baseline    <- predict_gam_scenario(flat_baseline)
pred_spike13     <- predict_gam_scenario(spike_day13)
pred_spike14     <- predict_gam_scenario(spike_day14)
pred_consecutive <- predict_gam_scenario(consecutive_spikes)


excess_13 <- pred_spike13 - pred_baseline
excess_14 <- pred_spike14 - pred_baseline

theoretical_additive <- excess_13 + excess_14
actual_consecutive   <- pred_consecutive - pred_baseline

plot_range <- 8:total_days
day_labels <- paste0("Day ", plot_range, " (", aug_2025_data$dow[plot_range], ")")

comparison_table_gam <- data.frame(
  Day                 = day_labels,
  Baseline_Pred       = pred_baseline[plot_range],
  Spike_13_Pred       = pred_spike13[plot_range],
  Spike_14_Pred       = pred_spike14[plot_range],
  Consecutive_Pred    = pred_consecutive[plot_range],
  Excess_Day_13_Only  = excess_13[plot_range],
  Excess_Day_14_Only  = excess_14[plot_range],
  Theoretical_Sum     = theoretical_additive[plot_range],
  Actual_Consecutive  = actual_consecutive[plot_range],
  Difference           = actual_consecutive[plot_range] -
    theoretical_additive[plot_range]
)

print(comparison_table_gam, row.names = FALSE)

cat("3-day theoretical additive effect:",
    sum(theoretical_additive[13:15]), "\n")

cat("3-day consecutive effect:",
    sum(actual_consecutive[13:15]), "\n")

cat("3-day difference:",
    sum(actual_consecutive[13:15]) -
      sum(theoretical_additive[13:15]), "\n")

cat("Sum of daily differences:",
    sum(
      actual_consecutive[13:15] -
        theoretical_additive[13:15]
    ), "\n")
#dlnm 2-day spikes 
temp_base  <- 31 # 25th percentile 30.8
temp_spike <- 36 # 90th percentile 35.8

total_days <- 31

flat_baseline <- rep(temp_base, total_days)

spike_day13 <- flat_baseline
spike_day13[13] <- temp_spike

spike_day14 <- flat_baseline
spike_day14[14] <- temp_spike

consecutive_spikes <- flat_baseline
consecutive_spikes[13:14] <- temp_spike

predict_dlnm_scenario <- function(temp_seq, template_df = aug_2025_data, model = dlnm_ae, original_cb = cb_ae) {
  cb_sc <- crossbasis(temp_seq, lag = 7, argvar = list(fun = "ns", df = 3), arglag = list(fun = "ns", df = 3))
  attr(cb_sc, "xrange") <- attr(original_cb, "xrange")
  attr(cb_sc, "lag")    <- attr(original_cb, "lag")
  
  df_sim <- template_df %>% mutate(cb_ae = cb_sc)
  predict(model, newdata = df_sim, type = "response")
}

pred_baseline    <- predict_dlnm_scenario(flat_baseline)
pred_spike13     <- predict_dlnm_scenario(spike_day13)
pred_spike14     <- predict_dlnm_scenario(spike_day14)
pred_consecutive <- predict_dlnm_scenario(consecutive_spikes)

# 3. Calculate excess attendances & interaction differences
excess_13 <- pred_spike13 - pred_baseline
excess_14 <- pred_spike14 - pred_baseline

theoretical_additive <- excess_13 + excess_14
actual_consecutive   <- pred_consecutive - pred_baseline

# 4. Print Table
plot_range <- 8:total_days
day_labels <- paste0("Day ", plot_range, " (", aug_2025_data$dow[plot_range], ")")

comparison_table <- data.frame(
  Day                = day_labels,
  Baseline_Pred      = round(pred_baseline[plot_range], 1),
  Spike_13_Pred      = round(pred_spike13[plot_range], 1),
  Spike_14_Pred      = round(pred_spike14[plot_range], 1),
  Consecutive_Pred   = round(pred_consecutive[plot_range], 1),
  Excess_Day_13_Only = round(excess_13[plot_range], 1),
  Excess_Day_14_Only = round(excess_14[plot_range], 1),
  Theoretical_Sum    = round(theoretical_additive[plot_range], 1),
  Actual_Consecutive = round(actual_consecutive[plot_range], 1),
  Difference         = round(actual_consecutive[plot_range] - theoretical_additive[plot_range], 1)
)

print(comparison_table, row.names = FALSE)

# Check the Tensor GAM values used in the summary function

p_base <- predict_gam_scenario(flat_baseline)
p_s13  <- predict_gam_scenario(spike_day13)
p_s14  <- predict_gam_scenario(spike_day14)
p_con  <- predict_gam_scenario(consecutive_spikes)

exc_13 <- p_s13 - p_base
exc_14 <- p_s14 - p_base
exc_con <- p_con - p_base

window <- 13:15

cat("Spike 1 only:", sum(exc_13[window]), "\n")
cat("Spike 2 only:", sum(exc_14[window]), "\n")
cat("Expected additive:", sum(exc_13[window] + exc_14[window]), "\n")
cat("Consecutive:", sum(exc_con[window]), "\n")
cat("Difference:",
    sum(exc_con[window]) -
      sum(exc_13[window] + exc_14[window]), "\n")
# Compare all models: 2-day spike experiment
library(dplyr)

get_compounding_metrics <- function(model_name, predict_fn) {
  
  # Generate scenario predictions
  p_base <- predict_fn(flat_baseline)
  p_s13  <- predict_fn(spike_day13)
  p_s14  <- predict_fn(spike_day14)
  p_con  <- predict_fn(consecutive_spikes)
  
  # Excess attendances above baseline
  exc_13  <- p_s13 - p_base
  exc_14  <- p_s14 - p_base
  exc_con <- p_con - p_base
  
  # 3-day assessment window
  window <- 13:15
  
  # Excess from each isolated spike
  spike_1 <- sum(exc_13[window])
  spike_2 <- sum(exc_14[window])
  
  # Expected additive effect
  theoretical_additive <- spike_1 + spike_2
  
  # Excess from consecutive two-day spike
  actual_consecutive <- sum(exc_con[window])
  
  # Difference between observed consecutive effect
  # and expected additive effect
  difference <- actual_consecutive - theoretical_additive
  
  data.frame(
    Model = model_name,
    Spike_1_only = spike_1,
    Spike_2_only = spike_2,
    Expected_additive = theoretical_additive,
    Consecutive_2day = actual_consecutive,
    Difference = difference
  )
}


# Apply to all three models
compounding_summary_table <- rbind(
  get_compounding_metrics("Tensor GAM", predict_gam_scenario),
  get_compounding_metrics("XGBoost", predict_xgb_scenario),
  get_compounding_metrics("LSTM", predict_lstm_scenario)
)

print(compounding_summary_table, row.names = FALSE)

# Apply to all three models
compounding_summary_table <- rbind(
  get_compounding_metrics("Tensor GAM", predict_gam_scenario),
  get_compounding_metrics("XGBoost", predict_xgb_scenario),
  get_compounding_metrics("LSTM", predict_lstm_scenario)
)

# Round values for presentation
compounding_summary_table_rounded <- compounding_summary_table %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 1)
    )
  )

print(compounding_summary_table_rounded, row.names = FALSE)
compounding_summary_table <- rbind(
  get_compounding_metrics("Tensor GAM",   predict_gam_scenario),
  get_compounding_metrics("XGBoost",      predict_xgb_scenario),
  get_compounding_metrics("LSTM",         predict_lstm_scenario)
)

print(compounding_summary_table, row.names = FALSE)

cat("LSTM MAE:", round(mean(abs(pred - actual)), 2), "\n")
cat("XGBoost MAE:", round(mean(abs(pred_standard - y_test)), 2), "\n")
cat("Tensor GAM MAE:", round(mae_gam, 2), "\n")

# probabilitic forecasting!!!

beta_hat  <- coef(dlnm_ae)          # coefficient point estimates
Sigma_hat <- vcov(dlnm_ae)          # variance-covariance matrix
theta_hat <- dlnm_ae$theta          # NB dispersion parameter

## reuse the EXACT knots/basis spec that cb_ae was built with.
argvar_used <- attr(cb_ae, "argvar")
arglag_used <- attr(cb_ae, "arglag")
maxlag      <- attr(cb_ae, "lag")[2]     # should be 7
maxlag

train_years <- dlnm_ae$xlevels[["factor(year)"]]
train_dows  <- dlnm_ae$xlevels[["factor(dow)"]]

build_forecast_crossbasis <- function(history_temp, forecast_temp) {
  stopifnot(length(history_temp) == maxlag)
  combined <- c(history_temp, forecast_temp)
  cb <- crossbasis(combined, lag = maxlag,
                   argvar = argvar_used, arglag = arglag_used)
  cb[(length(history_temp) + 1):length(combined), , drop = FALSE]
}

## actual forecast
## history - real observed temps immediately before forecast
history_temp <- c(27,28,34,33,25,30,31)

## assumed forecast for next week
forecast_temp_central <- c(33, 31, 29, 28, 27, 30, 35)

## forecast_dates: chooses 1st aug 2026
forecast_dates <- forecast_dates <- as.Date("2026-08-01") + 0:6

## dow
dow_forecast <- weekdays(forecast_dates)  # adjust to match your dow column exactly

n_days <- length(forecast_dates)

year_label <- as.character(
  unique(format(forecast_dates, "%Y"))
)

if (length(year_label) > 1) {
  
  stop(
    "Forecast window spans more than one calendar year."
  )
}

# 2026 was not in the training data, so use 2025 as
# the stand-in year effect.

if (!year_label %in% train_years) {
  
  message(
    "Forecast year ",
    year_label,
    " was not in the training data; using ",
    max(train_years),
    " as a stand-in for the year fixed effect."
  )
  
  year_label <- max(train_years)
}


# Monte Carlo simulation
n_sims <- 1000

# parameter uncertainty, draw 1000 vecs from multivar normal dist
beta_draws <- mvrnorm(n_sims, mu = beta_hat, Sigma = Sigma_hat)

# temp forecast uncertainty, days1-3 SD=1, days4-6 SD=2 etc.
temp_sd <- c(1, 1, 1, 2, 2, 2, 3)

# store one realised attendance val for each day of each sim in matrix
sim_daily <- matrix(NA_real_, nrow = n_sims, ncol = n_days)

for (s in seq_len(n_sims)) {
  
  # add uncertainty to temp forecast
  temp_sim <- forecast_temp_central + rnorm(n_days, mean = 0, sd = temp_sd)
  
  # construct crossbasis
  cb_sim <- build_forecast_crossbasis(history_temp, temp_sim)
  
  # create forecast data
  newdata_s <- data.frame(
    cb_ae = I(cb_sim),
    year  = factor(year_label, levels = train_years),
    dow   = factor(dow_forecast, levels = train_dows)
  )
  
  # construct model matrix
  X_s <- model.matrix(delete.response(terms(dlnm_ae)), data = newdata_s,
                      xlev = dlnm_ae$xlevels)
  
  # mean forecast using simulated coef vec
  eta_s <- as.numeric(X_s %*% beta_draws[s, ])
  mu_s  <- exp(eta_s)
  
  # generate realised admissions- one actual possible attendance count drawn from NB for eachd day
  sim_daily[s, ] <- rnbinom(n_days, size = theta_hat, mu = mu_s)
}

# weekly total per simulation
sim_weekly_total <- rowSums(sim_daily)
head(sim_weekly_total)


# historical mean attendance for each calendar date
historical_daily_mean <- data_complete %>%
  mutate(
    month_day = format(as.Date(date), "%m-%d")
  ) %>%
  group_by(month_day) %>%
  summarise(
    historical_mean = mean(ae_attendances, na.rm = TRUE),
    n_years = sum(!is.na(ae_attendances)),
    .groups = "drop"
  )

print(historical_daily_mean)


# daily probabilistic forecast 
excess_threshold <- 20


# calculate expected admissions and 90% prediction interval
daily_summary <- data.frame(
  
  # Forecast dates
  date = forecast_dates,
  
  # Mean of the 1,000 realised simulations
  expected_admissions = colMeans(sim_daily),
  
  # Lower bound of 90% prediction interval
  lower_PI = apply(
    sim_daily,
    2,
    quantile,
    probs = 0.05
  ),
  
  # Upper bound of 90% prediction interval
  upper_PI = apply(
    sim_daily,
    2,
    quantile,
    probs = 0.95
  )
)



# match historical mean to each calendar date
daily_summary <- daily_summary %>%
  mutate(
    month_day = format(
      date,
      "%m-%d"
    )
  ) %>%
  left_join(
    historical_daily_mean,
    by = "month_day"
  )



# calculate expected excess admissions
daily_summary <- daily_summary %>%
  mutate(
    
    # Forecast minus historical calendar-date mean
    expected_excess =
      expected_admissions - historical_mean
  )


# calculate P(excess > 20)
daily_summary$prob_excess_20 <- sapply(
  
  seq_len(nrow(daily_summary)),
  
  function(i) {
    
    # If there is no historical baseline
    # return NA
    if (
      is.na(
        daily_summary$historical_mean[i]
      )
    ) {
      return(NA_real_)
    }
    
    # Probability that realised admissions
    # exceed historical mean + 20
    mean(
      sim_daily[, i] >
        daily_summary$historical_mean[i] +
        excess_threshold
    )
  }
)

daily_summary$`90% PI` <- paste0(
  round(daily_summary$lower_PI),
  "–",
  round(daily_summary$upper_PI)
)

daily_summary$`P(excess > 20)` <-
  daily_summary$prob_excess_20

daily_summary_final <- daily_summary %>%
  dplyr::select(
    date,
    expected_admissions,
    `90% PI`,
    historical_mean,
    expected_excess,
    `P(excess > 20)`
  ) %>%
  rename(
    `Date` = date,
    `Expected attendances` = expected_admissions,
    `Historical mean` = historical_mean,
    `Expected excess` = expected_excess
  )
print(daily_summary_final)

# weekly threshold probabilities
# choose thresholds
thresholds <- c(
  2600,
  2650,
  2700,
  2750,
  2800,
  2850,
  2900,
  2950,
  3000,
  3050,
  3100,
  3150
)

probability_table <- data.frame(
  Threshold   = thresholds,
  Probability = sapply(thresholds, function(x) mean(sim_weekly_total > x))
)
print(probability_table)

# Convert probability to percentage
probability_table$Probability_percent <-
  probability_table$Probability * 100

print(probability_table)

ggplot(
  probability_table,
  aes(
    x = Threshold,
    y = Probability_percent
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  labs(
    x = "7-day total A&E attendances",
    y = "Probability of exceeding threshold (%)",
    title = "Probability of exceeding 7-day A&E attendance thresholds"
  ) +
  theme_minimal()

# plot daily expected excess
p_daily_excess <- ggplot(
  
  daily_summary,
  
  aes(
    x = date,
    y = expected_excess
  )
) +
  
  geom_col() +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  labs(
    x = "Date",
    y = "Expected excess admissions",
    title = "Expected excess A&E attendances above historical mean"
  ) +
  
  theme_minimal()


print(p_daily_excess)

# plot daily prob of meaningful excess
p_daily_probability <- ggplot(
  
  daily_summary,
  
  aes(
    x = date,
    y = probability_excess_above_threshold
  )
) +
  
  geom_col() +
  
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1)
  ) +
  
  labs(
    x = "Date",
    y = "Probability",
    title = paste0(
      "Probability of admissions exceeding historical mean by > ",
      excess_threshold
    )
  ) +
  
  theme_minimal()


print(p_daily_probability)

# plot weekly threshold exceedance prob
p_exceed_plot <- ggplot(
  
  probability_table,
  
  aes(
    x = Threshold,
    y = Probability
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point() +
  
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1)
  ) +
  
  labs(
    x = "7-day total attendance threshold",
    y = "Probability of exceeding threshold",
    title = "Probability of exceeding a given 7-day attendance total"
  ) +
  
  theme_minimal()


print(p_exceed_plot)

# plot distribution of simulated weekly totals
p_dist_plot <- ggplot(
  
  data.frame(
    total = sim_weekly_total
  ),
  
  aes(
    x = total
  )
) +
  
  geom_histogram(
    bins = 40
  ) +
  
  labs(
    x = "Simulated 7-day total A&E attendances",
    y = "Number of simulations",
    title = "Simulated distribution of 7-day A&E attendances"
  ) +
  
  theme_minimal()


print(p_dist_plot)

excess_thresholds <- c(
  0,
  10,
  20,
  30,
  40,
  50,
  75,
  100
)

daily_excess_probability <- expand.grid(
  date = forecast_dates,
  excess_threshold = excess_thresholds
)

daily_excess_probability$probability <- NA_real_

for (i in seq_len(nrow(daily_excess_probability))) {
  
  day_index <- match(
    daily_excess_probability$date[i],
    forecast_dates
  )
  
  historical_mean_i <- historical_daily_mean %>%
    filter(
      month_day ==
        format(
          daily_excess_probability$date[i],
          "%m-%d"
        )
    ) %>%
    pull(historical_mean)
  
  if (length(historical_mean_i) == 0 ||
      is.na(historical_mean_i)) {
    
    daily_excess_probability$probability[i] <- NA_real_
    
  } else {
    
    daily_excess_probability$probability[i] <-
      mean(
        sim_daily[, day_index] >
          historical_mean_i +
          daily_excess_probability$excess_threshold[i]
      )
  }
}

print(daily_excess_probability)

p_excess_probability <- ggplot(
  daily_excess_probability,
  aes(
    x = excess_threshold,
    y = probability,
    group = date
  )
) +
  geom_line(alpha = 0.35) +
  geom_point(alpha = 0.35) +
  labs(
    x = "Excess A&E attendances above historical mean",
    y = "Probability",
    title = "Probability of exceeding different levels of excess A&E attendance"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal()

print(p_excess_probability)

p_weekly_probability <- ggplot(
  probability_table,
  aes(
    x = Threshold,
    y = Probability
  )
) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Weekly A&E attendances",
    y = "Probability of exceeding threshold",
    title = "Probability of exceeding weekly A&E attendance thresholds"
  ) +
  theme_minimal()

print(p_weekly_probability)

weekly_probability_table <- probability_table %>%
  mutate(
    Probability = round(
      Probability * 100,
      1
    )
  )

print(weekly_probability_table)

ggplot(
  daily_summary,
  aes(
    x = date,
    y = expected_excess
  )
) +
  geom_col(
    aes(alpha = abs(expected_excess))
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Date",
    y = "Expected excess A&E attendances",
    title = "Expected excess A&E attendances above historical baseline"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

ggplot(
  daily_summary,
  aes(x = date)
) +
  geom_ribbon(
    aes(ymin = lower_PI, ymax = upper_PI),
    alpha = 0.20
  ) +
  geom_line(
    aes(y = expected_admissions),
    linewidth = 1.2
  ) +
  geom_line(
    aes(y = historical_mean),
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_point(
    aes(y = expected_admissions),
    size = 2.5
  ) +
  labs(
    x = "Date",
    y = "A&E attendances",
    title = "Probabilistic 7-day A&E forecast",
    subtitle = "Solid line = expected attendances; dashed line = historical calendar-date mean; shaded area = 90% prediction interval"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

ggplot(
  daily_summary,
  aes(
    x = date,
    y = prob_excess_20
  )
) +
  geom_col(
    alpha = 0.75
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed"
  ) +
  labs(
    x = "Date",
    y = "Probability",
    title = "Probability of >20 excess A&E attendances"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Calculate probability of >20 excess attendances
probability <- sapply(seq_len(nrow(daily_summary)), function(i) {
  mean(
    sim_daily[, i] >
      daily_summary$historical_mean[i] + 20
  )
})

# Create final table
table_data <- daily_summary %>%
  mutate(
    `Expected attendances` = round(expected_admissions, 0),
    `90% PI` = paste0(
      round(lower_PI), "–", round(upper_PI)
    ),
    `Historical mean` = round(historical_mean, 0),
    `Expected excess` = round(expected_excess, 0),
    `P(excess > 20)` = paste0(
      round(probability * 100), "%"
    )
  ) %>%
  dplyr::select(
    Date = date,
    `Expected attendances`,
    `90% PI`,
    `Historical mean`,
    `Expected excess`,
    `P(excess > 20)`
  )

# Display table
table_data

# Expected weekly excess
weekly_expected_excess <- sum(
  daily_summary$expected_excess,
  na.rm = TRUE
)

# Probability of >100 excess attendances over the week
baseline_total <- sum(
  daily_summary$historical_mean,
  na.rm = TRUE
)

weekly_excess_probability <- mean(
  sim_weekly_total > baseline_total + 100
)

# Number of days with >=20 expected excess attendances
n_heat_days <- sum(
  daily_summary$expected_excess >= 20,
  na.rm = TRUE
)

# Results
round(weekly_expected_excess)
round(weekly_excess_probability * 100)
n_heat_days

#conformal predictions
train_years <- 2015:2024
validate_years <- 2025

stopifnot(
  !is.unsorted(data_complete$date)
)

data_complete <- data_complete %>%
  mutate(year_numeric = as.numeric(format(date, "%Y")))

train_data <- data_complete %>%
  filter(year_numeric %in% train_years)

validate_data <- data_complete %>%
  filter(
    year_numeric %in% validate_years
  )

print(table(train_data$year_numeric))
print(table(validate_data$year_numeric))

# fit on training data
cb_train <- crossbasis(
  
  train_data$temp_max,
  
  lag = 7,
  
  argvar = list(
    fun = "ns",
    df = 3
  ),
  
  arglag = list(
    fun = "ns",
    df = 3
  ),
  
  groups = factor(
    train_data$year_numeric
  )
)


dlnm_train <- glm.nb(
  
  ae_attendances ~
    
    cb_train +
    
    factor(year_numeric) +
    
    factor(dow),
  
  data = train_data
)


summary(dlnm_train)

theta_train <- dlnm_train$theta

maxlag <- attr(
  cb_train,
  "lag"
)[2]

argvar_used <- attr(
  cb_train,
  "argvar"
)

arglag_used <- attr(
  cb_train,
  "arglag"
)


print(maxlag)

train_year_levels <- dlnm_train$xlevels[["factor(year_numeric)"]]
train_dow_levels <- dlnm_train$xlevels[["factor(dow)"]]

print(train_year_levels)
print(train_dow_levels)

history_temp <- tail(
  train_data$temp_max,
  maxlag
)

combined_temp <- c(
  history_temp,
  validate_data$temp_max
)

cb_validate_full <- crossbasis(
  
  combined_temp,
  
  lag = maxlag,
  
  argvar = argvar_used,
  
  arglag = arglag_used
)

cb_validate <- cb_validate_full[
  
  (maxlag + 1):
    nrow(cb_validate_full),
  
  ,
  drop = FALSE
]

print(
  dim(cb_validate)
)

print(
  nrow(validate_data)
)

stopifnot(
  nrow(cb_validate) ==
    nrow(validate_data)
)


stopifnot(
  sum(is.na(cb_validate)) == 0
)

# 2025 was not available when the model was trained
year_fallback <- max(
  train_years
)


message(
  
  "Validation year 2025 was not in the training data. ",
  
  "Using year=",
  
  year_fallback,
  
  " as the explicit fallback for the year fixed effect."
)


year_fallback <- 2024

message(
  "Validation year 2025 was not in training data. ",
  "Using year=", year_fallback,
  " as the fallback year effect."
)

X_cb <- as.matrix(
  cb_validate
)

cb_coef_names <- grep(
  "^cb_train",
  names(coef(dlnm_train)),
  value = TRUE
)

stopifnot(
  ncol(X_cb) == length(cb_coef_names)
)

colnames(X_cb) <- cb_coef_names

n_validate <- nrow(validate_data)

X_validate <- matrix(
  0,
  nrow = n_validate,
  ncol = length(coef(dlnm_train))
)

colnames(X_validate) <- names(
  coef(dlnm_train)
)

X_validate[, "(Intercept)"] <- 1

year_coef_name <- "factor(year_numeric)2024"

if (!year_coef_name %in% colnames(X_validate)) {
  
  stop(
    "Could not find the 2024 year coefficient. ",
    "Available coefficients are: ",
    paste(
      names(coef(dlnm_train)),
      collapse = ", "
    )
  )
}


X_validate[, year_coef_name] <- 1

dow_coef_names <- grep(
  "^factor\\(dow\\)",
  names(coef(dlnm_train)),
  value = TRUE
)


for (i in seq_len(n_validate)) {
  
  dow_name <- paste0(
    "factor(dow)",
    as.character(
      validate_data$dow[i]
    )
  )
  
  if (
    dow_name %in%
    dow_coef_names
  ) {
    
    X_validate[
      i,
      dow_name
    ] <- 1
  }
}

X_validate[
  ,
  cb_coef_names
] <- X_cb

print(
  dim(X_validate)
)

print(
  nrow(validate_data)
)

print(
  length(coef(dlnm_train))
)

print(
  colnames(X_validate)
)


stopifnot(
  nrow(X_validate) ==
    nrow(validate_data)
)

stopifnot(
  ncol(X_validate) ==
    length(coef(dlnm_train))
)

stopifnot(
  identical(
    colnames(X_validate),
    names(coef(dlnm_train))
  )
)
eta_validate <- as.numeric(
  
  X_validate %*%
    coef(dlnm_train)
)


mu_validate <- exp(
  eta_validate
)


print(
  head(mu_validate)
)

print(
  length(mu_validate)
)


stopifnot(
  length(mu_validate) ==
    nrow(validate_data)
)

nb_sd <- sqrt(
  
  mu_validate +
    
    (
      mu_validate^2 /
        theta_train
    )
)


conformal_df <- data.frame(
  
  date =
    validate_data$date,
  
  year =
    validate_data$year_numeric,
  
  temp_max =
    validate_data$temp_max,
  
  observed =
    validate_data$ae_attendances,
  
  predicted_mean =
    mu_validate,
  
  raw_score =
    abs(
      validate_data$ae_attendances -
        mu_validate
    ),
  
  nb_sd =
    nb_sd,
  
  std_score =
    abs(
      validate_data$ae_attendances -
        mu_validate
    ) /
    nb_sd
)


print(
  conformal_df
)

# overall error
mean_error <- mean(
  
  conformal_df$observed -
    conformal_df$predicted_mean,
  
  na.rm = TRUE
)


mean_absolute_error <- mean(
  
  conformal_df$raw_score,
  
  na.rm = TRUE
)


cat(
  "\nMean signed error:",
  round(
    mean_error,
    2
  ),
  "\n"
)


cat(
  "Mean absolute error:",
  round(
    mean_absolute_error,
    2
  ),
  "\n"
)

# standardised score summary
print(
  summary(
    conformal_df$std_score
  )
)

# score vs temp, standardised
p_score_vs_temp <- ggplot(
  
  conformal_df,
  
  aes(
    x = temp_max,
    y = std_score
  )
  
) +
  
  geom_point(
    alpha = 0.4
  ) +
  
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  
  labs(
    x = "Maximum temperature (°C)",
    y = "Standardised conformity score",
    title =
      "Standardised conformity score vs. maximum temperature"
  ) +
  
  theme_minimal()


print(
  p_score_vs_temp
)

#non standardised
p_absolute_score_vs_temp <- ggplot(
  conformal_df,
  aes(
    x = temp_max,
    y = raw_score
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  labs(
    x = "Maximum temperature (°C)",
    y = "Absolute conformity score (attendances)",
    title = "Absolute prediction error vs. maximum temperature"
  ) +
  theme_minimal()

print(p_absolute_score_vs_temp)

# heatwave vs normal
heatwave_threshold <- 35.8

conformal_df <- conformal_df %>%
  
  mutate(
    
    temp_group =
      if_else(
        
        temp_max >=
          heatwave_threshold,
        
        "Extreme heat",
        
        "Non-extreme heat"
      )
  )


print(
  table(
    conformal_df$temp_group
  )
)

score_percentiles <- conformal_df %>%
  
  group_by(
    temp_group
  ) %>%
  
  summarise(
    
    n = n(),
    
    median =
      quantile(
        std_score,
        0.50,
        na.rm = TRUE
      ),
    
    percentile_75 =
      quantile(
        std_score,
        0.75,
        na.rm = TRUE
      ),
    
    percentile_90 =
      quantile(
        std_score,
        0.90,
        na.rm = TRUE
      ),
    
    percentile_95 =
      quantile(
        std_score,
        0.95,
        na.rm = TRUE
      ),
    
    percentile_99 =
      quantile(
        std_score,
        0.99,
        na.rm = TRUE
      ),
    
    maximum =
      max(
        std_score,
        na.rm = TRUE
      ),
    
    mean_score =
      mean(
        std_score,
        na.rm = TRUE
      ),
    
    mean_absolute_error =
      mean(
        raw_score,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


print(
  score_percentiles
)

# heatwave vs normal boxplot
p_score_by_group <- ggplot(
  
  conformal_df,
  
  aes(
    x = temp_group,
    y = std_score
  )
  
) +
  
  geom_boxplot(
    outlier.alpha = 0.4
  ) +
  
  labs(
    x = NULL,
    y = "Standardised conformity score",
    title =
      "Conformity score: heatwave vs. normal days"
  ) +
  
  theme_minimal()


print(
  p_score_by_group
)

p_score_by_group <- ggplot(
  conformal_df,
  aes(
    x = temp_group,
    y = std_score
  )
) +
  geom_boxplot(
    outlier.alpha = 0.4
  ) +
  labs(
    x = NULL,
    y = "Standardised conformity score",
    title = "Conformity score: heatwave vs. normal days"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )

print(p_score_by_group)
p_absolute_score_by_group <- ggplot(
  conformal_df,
  aes(
    x = temp_group,
    y = raw_score
  )
) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(
    x = NULL,
    y = "Absolute conformity score (attendances)",
    title = "Absolute prediction error: heatwave vs. normal days"
  ) +
  theme_minimal()

print(p_absolute_score_by_group)

# over time, doesnt work
p_score_vs_time <- ggplot(
  
  conformal_df,
  
  aes(
    x = date,
    y = std_score
  )
  
) +
  
  geom_point(
    alpha = 0.4
  ) +
  
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  
  labs(
    x = "Date",
    y = "Standardised conformity score",
    title =
      "Standardised conformity score over 2025"
  ) +
  
  theme_minimal()


print(
  p_score_vs_time
)

score_thresholds <- c(
  1,
  1.5,
  2,
  2.5,
  3
)


exceed_table <- data.frame(
  
  std_threshold =
    score_thresholds,
  
  empirical_probability =
    sapply(
      
      score_thresholds,
      
      function(t) {
        
        mean(
          conformal_df$std_score > t,
          na.rm = TRUE
        )
      }
    ),
  
  normal_reference_probability =
    sapply(
      
      score_thresholds,
      
      function(t) {
        
        2 *
          (
            1 -
              pnorm(t)
          )
      }
    )
)


exceed_table$empirical_percent <-
  exceed_table$empirical_probability *
  100


exceed_table$normal_reference_percent <-
  exceed_table$normal_reference_probability *
  100


print(
  exceed_table
)

# extreme heat
extreme_days <- conformal_df %>%
  
  filter(
    temp_max >=
      heatwave_threshold
  )


cat(
  "\nNumber of ≥36°C days:",
  nrow(extreme_days),
  "\n"
)


cat(
  "Mean standardised score on ≥36°C days:",
  round(
    mean(
      extreme_days$std_score,
      na.rm = TRUE
    ),
    2
  ),
  "\n"
)


cat(
  "Mean absolute error on ≥36°C days:",
  round(
    mean(
      extreme_days$raw_score,
      na.rm = TRUE
    ),
    2
  ),
  "\n"
)

non_extreme_days <- conformal_df %>%
  filter(temp_max < 35.8)
cat(
  "Mean absolute error on non-extreme days:",
  round(
    mean(
      non_extreme_days$raw_score,
      na.rm = TRUE
    ),
    2
  ),
  "\n"
)
# Observed vs predicted A&E attendances: July-August 2025

p_observed_predicted <- ggplot(
  conformal_df,
  aes(x = date)
) +
  
  geom_line(
    aes(
      y = observed,
      linetype = "Observed"
    ),
    linewidth = 1
  ) +
  
  geom_line(
    aes(
      y = predicted_mean,
      linetype = "Predicted"
    ),
    linewidth = 1
  ) +
  
  labs(
    x = "Date",
    y = "A&E attendances",
    linetype = NULL,
    title = "Observed and predicted A&E attendances, July-August 2025"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "top"
  )

print(p_observed_predicted)


# new figures
# Panel A: weekly pattern
p_dow <- ggplot(data_complete, aes(x = dow, y = ae_attendances)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Day of the week",
    y = "Daily A&E attendances"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12)
  )
p_dow

# Panel A: weekly pattern
p_dow <- ggplot(data_complete, aes(x = dow, y = ae_attendances)) +
  geom_boxplot() +
  scale_x_discrete(
    limits = c(
      "Monday", "Tuesday", "Wednesday", "Thursday",
      "Friday", "Saturday", "Sunday"
    )
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Day of the week",
    y = "Daily A&E attendances"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12)
  )

p_dow
# Panel B: temporal pattern
p_year <- ggplot(data_complete, aes(x = date, y = ae_attendances)) +
  geom_line(linewidth = 0.6, color = "black") +
  facet_wrap(~year, nrow = 2, scales = "free_x") +
  scale_x_date(
    breaks = function(x) {
      year <- format(min(x), "%Y")
      as.Date(paste0(year, c("-07-01", "-08-01")))
    },
    date_labels = "%b",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    x = "Month",
    y = "Daily A&E attendances"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 11),
    axis.title.x = element_text(size = 12, margin = margin(t = 8)),
    axis.title.y = element_text(size = 12, margin = margin(r = 8)),
    strip.background = element_rect(
      fill = "gray95",
      color = "black",
      linewidth = 0.5
    ),
    strip.text = element_text(face = "bold", size = 11),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

# Combine into one figure
figure1 <- p_year + p_dow +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

figure1

d3 <- plot(
  cp_ae,
  ptype = "3d",
  
  xlab = "\n\n\nDaily maximum temperature (°C)",
  ylab = "\n\n\nLag (days)",
  zlab = "\n\n\nRelative risk",
  main = "",
  
  # Camera and projection
  theta = 215,
  phi = 25,
  expand = 0.65,
  ltheta = 170,
  shade = 0.35,
  
  # Tick formatting
  ticktype = "detailed",
  nticks = 5
)

# Reference temperature at the 50th percentile (32.2°C)
lines(
  trans3d(
    x = 32.2,
    y = 0:7,
    z = cp_ae$matRRfit[as.character(32), ],
    pmat = d3
  ),
  lwd = 2,
  col = "red"
)

d3

par(mfrow = c(2, 2),
    mar = c(4.5, 4.5, 2.5, 1.5),
    oma = c(0, 0, 1, 0))

# (a) T25 = 30.8°C
plot(
  cp_lag_selected,
  "slices",
  var = 30.8,
  xlab = "Lag (days)",
  ylab = "Relative risk",
  main = "",
  col = "black",
  lwd = 2
)
mtext("(a)", side = 3, line = 0.5, adj = 0, font = 1)

# (b) T75 = 34.0°C
plot(
  cp_lag_selected,
  "slices",
  var = 34.0,
  xlab = "Lag (days)",
  ylab = "Relative risk",
  main = "",
  col = "black",
  lwd = 2
)
mtext("(b)", side = 3, line = 0.5, adj = 0, font = 1)

# (c) T90 = 35.8°C
plot(
  cp_lag_selected,
  "slices",
  var = 35.8,
  xlab = "Lag (days)",
  ylab = "Relative risk",
  main = "",
  col = "black",
  lwd = 2
)
mtext("(c)", side = 3, line = 0.5, adj = 0, font = 1)

# (d) Maximum observed temperature = 40.6°C
plot(
  cp_lag_selected,
  "slices",
  var = 40.6,
  xlab = "Lag (days)",
  ylab = "Relative risk",
  main = "",
  col = "black",
  lwd = 2
)
mtext("(d)", side = 3, line = 0.5, adj = 0, font = 1)

par(mfrow = c(1, 1))


par(
  mfrow = c(3, 1),
  mar = c(4.5, 4.5, 2.5, 1.5),
  oma = c(0, 0, 1, 0)
)

# (a) LSTM
plot(
  actual,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "A&E attendances",
  xlab = "Validation days (2024 and 2025)",
  main = "",
  las = 1
)

lines(
  pred,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(a)", side = 3, line = 0.5, adj = 0, font = 1)


# (b) XGBoost
plot(
  actual_xgb,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "A&E attendances",
  xlab = "Validation days (2024 and 2025)",
  main = "",
  las = 1
)

lines(
  pred_xgb,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(b)", side = 3, line = 0.5, adj = 0, font = 1)


# (c) Tensor-product GAM
plot(
  actual_gam,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "A&E attendances",
  xlab = "Validation days (2024 and 2025)",
  main = "",
  las = 1
)

lines(
  pred_gam,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(c)", side = 3, line = 0.5, adj = 0, font = 1)

par(mfrow = c(1, 1))


# Create validation dates
validation_dates <- c(
  seq(as.Date("2024-07-01"), as.Date("2024-08-31"), by = "day"),
  seq(as.Date("2025-07-01"), as.Date("2025-08-31"), by = "day")
)

par(
  mfrow = c(3, 1),
  mar = c(4.5, 4.5, 2.5, 1.5),
  oma = c(0, 0, 1, 0)
)

# (a) LSTM
plot(
  validation_dates,
  actual,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "Date",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(validation_dates, pred, col = "red", lwd = 2)

axis(
  1,
  at = as.Date(c("2024-07-01", "2024-08-01",
                 "2025-07-01", "2025-08-01")),
  labels = c("Jul 2024", "Aug 2024", "Jul 2025", "Aug 2025")
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(a)", side = 3, line = 0.5, adj = 0, font = 1)


# (b) XGBoost
plot(
  validation_dates,
  actual_xgb,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "Date",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(validation_dates, pred_xgb, col = "red", lwd = 2)

axis(
  1,
  at = as.Date(c("2024-07-01", "2024-08-01",
                 "2025-07-01", "2025-08-01")),
  labels = c("Jul 2024", "Aug 2024", "Jul 2025", "Aug 2025")
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(b)", side = 3, line = 0.5, adj = 0, font = 1)


# (c) Tensor-product GAM
plot(
  validation_dates,
  actual_gam,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "Date",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(validation_dates, pred_gam, col = "red", lwd = 2)

axis(
  1,
  at = as.Date(c("2024-07-01", "2024-08-01",
                 "2025-07-01", "2025-08-01")),
  labels = c("Jul 2024", "Aug 2024", "Jul 2025", "Aug 2025")
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(c)", side = 3, line = 0.5, adj = 0, font = 1)

par(mfrow = c(1, 1))

# ---------------------------------------------------------
# Figure 5: Observed vs predicted A&E attendances
# ---------------------------------------------------------

# Model-specific validation dates
validation_dates_lstm <- val_df$date[8:nrow(val_df)]
validation_dates_xgb  <- xgdata$date[test_mask]
validation_dates_gam  <- gam_test$date


# Set up three vertically arranged panels
par(
  mfrow = c(3, 1),
  mar = c(3.5, 5, 2.5, 1.5),
  oma = c(2, 0, 0, 0)
)


# ---------------------------------------------------------
# (a) LSTM
# ---------------------------------------------------------

plot(
  validation_dates_lstm,
  actual,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  validation_dates_lstm,
  pred,
  col = "red",
  lwd = 2
)

axis.Date(
  1,
  at = as.Date(c(
    "2024-07-01", "2024-08-01",
    "2025-07-01", "2025-08-01"
  )),
  labels = c(
    "Jul 2024", "Aug 2024",
    "Jul 2025", "Aug 2025"
  )
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(a)", side = 3, line = 0.5, adj = 0, font = 1)


# ---------------------------------------------------------
# (b) XGBoost
# ---------------------------------------------------------

plot(
  validation_dates_xgb,
  actual_xgb,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  validation_dates_xgb,
  pred_xgb,
  col = "red",
  lwd = 2
)

axis.Date(
  1,
  at = as.Date(c(
    "2024-07-01", "2024-08-01",
    "2025-07-01", "2025-08-01"
  )),
  labels = c(
    "Jul 2024", "Aug 2024",
    "Jul 2025", "Aug 2025"
  )
)

mtext("(b)", side = 3, line = 0.5, adj = 0, font = 1)


# ---------------------------------------------------------
# (c) Tensor-product GAM
# ---------------------------------------------------------

plot(
  validation_dates_gam,
  actual_gam,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  validation_dates_gam,
  pred_gam,
  col = "red",
  lwd = 2
)

axis.Date(
  1,
  at = as.Date(c(
    "2024-07-01", "2024-08-01",
    "2025-07-01", "2025-08-01"
  )),
  labels = c(
    "Jul 2024", "Aug 2024",
    "Jul 2025", "Aug 2025"
  )
)

mtext("(c)", side = 3, line = 0.5, adj = 0, font = 1)


# Shared x-axis label
mtext(
  "Date",
  side = 1,
  outer = TRUE,
  line = 0.5
)


# Reset plotting parameters
par(mfrow = c(1, 1))

# ---------------------------------------------------------
# Figure 5: Observed vs predicted A&E attendances
# ---------------------------------------------------------

# Model-specific validation dates
validation_dates_lstm <- val_df$date[8:nrow(val_df)]
validation_dates_xgb  <- xgdata$date[test_mask]
validation_dates_gam  <- gam_test$date


# Set up three vertically arranged panels
par(
  mfrow = c(3, 1),
  mar = c(3.5, 5, 2.5, 1.5),
  oma = c(2, 0, 0, 0)
)


# ---------------------------------------------------------
# (a) LSTM
# ---------------------------------------------------------

x_lstm <- seq_along(actual)

plot(
  x_lstm,
  actual,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  x_lstm,
  pred,
  col = "red",
  lwd = 2
)

# Date labels
lstm_ticks <- c(
  which(format(validation_dates_lstm, "%Y-%m-%d") == "2024-07-01"),
  which(format(validation_dates_lstm, "%Y-%m-%d") == "2024-08-01"),
  which(format(validation_dates_lstm, "%Y-%m-%d") == "2025-07-01"),
  which(format(validation_dates_lstm, "%Y-%m-%d") == "2025-08-01")
)

axis(
  1,
  at = lstm_ticks,
  labels = c("Jul 2024", "Aug 2024", "Jul 2025", "Aug 2025")
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(a)", side = 3, line = 0.5, adj = 0, font = 1)


# ---------------------------------------------------------
# (b) XGBoost
# ---------------------------------------------------------

x_xgb <- seq_along(actual_xgb)

plot(
  x_xgb,
  actual_xgb,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  x_xgb,
  pred_xgb,
  col = "red",
  lwd = 2
)

xgb_ticks <- c(
  which(format(validation_dates_xgb, "%Y-%m-%d") == "2024-07-01"),
  which(format(validation_dates_xgb, "%Y-%m-%d") == "2024-08-01"),
  which(format(validation_dates_xgb, "%Y-%m-%d") == "2025-07-01"),
  which(format(validation_dates_xgb, "%Y-%m-%d") == "2025-08-01")
)

axis(
  1,
  at = xgb_ticks,
  labels = c("Jul 2024", "Aug 2024", "Jul 2025", "Aug 2025")
)

mtext("(b)", side = 3, line = 0.5, adj = 0, font = 1)


# ---------------------------------------------------------
# (c) Tensor-product GAM
# ---------------------------------------------------------

x_gam <- seq_along(actual_gam)

plot(
  x_gam,
  actual_gam,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  x_gam,
  pred_gam,
  col = "red",
  lwd = 2
)

gam_ticks <- c(
  which(format(validation_dates_gam, "%Y-%m-%d") == "2024-07-01"),
  which(format(validation_dates_gam, "%Y-%m-%d") == "2024-08-01"),
  which(format(validation_dates_gam, "%Y-%m-%d") == "2025-07-01"),
  which(format(validation_dates_gam, "%Y-%m-%d") == "2025-08-01")
)

axis(
  1,
  at = gam_ticks,
  labels = c("Jul 2024", "Aug 2024", "Jul 2025", "Aug 2025")
)

mtext("(c)", side = 3, line = 0.5, adj = 0, font = 1)


# Shared x-axis label
mtext(
  "Date",
  side = 1,
  outer = TRUE,
  line = 0.5
)

# Reset plotting parameters
par(mfrow = c(1, 1))

# ---------------------------------------------------------
# Figure 5: Observed vs predicted A&E attendances
# ---------------------------------------------------------

# Model-specific validation dates
validation_dates_lstm <- val_df$date[8:nrow(val_df)]
validation_dates_xgb  <- xgdata$date[test_mask]
validation_dates_gam  <- gam_test$date


# Function to create month/year tick positions
get_date_ticks <- function(dates) {
  
  dates <- as.Date(dates)
  
  # Identify the first observation for each month
  month_id <- format(dates, "%Y-%m")
  ticks <- !duplicated(month_id)
  
  tick_positions <- which(ticks)
  tick_dates <- dates[tick_positions]
  
  # Only label July and August
  keep <- format(tick_dates, "%m") %in% c("07", "08")
  
  list(
    positions = tick_positions[keep],
    labels = format(tick_dates[keep], "%b %Y")
  )
}


# ---------------------------------------------------------
# Set up three vertically arranged panels
# ---------------------------------------------------------

par(
  mfrow = c(3, 1),
  mar = c(3.5, 5, 2.5, 1.5),
  oma = c(2, 0, 0, 0)
)


# ---------------------------------------------------------
# (a) LSTM
# ---------------------------------------------------------

x_lstm <- seq_along(actual)

plot(
  x_lstm,
  actual,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  x_lstm,
  pred,
  col = "red",
  lwd = 2
)

ticks_lstm <- get_date_ticks(validation_dates_lstm)

axis(
  1,
  at = ticks_lstm$positions,
  labels = ticks_lstm$labels
)

legend(
  "topleft",
  legend = c("Observed", "Predicted"),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mtext("(a)", side = 3, line = 0.5, adj = 0, font = 1)


# ---------------------------------------------------------
# (b) XGBoost
# ---------------------------------------------------------

x_xgb <- seq_along(actual_xgb)

plot(
  x_xgb,
  actual_xgb,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  x_xgb,
  pred_xgb,
  col = "red",
  lwd = 2
)

ticks_xgb <- get_date_ticks(validation_dates_xgb)

axis(
  1,
  at = ticks_xgb$positions,
  labels = ticks_xgb$labels
)

mtext("(b)", side = 3, line = 0.5, adj = 0, font = 1)


# ---------------------------------------------------------
# (c) Tensor-product GAM
# ---------------------------------------------------------

x_gam <- seq_along(actual_gam)

plot(
  x_gam,
  actual_gam,
  type = "l",
  col = "blue",
  lwd = 2,
  ylab = "Daily A&E attendances",
  xlab = "",
  main = "",
  las = 1,
  xaxt = "n"
)

lines(
  x_gam,
  pred_gam,
  col = "red",
  lwd = 2
)

ticks_gam <- get_date_ticks(validation_dates_gam)

axis(
  1,
  at = ticks_gam$positions,
  labels = ticks_gam$labels
)

mtext("(c)", side = 3, line = 0.5, adj = 0, font = 1)


# Shared x-axis label
mtext(
  "Date",
  side = 1,
  outer = TRUE,
  line = 0.5
)


# Reset plotting parameters
par(mfrow = c(1, 1))

# Observed vs predicted A&E attendances: July–August 2025

p_observed_predicted <- ggplot(
  conformal_df,
  aes(x = date)
) +
  
  geom_line(
    aes(
      y = observed,
      colour = "Observed"
    ),
    linewidth = 1
  ) +
  
  geom_line(
    aes(
      y = predicted_mean,
      colour = "Predicted"
    ),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    values = c(
      "Observed" = "blue",
      "Predicted" = "red"
    )
  ) +
  
  labs(
    x = "Date",
    y = "Daily A&E attendances",
    colour = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "top"
  )

print(p_observed_predicted)

library(patchwork)

# ---------------------------------------------------------
# (a) Standardised conformity score by temperature group
# ---------------------------------------------------------

p_score_by_group <- ggplot(
  conformal_df,
  aes(
    x = temp_group,
    y = std_score
  )
) +
  geom_boxplot(
    outlier.alpha = 0.4
  ) +
  labs(
    x = NULL,
    y = "Standardised conformity score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    plot.title = element_blank()
  )


# ---------------------------------------------------------
# (b) Standardised conformity score vs temperature
# ---------------------------------------------------------

p_score_vs_temp <- ggplot(
  conformal_df,
  aes(
    x = temp_max,
    y = std_score
  )
) +
  geom_point(
    alpha = 0.4
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  labs(
    x = "Daily maximum temperature (°C)",
    y = "Standardised conformity score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_blank()
  )


# ---------------------------------------------------------
# Combine panels
# ---------------------------------------------------------

figure7 <- p_score_by_group + p_score_vs_temp +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(
      size = 14,
      face = "plain"
    )
  )

figure7

library(patchwork)

# ---------------------------------------------------------
# (a) Standardised conformity score by temperature group
# ---------------------------------------------------------

p_score_by_group <- ggplot(
  conformal_df,
  aes(
    x = temp_group,
    y = std_score
  )
) +
  geom_boxplot(
    outlier.alpha = 0.4
  ) +
  scale_x_discrete(
    labels = c(
      "normal" = "Non-extreme",
      "heatwave" = "Extreme"
    )
  ) +
  labs(
    x = NULL,
    y = "Standardised conformity score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 12),
    plot.title = element_blank()
  )


# ---------------------------------------------------------
# (b) Standardised conformity score vs temperature
# ---------------------------------------------------------

p_score_vs_temp <- ggplot(
  conformal_df,
  aes(
    x = temp_max,
    y = std_score
  )
) +
  geom_point(
    alpha = 0.4
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  labs(
    x = "Daily maximum temperature (°C)",
    y = "Standardised conformity score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_blank()
  )


# ---------------------------------------------------------
# Combine panels
# ---------------------------------------------------------

figure7 <- p_score_by_group + p_score_vs_temp +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  theme(
    plot.tag = element_text(
      size = 11,
      face = "plain"
    )
  )

figure7

# ==========================================
# NEATER FACET PLOTTING: A&E ATTENDANCES
# ==========================================

par(
  mfrow = c(3, 1),
  mar = c(1.5, 4.5, 2.0, 1),
  oma = c(8, 0, 3, 0)
)

plot_range <- 8:total_days
day_labels <- paste0("Day ", plot_range, " (", timeline_dows[plot_range], ")")

# ------------------------------------------
# PANEL 1: Typical Heatwave
# ------------------------------------------

ylim_a <- c(
  350,
  max(typical_df$upper[plot_range], na.rm = TRUE) + 10
)

plot(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  type = "n",
  ylim = ylim_a,
  xlab = "",
  ylab = "Daily A&E attendances",
  main = "",
  xaxt = "n",
  las = 1,
  cex.axis = 0.85,
  cex.lab = 0.9
)

# Heatwave window
rect(
  xleft = 11,
  ybottom = par("usr")[3],
  xright = 14,
  ytop = par("usr")[4],
  col = adjustcolor("gold", alpha.f = 0.10),
  border = NA
)

abline(v = c(11, 14), col = "gray70", lty = 2)

# Confidence interval
polygon(
  c(
    typical_df$t[plot_range],
    rev(typical_df$t[plot_range])
  ),
  c(
    typical_df$lower[plot_range],
    rev(typical_df$upper[plot_range])
  ),
  col = adjustcolor("gold", alpha.f = 0.25),
  border = NA
)

# Lines
lines(
  baseline_scenario$t[plot_range],
  baseline_scenario$predict_ae[plot_range],
  col = "blue",
  lwd = 1.5,
  lty = 3
)

lines(
  hot_scenario$t[plot_range],
  hot_scenario$predict_ae[plot_range],
  col = "red",
  lwd = 1.5,
  lty = 3
)

lines(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  col = "darkgreen",
  lwd = 1.5
)

lines(
  typical_df$t[plot_range],
  typical_df$predict_ae[plot_range],
  col = "gold",
  lwd = 3
)

mtext("(a)", side = 3, line = 0.3, adj = 0, font = 1, cex = 0.85)


# ------------------------------------------
# PANEL 2: Prolonged Heatwave
# ------------------------------------------

ylim_b <- c(
  350,
  max(prolonged_df$upper[plot_range], na.rm = TRUE) + 10
)

plot(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  type = "n",
  ylim = ylim_b,
  xlab = "",
  ylab = "Daily A&E attendances",
  main = "",
  xaxt = "n",
  las = 1,
  cex.axis = 0.85,
  cex.lab = 0.9
)

# Heatwave window
rect(
  xleft = 11,
  ybottom = par("usr")[3],
  xright = 20,
  ytop = par("usr")[4],
  col = adjustcolor("darkorange", alpha.f = 0.06),
  border = NA
)

abline(v = c(11, 20), col = "gray70", lty = 2)

# Confidence interval
polygon(
  c(
    prolonged_df$t[plot_range],
    rev(prolonged_df$t[plot_range])
  ),
  c(
    prolonged_df$lower[plot_range],
    rev(prolonged_df$upper[plot_range])
  ),
  col = adjustcolor("darkorange", alpha.f = 0.20),
  border = NA
)

# Lines
lines(
  baseline_scenario$t[plot_range],
  baseline_scenario$predict_ae[plot_range],
  col = "blue",
  lwd = 1.5,
  lty = 3
)

lines(
  hot_scenario$t[plot_range],
  hot_scenario$predict_ae[plot_range],
  col = "red",
  lwd = 1.5,
  lty = 3
)

lines(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  col = "darkgreen",
  lwd = 1.5
)

lines(
  prolonged_df$t[plot_range],
  prolonged_df$predict_ae[plot_range],
  col = "darkorange",
  lwd = 3
)

mtext("(b)", side = 3, line = 0.3, adj = 0, font = 1, cex = 0.85)


# ------------------------------------------
# PANEL 3: Future Heatwave
# ------------------------------------------

ylim_c <- c(350, 580)

plot(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  type = "n",
  ylim = ylim_c,
  xlab = "",
  ylab = "Daily A&E attendances",
  main = "",
  xaxt = "n",
  las = 1,
  cex.axis = 0.85,
  cex.lab = 0.9
)

# Heatwave window
rect(
  xleft = 11,
  ybottom = par("usr")[3],
  xright = 20,
  ytop = par("usr")[4],
  col = adjustcolor("purple", alpha.f = 0.05),
  border = NA
)

abline(v = c(11, 20), col = "gray70", lty = 2)

# Confidence interval
polygon(
  c(
    future_temps_df$t[plot_range],
    rev(future_temps_df$t[plot_range])
  ),
  c(
    future_temps_df$lower[plot_range],
    rev(future_temps_df$upper[plot_range])
  ),
  col = adjustcolor("purple", alpha.f = 0.15),
  border = NA
)

# Lines
lines(
  baseline_scenario$t[plot_range],
  baseline_scenario$predict_ae[plot_range],
  col = "blue",
  lwd = 1.5,
  lty = 3
)

lines(
  hot_scenario$t[plot_range],
  hot_scenario$predict_ae[plot_range],
  col = "red",
  lwd = 1.5,
  lty = 3
)

lines(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  col = "darkgreen",
  lwd = 1.5
)

lines(
  future_temps_df$t[plot_range],
  future_temps_df$predict_ae[plot_range],
  col = "purple",
  lwd = 3
)

mtext("(c)", side = 3, line = 0.3, adj = 0, font = 1, cex = 0.85)


# ==========================================
# COMMON X-AXIS
# ==========================================

axis(
  side = 1,
  at = plot_range,
  labels = day_labels,
  las = 2,
  cex.axis = 0.75
)

# ==========================================
# COMMON LEGEND
# ==========================================

par(
  fig = c(0, 1, 0, 1),
  oma = c(0, 0, 0, 0),
  mar = c(0, 0, 0, 0),
  new = TRUE
)

plot(
  0, 0,
  type = "n",
  bty = "n",
  xaxt = "n",
  yaxt = "n",
  xlim = c(0, 1),
  ylim = c(0, 1)
)

legend(
  "bottom",
  legend = c(
    "Min Historical",
    "Max Historical",
    "August 2025",
    "Scenario",
    "Heatwave Window"
  ),
  col = c(
    "blue",
    "red",
    "darkgreen",
    "black",
    adjustcolor("gray60", alpha.f = 0.2)
  ),
  lty = c(3, 3, 1, 1, NA),
  lwd = c(1.5, 1.5, 1.5, 3, NA),
  pch = c(NA, NA, NA, NA, 15),
  pt.cex = 1.0,
  bty = "n",
  horiz = TRUE,
  xpd = TRUE,
  cex = 0.7,
  x.intersp = 0.3,
  text.width = c(0.08, 0.15, 0.12, 0.14, 0.10)
)

par(mfrow = c(1, 1))

# ==========================================
# NEATER FACET PLOTTING: A&E ATTENDANCES
# ==========================================

par(
  mfrow = c(3, 1),
  mar = c(1.5, 4.5, 2.0, 1),
  oma = c(8, 0, 3, 0)
)

plot_range <- 8:total_days
day_labels <- paste0("Day ", plot_range, " (", timeline_dows[plot_range], ")")


# ------------------------------------------
# PANEL 1: Typical Heatwave
# ------------------------------------------

ylim_a <- c(
  350,
  max(typical_df$upper[plot_range], na.rm = TRUE) + 10
)

plot(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  type = "n",
  ylim = ylim_a,
  xlab = "",
  ylab = "Daily A&E attendances",
  main = "",
  xaxt = "n",
  las = 1,
  cex.axis = 0.9,
  cex.lab = 1.0
)

rect(
  xleft = 11,
  ybottom = par("usr")[3],
  xright = 14,
  ytop = par("usr")[4],
  col = adjustcolor("gold", alpha.f = 0.10),
  border = NA
)

abline(v = c(11, 14), col = "gray70", lty = 2)

polygon(
  c(
    typical_df$t[plot_range],
    rev(typical_df$t[plot_range])
  ),
  c(
    typical_df$lower[plot_range],
    rev(typical_df$upper[plot_range])
  ),
  col = adjustcolor("gold", alpha.f = 0.25),
  border = NA
)

lines(
  baseline_scenario$t[plot_range],
  baseline_scenario$predict_ae[plot_range],
  col = "blue",
  lwd = 1.5,
  lty = 3
)

lines(
  hot_scenario$t[plot_range],
  hot_scenario$predict_ae[plot_range],
  col = "red",
  lwd = 1.5,
  lty = 3
)

lines(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  col = "darkgreen",
  lwd = 1.5
)

lines(
  typical_df$t[plot_range],
  typical_df$predict_ae[plot_range],
  col = "gold",
  lwd = 3
)

mtext("(a)", side = 3, line = 0.3, adj = 0, font = 1, cex = 0.85)


# ------------------------------------------
# PANEL 2: Prolonged Heatwave
# ------------------------------------------

ylim_b <- c(
  350,
  max(prolonged_df$upper[plot_range], na.rm = TRUE) + 10
)

plot(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  type = "n",
  ylim = ylim_b,
  xlab = "",
  ylab = "Daily A&E attendances",
  main = "",
  xaxt = "n",
  las = 1,
  cex.axis = 0.9,
  cex.lab = 1.0
)

rect(
  xleft = 11,
  ybottom = par("usr")[3],
  xright = 20,
  ytop = par("usr")[4],
  col = adjustcolor("darkorange", alpha.f = 0.06),
  border = NA
)

abline(v = c(11, 20), col = "gray70", lty = 2)

polygon(
  c(
    prolonged_df$t[plot_range],
    rev(prolonged_df$t[plot_range])
  ),
  c(
    prolonged_df$lower[plot_range],
    rev(prolonged_df$upper[plot_range])
  ),
  col = adjustcolor("darkorange", alpha.f = 0.20),
  border = NA
)

lines(
  baseline_scenario$t[plot_range],
  baseline_scenario$predict_ae[plot_range],
  col = "blue",
  lwd = 1.5,
  lty = 3
)

lines(
  hot_scenario$t[plot_range],
  hot_scenario$predict_ae[plot_range],
  col = "red",
  lwd = 1.5,
  lty = 3
)

lines(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  col = "darkgreen",
  lwd = 1.5
)

lines(
  prolonged_df$t[plot_range],
  prolonged_df$predict_ae[plot_range],
  col = "darkorange",
  lwd = 3
)

mtext("(b)", side = 3, line = 0.3, adj = 0, font = 1, cex = 0.85)


# ------------------------------------------
# PANEL 3: Future Heatwave
# ------------------------------------------

ylim_c <- c(350, 580)

plot(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  type = "n",
  ylim = ylim_c,
  xlab = "",
  ylab = "Daily A&E attendances",
  main = "",
  xaxt = "n",
  las = 1,
  cex.axis = 0.9,
  cex.lab = 1.0
)

rect(
  xleft = 11,
  ybottom = par("usr")[3],
  xright = 20,
  ytop = par("usr")[4],
  col = adjustcolor("purple", alpha.f = 0.05),
  border = NA
)

abline(v = c(11, 20), col = "gray70", lty = 2)

polygon(
  c(
    future_temps_df$t[plot_range],
    rev(future_temps_df$t[plot_range])
  ),
  c(
    future_temps_df$lower[plot_range],
    rev(future_temps_df$upper[plot_range])
  ),
  col = adjustcolor("purple", alpha.f = 0.15),
  border = NA
)

lines(
  baseline_scenario$t[plot_range],
  baseline_scenario$predict_ae[plot_range],
  col = "blue",
  lwd = 1.5,
  lty = 3
)

lines(
  hot_scenario$t[plot_range],
  hot_scenario$predict_ae[plot_range],
  col = "red",
  lwd = 1.5,
  lty = 3
)

lines(
  forecast_df$t[plot_range],
  forecast_df$predict_ae[plot_range],
  col = "darkgreen",
  lwd = 1.5
)

lines(
  future_temps_df$t[plot_range],
  future_temps_df$predict_ae[plot_range],
  col = "purple",
  lwd = 3
)

mtext("(c)", side = 3, line = 0.3, adj = 0, font = 1, cex = 0.85)


# ==========================================
# COMMON X-AXIS
# ==========================================

axis(
  side = 1,
  at = plot_range,
  labels = day_labels,
  las = 2,
  cex.axis = 0.9
)


# ==========================================
# COMMON LEGEND
# ==========================================

par(
  fig = c(0, 1, 0, 1),
  oma = c(0, 0, 0, 0),
  mar = c(0, 0, 0, 0),
  new = TRUE
)

plot(
  0, 0,
  type = "n",
  bty = "n",
  xaxt = "n",
  yaxt = "n",
  xlim = c(0, 1),
  ylim = c(0, 1)
)

legend(
  "bottom",
  legend = c(
    "Min Historical",
    "Max Historical",
    "August 2025",
    "Scenario",
    "Heatwave Window"
  ),
  col = c(
    "blue",
    "red",
    "darkgreen",
    "black",
    adjustcolor("gray60", alpha.f = 0.2)
  ),
  lty = c(3, 3, 1, 1, NA),
  lwd = c(1.5, 1.5, 1.5, 3, NA),
  pch = c(NA, NA, NA, NA, 15),
  pt.cex = 1.0,
  bty = "n",
  horiz = TRUE,
  xpd = TRUE,
  cex = 0.75,
  x.intersp = 0.5,
  seg.len = 1.5
)

par(mfrow = c(1, 1))

p_forecast_7day <- ggplot(
  daily_summary,
  aes(x = date)
) +
  geom_ribbon(
    aes(ymin = lower_PI, ymax = upper_PI),
    alpha = 0.20
  ) +
  geom_line(
    aes(y = expected_admissions),
    linewidth = 1.0
  ) +
  geom_line(
    aes(y = historical_mean),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_point(
    aes(y = expected_admissions),
    size = 2.0
  ) +
  labs(
    x = "Date",
    y = "Daily A&E attendances"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      size = 10,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )

print(p_forecast_7day)

p_weekly_probability <- ggplot(
  probability_table,
  aes(
    x = Threshold,
    y = Probability
  )
) +
  geom_line(
    linewidth = 1.0
  ) +
  geom_point(
    size = 2.0
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Weekly A&E attendances",
    y = "Probability of exceeding threshold"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    plot.title = element_blank()
  )

print(p_weekly_probability)

library(tidyverse)

# ---- Reshape the crosspred RR matrix into long format ----
# cp_ae$matRRfit: rows = temperature grid (from `by = 0.5`), 
#                 columns = lag days (0 to 7)
library(tidyverse)

# ---- Reshape the crosspred RR matrix into long format ----
rr_df <- as.data.frame(cp_ae$matRRfit) %>%
  rownames_to_column("temp_max") %>%
  mutate(temp_max = as.numeric(temp_max)) %>%
  pivot_longer(-temp_max, names_to = "lag", values_to = "RR") %>%
  mutate(
    lag    = as.numeric(gsub("[^0-9.-]", "", lag)),  # strips "lag" prefix
    log_RR = log(RR)
  )

# ---- Heat map: RR, diverging colour scale centred at 1.0 (no change in risk) ----
p_heatmap_logRR <- ggplot(rr_df, aes(x = lag, y = temp_max, fill = RR)) +
  geom_tile() +
  geom_hline(yintercept = 32.2, color = "red", linewidth = 0.8) +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red",
    midpoint = 1.0,  # Baseline risk = 1.0
    name = "Relative risk (RR)"
  ) +
  scale_x_continuous(breaks = 0:7) +
  labs(
    x = "Lag (days)",
    y = "Daily maximum temperature (\u00b0C)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_text(size=11),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

p_heatmap_logRR

library(ggplot2)
library(patchwork)
library(grid)
library(gridGraphics)

# -----------------------------
# Panel A: 3D temperature-lag plot
# -----------------------------

par(mar = c(5, 5, 4, 5) + 0.1)

d3 <- plot(
  cp_ae,
  ptype = "3d",
  
  xlab = "\n\n\nDaily maximum temperature (°C)",
  ylab = "\n\n\nLag (days)",
  zlab = "\n\n\nRelative risk",
  main = "",
  
  # Camera and projection
  theta = 215,
  phi = 25,
  expand = 0.65,
  ltheta = 170,
  shade = 0.35,
  
  # Tick formatting
  ticktype = "detailed",
  nticks = 5
)

# Reference temperature at the 50th percentile (32.2°C)
lines(
  trans3d(
    x = 32.2,
    y = 0:7,
    z = cp_ae$matRRfit[as.character(32), ],
    pmat = d3
  ),
  lwd = 2,
  col = "red"
)

# Capture the 3D base-R plot
grid.echo()
p_3d <- grid.grab()


# -----------------------------
# Panel B: heatmap
# -----------------------------

p_heatmap_logRR <- ggplot(
  rr_df,
  aes(x = lag, y = temp_max, fill = log_RR)
) +
  geom_tile() +
  geom_hline(
    yintercept = 32.2,
    color = "red",
    linewidth = 0.8
  ) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "log(RR)"
  ) +
  scale_x_continuous(breaks = 0:7) +
  labs(
    x = "Lag (days)",
    y = "Daily maximum temperature (°C)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12)
  )


# -----------------------------
# Combine
# -----------------------------

figure_temp_lag <-
  wrap_elements(p_3d) +
  p_heatmap_logRR +
  plot_layout(ncol = 2) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

figure_temp_lag

library(ggplot2)
library(patchwork)
library(grid)
library(gridGraphics)
library(cowplot)

# -----------------------------
# Panel A: 3D temperature-lag plot
# -----------------------------

par(
  mar = c(4.5, 4.5, 1, 4.5),
  oma = c(0, 0, 0, 0)
)

d3 <- plot(
  cp_ae,
  ptype = "3d",
  xlab = "Daily maximum temperature (°C)",
  ylab = "Lag (days)",
  zlab = "Relative risk",
  main = "",
  theta = 215,
  phi = 25,
  expand = 0.65,
  ltheta = 170,
  shade = 0.35,
  ticktype = "detailed",
  nticks = 5
)

# Reference temperature
lines(
  trans3d(
    x = 32.2,
    y = 0:7,
    z = cp_ae$matRRfit[as.character(32), ],
    pmat = d3
  ),
  lwd = 2,
  col = "red"
)

# Capture the entire base plot
grid.echo()
p_3d <- grid.grab()

# Put the 3D plot into a full-size drawing area
p_3d_full <- ggdraw() +
  draw_grob(
    p_3d,
    x = -0.08,
    y = -0.08,
    width = 1.16,
    height = 1.16
  )


# -----------------------------
# Panel B: heatmap
# -----------------------------

p_heatmap_logRR <- ggplot(
  rr_df,
  aes(x = lag, y = temp_max, fill = log_RR)
) +
  geom_tile() +
  geom_hline(
    yintercept = 32.2,
    color = "red",
    linewidth = 0.8
  ) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "log(RR)"
  ) +
  scale_x_continuous(breaks = 0:7) +
  labs(
    x = "Lag (days)",
    y = "Daily maximum temperature (°C)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.text = element_text(color = "black")
  )


# -----------------------------
# Combine
# -----------------------------

figure_temp_lag <-
  p_3d_full +
  p_heatmap_logRR +
  plot_layout(
    ncol = 2,
    widths = c(1.3, 1)
  ) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

figure_temp_lag

#gemini combine
library(patchwork)

p_3d <- wrap_elements(~ {
  d3 <- plot(
    cp_ae,
    ptype = "3d",
    xlab = "\n\n\nDaily maximum temperature (°C)",
    ylab = "\n\n\nLag (days)",
    zlab = "\n\n\nRelative risk",
    main = "",
    theta = 215,
    phi = 25,
    expand = 0.65,
    ltheta = 170,
    shade = 0.35,
    ticktype = "detailed",
    nticks = 5
  )
  
  lines(
    trans3d(
      x = 32.2,
      y = 0:7,
      z = cp_ae$matRRfit[as.character(32), ],
      pmat = d3
    ),
    lwd = 2,
    col = "red"
  )
})

# 2. Combine with ggplot heatmap
combined_figure <- p_3d + p_heatmap_logRR +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) & 
  theme(plot.tag = element_text(face = "bold", size = 14))

# Display figure
combined_figure

library(cowplot)
library(gridGraphics)


draw_3d_plot <- function() {
  d3 <- plot(
    cp_ae,
    ptype = "3d",
    xlab = "\n\n\nDaily maximum temperature (°C)",
    ylab = "\n\n\nLag (days)",
    zlab = "\n\n\nRelative risk (RR)",
    main = "",
    theta = 215,
    phi = 25,
    expand = 0.65,
    ltheta = 170,
    shade = 0.35,
    ticktype = "detailed",
    nticks = 5
  )
  
  lines(
    trans3d(
      x = 32.2,
      y = 0:7,
      z = cp_ae$matRRfit[as.character(32), ],
      pmat = d3
    ),
    lwd = 2,
    col = "red"
  )
}

# 2. Combine using plot_grid
combined_figure <- plot_grid(
  draw_3d_plot, p_heatmap_logRR,
  labels = c("(a)", "(b)"),
  label_size = 14,
  label_fontface = "plain",
  ncol = 2,
  rel_widths = c(1, 1)
)

# Display figure
combined_figure

library(patchwork)

p_3d <- wrap_elements(~ {
  # par(mgp = c(axis title, axis labels, axis line))
  # Reducing mgp[1] pulls axis titles closer
  par(mar = c(2.5, 2, 1, 1), mgp = c(1.5, 0.5, 0)) 
  
  d3 <- plot(
    cp_ae,
    ptype = "3d",
    # Reduced from 3-4 newlines down to 1 (or 0)
    xlab = "\nDaily maximum temperature (°C)",
    ylab = "\nLag (days)",
    zlab = "\nRelative risk (RR)",
    main = "",
    theta = 215,
    phi = 25,
    expand = 0.65,
    ltheta = 170,
    shade = 0.35,
    ticktype = "detailed",
    nticks = 5
  )
  
  lines(
    trans3d(
      x = 32.2,
      y = 0:7,
      z = cp_ae$matRRfit[as.character(32), ],
      pmat = d3
    ),
    lwd = 2,
    col = "red"
  )
})

# Combine into final figure
combined_figure <- p_3d + p_heatmap_logRR +
  plot_layout(widths = c(1.1, 1)) +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  ) & 
  theme(
    plot.tag = element_text(face = "plain", size = 14),
    plot.tag.position = c(0, 1)
  )

combined_figure

# r shiny tool
#shiny::runApp("app.R")
