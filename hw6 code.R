library(tidyverse)
library(p8105.datasets)
library(ggplot2)
library(broom)
library(modelr)
library(glmnet)

## Problem 1

# Cleaning data
homicide_df =
  read_csv("hw6 data/homicide-data.csv") |> 
  mutate(
    victim_age = na_if(victim_age, "Unknown"),
    victim_age = as.numeric(victim_age),
    city_state = str_c(city, state, sep = ", ")
  ) |> 
  filter(victim_race == "White" | victim_race == "Black") |> 
  filter(!(city_state == "Dallas, TX" | city_state == "Phoenix, AZ"
           | city_state == "Kansas City, MO" | city_state == "Tulsa, AL"))

# city unsolved case counting
city_summary =
  homicide_df |>
  mutate(
    case_resolved = if_else(disposition == "Closed by arrest",
                            true = 1, false = 0)
  )

# Baltimore glm test
baltimore_summary =
  city_summary |> 
  filter(city_state == "Baltimore, MD")

baltimore_glm_test =
  glm(case_resolved ~ victim_age + victim_race +
        victim_sex, data = baltimore_summary, family = binomial)

# tidy baltimore results
baltimore_result =
  broom::tidy(baltimore_glm_test, exponentiate = TRUE, conf.int = TRUE) |> 
  filter(term == "victim_sexMale") |> 
  select(estimate, conf.low, conf.high)

knitr::kable(baltimore_result)

# Setting up the function
fit_logistic_model =
  function(df) {
  glm(case_resolved ~ victim_age + victim_race + victim_sex, 
      data = df, 
      family = binomial) |> 
    broom::tidy(exponentiate = TRUE, conf.int = TRUE)
}

# Run the function on all cities
city_results =
  city_summary |>
  nest(data = -city_state) |> 
  mutate(model_results = map(data, fit_logistic_model)) |> 
  unnest(model_results) |> 
  filter(term == "victim_sexMale") |> 
  select(city_state, estimate, conf.low, conf.high)

knitr::kable(city_results)

# Plot
city_results |>
  mutate(city_state = fct_reorder(city_state, estimate)) |>
  ggplot(aes(x = city_state, y = estimate)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point() +
  coord_flip() +
  labs(
    title = "Odds Ratio for Solving Homicides \n(Male vs Female Victims)",
    x = "City_State",
    y = "Estimated Odds Ratio (95% CI)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

## Problem 2
data("weather_df")

get_estimates = function(data) {
  boot_sample = data |> 
    slice_sample(prop = 1, replace = TRUE)
  
  fit = lm(tmax ~ tmin + prcp, data = boot_sample)

  r_squared = glance(fit) |> 
    pull(r.squared)

  coeffs = broom::tidy(fit)
  
  # Extract beta_1 (tmin) and beta_2 (prcp)
  beta_1 = coeffs |> 
    filter(term == "tmin") |> 
    pull(estimate)
  
  beta_2 = coeffs |> 
    filter(term == "prcp") %>%
    pull(estimate)
  
  # Calculate the ratio
  beta_ratio = beta_1 / beta_2
  
  # Return the results as a named list or data frame row
  tibble(
    r_squared = r_squared,
    beta_ratio = beta_ratio
  )
}

# Set seed for reproducibility
set.seed(20251125)

# Perform 5000 bootstrap iterations
bootstrap_results =
  map(1:5000, ~ get_estimates(weather_df)) |> 
  list_rbind() 

# Plotting
bootstrap_results |> 
  ggplot(aes(x = r_squared)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(
    title = expression("Bootstrap Distribution of" ~ R^2),
    x = expression(R^2 ~ "Estimate"),
    y = "Density"
  ) +
  theme_minimal()

bootstrap_results |> 
  ggplot(aes(x = beta_ratio)) +
  geom_density(fill = "coral", alpha = 0.7) +
  labs(
    title = expression("Bootstrap Distribution of" ~ hat(beta)[1] / hat(beta)[2]),
    x = expression(hat(beta)[1] / hat(beta)[2] ~ "Estimate"),
    y = "Density"
  ) +
  theme_minimal()

# Calculate the 95% CI for r^2
r_squared_ci =
  bootstrap_results |> 
  summarise(
    lower_ci = quantile(r_squared, 0.025),
    upper_ci = quantile(r_squared, 0.975)
  )

knitr::kable(r_squared_ci)

# Calculate the 95% CI for beta_ratio
beta_ratio_ci =
  bootstrap_results |> 
  summarise(
    lower_ci = quantile(beta_ratio, 0.025),
    upper_ci = quantile(beta_ratio, 0.975)
  )

knitr::kable(beta_ratio_ci)

## Problem 3
birthweight_df =
  read_csv("hw6 data/birthweight.csv") |> 
  janitor::clean_names() |>
  mutate(
    babysex = 
      case_match(babysex,
                 1 ~ "male",
                 2 ~ "female"
      ),
    babysex = fct_infreq(babysex),
    frace = 
      case_match(frace,
                 1 ~ "white",
                 2 ~ "black", 
                 3 ~ "asian", 
                 4 ~ "puerto_rican", 
                 8 ~ "other"),
    frace = fct_infreq(frace),
    mrace = 
      case_match(mrace,
                 1 ~ "white",
                 2 ~ "black", 
                 3 ~ "asian", 
                 4 ~ "puerto_rican",
                 8 ~ "other"),
    mrace = fct_infreq(mrace),
    malform = as.logical(malform))

# running stepwise
full_model = lm(bwt ~ ., data = birthweight_df)
null_model = lm(bwt ~ 1, data = birthweight_df)

final_model = 
  step(null_model, scope = list(lower = null_model, upper = full_model), 
                   direction = "both", trace = 0)
summary(final_model)

# plotting
birthweight_df = birthweight_df |> 
  add_predictions(final_model, var = "pred") |> 
  add_residuals(final_model, var = "resid")

ggplot(birthweight_df, aes(x = pred, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = "Residuals vs Fitted Values (Stepwise model)"
  ) +
  theme_minimal()

# comparing each other models
# M1 My stepwise model
M1_formula = formula(final_model)
# M2 (Simple)
M2_formula = bwt ~ blength + gaweeks
# M3 (Complex, with all interactions)
M3_formula = bwt ~ (bhead + blength + babysex)^3 

model_formulas =
  tibble(
  model_name = c("M1_Stepwise_Proxy", "M2_Simple", "M3_Complex"),
  formula = list(M1_formula, M2_formula, M3_formula)
)

# monte carlo validating
cv_results =
  birthweight_df |>
  crossv_mc(n = 100) |>
  mutate(models = list(model_formulas)) |>
  unnest(models) |>
  mutate(
    model_fit = map2(formula, train, ~ lm(.x, data = as_tibble(.y)))
  ) |>
  mutate(
    rmse = map2_dbl(model_fit, test, ~ modelr::rmse(.x, data = as_tibble(.y)))
  ) |>
  # results
  group_by(model_name) |>
  summarise(
    avg_cv_rmse = mean(rmse, na.rm = TRUE)
  )

print(cv_results)
