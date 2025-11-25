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
  nest(data = -city) |> 
  mutate(model_results = map(data, fit_logistic_model)) |> 
  unnest(model_results) |> 
  filter(term == "victim_sexMale") |> 
  select(city, estimate, conf.low, conf.high)

knitr::kable(city_results)

# Plot
city_results |>
  mutate(city = fct_reorder(city, estimate)) |>
  ggplot(aes(x = city, y = estimate)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_point() +
  coord_flip() +
  labs(
    title = "Odds Ratio for Solving Homicides \n(Male vs Female Victims)",
    x = "City",
    y = "Estimated Odds Ratio (95% CI)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
