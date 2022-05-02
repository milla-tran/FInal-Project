Train <- Credit %>%
  filter(Month <= yearmonth("2014 Feb"))

Holdout <- Credit %>%
  filter(Month > yearmonth("2014 Feb"))

credit_fit <- Train %>%
  model(
    Mean = MEAN(credit_in_millions),
    `Naïve` = NAIVE(credit_in_millions),
    `Seasonal naïve` = SNAIVE(credit_in_millions),
    Drift = RW(credit_in_millions~drift())
  )
# Generate forecasts for 14 quarters
credit_fc <- credit_fit %>% forecast(h = 12)
# Plot forecasts against actual values
credit_fc %>%
  autoplot(Train, level = NULL) +
  autolayer(
    filter_index(Credit, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Credits in millions",
    title = "Forecasts for monthly credits"
  ) +
  guides(colour = guide_legend(title = "Forecast"))










#transformed data attempting to fix seasonality and trend, but doesn't make much of a difference.
lambda <- Credit %>%
  features(credit_in_millions, features = guerrero) %>%
  pull(lambda_guerrero)
Credit %>%
  autoplot(box_cox(credit_in_millions, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed credit in millions with $\\lambda$ = ",
         round(lambda,2)))) 

