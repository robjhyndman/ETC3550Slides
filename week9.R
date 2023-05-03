library(fpp3)

## US leisure employment

leisure <- us_employment |>
  filter(
    Title == "Leisure and Hospitality",
    year(Month) > 2000
  ) |>
  mutate(Employed = Employed / 1000) |>
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )

leisure |>
  gg_tsdisplay(difference(Employed, 12),
               plot_type = "partial", lag = 36
  ) +
  labs(title = "Seasonally differenced", y = "")

leisure |>
  gg_tsdisplay(difference(Employed, 12) |> difference(),
               plot_type = "partial", lag = 36
  ) +
  labs(title = "Double differenced", y = "")

fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0, 1, 2) + PDQ(0, 1, 1)),
    arima210011 = ARIMA(Employed ~ pdq(2, 1, 0) + PDQ(0, 1, 1)),
    auto = ARIMA(Employed),
    best = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit
fit |> pivot_longer(everything(),
                    names_to = "Model name",
                    values_to = "Orders"
)

glance(fit) |>
  arrange(AICc) |>
  select(.model:BIC)

fit |>
  select(best) |>
  gg_tsresiduals(lag = 36)

report(fit |> select(best))
augment(fit) |>
  filter(.model == "best") |>
  features(.innov, ljung_box, lag = 24, dof = 4)

forecast(fit, h = 36) |>
  filter(.model == "best") |>
  autoplot(leisure) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )

# QUARTERLY CEMENT ETS vs ARIMA

cement <- aus_production |>
  select(Cement) |>
  filter_index("1988 Q1" ~ .)
cement |> autoplot(Cement)
train <- cement |> filter_index(. ~ "2007 Q4")
fit <- train |>
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement)
  )

fit |>
  select(arima) |>
  report()
gg_tsresiduals(fit |> select(arima), lag_max = 16)

fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 16, dof = 6)

fit |>
  select(ets) |>
  report()

gg_tsresiduals(fit |> select(ets), lag_max = 16)

fit |>
  select(ets) |>
  augment() |>
  features(.innov, ljung_box, lag = 16)

fit |>
  forecast(h = "2 years 6 months") |>
  accuracy(cement, level = 80)

fc <- cement |>
  model(arima = ARIMA(Cement)) |>
  forecast(h = "3 years")
fc |>
  autoplot(cement) +
  labs(
    title = "Cement production in Australia",
    y = "Tonnes ('000)"
  )
