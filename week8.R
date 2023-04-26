library(fpp3)

## EGYPTIAN EXPORTS

global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports, stepwise = FALSE, approximation = FALSE,
              order_constraint = p + q <= 10))
report(fit)

gg_tsresiduals(fit)

fit |>
  forecast(h = 50) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

global_economy |>
  filter(Code == "EGY") |>
  ACF(Exports) |>
  autoplot()
global_economy |>
  filter(Code == "EGY") |>
  PACF(Exports) |>
  autoplot()

global_economy |>
  filter(Code == "EGY") |>
  gg_tsdisplay(Exports, plot_type = "partial")

fit1 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4, 0, 0)))
report(fit1)
