library(fpp3)

## EGYPTIAN EXPORTS

global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))
report(fit)

gg_tsresiduals(fit)

augment(fit) |>
  fit() |>
  forecast(h = 10) |>
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

## CAF EXPORTS

global_economy |>
  filter(Code == "CAF") |>
  autoplot(Exports) +
  labs(
    title = "Central African Republic exports",
    y = "% of GDP"
  )

global_economy |>
  filter(Code == "CAF") |>
  model(ARIMA(Exports)) |>
  report()

global_economy |>
  filter(Code == "CAF") |>
  model(ARIMA(Exports)) |>
  forecast(h = 20) |>
  autoplot(global_economy)
