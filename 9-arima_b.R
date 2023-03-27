library(fpp3)

## H02 drugs

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

h02 |> autoplot(Cost)

h02 |> autoplot(log(Cost))

h02 |> autoplot(
  log(Cost) |> difference(12)
)

h02 |> autoplot(
  log(Cost) |> difference(12) |> difference(1)
)

h02 |>
  mutate(log_sales = log(Cost)) |>
  features(log_sales, feat_stl)

h02 |>
  mutate(log_sales = log(Cost)) |>
  features(log_sales, unitroot_nsdiffs)
h02 |>
  mutate(d_log_sales = difference(log(Cost), 12)) |>
  features(d_log_sales, unitroot_ndiffs)

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
