library(fpp3)

## ---- Holiday tourism by state------------------------------------------------

holidays <- tourism |>
  as_tibble() |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)


## Fit models ------------------------------------------------------------------

fit <- holidays |>
  model(
    Seasonal_naive = SNAIVE(Trips),
    Naive = NAIVE(Trips),
    Drift = RW(Trips ~ drift()),
    Mean = MEAN(Trips)
  )

## Check residuals

fit |>
  filter(State == "Victoria") |>
  select(Seasonal_naive) |>
  gg_tsresiduals()

augment(fit) |>
  filter(State == "Victoria", .model == "Seasonal_naive") |>
  features(.innov, ljung_box, lag = 8)

## Which model fits best?

accuracy(fit) |>
  group_by(.model) |>
  summarise(
    RMSSE = sqrt(mean(RMSSE^2)),
    MAPE = mean(MAPE)
  ) |>
  arrange(RMSSE)

## Produce forecasts

holidays_fc <- fit |>
  forecast(h = "4 years")

holidays_fc |>
  autoplot(holidays, level = NULL)

holidays_fc |>
  filter(.model == "Seasonal_naive") |>
  autoplot(holidays, show_gap = FALSE)

holidays_fc |>
  hilo(level = 95) |>
  mutate(
    lower = `95%`$lower,
    upper = `95%`$upper
  )

# Now try a decomposition forecasting model

stl_fit <- holidays |>
  model(
    stlf = decomposition_model(
      STL(Trips),
      NAIVE(season_adjust)
    ))
stl_fit |>
  forecast(h = "4 years") |>
  autoplot(holidays)

accuracy(stl_fit) |>
  summarise(
    RMSSE = sqrt(mean(RMSSE^2)),
    MAPE = mean(MAPE)
  )

# Use a test set of last 2 years to check forecast accuracy

training <- holidays |>
  filter(Quarter <= max(Quarter) - 8)

training_fit <- training |>
  model(
    Seasonal_naive = SNAIVE(Trips),
    Naive = NAIVE(Trips),
    Drift = RW(Trips ~ drift()),
    Mean = MEAN(Trips),
    stlf = decomposition_model(
      STL(Trips),
      NAIVE(season_adjust)
    )
  )

test_fc <- training_fit |>
  forecast(h = "4 years")

test_fc |>
  autoplot(holidays, level = NULL)

test_fc |>
  accuracy(holidays) |>
  group_by(.model) |>
  summarise(
    RMSSE = sqrt(mean(RMSSE^2)),
    MAPE = mean(MAPE)
  ) |>
  arrange(RMSSE)

# Now use time series cross-validation to check forecast accuracy

holiday_stretch <- holidays |>
  stretch_tsibble(.init = 12, .step = 1) |>
  filter(.id != max(.id))

cv_fit <- holiday_stretch |>
  model(
    Seasonal_naive = SNAIVE(Trips),
    Naive = NAIVE(Trips),
    Drift = RW(Trips ~ drift()),
    Mean = MEAN(Trips),
    stlf = decomposition_model(
      STL(Trips),
      NAIVE(season_adjust)
    )
  )

cv_fc <- cv_fit |>
  forecast(h = 12) |>
  group_by(.id, State, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Trips", distribution = Trips)

cv_fc |>
  accuracy(holidays, by = c("h", ".model", "State")) |>
  group_by(.model, h) |>
  summarise(RMSSE = sqrt(mean(RMSSE^2))) |>
  ggplot(aes(x=h, y=RMSSE, group=.model, col=.model)) +
  geom_line()

