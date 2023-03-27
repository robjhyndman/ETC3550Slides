library(fpp3)

## US CHANGE -------------------------------------------------------------------

us_change |>
  pivot_longer(Consumption:Unemployment,
    names_to = "var", values_to = "value"
  ) |>
  ggplot(aes(x = Quarter, y = value, colour = var)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(
    title = "US consumption and personal income",
    y = "Quarterly % change"
  ) +
  guides(colour = "none")

# Regression
fit <- us_change |>
  model(TSLM(Consumption ~ Income))
report(fit)
gg_tsresiduals(fit)

# Dynamic regression
fit <- us_change |>
  model(
    ARIMA(Consumption ~ Income)
  )
report(fit)
gg_tsresiduals(fit)

residuals(fit, type = "regression") |>
  gg_tsdisplay(.resid, plot_type = "partial") +
  ggtitle("Regression errors")

residuals(fit, type = "innovation") |>
  gg_tsdisplay(.resid, plot_type = "partial") +
  ggtitle("ARIMA errors")

augment(fit) |>
  features(.innov, ljung_box, dof = 5, lag = 12)

us_change_future <- new_data(us_change, 11) |>
  mutate(
    Income = max(us_change$Income),
    Savings = mean(us_change$Savings),
    Unemployment = mean(us_change$Unemployment)
  )
forecast(fit, new_data = us_change_future) |>
  autoplot(us_change) +
  labs(
    x = "Year", y = "Percentage change",
    title = "Forecasts from regression with ARIMA(4,0,4) errors"
  )

## DAILY VICTORIAN ELECTRICITY DEMAND

vic_elec_daily <- vic_elec |>
  filter(year(Time) == 2014) |>
  index_by(Date = date(Time)) |>
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) |>
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily |>
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(x = "Maximum temperature", y = "Electricity demand (GW)")

vic_elec_daily |>
  pivot_longer(c(Demand, Temperature),
    names_to = "var",
    values_to = "value"
  ) |>
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y")

fit <- vic_elec_daily |>
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
    (Day_Type == "Weekday")))
report(fit)

gg_tsresiduals(fit)

augment(fit) |>
  features(.resid, ljung_box, dof = 9, lag = 21)

augment(fit) |>
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")

# Let's try harder to find a good model

fit <- vic_elec_daily |>
  model(ARIMA(log(Demand) ~ Temperature + I(Temperature^2) +
    (Day_Type == "Weekday"), stepwise = FALSE, order_constraint = (p + q <= 8 & P + Q <= 5)))
report(fit)

gg_tsresiduals(fit)

augment(fit) |>
  features(.resid, ljung_box, dof = 13, lag = 21)

augment(fit) |>
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")

# Forecast one day ahead
vic_next_day <- new_data(vic_elec_daily, 1) |>
  mutate(Temperature = 26, Day_Type = "Holiday")
forecast(fit, new_data = vic_next_day)

vic_elec_future <- new_data(vic_elec_daily, 14) |>
  mutate(
    Temperature = c(rep(35, 7), rep(25, 7)),
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

forecast(fit, new_data = vic_elec_future) |>
  autoplot(vic_elec_daily) +
  labs(y = "Electricity demand (GW)")

## AUSTRALIAN VISITORS -------------------------------------------------

aus_airpassengers |>
  autoplot(Passengers) +
  labs(
    y = "Passengers (millions)",
    title = "Total annual air passengers"
  )

fit_deterministic <- aus_airpassengers |>
  model(deterministic = ARIMA(Passengers ~ 1 + trend() + pdq(d = 0)))
report(fit_deterministic)

fit_stochastic <- aus_airpassengers |>
  model(stochastic = ARIMA(Passengers ~ 1 + pdq(d = 1)))
report(fit_stochastic)

fc_deterministic <- forecast(fit_deterministic, h = 20)
fc_stochastic <- forecast(fit_stochastic, h = 20)

aus_airpassengers |>
  autoplot(Passengers) +
  autolayer(fc_stochastic, colour = "#0072B2", level = 95) +
  autolayer(fc_deterministic, colour = "#D55E00", alpha = 0.65, level = 95) +
  labs(
    y = "Air passengers (millions)",
    title = "Forecasts from trend models"
  )

## AUSTRALIAN CAFE DATA --------------------------------------------------

aus_cafe <- aus_retail |>
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) |>
  summarise(Turnover = sum(Turnover))
aus_cafe |> autoplot(Turnover)

fit <- aus_cafe |> model(
  `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0, 0, 0)),
  `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0, 0, 0)),
  `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0, 0, 0)),
  `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0, 0, 0)),
  `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0, 0, 0)),
  `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0, 0, 0))
)

glance(fit) |>
  select(.model, sigma2, log_lik, AIC, AICc, BIC)

## US GASOLINE ---------------------------------------------------

us_gasoline |> autoplot(Barrels)

gasfit <- us_gasoline |>
  model(
    fourier1 = ARIMA(Barrels ~ fourier(K = 1) + PDQ(0, 0, 0)),
    fourier2 = ARIMA(Barrels ~ fourier(K = 2) + PDQ(0, 0, 0)),
    fourier3 = ARIMA(Barrels ~ fourier(K = 3) + PDQ(0, 0, 0)),
    fourier4 = ARIMA(Barrels ~ fourier(K = 4) + PDQ(0, 0, 0)),
    fourier5 = ARIMA(Barrels ~ fourier(K = 5) + PDQ(0, 0, 0)),
    fourier6 = ARIMA(Barrels ~ fourier(K = 6) + PDQ(0, 0, 0)),
    fourier7 = ARIMA(Barrels ~ fourier(K = 7) + PDQ(0, 0, 0)),
    fourier8 = ARIMA(Barrels ~ fourier(K = 8) + PDQ(0, 0, 0)),
    fourier9 = ARIMA(Barrels ~ fourier(K = 9) + PDQ(0, 0, 0)),
    fourier10 = ARIMA(Barrels ~ fourier(K = 10) + PDQ(0, 0, 0)),
    fourier11 = ARIMA(Barrels ~ fourier(K = 11) + PDQ(0, 0, 0)),
    fourier12 = ARIMA(Barrels ~ fourier(K = 12) + PDQ(0, 0, 0)),
    fourier13 = ARIMA(Barrels ~ fourier(K = 13) + PDQ(0, 0, 0)),
    fourier14 = ARIMA(Barrels ~ fourier(K = 14) + PDQ(0, 0, 0)),
  )

best <- glance(gasfit) |>
  filter(AICc == min(AICc)) |>
  pull(.model)
gasfit |>
  select(!!best) |>
  report()
gasfit |>
  select(!!best) |>
  gg_tsresiduals()

gasfit |>
  select(!!best) |>
  forecast(h = "3 years") |>
  autoplot(us_gasoline)

## 5-minute CALL CENTRE DATA ------------------------------------------------

(calls <- readr::read_tsv("http://robjhyndman.com/data/callcenter.txt") |>
  rename(time = `...1`) |>
  pivot_longer(-time, names_to = "date", values_to = "volume") |>
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    datetime = as_datetime(date) + time
  ) |>
  as_tsibble(index = datetime))

calls |>
  fill_gaps() |>
  autoplot(volume)

calls |>
  fill_gaps() |>
  gg_season(volume, period = "day", alpha = 0.4) +
  guides(colour = "none")

calls_mdl <- calls |>
  mutate(idx = row_number()) |>
  update_tsibble(index = idx)
fit <- calls_mdl |>
  model(ARIMA(volume ~ fourier(169, K = 10) + pdq(d = 0) + PDQ(0, 0, 0)))
report(fit)

gg_tsresiduals(fit, lag = 338)

fit |>
  forecast(h = 1690) |>
  autoplot(calls_mdl)
