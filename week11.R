library(fpp3)

## Daily data with annual and weekly seasonality

vic_elec_daily <- vic_elec |>
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

elec_fit <- vic_elec_daily |>
  model(
    ets = ETS(Demand),
    arima = ARIMA(log(Demand)),
    dhr = ARIMA(log(Demand) ~ Temperature + I(Temperature^2) +
        (Day_Type == "Weekday") + fourier(period = "year", K = 4))
  )

accuracy(elec_fit)

# ETS
elec_fit |>
  select(ets) |>
  report()
elec_fit |>
  select(ets) |>
  gg_tsresiduals()

elec_fit |>
  select(ets) |>
  augment() |>
  filter(Date <= "2014-03-31") |>
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")

# ARIMA
elec_fit |>
  select(arima) |>
  report()
elec_fit |>
  select(arima) |>
  gg_tsresiduals()

elec_fit |>
  select(arima) |>
  augment() |>
  filter(Date <= "2014-03-31") |>
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")

# DHR
elec_fit |>
  select(dhr) |>
  report()
elec_fit |>
  select(dhr) |>
  gg_tsresiduals()

elec_fit |>
  select(dhr) |>
  augment() |>
  filter(Date <= "2014-03-31") |>
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")

# Forecast one day ahead
vic_next_day <- new_data(vic_elec_daily, 1) |>
  mutate(Temperature = 26, Day_Type = "Holiday")
forecast(elec_fit, new_data = vic_next_day) |>
  autoplot(vic_elec_daily |> tail(14), level = 80) +
  labs(y = "Electricity demand (GW)")

# Forecast 14 days ahead
vic_elec_future <- new_data(vic_elec_daily, 14) |>
  mutate(
    Temperature = c(rep(32, 7), rep(25, 7)),
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(elec_fit, new_data = vic_elec_future) |>
  autoplot(vic_elec_daily |> tail(14), level = 80) +
  labs(y = "Electricity demand (GW)")

# Forecast a year ahead using last year's temperatures
vic_elec_future <- new_data(vic_elec_daily, 365) |>
  mutate(
    Temperature = tail(vic_elec_daily$Temperature, 365),
    Holiday = Date %in% as.Date(c(
      "2015-01-01", "2015-01-26", "2015-03-09",
      "2015-04-03", "2015-04-06", "2015-04-25",
      "2015-06-08", "2015-10-02", "2015-11-03",
      "2015-12-25"
    )),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(elec_fit, new_data = vic_elec_future) |>
  filter(.model == "dhr") |>
  autoplot(vic_elec_daily |> tail(365), level = 80) +
  labs(y = "Electricity demand (GW)")


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
    best_lm = TSLM(Barrels ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 6)),
    best_lm2 = ARIMA(Barrels ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) +
                       fourier(K = 6) + pdq(0,0,0) + PDQ(0,0,0))
  )

glance(gasfit) |> arrange(AICc)

gasfit |>
  select(fourier6) |>
  report()
gasfit |>
  select(fourier6) |>
  gg_tsresiduals()

gasfit |>
  select(fourier6) |>
  forecast(h = "3 years") |>
  autoplot(us_gasoline)

## 5-minute CALL CENTRE DATA ------------------------------------------------

calls <- readr::read_tsv("http://robjhyndman.com/data/callcenter.txt") |>
  rename(time = `...1`) |>
  pivot_longer(-time, names_to = "date", values_to = "volume") |>
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    datetime = as_datetime(date) + time
  ) |>
  as_tsibble(index = datetime)

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
calls_fit <- calls_mdl |>
  model(ARIMA(volume ~ fourier(169, K = 10) + pdq(d = 0) + PDQ(0, 0, 0)))
report(calls_fit)

gg_tsresiduals(calls_fit, lag = 338)

calls_fit |>
  forecast(h = 1690) |>
  autoplot(calls_mdl |> filter(idx > 20000))
