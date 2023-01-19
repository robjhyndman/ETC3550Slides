library(fpp3)

## Daily data with annual and weekly seasonality

vic_elec_daily <- vic_elec %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature),
    names_to = "var",
    values_to = "value"
  ) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y")

elec_fit <- vic_elec_daily %>%
  model(
    ets = ETS(Demand),
    arima = ARIMA(log(Demand),
      stepwise = FALSE, order_constraint = (p + q <= 8 & P + Q <= 5)
    ),
    dhr = ARIMA(
      log(Demand) ~ Temperature + I(Temperature^2) +
        (Day_Type == "Weekday") + fourier(period = "year", K = 4),
      stepwise = FALSE, order_constraint = (p + q <= 8 & P + Q <= 5)
    )
  )

accuracy(elec_fit)

# ETS
elec_fit %>%
  select(ets) %>%
  report()
elec_fit %>%
  select(ets) %>%
  gg_tsresiduals()

elec_fit %>%
  select(ets) %>%
  augment() %>%
  filter(Date <= "2012-03-31") %>%
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")

# ARIMA
elec_fit %>%
  select(arima) %>%
  report()
elec_fit %>%
  select(arima) %>%
  gg_tsresiduals()

elec_fit %>%
  select(arima) %>%
  augment() %>%
  filter(Date >= "2014-10-01") %>%
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")

# DHR
elec_fit %>%
  select(dhr) %>%
  report()
elec_fit %>%
  select(dhr) %>%
  gg_tsresiduals()

elec_fit %>%
  select(dhr) %>%
  augment() %>%
  filter(Date <= "2012-03-31") %>%
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")


# Forecast one day ahead
vic_next_day <- new_data(vic_elec_daily, 1) %>%
  mutate(Temperature = 26, Day_Type = "Holiday")
forecast(elec_fit, new_data = vic_next_day) %>%
  autoplot(vic_elec_daily %>% tail(14), level = 80) +
  labs(y = "Electricity demand (GW)")

# Forecast 14 days ahead
vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = c(rep(42, 7), rep(25, 7)),
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(elec_fit, new_data = vic_elec_future) %>%
  autoplot(vic_elec_daily %>% tail(14), level = 80) +
  labs(y = "Electricity demand (GW)")

# Forecast a year ahead using last year's temperatures
vic_elec_future <- new_data(vic_elec_daily, 365) %>%
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
forecast(elec_fit, new_data = vic_elec_future) %>%
  filter(.model == "dhr") %>%
  autoplot(vic_elec_daily %>% tail(365), level = 80) +
  labs(y = "Electricity demand (GW)")


## TV ADVERTISING ----------------------------------------------------------

insurance

insurance %>%
  pivot_longer(c(Quotes, TVadverts)) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = NULL, title = "Insurance advertising and quotations")

insurance %>%
  mutate(
    lag1 = lag(TVadverts),
    lag2 = lag(lag1)
  ) %>%
  as_tibble() %>%
  select(-Month) %>%
  rename(lag0 = TVadverts) %>%
  pivot_longer(-Quotes, names_to = "Lag", values_to = "TV_advert") %>%
  ggplot(aes(x = TV_advert, y = Quotes)) +
  geom_point() +
  facet_grid(. ~ Lag) +
  labs(title = "Insurance advertising and quotations")

fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) %>%
  # Estimate models
  model(
    ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts)),
    ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) +
      lag(TVadverts, 2)),
    ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) +
      lag(TVadverts, 2) + lag(TVadverts, 3))
  )

glance(fit)

glance(fit) %>%
  transmute(`Lag order` = 0:3, sigma2, log_lik, AIC, AICc, BIC)

fit %>%
  select(2) %>%
  report()

fit_best <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts)))
report(fit_best)

advert_a <- new_data(insurance, 20) %>%
  mutate(TVadverts = 12)
forecast(fit_best, advert_a) %>% autoplot(insurance)
