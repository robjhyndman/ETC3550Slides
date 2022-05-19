
## Now fit to multiple years and handle annual seasonality

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

fit <- vic_elec_daily %>%
  model(ARIMA(log(Demand) ~ Temperature + I(Temperature^2) +
                (Day_Type == "Weekday") +
                fourier(period="year",K=4),
              stepwise=FALSE, order_constraint = (p+q <= 8 & P+Q <= 5)))
report(fit)

gg_tsresiduals(fit)

augment(fit) %>%
  ggplot(aes(x=Date, y=Demand)) +
  geom_line() +
  geom_line(aes(y=.fitted), col="red")

augment(fit) %>%
  features(.resid, ljung_box, dof = 9, lag = 21)


# Forecast one day ahead
vic_next_day <- new_data(vic_elec_daily, 1) %>%
  mutate(Temperature = 26, Day_Type = "Holiday")
forecast(fit, new_data = vic_next_day)

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

forecast(fit, new_data = vic_elec_future) %>%
  autoplot(vic_elec_daily) +
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
  mutate(TVadverts = 10)
forecast(fit_best, advert_a) %>% autoplot(insurance)
