library(fpp3)
library(lubridate)


## US CHANGE -------------------------------------------------------------------

us_change %>%
  gather(key='variable', value='value') %>%
  ggplot(aes(y=value, x=Quarter, group=variable, colour=variable)) +
  geom_line() + facet_grid(variable ~ ., scales='free_y') +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumption and personal income") +
  guides(colour="none")

# Regression
fit <- us_change %>% model(TSLM(Consumption ~ Income + Production + Savings + Unemployment))
gg_tsresiduals(fit)

# Dynamic regression
fit <- us_change %>%
  model(
    ARIMA(Consumption ~ Income + Savings + Unemployment + pdq(d=0) + PDQ(0,0,0),
          stepwise=FALSE, approximation=FALSE, order_constraint = p+q <= 10)
  )
report(fit)
gg_tsresiduals(fit)

residuals(fit, type='regression') %>%
  gg_tsdisplay(.resid, plot_type = 'partial') +
  ggtitle("Regression errors")

residuals(fit, type='innovation') %>%
  gg_tsdisplay(.resid, plot_type = 'partial') +
  ggtitle("ARIMA errors")

augment(fit) %>%
  features(.resid, ljung_box, dof = 5, lag = 12)

us_change_future <- new_data(us_change, 11) %>%
  mutate(
    Income = mean(us_change$Income),
    Savings = mean(us_change$Savings),
    Unemployment = mean(us_change$Unemployment)
  )
forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(x = "Year", y = "Percentage change",
       title = "Forecasts from regression with ARIMA(4,0,4) errors")


## DAILY VICTORIAN ELECTRICITY DEMAND

vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand)/1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily %>%
  ggplot(aes(x=Temperature, y=Demand, colour=Day_Type)) +
  geom_point() +
  labs(x = "Maximum temperature", y = "Electricity demand (GW)")

vic_elec_daily %>%
  pivot_longer(c(Demand,Temperature), names_to="var",
               values_to="value") %>%
  ggplot(aes(x = Date, y = value)) + geom_line() +
  facet_grid(vars(var), scales = "free_y")

fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
                (Day_Type=="Weekday") + PDQ(Q=0:3)))
report(fit)

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, dof = 9, lag = 21)

# Forecast one day ahead
vic_next_day <- new_data(vic_elec_daily, 1) %>%
  mutate(Temperature = 26, Day_Type = "Holiday")
forecast(fit, new_data=vic_next_day)

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = c(rep(36,7),rep(25,7)),
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

forecast(fit, new_data=vic_elec_future) %>%
  autoplot(vic_elec_daily) + ylab("Electricity demand (GW)")


## AUSTRALIAN VISITORS -------------------------------------------------

aus_visitors <- as_tsibble(fpp2::austa)
aus_visitors %>%
  autoplot(value) +
  labs(x = "Year", y = "millions of people",
       title = "Total annual international visitors to Australia")

fit_deterministic <- aus_visitors %>%
  model(Deterministic = ARIMA(value ~ trend() + pdq(d = 0)))
report(fit_deterministic)

fit_stochastic <- aus_visitors %>%
  model(Stochastic = ARIMA(value ~ pdq(d = 1)))
report(fit_stochastic)

fc_deterministic <- forecast(fit_deterministic, h = 10)
fc_stochastic <- forecast(fit_stochastic, h = 10)

rbind(fc_deterministic, fc_stochastic) %>%
  autoplot(aus_visitors, alpha = 0.5) +
  guides(colour=guide_legend(title="Forecast")) +
  labs(x = "Year", y = "Visitors to Australia (millions)",
       title = "Forecasts from trend models")


## AUSTRALIAN CAFE DATA --------------------------------------------------

aus_cafe <- aus_retail %>% filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) %>% summarise(Turnover = sum(Turnover))
aus_cafe %>% autoplot(Turnover)


fit <- aus_cafe %>% model(
    `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0,0,0)),
    `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0,0,0)),
    `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0,0,0)),
    `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0,0,0)),
    `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0,0,0)),
    `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0,0,0)))

glance(fit) %>%
  select(.model, sigma2, log_lik, AIC, AICc, BIC)


## US GASOLINE ---------------------------------------------------

us_gasoline %>% autoplot(Barrels)

fit <- us_gasoline %>%
  model(
    fourier1 = ARIMA(Barrels ~ fourier(K = 1) + PDQ(0,0,0)),
    fourier2 = ARIMA(Barrels ~ fourier(K = 2) + PDQ(0,0,0)),
    fourier3 = ARIMA(Barrels ~ fourier(K = 3) + PDQ(0,0,0)),
    fourier4 = ARIMA(Barrels ~ fourier(K = 4) + PDQ(0,0,0)),
    fourier5 = ARIMA(Barrels ~ fourier(K = 5) + PDQ(0,0,0)),
    fourier6 = ARIMA(Barrels ~ fourier(K = 6) + PDQ(0,0,0)),
    fourier7 = ARIMA(Barrels ~ fourier(K = 7) + PDQ(0,0,0)),
    fourier8 = ARIMA(Barrels ~ fourier(K = 8) + PDQ(0,0,0)),
    fourier9 = ARIMA(Barrels ~ fourier(K = 9) + PDQ(0,0,0)),
    fourier10 = ARIMA(Barrels ~ fourier(K = 10) + PDQ(0,0,0)),
    fourier11 = ARIMA(Barrels ~ fourier(K = 11) + PDQ(0,0,0)),
    fourier12 = ARIMA(Barrels ~ fourier(K = 12) + PDQ(0,0,0)),
    fourier13 = ARIMA(Barrels ~ fourier(K = 13) + PDQ(0,0,0)),
    fourier14 = ARIMA(Barrels ~ fourier(K = 14) + PDQ(0,0,0)),
  )

library(purrr)
models <- as.list(seq(26))
model_defs <- models %>%
  map(~ ARIMA(Barrels ~ fourier(K=!!.[1]) + PDQ(0,0,0)))
model_defs <- model_defs %>%
  set_names(map_chr(models, ~ sprintf("fourier%i", .[1])))
fit <- us_gasoline %>%
  model(!!!model_defs)

best <- glance(fit) %>%
  filter(AICc==min(AICc)) %>%
  pull(.model)
fit %>% select(!!best) %>% report(fit)

fit %>%
  select(!!best) %>%
  forecast(h = "3 years") %>%
    autoplot(us_gasoline)

## 5-minute CALL CENTRE DATA ------------------------------------------------

(calls <- readr::read_tsv("http://robjhyndman.com/data/callcenter.txt") %>%
    rename(time = X1) %>%
    pivot_longer(-time, names_to = "date", values_to = "volume") %>%
    mutate(
      date = as.Date(date, format = "%d/%m/%Y"),
      datetime = as_datetime(date) + time
    ) %>%
    as_tsibble(index = datetime))

calls %>% fill_gaps() %>% autoplot(volume)

calls %>% fill_gaps() %>%
  gg_season(volume, period = "day", alpha = 0.1) +
  guides(colour = FALSE)

library(sugrrants)
calls %>% filter(month(date, label = TRUE) == "Apr") %>%
  ggplot(aes(x = time, y = volume)) +
  geom_line() + facet_calendar(date)

calls_mdl <- calls %>%
  mutate(idx = row_number()) %>%
  update_tsibble(index = idx)
fit <- calls_mdl %>%
  model(ARIMA(volume ~ fourier(169, K = 10) + pdq(d=0) + PDQ(0,0,0)))
report(fit)

gg_tsresiduals(fit, lag=338)

fit %>% forecast(h = 1690) %>%
  autoplot(calls_mdl)

## TV ADVERTISING ----------------------------------------------------------

insurance <- as_tsibble(fpp2::insurance, pivot_longer = FALSE) %>%
  rename(Month = index)

insurance %>%
  pivot_longer(c(Quotes, TV.advert)) %>%
  ggplot(aes(x = Month, y = value)) + geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(x = "Year", y = NULL, title = "Insurance advertising and quotations")

insurance %>%
  mutate(
    lag1 = lag(TV.advert),
    lag2 = lag(lag1)
  ) %>%
  as_tibble() %>%
  select(-Month) %>%
  rename(lag0 = TV.advert) %>%
  pivot_longer(-Quotes, names_to="Lag", values_to="TV_advert") %>%
  ggplot(aes(x = TV_advert, y = Quotes)) + geom_point() +
  facet_grid(. ~ Lag) +
  labs(title = "Insurance advertising and quotations")

fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA,NA,NA,Quotes[4:40])) %>%
  # Estimate models
  model(
    ARIMA(Quotes ~ pdq(d = 0) + TV.advert),
    ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert)),
    ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert) +
            lag(TV.advert, 2)),
    ARIMA(Quotes ~ pdq(d = 0) + TV.advert + lag(TV.advert) +
            lag(TV.advert, 2) + lag(TV.advert, 3))
  )

glance(fit)

glance(fit) %>%
  transmute(`Lag order` = 0:3, sigma2, log_lik, AIC, AICc, BIC)

fit %>% select(2) %>% report()

fit <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d=0) + TV.advert + lag(TV.advert)))
report(fit)

advert_a <- new_data(insurance, 20) %>%
  mutate(TV.advert = 10)
forecast(fit, advert_a) %>% autoplot(insurance)

advert_b <- new_data(insurance, 20) %>%
  mutate(TV.advert = 8)
forecast(fit, advert_b) %>% autoplot(insurance)

advert_c <- new_data(insurance, 20) %>%
  mutate(TV.advert = 6)
forecast(fit, advert_c) %>% autoplot(insurance)

