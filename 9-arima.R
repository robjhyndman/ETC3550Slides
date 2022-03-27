library(fpp3)

#### GOOGLE STOCK PRICE 2018 ----------------

google_2018 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

google_2018 %>% autoplot(Close)
google_2018 %>%
  ACF(Close) %>%
  autoplot()

google_2018 %>% autoplot(difference(Close)) +
  labs(y = "Google closing stock price", x = "Day")

google_2018 %>%
  ACF(difference(Close)) %>%
  autoplot()

google_2018 %>%
  features(Close, unitroot_kpss)
google_2018 %>%
  features(difference(Close), unitroot_kpss)
google_2018 %>%
  features(Close, unitroot_ndiffs)

### WWW usage

wwwusage <- as_tsibble(WWWusage)
wwwusage %>% autoplot(value)
wwwusage %>% autoplot(difference(value))
wwwusage %>% autoplot(difference(value, differences = 2))
wwwusage %>% features(value, unitroot_kpss)
wwwusage %>% features(difference(value), unitroot_pp)

wwwusage %>% features(difference(value), unitroot_ndiffs)

## A10 drugs

a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost) / 1e6)

a10 %>% autoplot(Cost)

a10 %>% autoplot(log(Cost))

a10 %>% autoplot(
  log(Cost) %>% difference(12)
)

## H02 drugs

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost) / 1e6)

h02 %>% autoplot(Cost)

h02 %>% autoplot(log(Cost))

h02 %>% autoplot(
  log(Cost) %>% difference(12)
)

h02 %>% autoplot(
  log(Cost) %>% difference(12) %>% difference(1)
)

h02 %>%
  mutate(log_sales = log(Cost)) %>%
  features(log_sales, list(unitroot_nsdiffs, feat_stl))

h02 %>%
  mutate(log_sales = log(Cost)) %>%
  features(log_sales, unitroot_nsdiffs)
h02 %>%
  mutate(d_log_sales = difference(log(Cost), 12)) %>%
  features(d_log_sales, unitroot_ndiffs)

## Australian tourism --------------------------

total_trips <- tourism %>%
  summarise(Trips = sum(Trips))

total_trips %>% autoplot(Trips)

total_trips %>% features(Trips, unitroot_nsdiffs)

total_trips %>% features(difference(Trips, 4), unitroot_ndiffs)

total_trips %>%
  autoplot(Trips %>% difference(4))

total_trips %>%
  autoplot(Trips %>% difference(4) %>% difference(1))

## EGYPTIAN EXPORTS

global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

fit <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit)

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 4)

fit %>%
  forecast(h = 10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

global_economy %>%
  filter(Code == "EGY") %>%
  ACF(Exports) %>%
  autoplot()
global_economy %>%
  filter(Code == "EGY") %>%
  PACF(Exports) %>%
  autoplot()

global_economy %>%
  filter(Code == "EGY") %>%
  gg_tsdisplay(Exports, plot_type = "partial")

fit1 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4, 0, 0)))
report(fit1)

fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit2)

## CAF EXPORTS

global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(
    title = "Central African Republic exports",
    y = "% of GDP"
  )

global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type = "partial")

caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(
    arima210 = ARIMA(Exports ~ pdq(2, 1, 0)),
    arima013 = ARIMA(Exports ~ pdq(0, 1, 3)),
    stepwise = ARIMA(Exports),
    search = ARIMA(Exports, stepwise = FALSE)
  )

caf_fit

glance(caf_fit) %>%
  arrange(AICc) %>%
  select(.model:BIC)

caf_fit %>%
  select(search) %>%
  gg_tsresiduals()

caf_fit %>%
  select(search) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

caf_fit %>%
  forecast(h = 50) %>%
  filter(.model == "search") %>%
  autoplot(global_economy)

library(fpp3)

## WWW usage -----------------------------------------------------------------------

web_usage <- as_tsibble(WWWusage)
web_usage %>% gg_tsdisplay(value, plot_type = "partial")
web_usage %>% gg_tsdisplay(difference(value), plot_type = "partial")

fit <- web_usage %>%
  model(arima = ARIMA(value ~ pdq(3, 1, 0))) %>%
  report()

web_usage %>%
  model(auto = ARIMA(value ~ pdq(d = 1))) %>%
  report()

web_usage %>%
  model(auto2 = ARIMA(value ~ pdq(d = 1),
    stepwise = FALSE, approximation = FALSE
  )) %>%
  report()

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 3)

fit %>%
  forecast(h = 10) %>%
  autoplot(web_usage)

## GDP --------------------------------------------------------------------------

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(log(GDP))

fit <- global_economy %>%
  model(
    ARIMA(log(GDP))
  )

fit

fit %>%
  filter(Country == "Australia") %>%
  report()
fit %>%
  filter(Country == "Australia") %>%
  gg_tsresiduals()
fit %>%
  filter(Country == "Australia") %>%
  augment() %>%
  features(.resid, ljung_box, dof = 2, lag = 15)
fit %>%
  filter(Country == "Australia") %>%
  forecast(h = 10) %>%
  autoplot(global_economy) +
  scale_y_log10()

## US leisure employment

leisure <- us_employment %>%
  filter(
    Title == "Leisure and Hospitality",
    year(Month) > 2000
  ) %>%
  mutate(Employed = Employed / 1000) %>%
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )

leisure %>%
  gg_tsdisplay(difference(Employed, 12),
    plot_type = "partial", lag = 36
  ) +
  labs(title = "Seasonally differenced", y = "")

leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(),
    plot_type = "partial", lag = 36
  ) +
  labs(title = "Double differenced", y = "")

fit <- leisure %>%
  model(
    arima012011 = ARIMA(Employed ~ pdq(0, 1, 2) + PDQ(0, 1, 1)),
    arima210011 = ARIMA(Employed ~ pdq(2, 1, 0) + PDQ(0, 1, 1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit %>% pivot_longer(everything(),
  names_to = "Model name",
  values_to = "Orders"
)

glance(fit) %>%
  arrange(AICc) %>%
  select(.model:BIC)

fit %>%
  select(auto) %>%
  gg_tsresiduals(lag = 36)

augment(fit) %>% features(.innov, ljung_box, lag = 24, dof = 4)

forecast(fit, h = 36) %>%
  filter(.model == "auto") %>%
  autoplot(leisure) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )

## h02 drugs ----------------------------------------------------------------------

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))

h02 %>% autoplot(Cost)

## Models using logs

h02 %>% autoplot(log(Cost))
h02 %>% gg_tsdisplay(difference(log(Cost), 12), lag_max = 36, plot_type = "partial")

# My best guess
fit <- h02 %>%
  model(best = ARIMA(log(Cost) ~ 0 + pdq(3, 0, 1) + PDQ(0, 1, 2)))
report(fit)
gg_tsresiduals(fit, lag_max = 36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

# Letting R choose
fit <- h02 %>% model(auto = ARIMA(log(Cost), stepwise = FALSE))
report(fit)
gg_tsresiduals(fit, lag_max = 36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

# Letting R work hard to choose
fit <- h02 %>%
  model(best = ARIMA(log(Cost),
    stepwise = FALSE,
    approximation = FALSE,
    order_constraint = p + q + P + Q <= 9 & (constant + d + D <= 2)
  ))
report(fit)
gg_tsresiduals(fit, lag_max = 36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 9)

# The forecasts
fit %>%
  forecast() %>%
  autoplot(h02) +
  labs(y = "H02 Expenditure ($AUD)")

## AUS ECONOMY ETS vs ARIMA
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Population = Population / 1e6)
aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    eta = ETS(Population),
    arima = ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy) %>%
  select(.model, ME:RMSSE)

aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy) +
  labs(
    title = "Australian population",
    y = "People (millions)"
  )

# QUARTERLY CEMENT ETS vs ARIMA

cement <- aus_production %>%
  select(Cement) %>%
  filter_index("1988 Q1" ~ .)
train <- cement %>% filter_index(. ~ "2007 Q4")
fit <- train %>%
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement)
  )

fit %>%
  select(arima) %>%
  report()

fit %>%
  select(ets) %>%
  report()

gg_tsresiduals(fit %>% select(arima), lag_max = 16)

gg_tsresiduals(fit %>% select(ets), lag_max = 16)

fit %>%
  select(arima) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

fit %>%
  select(ets) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

fit %>%
  forecast(h = "2 years 6 months") %>%
  accuracy(cement) %>%
  select(-ME, -MPE, -ACF1)

fit %>%
  select(arima) %>%
  forecast(h = "3 years") %>%
  autoplot(cement) +
  labs(
    title = "Cement production in Australia",
    y = "Tonnes ('000)"
  )
