library(fpp3)


## EGYPTIAN EXPORTS

global_economy %>%
  filter(Code == "EGY") %>%
  gg_tsdisplay(Exports, plot_type="partial")

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
  labs(title = "Central African Republic exports", y = "% of GDP")

global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type = "partial")

caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(
    arima210 = ARIMA(Exports ~ pdq(2, 1, 0)),
    arima013 = ARIMA(Exports ~ pdq(0, 1, 3)),
    stepwise = ARIMA(Exports),
    search = ARIMA(Exports, stepwise = FALSE, approximation=FALSE, order_constraint = (p+q <= 10))
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
  forecast(h = 5) %>%
  filter(.model == "search") %>%
  autoplot(global_economy)

# Fit models to GDP for all countries

fit <- global_economy %>%
  model(arima = ARIMA(log(GDP)))

fit %>%
  filter(Country=="Australia")  %>%
  gg_tsresiduals()

fc <- fit %>%
  forecast(h=5)

fc %>%
  filter(Country=="Australia") %>%
  autoplot(global_economy)

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
    auto = ARIMA(Employed),
    best = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit
fit %>% pivot_longer(everything(),
  names_to = "Model name",
  values_to = "Orders"
)

glance(fit) %>%
  arrange(AICc) %>%
  select(.model:BIC)

fit %>%
  select(best) %>%
  gg_tsresiduals(lag = 36)

report(fit %>% select(best))
augment(fit) %>%
  filter(.model == "best") %>%
  features(.innov, ljung_box, lag = 24, dof = 4)

forecast(fit, h = 36) %>%
  filter(.model == "best") %>%
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
  model(arima3000210 = ARIMA(log(Cost) ~ pdq(3, 0, 0) + PDQ(2, 1, 0)))
report(fit)
gg_tsresiduals(fit, lag_max = 36)
augment(fit) %>%
  features(.innov, ljung_box, lag = 36, dof = 6)

# Letting R choose
fit <- h02 %>% model(auto = ARIMA(log(Cost), stepwise = FALSE))
report(fit)
gg_tsresiduals(fit, lag_max = 36)
augment(fit) %>%
  features(.innov, ljung_box, lag = 36, dof = 6)

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
  features(.innov, ljung_box, lag = 36, dof = 9)

# The forecasts
fit %>%
  forecast() %>%
  autoplot(h02) +
  labs(y = "H02 Expenditure ($AUD)")

## AUS ECONOMY ETS vs ARIMA

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Population = Population / 1e6)
aus_economy %>% autoplot(Population)
aus_economy %>%
  model(
    ets = ETS(Population),
    arima = ARIMA(Population)
  ) %>%
  glance()
aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ets = ETS(Population),
    arima = ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy) %>%
  select(.model, ME:RMSSE)

aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy %>% filter(Year > 2000)) +
  labs(
    title = "Australian population",
    y = "People (millions)"
  )

# QUARTERLY CEMENT ETS vs ARIMA

cement <- aus_production %>%
  select(Cement) %>%
  filter_index("1988 Q1" ~ .)
cement %>% autoplot(Cement)
train <- cement %>% filter_index(. ~ "2007 Q4")
fit <- train %>%
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement)
  )

fit %>%
  select(arima) %>%
  report()
gg_tsresiduals(fit %>% select(arima), lag_max = 16)

fit %>%
  select(arima) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

fit %>%
  select(ets) %>%
  report()

gg_tsresiduals(fit %>% select(ets), lag_max = 16)

fit %>%
  select(ets) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

fit %>%
  forecast(h = "2 years 6 months") %>%
  accuracy(cement, level = 80)

fit %>%
  select(arima) %>%
  forecast(h = "3 years") %>%
  autoplot(cement) +
  labs(
    title = "Cement production in Australia",
    y = "Tonnes ('000)"
  )
