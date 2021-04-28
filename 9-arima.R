library(fpp3)

#### GOOGLE STOCK PRICE 2018 ----------------

google_2018 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

google_2018 %>% autoplot(Close)
google_2018 %>% ACF(Close) %>% autoplot()

google_2018 %>% autoplot(difference(Close)) +
   ylab("Google closing stock price") + xlab("Day")

google_2018 %>% ACF(difference(Close)) %>% autoplot()

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
wwwusage %>% autoplot(difference(value, differences=2))
wwwusage %>% features(value, unitroot_kpss)
wwwusage %>% features(difference(value), unitroot_pp)

wwwusage %>% features(difference(value), unitroot_ndiffs)

## A10 drugs

a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost)/1e6)

a10 %>% autoplot(Cost)

a10 %>% autoplot(log(Cost))

a10 %>% autoplot(
  log(Cost) %>% difference(12)
)

## H02 drugs

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>% autoplot(Cost)

h02 %>% autoplot(log(Cost))

h02 %>% autoplot(
  log(Cost) %>% difference(12)
)

h02 %>% autoplot(
  log(Cost) %>% difference(12) %>% difference(1)
)

h02 %>% mutate(log_sales = log(Cost)) %>%
  features(log_sales, list(unitroot_nsdiffs, feat_stl))

h02 %>% mutate(log_sales = log(Cost)) %>%
  features(log_sales, unitroot_nsdiffs)
h02 %>% mutate(d_log_sales = difference(log(Cost), 12)) %>%
  features(d_log_sales, unitroot_ndiffs)


## Australian tourism --------------------------

total_trips <- tourism %>%
  summarise(Trips = sum(Trips))

total_trips %>% autoplot(Trips)

total_trips %>% features(Trips, unitroot_nsdiffs)

total_trips %>% features(difference(Trips,4), unitroot_ndiffs)

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
  forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

global_economy %>% filter(Code == "EGY") %>% ACF(Exports) %>% autoplot()
global_economy %>% filter(Code == "EGY") %>% PACF(Exports) %>% autoplot()

global_economy %>% filter(Code == "EGY") %>%
  gg_tsdisplay(Exports, plot_type='partial')

fit1 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit1)

fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit2)

## CAF EXPORTS

global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")

global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type='partial')

caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))

caf_fit

caf_fit %>% pivot_longer(!Country, names_to = "Model name",
                         values_to = "Orders")

glance(caf_fit) %>% arrange(AICc) %>% select(.model:BIC)

caf_fit %>%
  select(search) %>%
  gg_tsresiduals()

augment(caf_fit) %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

caf_fit %>%
  forecast(h=5) %>%
  filter(.model=='search') %>%
  autoplot(global_economy)


## MINKS --------------------------------------------------------------------------

mink <- as_tsibble(fma::mink)
mink %>% autoplot(value) +
  labs(y="Minks trapped (thousands)",
       title = "Annual number of minks trapped")

mink %>% ACF(value) %>% autoplot()
mink %>% PACF(value) %>% autoplot()

fit <- mink %>%
  model(
    ar4 = ARIMA(value ~ pdq(4,0,0)),
    auto = ARIMA(value),
    best = ARIMA(value, stepwise=FALSE, approximation=FALSE)
  )

glance(fit)

fit %>% select(best) %>% report()

fit %>% select(best) %>% gg_tsresiduals()

fit %>% select(best) %>% forecast(h=20) %>% autoplot(mink)

## WWW usage -----------------------------------------------------------------------

web_usage <- as_tsibble(WWWusage)
web_usage %>% gg_tsdisplay(value, plot_type = 'partial')

web_usage %>% gg_tsdisplay(difference(value), plot_type = 'partial')

fit <- web_usage %>%
  model(arima = ARIMA(value ~ pdq(3, 1, 0))) %>%
  report()

web_usage %>%
  model(auto = ARIMA(value ~ pdq(d=1))) %>%
  report()

web_usage %>%
  model(auto2 = ARIMA(value ~ pdq(d=1),
       stepwise = FALSE, approximation = FALSE)) %>%
  report()

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 3)

fit %>% forecast(h = 10) %>% autoplot(web_usage)


## GDP --------------------------------------------------------------------------

global_economy %>%
  filter(Country=="United States") %>%
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
  features(.resid, ljung_box, dof=2, lag=15)
fit %>%
  filter(Country == "Australia") %>%
  forecast(h=10) %>%
  autoplot(global_economy) +
  scale_y_log10()









## h02 drugs ----------------------------------------------------------------------

h02 <- tsibbledata::PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))

h02 %>% autoplot(Cost)

## Models using logs

h02 %>% autoplot(log(Cost))
h02 %>% gg_tsdisplay(difference(log(Cost),12), lag_max = 36, plot_type='partial')

# My best guess
fit <- h02 %>%
  model(best = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

# Letting R choose
fit <- h02 %>% model(auto = ARIMA(log(Cost), stepwise = FALSE))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

# Letting R work hard to choose
fit <- h02 %>%
  model(best = ARIMA(log(Cost), stepwise = FALSE,
                 approximation = FALSE,
                 order_constraint = p + q + P + Q <= 9))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 9)

# The forecasts
fit %>% forecast %>% autoplot(h02) +
  ylab("H02 Expenditure ($AUD)") + xlab("Year")


## Models without using logs

h02 %>% gg_tsdisplay(difference(Cost,12), lag_max = 36, plot_type='partial')

# My best guess
fit <- h02 %>%
  model(best = ARIMA(Cost ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

# Letting R choose
fit <- h02 %>% model(auto = ARIMA(Cost, stepwise = FALSE))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 7)

# Letting R work hard to choose
fit <- h02 %>%
  model(best = ARIMA(Cost, stepwise = FALSE,
                     approximation = FALSE,
                     order_constraint = p + q + P + Q <= 9))
report(fit)
gg_tsresiduals(fit, lag_max=36)
augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 8)

# The forecasts
fit %>% forecast %>% autoplot(h02) +
  ylab("H02 Expenditure ($AUD)") + xlab("Year")


