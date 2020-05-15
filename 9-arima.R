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
wwwusage %>% features(difference(value), unitroot_kpss)
wwwusage %>% features(difference(value), unitroot_ndiffs)


####  US Monthly Electricity ---------------------------------------

usmelec <- as_tsibble(fpp2::usmelec) %>%
  rename(Month = index, Generation = value)

usmelec %>% autoplot(Generation)

usmelec %>% autoplot(log(Generation))

usmelec %>% autoplot(
  log(Generation) %>% difference(12)
)

usmelec %>% autoplot(
  log(Generation) %>% difference(12) %>% difference(1)
)

usmelec %>% mutate(log_gen = log(Generation)) %>%
  features(log_gen, list(unitroot_nsdiffs, feat_stl))

usmelec %>% mutate(log_gen = log(Generation)) %>%
  features(log_gen, unitroot_nsdiffs)

usmelec %>% mutate(d_log_gen = difference(log(Generation), 12)) %>%
  features(d_log_gen, unitroot_ndiffs)


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


## US Consumption ------------------------------------------------------------------

us_change %>% autoplot(Consumption) +
  xlab("Year") +
  ylab("Quarterly percentage change") +
  ggtitle("US consumption")

fit <- us_change %>%
  model(arima = ARIMA(Consumption ~ pdq(2,0,1) + PDQ(0,0,0)))
report(fit)

fit %>% forecast(h=100) %>%
  autoplot(us_change)

fit %>% forecast(h=10) %>%
  autoplot(tail(us_change, 80))


## MINKS --------------------------------------------------------------------------

mink <- as_tsibble(fma::mink)
mink %>% autoplot(value) +
  xlab("Year") +
  ylab("Minks trapped (thousands)") +
  ggtitle("Annual number of minks trapped")

mink %>% ACF(value) %>% autoplot()
mink %>% PACF(value) %>% autoplot()
mink %>% gg_tsdisplay(value)


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


## Electrical Equipment --------------------------------------------------------

elecequip <- as_tsibble(fpp2::elecequip)
dcmp <- elecequip %>%
  model(STL(value ~ season(window = "periodic"))) %>%
  components() %>%
  select(-.model)
dcmp %>% as_tsibble %>%
  autoplot(season_adjust) + xlab("Year") +
  ylab("Seasonally adjusted new orders index")

dcmp %>% gg_tsdisplay(difference(season_adjust), plot_type = 'partial')

fit <- dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(d=1, p=0:11) + PDQ(0,0,0),
                  stepwise=FALSE, approximation=FALSE,
                  order_constraint = p + q + P + Q <= 11)
  )
report(fit)

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 24, dof = 4)

fit %>% forecast %>% autoplot(dcmp)

## GDP --------------------------------------------------------------------------

global_economy %>%
  filter(Country=="United States") %>%
  autoplot(log(GDP))

fit <- global_economy %>%
  model(
    ARIMA(log(GDP))
  )

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



### US Monthly Electricity -----------------------------------------------------

usmelec %>% autoplot(
  Generation
)


usmelec %>% autoplot(
  log(Generation)
)

usmelec %>% autoplot(
  log(Generation) %>% difference(12)
)

usmelec %>% autoplot(
  log(Generation) %>% difference(12) %>% difference()
)

usmelec %>% gg_tsdisplay(
  log(Generation) %>% difference(12) %>% difference(),
  plot_type = "partial")

usmelec %>%
  model(arima = ARIMA(log(Generation) ~ pdq(0,1,3) + PDQ(0,1,1))) %>%
  report()

usmelec %>%
  model(arima = ARIMA(log(Generation))) %>%
  report()

fit <- usmelec %>%
  model(arima = ARIMA(log(Generation)))
gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 24, dof = 5)

usmelec %>%
  model(arima = ARIMA(log(Generation))) %>%
  forecast(h = "3 years") %>%
  autoplot(usmelec)

usmelec %>%
  model(arima = ARIMA(log(Generation))) %>%
  forecast(h = "3 years") %>%
  autoplot(filter(usmelec, year(Month) >= 2005))

## h02 drugs ----------------------------------------------------------------------

h02 <- tsibbledata::PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))

h02 %>% autoplot(Cost)
ho2 %>% autoplot(log(Cost))
h02 %>% gg_tsdisplay(difference(log(Cost),12), lag_max = 36)

fit <- h02 %>%
  model(best = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
report(fit)
gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 6)

fit <- h02 %>% model(auto = ARIMA(log(Cost)))
report(fit)

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 5)

fit <- h02 %>%
  model(best = ARIMA(log(Cost), stepwise = FALSE,
                 approximation = FALSE,
                 order_constraint = p + q + P + Q <= 9))
report(fit)

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 36, dof = 9)

fit <- h02 %>%
  model(ARIMA(Cost ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))

fit %>% forecast %>% autoplot(h02) +
  ylab("H02 Expenditure ($AUD)") + xlab("Year")

