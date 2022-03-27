library(fpp3)

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
