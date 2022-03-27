library(fpp3)

## US RETAIL EMPLOYMENT

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>%
  autoplot(Employed)

dcmp <- us_retail_employment %>%
  model(STL(Employed)) %>%
  components()

autoplot(dcmp)

dcmp <- dcmp %>% select(-.model)
dcmp %>% autoplot(season_adjust)

dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(title = "Naive forecasts of seasonally adjusted data")

dcmp %>% autoplot(season_year)

dcmp %>%
  model(SNAIVE(season_year)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(title = "Seasonal naive forecasts of seasonal component")

us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed),
    NAIVE(season_adjust)
  )) %>%
  forecast() %>%
  autoplot(us_retail_employment)

## BEER PRODUCTION

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
recent_production %>% autoplot(Beer)
train <- recent_production %>%
  filter(year(Quarter) <= 2007)
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    Naive = NAIVE(Beer),
    Seasonal_naive = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )
beer_fc <- beer_fit %>%
  forecast(h = 10)

accuracy(beer_fc, recent_production)
accuracy(beer_fit)

## CROSS-VALIDATION: FACEBOOK

fb_stock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

fb_stock %>%
  autoplot(Close)

fb_stretch <- fb_stock %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  filter(.id != max(.id))

fit_cv <- fb_stretch %>%
  model(RW(Close ~ drift()))

fc_cv <- fit_cv %>%
  forecast(h = 1)

fc_cv %>% accuracy(fb_stock)

fb_stock %>%
  model(RW(Close ~ drift())) %>%
  accuracy()
