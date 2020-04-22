library(fpp3)

## ---- GDP ----------------------------------------------------------------

global_economy %>%
  filter(Country=="Sweden") %>%
  autoplot(GDP) +
    ggtitle("GDP for Sweden") + ylab("$US billions")

fit <- global_economy %>%
  model(trend_model = TSLM(GDP ~ trend()))

fit %>% forecast(h = "3 years") %>%
  filter(Country=="Sweden") %>%
  autoplot(global_economy) +
    ggtitle("GDP for Sweden") + ylab("$US billions")

## ---- Bricks ------------------------------------------------------------

brick_fit <-  aus_production %>%
  filter(!is.na(Bricks)) %>%
  model(
    Seasonal_naive = SNAIVE(Bricks),
    Naive = NAIVE(Bricks),
    Drift = RW(Bricks ~ drift()),
    Mean = MEAN(Bricks)
  )

brick_fc <- brick_fit %>%
  forecast(h = "5 years")

brick_fc %>%
  autoplot(aus_production, level = NULL) +
  ggtitle("Forecasts for quarterly clay brick production") +
  xlab("Year") + ylab("Millions of bricks") +
  guides(colour = guide_legend(title = "Forecast"))


## ---- Facebook -------------------------------------------------------------------

# Extract training data
fb_stock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index=trading_day, regular=TRUE)

fb_stock %>% autoplot(Close)

# Specify, estimate and forecast
fb_stock %>%
  model(
    Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift())
  ) %>%
  forecast(h=42) %>%
  autoplot(fb_stock, level = NULL) +
  ggtitle("Facebook closing stock price") +
  xlab("Day") + ylab("") +
  guides(colour=guide_legend(title="Forecast"))

fit <- fb_stock %>% model(NAIVE(Close))

augment(fit) %>%
  filter(trading_day > 1200) %>%
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = Close, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit) %>%
  autoplot(.resid) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

augment(fit) %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 150) +
  ggtitle("Histogram of residuals")

augment(fit) %>%
  ACF(.resid) %>%
  autoplot() + ggtitle("ACF of residuals")

gg_tsresiduals(fit)


## -- Finished to here on 9 April

augment(fit) %>%
  features(.resid, ljung_box, lag=10, dof=0)


## BEER -------------------------------------------

recent <- aus_production %>% filter(year(Quarter) >= 1992)
recent %>% autoplot(Beer)
fit <- recent %>% model(SNAIVE(Beer))
fit %>% forecast() %>% autoplot(recent)

augment(fit) %>%
  features(.resid, ljung_box, lag=10, dof=0)

gg_tsresiduals(fit)

fit %>% forecast %>% hilo(level = c(50,95))

## FOOD RETAILING

food <- aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover))

fit <- food %>%
  model(SNAIVE(log(Turnover)))

fc <- fit %>%
  forecast(h = "3 years")

fc %>% autoplot(food)

fc %>% autoplot(filter(food, year(Month) > 2010))


## EGG PRICES

eggs <- as_tsibble(fma::eggs)
autoplot(eggs)
fit <- eggs %>% model(RW(log(value) ~ drift()))
fc <- fit %>% forecast(h=50)
fc_biased <- fit %>% forecast(h=50, bias_adjust = FALSE)
eggs %>% autoplot(value) + xlab("Year") +
  autolayer(fc_biased, level = 80) +
  autolayer(fc, colour = "red", level = NULL)


## US RETAIL EMPLOYMENT

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

autoplot(us_retail_employment, Employed)

dcmp <- us_retail_employment %>%
  model(STL(Employed)) %>%
  components() %>%
  select(-.model)

dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  ggtitle("Naive forecasts of seasonally adjusted data")

us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
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
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )
beer_fc <- beer_fit %>%
  forecast(h = 24)

accuracy(beer_fc, recent_production)
accuracy(beer_fit)

## CROSS-VALIDATION: FACEBOOK

fb_stock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index=trading_day, regular=TRUE)

fb_stock %>% autoplot(Close)

fb_stretch <- fb_stock %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  filter(.id != max(.id))

fit_cv <- fb_stretch %>%
  model(RW(Close ~ drift()))

fc_cv <- fit_cv %>%
  forecast(h=1)

fc_cv %>% accuracy(fb_stock)

fb_stock %>%
  model(RW(Close ~ drift())) %>%
  accuracy()

