library(fpp3)
library(lubridate)


## US change ------------------------------------------------------------------

us_change %>%
  gather("Measure", "Change", Consumption, Income) %>%
  autoplot(Change) +
  ylab("% change") + xlab("Year")

us_change %>%
  ggplot(aes(x=Income, y=Consumption)) +
    ylab("Consumption (quarterly % change)") +
    xlab("Income (quarterly % change)") +
    geom_point() + geom_smooth(method="lm", se=FALSE)

fit_cons <- us_change %>%
  model(lm = TSLM(Consumption ~ Income))
report(fit_cons)

us_change %>%
  gather("Measure", "Change", Consumption, Income, Production, Savings, Unemployment) %>%
  ggplot(aes(x = Quarter, y = Change, colour = Measure)) +
  geom_line() +
  facet_grid(vars(Measure), scales = "free_y") +
  ylab("") + xlab("Year") +
  guides(colour="none")

us_change %>%
  as_tibble() %>%
  select(Income, Production, Savings, Unemployment, Consumption) %>%
  GGally::ggpairs()

fit_consMR <- us_change %>%
  model(lm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))
report(fit_consMR)

augment(fit_consMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  xlab("Year") + ylab("") +
  ggtitle("Percentage change in US consumption expenditure") +
  guides(colour=guide_legend(title=""))

augment(fit_consMR) %>%
  ggplot(aes(x=.fitted, y=Consumption)) +
    geom_point() +
    xlab("Fitted (predicted values)") +
    ylab("Data (actual values)") +
    ggtitle("Percentage change in US consumption expenditure") +
    geom_abline(intercept=0, slope=1)

gg_tsresiduals(fit_consMR)

fit_consBest <- us_change %>%
  model(
    TSLM(Consumption ~ Income + Savings + Unemployment)
  )

down_future <- new_data(us_change, 4) %>%
  mutate(Income = -1, Savings = -0.5, Unemployment = 0)
fc_down <- forecast(fit_consBest, new_data = down_future)

up_future <- new_data(us_change, 4) %>%
  mutate(Income = c(1,2,3,3), Savings = 0.5, Unemployment = 0)
fc_up <- forecast(fit_consBest, new_data = up_future)

us_change %>% autoplot(Consumption) +
  ylab("% change in US consumption") +
  autolayer(fc_up) +
  autolayer(fc_down) +
  guides(colour = guide_legend(title = "Scenario"))


## BEER ---------------------------------------------------------------------

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
recent_production %>% autoplot(Beer) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Australian quarterly beer production")

fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")

augment(fit_beer) %>%
  ggplot(aes(x=Beer, y=.fitted, colour=factor(quarter(Quarter)))) +
    geom_point() +
    ylab("Fitted") + xlab("Actual values") +
    ggtitle("Quarterly beer production") +
    scale_colour_brewer(palette="Dark2", name="Quarter") +
    geom_abline(intercept=0, slope=1)

gg_tsresiduals(fit_beer)

fit_beer %>%
  forecast(h="3 years") %>%
  autoplot(recent_production)

fourier_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + fourier(K=2)))
glance(fourier_beer)
report(fourier_beer)

recent_production <- aus_production %>% filter(year(Quarter) >= 1992)
fit_beer <- recent_production %>%
  model(
    season = TSLM(Beer ~ trend() + season()),
    fourier = TSLM(Beer ~ trend() + fourier(K=2))
  )
fc_beer <- forecast(fit_beer)
fc_beer %>% autoplot(recent_production) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")


## Boston Marathon -------------------------------------------------------------

marathon <- boston_marathon %>%
  filter(Event == "Men's open division") %>%
  select(-Event) %>%
  mutate(Minutes = as.numeric(Time)/60)
marathon %>% autoplot(Minutes) +
  xlab("Year") +  ylab("Winning times in minutes")


fit_trends <- marathon %>%
  model(
    # Linear trend
    linear = TSLM(Minutes ~ trend()),
    # Exponential trend
    exponential = TSLM(log(Minutes) ~ trend()),
    # Piecewise linear trend
    piecewise = TSLM(Minutes ~ trend(knots = c(1940, 1980))),
    better = TSLM(Minutes ~ trend(knots = c(1919,1930,1940,1980)))
  )
glance(fit_trends) %>%
  select(.model, r_squared, adj_r_squared, AICc, BIC, CV, log_lik)


fit_trends %>%
  forecast(h=10) %>%
  autoplot(marathon, alpha = 0.5) +
  geom_line(aes(y = .fitted, colour = .model), data = augment(fit_trends)) +
  labs(x = "Year", y = "Winning times in minutes", title = "Boston Marathon") +
  guides(colour=guide_legend(title=NULL))

fit_trends %>%
  select(piecewise) %>%
  gg_tsresiduals()

glance(fit_trends) %>%
  select(.model, r_squared, adj_r_squared, AICc, BIC, CV, log_lik)


