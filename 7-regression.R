options(
  digits = 3,
  width = 60,
  ggplot2.continuous.colour="viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)
library(fpp3)

# US consumption quarterly changes

us_change %>%
  pivot_longer(c(Consumption, Income), names_to="Series") %>%
  autoplot(value) +
  labs(y="% change")

us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
    labs(y = "Consumption (quarterly % change)",
         x = "Income (quarterly % change)") +
    geom_point() + geom_smooth(method = "lm", se = FALSE)

fit_cons <- us_change %>%
  model(lm = TSLM(Consumption ~ Income))
report(fit_cons)

us_change %>%
  pivot_longer(-Quarter, names_to="Measure", values_to="Change") %>%
  ggplot(aes(x = Quarter, y = Change, colour = Measure)) +
  geom_line() +
  facet_grid(vars(Measure), scales = "free_y") +
  labs(y="") +
  guides(colour="none")

us_change %>%
  GGally::ggpairs(columns = 2:6)

fit_consMR <- us_change %>%
  model(lm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))
report(fit_consMR)

augment(fit_consMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
    title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consMR) %>%
  ggplot(aes(x=.fitted, y=Consumption)) +
    geom_point() +
    labs(y="Fitted (predicted values)",
         x="Data (actual values)",
         title ="Percentage change in US consumption expenditure") +
    geom_abline(intercept=0, slope=1)

fit_consMR %>% gg_tsresiduals()


# Australian beer production

recent_production <- aus_production %>% filter(year(Quarter) >= 1992)
recent_production %>% autoplot(Beer) +
  labs(y="Megalitres",title ="Australian quarterly beer production")

fit_beer <- recent_production %>% model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y="Megalitres",title ="Australian quarterly beer production") +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00"))

augment(fit_beer) %>%
  ggplot(aes(x=Beer, y=.fitted, colour=factor(quarter(Quarter)))) +
    geom_point() +
    labs(y="Fitted", x="Actual values",
         title = "Quarterly beer production") +
    scale_colour_brewer(palette="Dark2", name="Quarter") +
    geom_abline(intercept=0, slope=1)

fit_beer %>% gg_tsresiduals()

fit_beer %>% forecast %>% autoplot(recent_production)

fourier_beer <- recent_production %>% model(TSLM(Beer ~ trend() + fourier(K=2)))
report(fourier_beer)

## Boston Marathon

marathon <- boston_marathon %>%
  filter(Event == "Men's open division") %>%
  select(-Event) %>%
  mutate(Minutes = as.numeric(Time)/60)
marathon %>% autoplot(Minutes) +
  labs(y="Winning times in minutes")

fit_trends <- marathon %>%
  model(
    # Linear trend
    linear = TSLM(Minutes ~ trend()),
    # Exponential trend
    exponential = TSLM(log(Minutes) ~ trend()),
    # Piecewise linear trend
    piecewise = TSLM(Minutes ~ trend(knots = c(1940, 1980)))
  )

fit_trends

fc_trends <- fit_trends %>% forecast(h = 10)
marathon %>%
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes",
       title = "Boston marathon winning times")

fit_trends %>% 
  select(piecewise) %>%
  gg_tsresiduals()

glance(fit_trends) %>%
  select(.model, r_squared, adj_r_squared, AICc, CV)

# US consumption quarterly changes

fit_consBest <- us_change %>%
  model(
    TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change %>% autoplot(Consumption) +
  labs(y="% change in US consumption") +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

