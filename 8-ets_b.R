library(fpp3)

## National populations

fit <- global_economy %>%
  mutate(Pop = Population / 1e6) %>%
  model(ets = ETS(Pop))
fit %>% forecast(h = 5)

## Example: Australian holiday tourism

holidays <- tourism %>%
  filter(Purpose == "Holiday")
fit <- holidays %>%
  model(ets = ETS(Trips))
fit %>%
  filter(Region == "Snowy Mountains") %>%
  report()
fit %>%
  filter(Region == "Snowy Mountains") %>%
  components(fit) %>%
  autoplot()
fit %>%
  filter(Region == "Snowy Mountains") %>%
  forecast() %>%
  autoplot(holidays, show_gap = FALSE) +
  xlab("Year") + ylab("Overnight trips (thousands)")

# Sum over regions

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))
aus_holidays %>% autoplot()

fit <- aus_holidays %>% model(ETS(Trips))
report(fit)

components(fit) %>%
  autoplot() +
  ggtitle("ETS(M,N,M) components")

fit %>% augment()

residuals(fit)
residuals(fit, type = "response")

fit %>%
  gg_tsresiduals()

## H02

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))
h02 %>%
  autoplot(Cost)

h02 %>%
  model(ETS(Cost)) %>%
  report()

h02 %>%
  model(ETS(Cost ~ error("A") + trend("A") + season("A"))) %>%
  report()

h02 %>%
  model(ETS(Cost)) %>%
  forecast() %>%
  autoplot(h02)

fit <- h02 %>%
  model(
    auto = ETS(Cost),
    AAA = ETS(Cost ~ error("A") + trend("A") + season("A")),
    damped = ETS(Cost ~ trend("Ad")),
    forbidden = ETS(Cost ~ error("A") + trend("Ad") + season("M"))
  )

fit %>% accuracy()
fit %>% glance()
fit %>% tidy()

# Example of STL + ETS

h02 %>%
  model(
    decomposition_model(
      STL(Cost),
      ETS(season_adjust),
      SNAIVE(season_year)
    )
  ) %>%
  forecast(h = 24) %>%
  autoplot(h02)
