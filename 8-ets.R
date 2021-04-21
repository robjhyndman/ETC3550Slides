library(fpp3)

# Algerian Exports

algeria_economy <- global_economy %>%
  filter(Country == "Algeria")
algeria_economy %>% autoplot(Exports)
fit <- algeria_economy %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    autoNN = ETS(Exports ~ trend("N") + season("N")),
  )
fit %>%
  select(ANN) %>%
  report()
fit %>%
  select(MNN) %>%
  report()
fit %>%
  select(autoNN) %>%
  report()

tidy(fit)
glance(fit)


components(fit) %>% autoplot()

components(fit) %>%
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit %>%
  forecast(h = 5) %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")


# Australian population

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population/1e6)
aus_economy %>% autoplot(Pop)
aus_economy %>%
  model(auto = ETS(Pop)) %>%
  report()

fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("M") + trend("A") + season("N")))
report(fit)

components(fit) %>% autoplot()

components(fit) %>%
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit %>%
  forecast(h = 10) %>%
  autoplot(aus_economy) +
  ylab("Population") + xlab("Year")

aus_economy %>%
  model(holt = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) %>%
  report()

aus_economy %>%
  model(holt = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 10) %>%
  autoplot(aus_economy)
aus_economy %>%
  filter(Year <= 2010) %>%
  autoplot(Pop)
fit <- aus_economy %>%
  filter(Year <= 2010) %>%
  model(
    ses = ETS(Pop ~ error("A") + trend("N") + season("N")),
    holt = ETS(Pop ~ error("A") + trend("A") + season("N")),
    damped = ETS(Pop ~ error("A") + trend("Ad") + season("N"))
  )

tidy(fit)
glance(fit)
accuracy(fit)
forecast(fit) %>% accuracy(aus_economy)

fit <- global_economy %>%
  model(
    ets = ETS(Population)
  )
fc <- fit %>%
  forecast(h=10)


# egg prices

egg_prices <- prices %>%
  filter(!is.na(eggs))
egg_prices %>%
  autoplot(eggs)
fit <- egg_prices %>%
  model(
    ses = ETS(log(eggs) ~ trend("N")),
    holt = ETS(log(eggs) ~ trend("A")),
    damped = ETS(log(eggs) ~ trend("Ad"))
  )
fit %>%
  forecast(h=100) %>%
  autoplot(egg_prices, level=NULL)

fit %>% glance()

fit %>%
  select(holt) %>%
  report()

fit %>%
  select(holt) %>%
  gg_tsresiduals()

fit %>%
  augment() %>%
  filter(.model=="holt") %>%
  features(.resid, ljung_box, dof=4, lag=10)


# J07

j07 <- PBS %>%
  filter(ATC2 == "J07") %>%
  summarise(Cost = sum(Cost))
j07 %>% autoplot(Cost)

j07 %>%
  model(ETS(Cost ~ error("A") + trend("N") + season("A"))) %>%
  forecast(h=36) %>%
  autoplot(j07, level=NULL)

j07 %>%
  model(ETS(Cost ~ error("M") + trend("N") + season("M"))) %>%
  forecast(h=36) %>%
  autoplot(j07, level=NULL)

## Aus holidays

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))
aus_holidays %>% autoplot(Trips)
fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
  )
fit %>%
  select(multiplicative) %>%
  report()
fc <- fit %>% forecast()

fc %>%
  autoplot(aus_holidays) + xlab("Year") +
  ylab("Overnight trips (thousands)")

components(fit) %>% autoplot()

fit %>% select(multiplicative) %>% components() %>% autoplot()


# Gas production

aus_production %>% autoplot(Gas)
fit <- aus_production %>%
  model(
    hw = ETS(Gas ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
  )

fit %>% tidy()
fit %>% glance()

fit %>%
  select(hw) %>%
  gg_tsresiduals()

fit %>%
  select(hw) %>%
  report()

fit %>%
  augment() %>%
  filter(.model=="hw") %>%
  features(.resid, ljung_box, dof=8, lag=16)

fit %>%
  forecast(h=36) %>%
  filter(.model == "hw") %>%
  autoplot(aus_production)


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

h02 %>% model(ETS(Cost)) %>% report()

h02 %>% model(ETS(Cost ~ error("A") + trend("A") + season("A"))) %>%
  report()

h02 %>% model(ETS(Cost)) %>% forecast() %>% autoplot(h02)

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

