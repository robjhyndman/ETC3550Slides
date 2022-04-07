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
  filter(.model == "MNN") %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")

# Australian population

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)
aus_economy %>% autoplot(Pop)
aus_economy %>%
  model(auto = ETS(Pop)) %>%
  report()

fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))
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
accuracy(fit)
glance(fit)
forecast(fit) %>% accuracy(aus_economy)

fit <- global_economy %>%
  model(
    ets = ETS(Population)
  )
fc <- fit %>%
  forecast(h = 10)

## Aus holidays

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))
aus_holidays %>% autoplot(Trips)
fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M")),
    auto = ETS(Trips)
  )
fit %>%
  select(multiplicative) %>%
  report()
fc <- fit %>% forecast()

fc %>%
  autoplot(aus_holidays) + xlab("Year") +
  ylab("Overnight trips (thousands)")

components(fit) %>% autoplot()

fit %>%
  select(multiplicative) %>%
  components() %>%
  autoplot()

# Daily pedestrian data

sth_cross_ped <- pedestrian %>%
  filter(
    Date >= "2016-07-01",
    Sensor == "Southern Cross Station"
  ) %>%
  index_by(Date) %>%
  summarise(Count = sum(Count) / 1000)
sth_cross_ped %>%
  filter(Date <= "2016-07-31") %>%
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped %>% filter(Date <= "2016-08-14")) +
  labs(
    title = "Daily traffic: Southern Cross",
    y = "Pedestrians ('000)"
  )
