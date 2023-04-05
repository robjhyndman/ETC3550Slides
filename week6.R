library(fpp3)

## Example: Australian holiday tourism by Region

holidays <- tourism |>
  filter(Purpose == "Holiday")
fit <- holidays |>
  model(ets = ETS(Trips))
fit |>
  filter(Region == "Snowy Mountains") |>
  report()
fit |>
  filter(Region == "Snowy Mountains") |>
  components(fit) |>
  autoplot()
fit |>
  filter(Region == "Snowy Mountains") |>
  forecast() |>
  autoplot(holidays, show_gap = FALSE) +
  xlab("Year") + ylab("Overnight trips (thousands)")

# Sum over regions

aus_holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips))
aus_holidays |> autoplot()

fit <- aus_holidays |>
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M")),
    auto = ETS(Trips)
  )

fit |>
  select(multiplicative) |>
  report()

components(fit) |> autoplot()

fit |>
  select(multiplicative) |>
  components() |>
  autoplot()

fit |> augment()

residuals(fit)
residuals(fit, type = "innov")

fit |>
  select(multiplicative) |>
  gg_tsresiduals()

fc <- fit |> forecast()

fc |>
  autoplot(aus_holidays) + xlab("Year") +
  ylab("Overnight trips (thousands)")


## H02

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost))
h02 |>
  autoplot(Cost)

h02 |>
  model(ETS(Cost)) |>
  report()

h02 |>
  model(ETS(Cost ~ error("A") + trend("A") + season("A"))) |>
  report()

h02 |>
  model(ETS(Cost)) |>
  forecast() |>
  autoplot(h02)

fit <- h02 |>
  model(
    auto = ETS(Cost),
    AAA = ETS(Cost ~ error("A") + trend("A") + season("A")),
    damped = ETS(Cost ~ trend("Ad")),
    forbidden = ETS(Cost ~ error("A") + trend("Ad") + season("M"))
  )

fit |> glance()
fit |> accuracy()
fit |> tidy()

# Example of STL + ETS

stl_fit <- h02 |>
  model(
    decomposition_model(
      STL(Cost),
      ETS(season_adjust),
      SNAIVE(season_year)
    )
  )
stl_fit |> report()

stl_fit |>
  forecast(h = 24) |>
  autoplot(h02)
