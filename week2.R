library(fpp3)

## LOTS OF EXAMPLES -------------------------------------------------------------

aus_production |>
  filter(year(Quarter) >= 1980) |>
  autoplot(Electricity) +
  labs(
    y = "GWh",
    title = "Australian electricity production"
  )

aus_production |>
  autoplot(Tobacco) +
  labs(
    title = "Australian clay brick production",
    y = "million units"
  )

us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980) |>
  autoplot(Employed / 1e3) +
  labs(
    title = "Retail employment, USA",
    y = "Million people"
  )

gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  autoplot(Close) +
  labs(
    title = "Amazon closing stock price",
    y = "$US"
  )



## Snowy mountains tourism -------------------------------------------------------------------------

snowy <- tourism |>
  filter(Region == "Snowy Mountains") |>
  summarise(Trips = sum(Trips))
snowy |> autoplot(Trips)

snowy |>
  gg_season(Trips, labels = "right")
snowy |> gg_subseries(Trips)

snowy |> gg_lag(Trips)
snowy |> gg_lag(Trips, geom = "point", lags = 1:16)
snowy |> ACF(Trips, lag_max = 16)
snowy |>
  ACF(Trips, lag_max = 20) |>
  autoplot()
snowy |>
  ACF(Trips) |>
  autoplot()


## RETAIL TRADE ------------------------------------------------------------------

retail <- us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980)
retail |> autoplot(Employed)

retail |>
  ACF(Employed, lag_max = 48) |>
  autoplot()

# Pelts

pelt |>
  autoplot(Lynx) +
  labs(
    title = "Annual Canadian Lynx Trappings",
    y = "Number trapped"
  )

pelt |>
  ACF(Lynx, lag_max = 25) |>
  autoplot()


## WHITE NOISE --------------------------------------------------------------------

set.seed(30)
wn <- tsibble(t = seq(50), y = rnorm(50), index = t)
wn |> autoplot(y)

wn |> ACF(y, lag_max = 10)

wn |> ACF(y) |> autoplot()

## PIGS ---------------------------------------------------------------------------

pigs <- aus_livestock |>
  filter(
    State == "Victoria", Animal == "Pigs",
    year(Month) >= 2014
  )
pigs |> autoplot(Count / 1e3) +
  labs(
    y = "Thousands",
    title = "Number of pigs slaughtered in Victoria"
  )
pigs |>
  ACF(Count, lag_max = 36) |>
  autoplot()


## Google 2015 -------------------------------------------------------------------

google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015) |>
  select(Date, Close)

google_2015 |> autoplot(Close)

google_2015 |>
  ACF(Close, lag_max = 100) |>
  autoplot()

google_2015 |>
  mutate(diff = difference(Close)) |>
  autoplot(diff)

google_2015 |>
  mutate(diff = difference(Close)) |>
  ACF(diff, lag_max = 100) |>
  autoplot()
