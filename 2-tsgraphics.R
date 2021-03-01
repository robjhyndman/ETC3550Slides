library(fpp3)

global_economy

tourism

## PRISON ----------------------------------------------------------------------

prison <- readr::read_csv("data/prison_population.csv") %>%
  mutate(Quarter = yearquarter(date)) %>%
  select(-date) %>%
  as_tsibble(
    index = Quarter,
    key = c(state, gender, legal, indigenous)
  )

## PBS ----------------------------------------------------------------------

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(total_cost = sum(Cost)) %>%
  mutate(total_cost = total_cost / 1e6) -> a10

a10

a10 %>%
  autoplot(total_cost)

a10 %>% gg_season(total_cost, labels = "both") +
  labs(
    y = "$ million",
    title = "Seasonal plot: antidiabetic drug sales"
  )

a10 %>%
  gg_subseries(total_cost) + 
  labs(
    y = "$ million",
    title = "Subseries plot: antidiabetic drug sales"
  )

## ANSETT ----------------------------------------------------------------------

ansett %>%
  autoplot(Passengers)

ansett %>%
  filter(Class == "Economy") %>%
  autoplot(Passengers)

ansett %>%
  filter(Airports == "MEL-SYD") %>%
  autoplot(Passengers)

## MAX TEMP ------------------------------------------------------------

maxtemp <- vic_elec %>%
  index_by(Day = date(Time)) %>%
  summarise(Temperature = max(Temperature))
maxtemp %>%
  autoplot(Temperature) +
  labs(y = "Max temperature")


## LOTS OF EXAMPLES -------------------------------------------------------------

aus_production %>%
  filter(year(Quarter) >= 1980) %>%
  autoplot(Electricity) + 
  labs(
    y = "GWh",
    title = "Australian electricity production"
  )

aus_production %>%
  autoplot(Bricks) +
  labs(
    title = "Australian clay brick production",
    y = "million units"
  )

us_employment %>%
  filter(Title == "Retail Trade", year(Month) >= 1980) %>%
  autoplot(Employed / 1e3) +
  labs(
    title = "Retail employment, USA",
    y = "Million people"
  )

gafa_stock %>%
  filter(Symbol == "AMZN", year(Date) >= 2018) %>%
  autoplot(Close) +
  labs(
    title = "Amazon closing stock price",
    y = "$US"
  )

pelt %>%
  autoplot(Lynx) +
  labs(
    title = "Annual Canadian Lynx Trappings",
    y = "Number trapped"
  )

## BEER -------------------------------------------------------------------------

beer <- aus_production %>%
  select(Quarter, Beer) %>%
  filter(year(Quarter) >= 1992)
beer %>% autoplot(Beer)

beer %>% gg_season(Beer, labels="right")
beer %>% gg_subseries(Beer)

beer %>% gg_lag(Beer)
beer %>% gg_lag(Beer, geom='point')
beer %>% ACF(Beer, lag_max = 9)
beer %>% ACF(Beer, lag_max = 9) %>% autoplot()
beer %>% ACF(Beer) %>% autoplot()

## ELECTRICITY DEMAND ---------------------------------------------------------

vic_elec

vic_elec %>% gg_season(Demand)
vic_elec %>% gg_season(Demand, period = "week")
vic_elec %>% gg_season(Demand, period = "day")

## HOLIDAYS --------------------------------------------------------------------

holidays <- tourism %>%
  mutate(
    State = recode(State,
                   "Australian Capital Territory" = "ACT",
                   "New South Wales" = "NSW",
                   "Northern Territory" = "NT",
                   "Queensland" = "QLD",
                   "South Australia" = "SA",
                   "Tasmania" = "TAS",
                   "Victoria" = "VIC",
                   "Western Australia" = "WA"
    )
  ) %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

holidays

holidays %>% 
  autoplot(Trips) +
  labs(
    y = "thousands of trips",
    title = "Australian domestic holiday nights"
  )

holidays %>% 
  gg_season(Trips) +
  labs(
    y = "thousands of trips",
    title = "Australian domestic holiday nights"
  )

holidays %>% 
  gg_subseries(Trips) +
  labs(
    y = "thousands of trips",
    title = "Australian domestic holiday nights"
  )

## RETAIL TRADE ------------------------------------------------------------------

retail <- us_employment %>%
  filter(Title == "Retail Trade", year(Month) >= 1980)
retail %>% autoplot(Employed)

retail %>%
  ACF(Employed, lag_max = 48) %>%
  autoplot()

## Google 2015 -------------------------------------------------------------------

google_2015 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  select(Date, Close)
google_2015

google_2015 %>% autoplot(Close)

google_2015 <- google_2015 %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)
google_2015

google_2015 %>%
  ACF(Close, lag_max = 100) %>%
  autoplot()

## WHITE NOISE --------------------------------------------------------------------

set.seed(1)
wn <- tsibble(t = seq(36), y = rnorm(36), index = t)
wn %>% autoplot(y)

wn %>% ACF(y, lag_max = 10)

wn %>% ACF(y) %>% autoplot()

## PIGS ---------------------------------------------------------------------------

pigs <- aus_livestock %>%
  filter(State == "Victoria", Animal == "Pigs",
         year(Month) >= 2014)
pigs %>% autoplot(Count/1e3) +
  labs(
    y = "Thousands",
    title = "Number of pigs slaughtered in Victoria"
  )

pigs %>% ACF(Count) %>% autoplot()

## GOOGLE change in closing price ACF ---------------------------------------------

gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index=trading_day, regular=TRUE) %>%
  mutate(diff = difference(Close)) %>%
  ACF(diff) %>%
  autoplot()

