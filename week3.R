library(fpp3)

# Find a transformation for the following

global_economy |>
  filter(Country == "United States") |>
  autoplot(GDP)

aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  autoplot(Count)

vic_elec |>
  autoplot(Demand)

aus_production |>
  autoplot(Gas)

canadian_gas |>
  autoplot(Volume)


## US retail employment ----------------------------------------------------------

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

us_retail_employment |>
  autoplot(Employed) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))

dcmp |>
  components() |>
  autoplot()

components(dcmp) |> gg_subseries(season_year)

us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), trend, color = "red") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), season_adjust, color = "blue") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

us_retail_employment |>
  model(STL(Employed ~ season(window = 13) + trend(window = 7), robust = TRUE)) |>
  components() |>
  autoplot() +
  labs(title = "STL decomposition: US retail employment")
