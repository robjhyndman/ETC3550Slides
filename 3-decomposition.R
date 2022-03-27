library(fpp3)

## GDP --------------------------------------------------------------------------

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP)

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP / Population)

## Print retail adjusted by CPI --------------------------------------------------

print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))

print_retail %>% autoplot(Turnover)

aus_economy <- global_economy %>%
  filter(Code == "AUS")

print_retail <- print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adj_turnover = Turnover / CPI * 100) %>%
  pivot_longer(c(Turnover, Adj_turnover),
    names_to = "Type", values_to = "Turnover"
  )

# Plot both on same graph
print_retail %>%
  ggplot(aes(x = Year, y = Turnover, col = Type)) +
  geom_line() +
  labs(
    title = "Turnover: Australian print media industry",
    y = "$AU"
  )

# Use faceting
print_retail %>%
  mutate(Type = factor(Type,
    levels = c("Turnover", "Adj_turnover")
  )) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(Type ~ ., scales = "free_y") +
  labs(
    title = "Turnover: Australian print media industry",
    y = "$AU"
  )

## Australian food retail --------------------------------------------------------

food <- aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover))

food %>% autoplot(Turnover) +
  labs(y = "Turnover ($AUD)")

food %>% autoplot(sqrt(Turnover)) +
  labs(y = "Square root turnover")

food %>% autoplot(log(Turnover)) +
  labs(y = "Log turnover")

food %>%
  features(Turnover, features = guerrero)

food %>% autoplot(box_cox(Turnover, 0.0524)) +
  labs(y = "Box-Cox transformed turnover")

## US retail employment ----------------------------------------------------------

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>%
  autoplot(Employed) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp)

us_retail_employment %>%
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), trend, color = "red") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

components(dcmp) %>% autoplot()

components(dcmp) %>% gg_subseries(season_year)

us_retail_employment %>%
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), season_adjust, color = "blue") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

us_retail_employment %>%
  model(STL(Employed ~ season(window = 13) + trend(window = 7), robust = TRUE)) %>%
  components() %>%
  autoplot() +
  labs(title = "STL decomposition: US retail employment")
