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

aus_economy <- filter(global_economy, Code == "AUS")

print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adj_turnover = Turnover / CPI) %>%
  pivot_longer(c(Turnover, Adj_turnover),
    names_to = "Type", values_to = "Turnover"
  ) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  xlab("Years") + ylab(NULL) +
  ggtitle("Turnover: Australian print media industry")


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
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp)

us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), trend, color='red') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

components(dcmp) %>% autoplot() + xlab("Year")

components(dcmp) %>% gg_subseries(season_year)

us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), season_adjust, color='blue') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

us_retail_employment %>%
  model(STL(Employed ~ season(window=9) + trend(window=15), robust=TRUE)) %>%
  components() %>% 
  autoplot() +
    ggtitle("STL decomposition: US retail employment")

