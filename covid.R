library(tidyverse)
library(fpp3)

US <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  filter(date >= "2020-02-26") %>%
  mutate(time = row_number()) %>%
  as_tsibble(index=date)

autoplot(US, new_deaths)

poly_fc <- US %>%
  model(
    linear = TSLM(log(new_deaths + 1) ~ trend()),
    quadratic = TSLM(log(new_deaths + 1) ~ trend() + I(trend()^2)),
    cubic = TSLM(log(new_deaths + 1) ~ trend() + I(trend()^2) +
                   I(trend()^3)),
    quartic = TSLM(log(new_deaths + 1) ~ trend() + I(trend()^2) +
                   I(trend()^3) + I(trend()^4))
  ) %>%
  forecast(h = "20 days")

p <- poly_fc %>%
  autoplot(US, level=NULL) +
  labs(title = "How a polynomial model's predictions change based on # of degrees",
       subtitle = "Fit to log(deaths + 1). Source of death counts: NY Times",
       y = "Daily US deaths",
       x = "")
p
p + scale_y_log10()

