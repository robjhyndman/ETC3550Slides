library(fpp3)

#### GOOGLE STOCK PRICE 2018 ----------------

google_2018 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2018)

google_2018 %>% autoplot(Close)
google_2018 %>%
  ACF(Close) %>%
  autoplot()

google_2018 %>% autoplot(difference(Close)) +
  ylab("Google closing stock price") + xlab("Day")

google_2018 %>%
  ACF(difference(Close)) %>%
  autoplot()

google_2018 %>%
  features(Close, unitroot_kpss)
google_2018 %>%
  features(difference(Close), unitroot_kpss)
google_2018 %>%
  features(Close, unitroot_ndiffs)

## A10 drugs

a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost) / 1e6)

a10 %>% autoplot(Cost)

a10 %>% autoplot(log(Cost))

a10 %>% autoplot(
  log(Cost) %>% difference(lag = 12)
)

## H02 drugs

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost) / 1e6)

h02 %>% autoplot(Cost)

h02 %>% autoplot(log(Cost))

h02 %>% autoplot(
  log(Cost) %>% difference(12)
)

h02 %>% autoplot(
  log(Cost) %>% difference(12) %>% difference(1)
)

h02 %>%
  mutate(log_sales = log(Cost)) %>%
  features(log_sales, feat_stl)

h02 %>%
  mutate(log_sales = log(Cost)) %>%
  features(log_sales, unitroot_nsdiffs)
h02 %>%
  mutate(d_log_sales = difference(log(Cost), 12)) %>%
  features(d_log_sales, unitroot_ndiffs)
