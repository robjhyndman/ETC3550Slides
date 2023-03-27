## ----setup, include=FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE, cache = TRUE,
  dev.args = list(pointsize = 11)
)
options(digits = 3, width = 60)
library(fpp3)
library(patchwork)
library(purrr)

## --------------------------------------------------------------------------------------------
gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2018) |>
  autoplot(Close) +
  labs(y = "Google closing stock price", x = "Day")

## --------------------------------------------------------------------------------------------
gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2018) |>
  autoplot(difference(Close)) +
  labs(y = "Google closing stock price", x = "Day")

## --------------------------------------------------------------------------------------------
global_economy |>
  filter(Country == "Algeria") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Algerian Exports")

## --------------------------------------------------------------------------------------------
aus_production |>
  autoplot(Bricks) +
  labs(title = "Clay brick production in Australia")

## --------------------------------------------------------------------------------------------
prices |>
  filter(year >= 1900) |>
  autoplot(eggs) +
  labs(y = "$US (1993)", title = "Price of a dozen eggs")

## --------------------------------------------------------------------------------------------
aus_livestock |>
  filter(
    Animal == "Pigs", State == "Victoria",
  ) |>
  autoplot(Count / 1e3) +
  labs(y = "thousands", title = "Total pigs slaughtered in Victoria")

## --------------------------------------------------------------------------------------------
aus_livestock |>
  filter(
    Animal == "Pigs", State == "Victoria", year(Month) >= 2010
  ) |>
  autoplot(Count / 1e3) +
  labs(y = "thousands", title = "Total pigs slaughtered in Victoria")

## --------------------------------------------------------------------------------------------
aus_livestock |>
  filter(
    Animal == "Pigs", State == "Victoria", year(Month) >= 2015
  ) |>
  autoplot(Count / 1e3) +
  labs(y = "thousands", title = "Total pigs slaughtered in Victoria")

## --------------------------------------------------------------------------------------------
pelt |>
  autoplot(Lynx) +
  labs(
    y = "Number trapped",
    title = "Annual Canadian Lynx Trappings"
  )

## --------------------------------------------------------------------------------------------
google_2018 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE)

## --------------------------------------------------------------------------------------------
google_2018 |>
  autoplot(Close) +
  labs(y = "Closing stock price ($USD)")

## ---- fig.height=3.5-------------------------------------------------------------------------
google_2018 |>
  ACF(Close) |>
  autoplot()

## --------------------------------------------------------------------------------------------
google_2018 |>
  autoplot(difference(Close)) +
  labs(y = "Change in Google closing stock price ($USD)")

## --------------------------------------------------------------------------------------------
google_2018 |>
  ACF(difference(Close)) |>
  autoplot()

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
a10 <- PBS |>
  filter(ATC2 == "A10") |>
  summarise(Cost = sum(Cost) / 1e6)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
a10 |> autoplot(
  Cost
)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
a10 |> autoplot(
  log(Cost)
)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
a10 |> autoplot(
  log(Cost) |> difference(12)
)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 |> autoplot(
  Cost
)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 |> autoplot(
  log(Cost)
)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 |> autoplot(
  log(Cost) |> difference(12)
)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 |> autoplot(
  log(Cost) |> difference(12) |> difference(1)
)

## ---- echo=TRUE------------------------------------------------------------------------------
google_2018 |>
  features(Close, unitroot_kpss)

## ---- echo=TRUE------------------------------------------------------------------------------
google_2018 |>
  features(Close, unitroot_ndiffs)

## ---- echo=TRUE------------------------------------------------------------------------------
h02 |>
  mutate(log_sales = log(Cost)) |>
  features(log_sales, list(unitroot_nsdiffs, feat_stl))

## ---- echo=TRUE------------------------------------------------------------------------------
h02 |>
  mutate(log_sales = log(Cost)) |>
  features(log_sales, unitroot_nsdiffs)
h02 |>
  mutate(d_log_sales = difference(log(Cost), 12)) |>
  features(d_log_sales, unitroot_ndiffs)

## ----arp, echo=FALSE, fig.height=3-----------------------------------------------------------
set.seed(1)
p1 <- tsibble(idx = seq_len(100), sim = 10 + arima.sim(list(ar = -0.8), n = 100), index = idx) |>
  autoplot(sim) + labs(y = "", title = "AR(1)")
p2 <- tsibble(idx = seq_len(100), sim = 20 + arima.sim(list(ar = c(1.3, -0.7)), n = 100), index = idx) |>
  autoplot(sim) + labs(y = "", title = "AR(2)")
p1 | p2

## ---- echo=FALSE-----------------------------------------------------------------------------
p1

## ---- echo=FALSE-----------------------------------------------------------------------------
p2

## ----maq, fig.height=2.5, echo=FALSE---------------------------------------------------------
set.seed(2)
p1 <- tsibble(idx = seq_len(100), sim = 20 + arima.sim(list(ma = 0.8), n = 100), index = idx) |>
  autoplot(sim) + labs(y = "", title = "MA(1)")
p2 <- tsibble(idx = seq_len(100), sim = arima.sim(list(ma = c(-1, +0.8)), n = 100), index = idx) |>
  autoplot(sim) + labs(y = "", title = "MA(2)")

p1 | p2

## ---- echo=FALSE-----------------------------------------------------------------------------
p1

## ---- echo=FALSE-----------------------------------------------------------------------------
p2

## ----egyptexportsauto, echo=TRUE-------------------------------------------------------------
global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

## ---- echo=TRUE, dependson="egyptexportsauto"------------------------------------------------
fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))
report(fit)

## ----egyptexportsmodel, include=FALSE, warning=FALSE, dependson="egyptexportsauto"-----------
stopifnot(identical(
  unlist(fit[1, 2][[1]][[1]]$fit$spec),
  c(p = 2L, d = 0L, q = 1L, P = 0, D = 0, Q = 0, constant = TRUE, period.year = 1)
))
coef <- rlang::set_names(tidy(fit)$estimate, tidy(fit)$term)

## ---- echo=TRUE, fig.height=4----------------------------------------------------------------
gg_tsresiduals(fit)

## ---- echo = TRUE----------------------------------------------------------------------------
augment(fit) |>
  features(.resid, ljung_box, lag = 10, dof = 3)

## ----egyptexportsf, fig.cap="Forecasts of Egyptian exports.", fig.asp=0.55, dependson="egyptexportsauto"----
fit |>
  forecast(h = 10) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

## ---- eval=FALSE-----------------------------------------------------------------------------
## egypt <- global_economy |> filter(Code == "EGY")
## egypt |> ACF(Exports) |> autoplot()
## egypt |> PACF(Exports) |> autoplot()

## ---- echo=FALSE-----------------------------------------------------------------------------
p1 <- global_economy |>
  filter(Code == "EGY") |>
  ACF(Exports) |>
  autoplot()
p2 <- global_economy |>
  filter(Code == "EGY") |>
  PACF(Exports) |>
  autoplot()
p1 | p2

## ---- echo=TRUE------------------------------------------------------------------------------
global_economy |>
  filter(Code == "EGY") |>
  gg_tsdisplay(Exports, plot_type = "partial")

## ---- echo=TRUE------------------------------------------------------------------------------
global_economy |>
  filter(Code == "EGY") |>
  gg_tsdisplay(Exports, plot_type = "partial")

## ---- echo=TRUE, fig.height=4----------------------------------------------------------------
fit1 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4, 0, 0)))
report(fit1)

## ---- echo=TRUE, fig.height=4----------------------------------------------------------------
fit2 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))
report(fit2)

## --------------------------------------------------------------------------------------------
global_economy |>
  filter(Code == "CAF") |>
  autoplot(Exports) +
  labs(
    title = "Central African Republic exports",
    y = "% of GDP"
  )

## ----caf2, warning=FALSE---------------------------------------------------------------------
global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(difference(Exports), plot_type = "partial")

## ----caf_fit---------------------------------------------------------------------------------
caf_fit <- global_economy |>
  filter(Code == "CAF") |>
  model(
    arima210 = ARIMA(Exports ~ pdq(2, 1, 0)),
    arima013 = ARIMA(Exports ~ pdq(0, 1, 3)),
    stepwise = ARIMA(Exports),
    search = ARIMA(Exports, stepwise = FALSE)
  )

## ----caf_fit2--------------------------------------------------------------------------------
caf_fit |> pivot_longer(!Country,
  names_to = "Model name",
  values_to = "Orders"
)

## ----caf_fit3, dependson=c("digits","caf_fit2")----------------------------------------------
glance(caf_fit) |>
  arrange(AICc) |>
  select(.model:BIC)

## ----cafres, dependson='caf_fit'-------------------------------------------------------------
caf_fit |>
  select(search) |>
  gg_tsresiduals()

## ----caf_lb, dependson='caf_fit'-------------------------------------------------------------
augment(caf_fit) |>
  features(.innov, ljung_box, lag = 10, dof = 3)

## ----caffc, dependson="caf_fit"--------------------------------------------------------------
caf_fit |>
  forecast(h = 5) |>
  filter(.model == "search") |>
  autoplot(global_economy)

## ---- fig.height=3---------------------------------------------------------------------------
leisure <- us_employment |>
  filter(
    Title == "Leisure and Hospitality",
    year(Month) > 2000
  ) |>
  mutate(Employed = Employed / 1000) |>
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )

## --------------------------------------------------------------------------------------------
leisure |>
  gg_tsdisplay(difference(Employed, 12),
    plot_type = "partial", lag = 36
  ) +
  labs(title = "Seasonally differenced", y = "")

## --------------------------------------------------------------------------------------------
leisure |>
  gg_tsdisplay(difference(Employed, 12) |> difference(),
    plot_type = "partial", lag = 36
  ) +
  labs(title = "Double differenced", y = "")

## --------------------------------------------------------------------------------------------
fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0, 1, 2) + PDQ(0, 1, 1)),
    arima210011 = ARIMA(Employed ~ pdq(2, 1, 0) + PDQ(0, 1, 1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit |> pivot_longer(everything(),
  names_to = "Model name",
  values_to = "Orders"
)

## --------------------------------------------------------------------------------------------
glance(fit) |>
  arrange(AICc) |>
  select(.model:BIC)

## --------------------------------------------------------------------------------------------
fit |>
  select(auto) |>
  gg_tsresiduals(lag = 36)

## --------------------------------------------------------------------------------------------
augment(fit) |> features(.innov, ljung_box, lag = 24, dof = 4)

## --------------------------------------------------------------------------------------------
forecast(fit, h = 36) |>
  filter(.model == "auto") |>
  autoplot(leisure) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 |> autoplot(
  Cost
)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 |> autoplot(
  log(Cost)
)

## ---- echo=TRUE, fig.height=3.5--------------------------------------------------------------
h02 |> autoplot(
  log(Cost) |> difference(12)
)

## ----h02b------------------------------------------------------------------------------------
h02 |> gg_tsdisplay(difference(log(Cost), 12),
  lag_max = 36, plot_type = "partial"
)

## ----h02aicc, echo=FALSE---------------------------------------------------------------------
models <- list(
  c(3, 0, 0, 2, 1, 0),
  c(3, 0, 1, 2, 1, 0),
  c(3, 0, 2, 2, 1, 0),
  c(3, 0, 1, 1, 1, 0),
  c(3, 0, 1, 0, 1, 1),
  c(3, 0, 1, 0, 1, 2),
  c(3, 0, 1, 1, 1, 1)
)
model_defs <- map(models, ~ ARIMA(log(Cost) ~ 0 + pdq(!!.[1], !!.[2], !!.[3]) + PDQ(!!.[4], !!.[5], !!.[6])))
model_defs <- set_names(model_defs, map_chr(
  models,
  ~ sprintf("ARIMA(%i,%i,%i)(%i,%i,%i)[12]", .[1], .[2], .[3], .[4], .[5], .[6])
))

fit <- h02 |>
  model(!!!model_defs)

fit |>
  glance() |>
  arrange(AICc) |>
  select(.model, AICc) |>
  knitr::kable(digits = 2, row.names = FALSE, align = "cc", booktabs = TRUE)

## ----arimah02, echo=TRUE---------------------------------------------------------------------
fit <- h02 |>
  model(best = ARIMA(log(Cost) ~ 0 + pdq(3, 0, 1) + PDQ(0, 1, 2)))
report(fit)

## ----h02res, echo=TRUE, fig.height=4, dependson='arimah02'-----------------------------------
gg_tsresiduals(fit)

## ----h02resb, echo = TRUE, fig.height=4, dependson='arimah02'--------------------------------
augment(fit) |>
  features(.resid, ljung_box, lag = 36, dof = 6)

## ----h02auto, echo=TRUE, fig.height=3.6------------------------------------------------------
fit <- h02 |> model(auto = ARIMA(log(Cost)))
report(fit)

## ---- echo=TRUE, fig.height=4, dependson='h02auto'-------------------------------------------
gg_tsresiduals(fit)

## ---- echo = TRUE, dependson='h02auto'-------------------------------------------------------
augment(fit) |>
  features(.resid, ljung_box, lag = 36, dof = 3)

## ----h02tryharder, echo=TRUE, fig.height=3.6-------------------------------------------------
fit <- h02 |>
  model(best = ARIMA(log(Cost),
    stepwise = FALSE,
    approximation = FALSE,
    order_constraint = p + q + P + Q <= 9
  ))
report(fit)

## ---- echo=TRUE, fig.height=4, dependson='h02tryharder'--------------------------------------
gg_tsresiduals(fit)

## ---- echo = TRUE, dependson='h02tryharder'--------------------------------------------------
augment(fit) |>
  features(.resid, ljung_box, lag = 36, dof = 9)

## ----h02-rmse, cache=TRUE, echo=FALSE--------------------------------------------------------
models <- list(
  c(3, 0, 1, 0, 1, 2),
  c(3, 0, 1, 1, 1, 1),
  c(3, 0, 1, 0, 1, 1),
  c(3, 0, 1, 2, 1, 0),
  c(3, 0, 0, 2, 1, 0),
  c(3, 0, 2, 2, 1, 0),
  c(3, 0, 1, 1, 1, 0),
  c(2, 1, 0, 0, 1, 1),
  c(4, 1, 1, 2, 1, 2)
)

model_defs <- map(models, ~ ARIMA(log(Cost) ~ 0 + pdq(!!.[1], !!.[2], !!.[3]) + PDQ(!!.[4], !!.[5], !!.[6])))
model_defs <- set_names(model_defs, map_chr(
  models,
  ~ sprintf("ARIMA(%i,%i,%i)(%i,%i,%i)[12]", .[1], .[2], .[3], .[4], .[5], .[6])
))

fit <- h02 |>
  filter_index(~"2006 Jun") |>
  model(!!!model_defs)

fit |>
  forecast(h = "2 years") |>
  accuracy(h02) |>
  arrange(RMSE) |>
  select(.model, RMSE) |>
  knitr::kable(digits = 4)

## ----h02f, echo=TRUE, fig.height=3-----------------------------------------------------------
fit <- h02 |>
  model(ARIMA(Cost ~ 0 + pdq(3, 0, 1) + PDQ(0, 1, 2)))
fit |>
  forecast() |>
  autoplot(h02) +
  labs(y = "H02 Expenditure ($AUD)")

## ----venn, echo=FALSE------------------------------------------------------------------------
library(latex2exp)
cols <- c(ets = "#D55E00", arima = "#0072b2")
tibble(
  x = c(-0.866, 0.866),
  y = c(-0.5, -0.5),
  labels = c("ets", "arima"),
) |>
  ggplot(aes(color = labels, fill = labels)) +
  ggforce::geom_circle(aes(x0 = x, y0 = y, r = 1.5), alpha = 0.3, size = 1) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  coord_fixed() +
  guides(fill = FALSE) +
  geom_text(aes(label = "ETS models", x = -1.5, y = 1.15), col = cols["ets"], fontface = "bold", size = 6) +
  geom_text(aes(label = "Combination\n of components", x = -1.5, y = 0.3), col = cols["ets"]) +
  geom_text(aes(label = "9 non-additive\n ETS models", x = -1.5, y = -0.6), col = cols["ets"]) +
  geom_text(aes(label = "All ETS models\n with M components", x = -.95, y = -1.6), col = cols["ets"]) +
  geom_text(aes(label = "ARIMA models", x = 1.5, y = 1.15), col = cols["arima"], fontface = "bold", size = 6) +
  geom_text(aes(label = "Modelling\n autocorrelations", x = 1.5, y = 0.3), col = cols["arima"]) +
  annotate("text", label = TeX("Potentially $\\infty$ models"), x = 1.5, y = -0.6, col = cols["arima"]) +
  geom_text(aes(label = "All stationary models\n Many large models", x = 1.01, y = -1.6), col = cols["arima"]) +
  geom_text(aes(label = "6 additive\n ETS models", x = 0, y = -0.6), col = "#6b6859") +
  guides(col = FALSE, fill = FALSE) +
  theme_void()

## ----tscvpop, echo=TRUE, warning=FALSE-------------------------------------------------------
aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Population = Population / 1e6)
aus_economy |>
  slice(-n()) |>
  stretch_tsibble(.init = 10) |>
  model(
    eta = ETS(Population),
    arima = ARIMA(Population)
  ) |>
  forecast(h = 1) |>
  accuracy(aus_economy) |>
  select(.model, ME:RMSSE)

## ----popetsplot, echo=TRUE, fig.height=3.1---------------------------------------------------
aus_economy |>
  model(ETS(Population)) |>
  forecast(h = "5 years") |>
  autoplot(aus_economy) +
  labs(
    title = "Australian population",
    y = "People (millions)"
  )

## ----qcement1, echo=TRUE---------------------------------------------------------------------
cement <- aus_production |>
  select(Cement) |>
  filter_index("1988 Q1" ~ .)
train <- cement |> filter_index(. ~ "2007 Q4")
fit <- train |>
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement)
  )

## ----qcement2, dependson="qcement1"----------------------------------------------------------
fit |>
  select(arima) |>
  report()

## ----qcement3, dependson="qcement1"----------------------------------------------------------
fit |>
  select(ets) |>
  report()

## ----qcement4, dependson="qcement1"----------------------------------------------------------
gg_tsresiduals(fit |> select(arima), lag_max = 16)

## ----qcement5, dependson="qcement1"----------------------------------------------------------
gg_tsresiduals(fit |> select(ets), lag_max = 16)

## ----qcement6, dependson="qcement1"----------------------------------------------------------
fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 16, dof = 6)

## ----qcement7, dependson="qcement1"----------------------------------------------------------
fit |>
  select(ets) |>
  augment() |>
  features(.innov, ljung_box, lag = 16, dof = 6)

## ----qcement8, dependson=c("qcement2","qcement3")--------------------------------------------
fit |>
  forecast(h = "2 years 6 months") |>
  accuracy(cement) |>
  select(-ME, -MPE, -ACF1)

## ----qcement9, echo=TRUE, dependson="qcement1", fig.height=3---------------------------------
fit |>
  select(arima) |>
  forecast(h = "3 years") |>
  autoplot(cement) +
  labs(
    title = "Cement production in Australia",
    y = "Tonnes ('000)"
  )
