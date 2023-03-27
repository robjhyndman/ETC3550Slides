library(fpp3)

# Algerian Exports

algeria_economy <- global_economy |>
  filter(Country == "Algeria")
algeria_economy |>
  autoplot(Exports)
fit <- algeria_economy |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    auto = ETS(Exports)
  )
fit |>
  select(ANN) |>
  report()

tidy(fit)
accuracy(fit)
glance(fit)

components(fit) |> autoplot()

components(fit) |>
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fc <- fit |>
  forecast(h=5)

fc |>
  filter(.model == "MNN") |>
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")

# Repeat with test set

fit <- algeria_economy |>
  filter(Year <= 2012) |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    auto = ETS(Exports)
  )

fc <- fit |>
  forecast(h=5)

fc |>
  autoplot(algeria_economy, level=NULL) +
  ylab("Exports (% of GDP)") + xlab("Year")

fc |> accuracy(algeria_economy)

# Repeat with tscv

alg_exports_stretch <- algeria_economy |>
  stretch_tsibble(.init = 5, .step = 1)

cv_fit <- alg_exports_stretch |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    naive = NAIVE(Exports),
    drift = RW(Exports ~ drift()),
    autoNN = ETS(Exports ~ trend("N") + season("N")),
  )

cv_fc <- cv_fit |>
  forecast(h = 12) |>
  group_by(.id, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Exports", distribution = Exports)

cv_fc |>
  accuracy(algeria_economy, by = c("h", ".model")) |>
  group_by(.model, h) |>
  summarise(RMSSE = sqrt(mean(RMSSE^2))) |>
  ggplot(aes(x=h, y=RMSSE, group=.model, col=.model)) +
  geom_line()

cv_fc |>
  accuracy(algeria_economy, by = c("h", ".model")) |>
  group_by(.model) |>
  summarise(RMSSE = sqrt(mean(RMSSE^2))) |>
  arrange(RMSSE)
