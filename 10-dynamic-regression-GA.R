library(fpp3)
library(lubridate)


## US CHANGE --------------------------

us_change %>%
  pivot_longer(Consumption:Unemployment,
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value, colour=var)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income",
       y = "Quarterly % change") +
  guides(colour="none")

# Let's do the simple regression model

# Recall previously
# Regression 
fit <- us_change %>%
  model(TSLM(Consumption ~ Income))
report(fit)
gg_tsresiduals(fit) # Inference is not correct - estimates are not efficient
fit %>% residuals() %>% gg_tsdisplay(plot_type = 'partial')		 

# Dynamic Regression 
# regression part of the model the same as before
# ARIMA() will take care of dynamics

fit <- us_change %>% 
  model(ARIMA(Consumption ~ Income))
report(fit)
fit %>% gg_tsresiduals()
# These now look pretty good
# Switch to slides and write out coefficients

# You can look at the regression errors
fit %>% residuals(type='regression') %>%
  gg_tsdisplay(.resid, plot_type = 'partial') +
  ggtitle("Regression errors")

# By default you get the following
fit %>% residuals(type='innovation') %>% #type='innovation'
  gg_tsdisplay(.resid, plot_type = 'partial') +
  ggtitle("ARIMA errors - innovations")

# I would encourage you to explore the following code
# ------ Skip 

# Just to demonstrate
augment(fit)

# If I took logs
fit_log <- us_change %>% 
  model(ARIMA(log(Consumption) ~ Income))
augment(fit_log)

us_change

# Notice the NAs - here's a cool trick
fit_log <- us_change %>% 
  model(ARIMA(log(Consumption+abs(min(us_change$Consumption))+1) ~ Income))
augment(fit_log)

# to make sure always use .innov
# .resid=y-.fitted(yhat)

augment(fit) %>%
  features(.innov, ljung_box, dof = 5, lag = 12)
# Quarterly data, lag =12, 3-years
# We usually choose 2 or 3 lengths of the seasonality

# ------ Skip 


# Let's do some forecasting
# Setup the new_data tsibble
us_change_future <- new_data(us_change, 8) %>% # set up the new tsibble with dates
  mutate(Income = mean(us_change$Income)) # Use historical averages

fit %>% forecast(new_data = us_change_future) %>%
  autoplot(filter(us_change, year(Quarter)>=1990)) +
  labs(x = "Year", y = "Percentage change",
       title = "Forecasts from regression with ARIMA(1,0,2) errors")
# It knows to generate 8 forecasts because of the 
# new_data tsibble 	  

# Let's do some scenarios
us_change_future <- new_data(us_change, 11) %>%
  mutate(
    Income = max(us_change$Income), #min, mean
  )

forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(x = "Year", y = "Percentage change",
       title = "Forecasts from regression with ARIMA(1,0,2) errors")

# Let's look at all the predictors now
fit <- us_change %>% 
  model(ARIMA(Consumption ~ Income + Production + 
                Savings + Unemployment+ pdq(d=0)+ PDQ(0,0,0), 
              stepwise = FALSE, approximation = FALSE))

fit %>% report()

fit %>% gg_tsresiduals()
# Get ARIMA() to work harder 
# Add - the PDQ(0,0,0) will skip seasonal models so much faster
# + PDQ(0,0,0), stepwise = FALSE, approximation = FALSE

# You would try and drop some of the predictors
# next to get a better model
# Try dropping - Production
fit <- us_change %>% 
  model(
    ARIMA(
      Consumption ~ Income  + Savings + Unemployment + 
        pdq(d=0) + PDQ(0,0,0), 
      stepwise = FALSE, approximation = FALSE))
fit %>% report()
# try dropping predictors 

# Forecasting with this model now
# Do 5-years ahead now to see dynamics 
us_change_future <- new_data(us_change, 20) %>%
  mutate(Income = mean(us_change$Income),
         Savings = mean(us_change$Savings),
         Unemployment = mean(us_change$Unemployment))

fit %>% forecast(new_data = us_change_future) %>%
  autoplot(filter(us_change)) 
# Notice the short-run dynamics from ARMA


## DAILY VICTORIAN ELECTRICITY DEMAND -------

# Turn half-hourly data into daily
vic_elec 
vic_elec %>% tail()

vic_elec_daily <- vic_elec %>%   
  filter(year(Time) == 2014) %>% # filter by 2014
  index_by(Date = date(Time)) %>% # index by date to turn into daily
  summarise(                        # summarise() below
    Demand = sum(Demand)/1e3,       # Total daily and scaling Mega to Gigawatts
    Temperature = max(Temperature), # take highest temperature for the day
    Holiday = any(Holiday)          # Hol for any half hour is Hol for day
  ) %>% # create new variable Day_Type
  mutate(Day_Type = case_when(       # Separate weekdays, weekends and holidays
    Holiday ~ "Holiday",             # If Holiday=TRUE call it a Holiday  
    wday(Date) %in% 2:6 ~ "Weekday", # wday() returns 1:7 starting from a Sunday
    TRUE ~ "Weekend"                 # Call everything else a weekend
  )) %>% 
  select(Date, Demand, Temperature, Day_Type)

# Lets have a look at the data
# Scatter plot is useful in this case
vic_elec_daily %>%
  ggplot(aes(x=Temperature, y=Demand, colour=Day_Type)) +
  geom_point() +
  labs(x = "Maximum temperature", y = "Electricity demand (GW)")
# Higher demand during higher temperatures - nonlinear
# Notice holidays clustered within Weekends although they
# may weekdays

# Make longer to plot time series nicely
vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature),
    names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, colour=Variable)) + 
  geom_line() +
  facet_grid(vars(Variable), scales = "free_y") +
  guides(colour="none")

# Let's try with ARIMA errors
fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) + # I() treats this a new variable rather than interaction
                (Day_Type=="Weekday")))

fit %>% report()
gg_tsresiduals(fit)
# High variance in warmer months - lower in cooler months
# so monotonic transformations will not work. We need to 
# do something else. In fact the models used are much more 
# complicated cubic splines (similar to piecewise linear) 
# are used instead of quadratic terms. A separate model for 
# each day of the week - days are very different 
# separate models for seasons

vic_next_day <- new_data(vic_elec_daily, 1) %>% 
  mutate(Temperature = 26, Day_Type = "Holiday")
forecast(fit, vic_next_day)

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26, #
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

fit %>% 
  forecast( new_data=vic_elec_future) %>%
  autoplot(vic_elec_daily %>% tail(360)) + 
  ylab("Electricity demand (GW)")


# Let's forecast a few days ahead
# In practice you would use many more scenarios
# BOM forecasts are very accurate 4-5 days ahead
# They will give you forecasts up to to 7-days ahead.

# You can simulate future scenarios of temperature for 
# longer horizons from a temperature model 
# and get many demand profiles.
vic_elec_future <- new_data(vic_elec_daily, 28) %>%
  mutate(
    Temperature = c(rep(32,14),rep(25,14)), #
    Holiday = c(TRUE, rep(FALSE, 27)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

fit %>% 
  forecast( new_data=vic_elec_future) %>%
  autoplot(vic_elec_daily %>% tail(360)) + 
  ylab("Electricity demand (GW)")


# Seasonal patterns 
vic_elec_daily %>% gg_season(period = "week")
# Weekdays higher than weekends
vic_elec_daily %>% gg_season(period = "year")
# higher demand in summer and in winter days
# higher variation in summer


fit <- vic_elec_daily %>% 
  model(TSLM(Demand ~ Temperature + I(Temperature^2) + 
                (Day_Type=="Weekday")))

# I() treats this a new variable rather than interaction
# Modelling with quadratic is fine in this case
# within the range of the data/temperature
# y=f(x,x^2) not y=f(t,t^2)


report(fit)
fit %>% residuals() %>% gg_tsdisplay(plot_type='partial')
# So lots of dynamics left over

# Let's try with ARIMA errors
fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) + # I() treats this a new variable rather than interaction
                (Day_Type=="Weekday")))

fit %>% report()
report(fit)
gg_tsresiduals(fit)
# High variance in warmer months - lower in cooler months
# so monotonic transformations will not work. We need to 
# do something else. In fact the models used are much more 
# complicated cubic splines (similar to piecewise linear) 
# are used instead of quadratic terms. A separate model for 
# each day of the week - days are very different 
# separate models for seasons

# Let's forecast a few days ahead
# In practice you would use many more scenarios
# BOM forecasts are very accurate 4-5 days ahead
# They will give you forecasts up to to 7-days ahead.

# You can simulate future scenarios of temperature for 
# longer horizons from a temperature model 
# and get many demand profiles.

vic_elec_future <- new_data(vic_elec_daily, 28) %>%
  mutate(
    Temperature = c(rep(32,14),rep(25,14)), #
    Holiday = c(TRUE, rep(FALSE, 27)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

fit %>% 
  forecast( new_data=vic_elec_future) %>%
  autoplot(vic_elec_daily %>% tail(360)) + 
  ylab("Electricity demand (GW)")

## AUSTRALIAN CAFE DATA --------------------------------------------------

aus_cafe <- aus_retail %>% filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) %>% summarise(Turnover = sum(Turnover)) # add up across the states
aus_cafe %>% autoplot(Turnover)
# Total monthly turnover across all states

# Monthly data so usually don't need Fourier terms
# An easy example to start with
# 6 is the max Fourier terms and ARIMA deals only with non-seaosal bits
fit <- aus_cafe %>% model(
    `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0,0,0)),
    `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0,0,0)),
    `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0,0,0)),
    `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0,0,0)),
    `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0,0,0)),
    `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0,0,0))
    )

fit %>% select("K = 2") %>% report()
glance(fit) %>%
  select(.model, sigma2, log_lik, AIC, AICc, BIC)
# Not surprising that we need all terms to deal 
# with this complicated
# seasonal pattern - so using max dof to use  

# Switch to Slides to show patterns


## US GASOLINE ---------------------------------------------------
# Weekly data
us_gasoline %>% autoplot(Barrels) 

# The ugly way 
# Assuming 52 weeks in the year
# What can K go up to?

fit <- us_gasoline %>%
  model(
    F1 = ARIMA(Barrels ~ fourier(K = 1) + PDQ(0,0,0)),
    F2 = ARIMA(Barrels ~ fourier(K = 2) + PDQ(0,0,0)),
    F3 = ARIMA(Barrels ~ fourier(K = 3) + PDQ(0,0,0)),
    F4 = ARIMA(Barrels ~ fourier(K = 4) + PDQ(0,0,0)),
    F5 = ARIMA(Barrels ~ fourier(K = 5) + PDQ(0,0,0)),
    F6 = ARIMA(Barrels ~ fourier(K = 6) + PDQ(0,0,0)),
    F7 = ARIMA(Barrels ~ fourier(K = 7) + PDQ(0,0,0)),
    F8 = ARIMA(Barrels ~ fourier(K = 8) + PDQ(0,0,0)),
    F9 = ARIMA(Barrels ~ fourier(K = 9) + PDQ(0,0,0)),
    F10 = ARIMA(Barrels ~ fourier(K = 10) + PDQ(0,0,0)),
    F11 = ARIMA(Barrels ~ fourier(K = 11) + PDQ(0,0,0)),
    F12 = ARIMA(Barrels ~ fourier(K = 12) + PDQ(0,0,0)),
    F13 = ARIMA(Barrels ~ fourier(K = 13) + PDQ(0,0,0)),
    F14 = ARIMA(Barrels ~ fourier(K = 14) + PDQ(0,0,0))
    )

# Let's us purrr 
library(purrr) # uses a map function for when looping over things

models <- as.list(seq(26))  # Gives you a list of all the different 
                            # K's we are going to use
                            # so now using max(K)=26

# Sets up the model definitions
# Take models object and map into formula
# !!.[1] grabs the value of the list and adds it here
model_defs <- models %>%
  map(~ ARIMA(Barrels ~ fourier(K=!!.[1]) + PDQ(0,0,0)))

# model_defs %>% View()

# Let's give these some names
model_defs <- model_defs %>%
  set_names(map_chr(models, ~ sprintf("fourier%i", .[1])))

# Run the data into the model() and estimate 
# each of 26 models
fit_gas <- us_gasoline %>%
  model(!!!model_defs) # Bang bang bang 
                      # operator grabs the model definitions

# fit contains all 26 models 

fit_gas %>% select(fourier1) %>% report()
fit_gas %>% glance()

# the name of the best model
best <- glance(fit_gas) %>%
  filter(AICc==min(AICc)) %>%   # filter() the one with min(AICc)
  pull(.model)                  # pull() the model 

# !!best treats this as a name not a string
fit_gas %>% select(!!best) %>% report(fit) 

fit_gas %>%
  select(!!best) %>%
  forecast(h = "3 years") %>%
  autoplot(us_gasoline)

## 5-minute CALL CENTRE DATA ------------------------------------------------

# tsv - tab seperated file - like csv - comma separated files
# Time of the day across different dates

# Calls at a call center in the US
# Lets turn this into a useful tsibble

(calls <- readr::read_tsv("http://robjhyndman.com/data/callcenter.txt") %>%
    rename(time = `...1`) %>% #rename 
    pivot_longer(-time, names_to = "date", values_to = "volume") %>%
    mutate(
      date = as.Date(date, format = "%d/%m/%Y"), # turn the date variable into a date - currently a character
      datetime = as_datetime(date) + time        # combine the date and the time
    ) %>% 
    as_tsibble(index = datetime) 
 )

calls %>% tail()
# 7am to 9pm

calls %>% autoplot(volume)
# R does not know what to do with the gaps

calls %>% fill_gaps() %>% autoplot(volume)
# Back to slides

# Let's explore with some plots
# Time of day and day of week 
# Let's start with time of day

calls %>% fill_gaps() %>%
  gg_season(volume, period = "day", alpha = 0.2) #+ # make transparent
#  guides(colour = FALSE)
# Over 260 days plotted on top of each other

# Day of the week
calls %>% fill_gaps() %>%
  gg_season(volume, period = "week", alpha = 0.2) #+
#  guides(colour = FALSE)

# Earo Wang
# library(sugrrants)
# calls %>% filter(month(date, label = TRUE) == "Apr") %>%
#   ggplot(aes(x = time, y = volume)) +
#   geom_line() + facet_calendar(date)

# Need to re_index because of missing days 
calls_mdl <- calls %>%
  mutate(idx = row_number()) %>%
  update_tsibble(index = idx)

# Need to tell it period
# 169 5 minute intervals in each of the working days

# Just fit a stationary ARIMA with no season 
# Fourier will take care of season

fit_calls <- calls_mdl %>%
  model(ARIMA(volume ~ fourier(169, K = 10) 
              + pdq(d=0) + PDQ(0,0,0)))
report(fit_calls)

#169*2
gg_tsresiduals(fit_calls, lag=338)
1/sqrt(nrow(calls_mdl))

# let's forecast 10 days ahead
fit_calls %>% forecast(h = 1690) %>%
  autoplot(calls_mdl)




## DAILY VICTORIAN ELECTRICITY DEMAND EXTENDED EXAMPLE -------

# Turn half-hourly data into daily
vic_elec 
vic_elec %>% tail()

vic_elec_daily <- vic_elec %>%   
  #filter(year(Time) == 2014) %>% # do not filter by 2014
  index_by(Date = date(Time)) %>% # index by date to turn into daily
  summarise(                        # summarise() below
    Demand = sum(Demand)/1e3,       # Total daily and scaling Mega to Gigawatts
    Temperature = max(Temperature), # take highest temperature for the day
    Holiday = any(Holiday)          # Hol for any half hour is Hol for day
  ) %>% # create new variable Day_Type
  mutate(Day_Type = case_when(       # Separate weekdays, weekends and holidays
    Holiday ~ "Holiday",             # If Holiday=TRUE call it a Holiday  
    wday(Date) %in% 2:6 ~ "Weekday", # wday() returns 1:7 starting from a Sunday
    TRUE ~ "Weekend"                 # Call everything else a weekend
  )) %>% 
  select(Date, Demand, Temperature, Day_Type)

# Lets have a look at the data
# Scatter plot is useful in this case
vic_elec_daily %>%
  ggplot(aes(x=Temperature, y=Demand, colour=Day_Type)) +
  geom_point() +
  labs(x = "Maximum temperature", y = "Electricity demand (GW)")

# Highly non-linear pattern
# Heating for below maybe 18 degrees
# min demand (18-25)
# Cooling above 25 degrees
# Weekdays higher than weekends
# Holidays clustered within/similar to Weekends 
# Some really extreme days


# Make longer to plot time series nicely
vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature),
               names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, colour=Variable)) + 
  geom_line() +
  facet_grid(vars(Variable), scales = "free_y") +
  guides(colour="none")

# Multiple seasonal patterns - lets explore below
# Seasonal patterns 
vic_elec_daily %>% gg_season(period = "week")
# Weekdays higher than weekends
vic_elec_daily %>% gg_season(period = "year")
# higher demand in summer and in winter days
# higher variation in summer


# Let's try with a regression model
fit_regr <- vic_elec_daily %>% 
  model(TSLM(Demand ~ Temperature + I(Temperature^2) + 
               (Day_Type=="Weekday")))

# I() treats this a new variable 
# Modelling with quadratic is fine in this case
# within the range of the data/temperature
# y=f(x,x^2) not y=f(t,t^2)

fit_regr %>% report()
fit_regr %>% gg_tsresiduals()
# So lots of dynamics left over

# Let's try with ARIMA errors
fit_dyn <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) + # I() treats this a new variable rather than interaction
                (Day_Type=="Weekday")))

fit_dyn %>% report()
fit_dyn %>% gg_tsresiduals()
# Resids much better 
# High variance in warmer months - lower in cooler months
# so monotonic transformations will not work. We need to 
# do something else. In fact the models used are much more 
# complicated cubic splines (similar to piecewise linear) 
# are used instead of quadratic terms. A separate model for 
# each day of the week - days are very different 
# separate models for seasons

vic_next_day <- new_data(vic_elec_daily, 1) %>% 
  mutate(Temperature = 26, Day_Type = "Holiday")
fit_dyn %>% forecast(vic_next_day)

# Let's forecast a few days ahead
# In practice you would use many more scenarios
# BOM forecasts are very accurate 4-5 days ahead
# They will give you forecasts up to to 7-days ahead.

# You can simulate future scenarios of temperature for 
# longer horizons from a temperature model 
# and get many demand profiles.
vic_elec_future <- new_data(vic_elec_daily, 28) %>%
  mutate(
    Temperature = c(rep(32,14),rep(25,14)), #
    Holiday = c(TRUE, rep(FALSE, 27)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

fit_dyn %>% 
  forecast( new_data=vic_elec_future) %>%
  autoplot(vic_elec_daily %>% tail(360)) + 
  ylab("Electricity demand (GW)")


# Pure time series
fit_ETS <- vic_elec_daily %>% 
  model(ETS(Demand))

fit_ETS %>% report()
# ETS(M,A,M) estimated model 
# with alpha, beta, gamma
# l[0], b[0], s[0],s[-1],s[-2],...,s[-6]

fit_ETS %>% gg_tsresiduals()

augment(fit_ETS) %>% 
  autoplot(Demand) +
  geom_line(aes(y = .fitted), colour = "red")
# model states adapt well within sample 
# BUT

fit_ETS %>% 
  forecast(h="1 years") %>% #"2 years"
  autoplot(vic_elec_daily %>% tail(740), level=NULL) + 
  ylab("Electricity demand (GW)")

# Dynamic harmonic regression - ignoring TEMPERATURE VARIABLE

# Do not run
fit_DHR <- vic_elec_daily %>%
  model(ARIMA(Demand ~ fourier(period="year",K=4) + 
                PDQ(period = "week")))
# Notice only one difference D=1

fit_DHR %>% report() # Count the number of parameters estimated
fit_DHR %>% gg_tsresiduals() 
# More short run dynamics to be explored 

# Let's see how we have done in terms of fit
augment(fit_DHR) %>% 
  autoplot(Demand) +
  geom_line(aes(y = .fitted), colour = "red")

fit_DHR %>% 
  forecast(h="1 year") %>%
  autoplot(vic_elec_daily %>% tail(360), level=NULL) + 
  ylab("Electricity demand (GW)")
# Obviously we can do better 
# but we now have the annual seasonal pattern


# Let's combine regressors the DHR
fit_better <- vic_elec_daily %>%
  model(
    ARIMA(
      Demand ~ Temperature + I(Temperature^2) + (Day_Type == "Weekday") +
        fourier(period="year",K=4) + 
        PDQ(period = "week") + 
        pdq(0:7),
      stepwise=FALSE, order_constraint = p + q + P + Q <= 10)
  )

fit_better %>% report()
fit_better %>% gg_tsresiduals()

# You can simulate temperatures etc. for longer term forecasts.


vic_elec_future <- new_data(vic_elec_daily, 28) %>%
  mutate(
    Temperature = c(rep(32,14),rep(25,14)), #
    Holiday = c(TRUE, rep(FALSE, 27)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

fit_better %>% 
  forecast( new_data=vic_elec_future) %>%
  autoplot(vic_elec_daily %>% tail(360)) + 
  ylab("Electricity demand (GW)")


## AUSTRALIAN VISITORS -------------------------------------------------

aus_visitors <- as_tsibble(fpp2::austa)
# Total international visitors to Australia (in millions). 1980-2015.
# Different from the slides just to show you another one

aus_visitors %>%
  autoplot(value) +
  labs(x = "Year", y = "millions of people",
       title = "Total annual international visitors to Australia")

fit_deterministic <- aus_visitors %>%
  model(Deterministic = ARIMA(value ~ 1 + trend() + pdq(d = 0)))
report(fit_deterministic)

fit_stochastic <- aus_visitors %>%
  model(Stochastic = ARIMA(value ~ 1 +pdq(d=1)))
report(fit_stochastic)

bind_cols(fit_deterministic, fit_stochastic) %>%
  rename(`Deterministic trend` = 1, `Stochastic trend` = 2) %>%
  forecast(h = 10) %>%
  autoplot(aus_visitors) +
  facet_grid(vars(.model)) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models") +
  guides(colour = FALSE)

aus_visitors %>%
  autoplot(value) +
  autolayer(fit_stochastic %>% forecast(h = 20),
            colour = "#0072B2", level = 95) +
  autolayer(fit_deterministic %>% forecast(h = 20),
            colour = "#D55E00", alpha = 0.7, level = 95) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models")

## AUSTRALIAN AIR PASSENGERS -------------------------------------------------
aus_airpassengers %>%
  autoplot(Passengers) +
  labs(y = "Passengers (millions)",
       title = "Total annual air passengers")

fit_deterministic <- aus_airpassengers %>%
  model(deterministic = ARIMA(Passengers ~ 1 + trend() + pdq(d = 0)))
report(fit_deterministic)

fit_stochastic <- aus_airpassengers %>%
  model(stochastic = ARIMA(Passengers ~ 1 + pdq(d = 1)))
report(fit_stochastic)

fc_deterministic <- forecast(fit_deterministic, h = 200)
fc_stochastic <- forecast(fit_stochastic, h = 200)

aus_airpassengers %>%
  autoplot(Passengers) +
  autolayer(fc_stochastic, colour = "#0072B2", level = 95) +
  autolayer(fc_deterministic, colour = "#D55E00", alpha = 0.65, level = 95) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models")


## TV ADVERTISING ----------------------------------------------------------

# Data a bit outdated from an old consulting project 
# but interesting 

insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) + geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = NULL, title = "Insurance advertising and quotations")

# Visually there seems to be a direct effect of the advertising 
# expenditure directly on their quotes

# Useful to look at scatterplots as well

# Let's use the lag function
insurance %>%
  mutate(
    lag1 = lag(TVadverts),
    lag2 = lag(TVadverts,2),
    lag3 = lag(TVadverts,3)
  ) %>%
  as_tibble() %>% # turn into a tibble to get a nice scatter plot
  select(-Month) %>%
  rename(lag0 = TVadverts) %>%
  pivot_longer(-Quotes, names_to="Lag", values_to="TV_adverts") %>%
  ggplot(aes(x = TV_adverts, y = Quotes)) + geom_point() +
  facet_grid(. ~ Lag) +
  labs(title = "Insurance advertising and quotations") 


# Let's fit some models
fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA,NA,NA,Quotes[4:40])) %>% # missing values for first three obs
  # Estimate models
  model(
    ARIMA(Quotes ~ pdq(d = 0)+PDQ(0,0,0) + TVadverts),
    ARIMA(Quotes ~ pdq(d = 0)+PDQ(0,0,0) + TVadverts + lag(TVadverts)), # not using first obs
    ARIMA(Quotes ~ pdq(d = 0)+PDQ(0,0,0) + TVadverts + lag(TVadverts) + # not using first two obs
            lag(TVadverts, 2)),
    ARIMA(Quotes ~ pdq(d = 0)+PDQ(0,0,0) + TVadverts + lag(TVadverts) +
            lag(TVadverts, 2) + lag(TVadverts, 3))
  )

# But not using the same data
glance(fit)

# Revised estimate models are now different

# Get a nicer table
glance(fit) %>%
  transmute(`Lag order` = 0:3, sigma2, log_lik, AIC, AICc, BIC)

# Let's refit the model using all the available data
fit <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d=0)+PDQ(0,0,0) + TVadverts + lag(TVadverts)))
report(fit)

advert_a <- new_data(insurance, 20) %>%
  mutate(TVadverts = 10)
forecast(fit, advert_a) %>% autoplot(insurance)

advert_b <- new_data(insurance, 20) %>%
  mutate(TVadverts = 8)
forecast(fit, advert_b) %>% autoplot(insurance)

advert_c <- new_data(insurance, 20) %>%
  mutate(TVadverts = 6)
forecast(fit, advert_c) %>% autoplot(insurance)
