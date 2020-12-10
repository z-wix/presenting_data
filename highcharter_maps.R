# Highcharter -------------------------------------------------------------

library(tidyverse)
library(highcharter)

# classic scatterplot
hchart(diamonds %>% sample_n(100), "scatter",
       hcaes(x=carat, y = price, group = color))

# histogram
hchart(diamonds$price)


# Line
AirPassengers
class(AirPassengers)
hchart(AirPassengers)

# Forecasting
library(forecast)

airforecast <- forecast(auto.arima(AirPassengers), level = 95)
hchart(airforecast)


# Map
mapdata <-get_data_from_map(download_map_data("countries/us/us-all"))
hcmap("countries/us/us-all", showInLegend = FALSE)


data_fake <- mapdata %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = 1e5 *abs(rt(nrow(.), df = 10)))

hcmap(
  "countries/us/us-all",
  data = data_fake,
  value = "value",
  joinBy = c("hc-a2", "code"),
  name = "Fake data"
)



# ggmap -------------------------------------------------------------------

library(maps)

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

states_map <- map_data("state")
county_map <- map_data("county")

ggplot(crimes) +
  geom_map(aes(map_id = state, fill = Murder), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat)

