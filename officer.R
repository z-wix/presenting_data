# Officer 

# Package
library(officer)
library(tidyverse)
library(here)

# Houses dataset
houses <- read_csv(here::here("data", "cleaned_parcels.csv"))

houses <- houses %>%
  select(parcel_number, property_add, market_value, year_built) %>%
  separate(property_add, c("address", "city"), sep = ",") %>%
  mutate(city = trimws(city)) %>%
  filter(market_value > 0,
         year_built > 0,
         city %in% c("WOODLAND", "STRAWBERRY", "MIDWAY", "KAMAS", "HEBER"),
         market_value < max(market_value),
         year_built < max(year_built))


# Read in your powerpoint template
my_pres <- read_pptx(here("in_class", "officer.pptx"))

# annotate_base(my_pres)

# Add title slide
my_pres <- my_pres %>% 
  add_slide(layout = "Title Slide", master = "zack_3")

# Fill slide
my_pres <- my_pres %>% 
  ph_with(value = "Example Presentation", location = ph_location_label(ph_label  = "Title 1"))

# Make plot
my_plot <- houses %>% 
  ggplot() +
  geom_histogram(aes(x = year_built)) +
  labs(title = "Overall")

my_pres <- my_pres %>% 
  add_slide(layout = "Center Content", master = "zack_3") %>% 
  ph_with(value = my_plot, location = ph_location_label(ph_label = "Chart Placeholder 5"))
  
# need to install xquartz if on Mac and you have Cairo errors

# make for loop for city based histograms

my_pres
print(my_pres, "in_class/officer_example.pptx")




# Scales Package ----------------------------------------------------------


# library(scales) # don't load the library because of overwriting other functions

# Before
houses %>% 
  ggplot() +
  geom_point(aes( x = year_built,y = market_value, color = city), alpha = 0.3)

# After
houses %>% 
  ggplot() +
  geom_point(aes(year_built, market_value, color = city), alpha = 0.3) +
  scale_y_continuous(labels = scales::dollar_format(), n_breaks = 10) +
  scale_x_continuous(labes = c(1980, 2018))

# Before
diamonds %>% 
  ggplot() +
  geom_point(aes(carat, price, color = color))

# After
diamonds %>% 
  ggplot() +
  geom_point(aes(carat, price, color = color)) +
  scale_y_continuous(labels = scales::label_number_si())

# Before
economics %>% 
  filter(date < ymd("1970-01-01")) %>% 
  ggplot(aes(date, pce)) +
  geom_line()

# After
economics %>% 
  filter(date < ymd("1970-01-01")) %>% 
  ggplot(aes(date, pce)) +
  geom_line() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("3 months"),
               labels = scales::label_date_short()) +
  scale_y_continuous(
    "Personal Consumption Expenditure",

  )
