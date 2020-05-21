# Code used to create weather donuts. Mmm donuts...
# Loading packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(weathercan)
library(sp) # required to search weathercan stations by coordinates
library(naniar)
library(ggthemes)

# Search for stations near the target cities based on coordinates using weathercan::stations_search. Requires the `sp` package to search by coordinates
site.list <- stations_search(coords = c(44.6488, -63.5752),
                             interval = "day", 
                             dist = 20,
                             ends_earliest = 2018) 

# Station numbers and names are created 
site.id <- tribble(~station_id, ~city_name,
             51442, "Vancouver, BC",
             50430, "Calgary, AB",
             28011, "Regina, SK",
             27174, "Winnipeg, MB",
             51459, "Toronto, ON",
             49568, "Ottawa, ON",
             51157, "Montreal, QC",
             26892, "Québec City, QC",
             50620, "Halifax, NS",
             50089, "St. John's, NL",
             51058, "Yellowknife, NT",
             42503, "Iqaluit, NU")

# Downloads the weather data using weathercan::weather_dl
data.weather <- weather_dl(station_ids = site.id$station_id, 
                           start = "2014-01-01", 
                           end = "2018-12-31",
                           interval = "day", 
                           quiet = TRUE, 
                           verbose = TRUE
                           ) %>%
  select(station_id:date,
         min_temp, mean_temp, max_temp, 
         total_precip, total_snow, total_rain, 
         spd_max_gust
         ) %>%
  mutate(yday = yday(date),
         year = year(date))


# For each site, checks to see how much of each column is NA. If one of the sites has too many NAs, search for an alternative one for that city 
p.missing <- data.weather %>%
  gg_miss_var(show_pct = TRUE, facet = station_id) + 
  labs(title = "Percentage of missing data")

# Join site.id and weather data by the station_id
data.plot <- left_join(site.id, data.weather, by = c("station_id"))

# Converts the city_name to a factor and orders based on their appearance in the data.frame
data.plot$city_name <- forcats::fct_inorder(data.plot$city_name)

# Plot the donuts
p.main <- ggplot() +
  geom_tile(data = data.plot,
            aes(x = yday,
                y = year,
                colour = mean_temp,
                fill = mean_temp
                )
            ) +
  expand_limits(y = 2010) + # Creates the hollow center of the donut
  coord_polar() +
  facet_wrap( ~ city_name, ncol = 4) +
  scale_x_continuous(breaks = c(1, 182),
                     label = c("January", "July")
                     ) +
  # Visual aesthetics elements below
  scale_fill_gradient2(low = "#4575b4",
                       high = "#d73027",
                       mid = "#e0f3f8",
                       midpoint = 10,
                       na.value = "grey70",
                       name = "Daily Mean Temperature (°C)",
                       breaks = c(min(data.plot$mean_temp, na.rm = T),
                                  0,
                                  max(data.plot$mean_temp, na.rm = T)
                                  ),
                       aesthetics = c("colour", "fill"),
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               label.hjust = 0.5,
                                               direction = "horizontal",
                                               frame.colour = "white",
                                               ticks = F,
                                               barwidth = 14,
                                               barheight = 1.5
                                               )
                       ) +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom"
        )
# Save a png version of the weather donut for later consumption (full figure w16, h12)
ggsave("weather_donut_example.png", p.main, device = "png", width = 16, height = 12, units = "in", dpi = 200)
