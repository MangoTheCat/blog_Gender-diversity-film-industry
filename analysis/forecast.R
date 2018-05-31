library(forecast)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

### Forecast 

# Time series
ts <- ts(percentages, start = 2007, end = 2017, frequency = 1)

# Auto forecast directors 2018
arma_fit_director <- auto.arima(ts[ ,2])
arma_forecast_director <- forecast(arma_fit_director, h = 1)
dir_2018 <- arma_forecast_director$fitted[1] # value predicted

# Auto forecast writers 2018
arma_fit_writer <- auto.arima(ts[ ,3])
arma_forecast_writer <- forecast(arma_fit_writer, h = 1)
writ_2018 <- arma_forecast_writer$fitted[1] # value predicted

# Idem for producers, sound, music, art, makeup and costume

# Create a data frame for 2018 fitted values
percentages_2018 <- data.frame(year = ymd(2018, truncated = 2L), 
                               women_directors = dir_2018, 
                               women_writers = writ_2018, 
                               women_producers = prod_2018, 
                               women_sound = sound_2018,
                               women_music = music_2018,
                               women_art = art_2018,
                               women_makeup = makeup_2018,
                               women_costume = costu_2018, 
                               stringsAsFactors = FALSE)

# Values from 2007 to 2017 + 2018 fitted values
percentages_fitted_2018 <- bind_rows(percentages, percentages_2018)

# From wide to long dataframe
colnames(percentages_fitted_2018) <- c("year", "directors", "writers","producers", "sound",    
                                       "music", "art", "makeup", "costume")
percentages_long_f2018 <- percentages_fitted_2018 %>%
  gather(key = category, value = percentage, -year)
percentages_long_f2018$year <- ymd(percentages_long_f2018$year, truncated = 2L) # year as date

# Forecast plot for 2018 
forecast_2018 <- ggplot(percentages_long_f2018, aes(x = year,
                                                    y = percentage,
                                                    group = category,
                                                    colour = category)) +
  geom_line(size = 2)+
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) + # center the title
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = brewer.pal(8, "Set1")) +
  labs(title = "Percentages of women in the film industry from 2007 to 2017\n Fitted values for 2018",
       x = "",
       y = "Percentages")

forecast_2018