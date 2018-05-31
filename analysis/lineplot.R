library(magrittr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

### Evolution through the last decade 

# From wide to long dataframe
colnames(percentages) <- c("year", "directors", "writers","producers", "sound",    
                           "music", "art", "makeup", "costume")
percentages_long <- percentages %>%
  gather(key = category, value = percentage, -year)
percentages_long$year <- ymd(percentages_long$year, truncated = 2L) # year as date 

# line plot
evolution_10 <- ggplot(percentages_long, aes(x = year,
                                             y = percentage,
                                             group = category,
                                             colour = category)) +
  geom_line(size = 2) +
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) + # center the title
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = brewer.pal(8, "Set1")) +
  labs(title = "Percentages of women in the film industry from 2007 to 2017",
       x = "",
       y = "Percentages")

evolution_10