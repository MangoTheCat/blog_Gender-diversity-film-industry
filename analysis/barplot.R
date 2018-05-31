library(ggplot2)
library(RColorBrewer)

### Barplot of year 2017 

# Formating our dataframe
percentages_t <- data.frame(t(percentages), stringsAsFactors = FALSE)
colnames(percentages_t) <- percentages_t[1, ]
percentages_t <- percentages_t[-1, ]
rownames(percentages_t) <- c("directors", "writers", "producers", "sound", "music", "art", "makeup", "costume")

# Ploting our barplot
percentages_2017 <- percentages_t$`2017`
y <- as.matrix(percentages_2017)

p <- ggplot(percentages_t, aes(x = rownames(percentages_t),
                               y = percentages_2017, 
                               fill = rownames(percentages_t))) + 
  geom_bar(stat = "identity") +
  coord_flip() + # Horizontal bar plot
  geom_text(aes(label=format(y, digits = 2)), hjust=-0.1, size=3.5) + # pecentages next to bars
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5)) + # center the title
  labs(title = "Percentages of women in the film industry in 2017") +
  guides(fill = guide_legend(reverse=TRUE)) + # reverse the order of the legend
  scale_fill_manual(values = brewer.pal(8, "Spectral")) # palette used to fill the bars and legend boxs

p