library(dplyr)

### Gather the 11 data frames into 1

top_movies <- bind_rows(top_2017, top_2016, top_2015,
                        top_2014, top_2013, top_2012,
                        top_2011, top_2010, top_2009,
                        top_2008, top_2007)