library(dplyr)

### Percentage of women in categories

percentages <- data.frame(year = numeric(), 
                          women_directors = numeric(), 
                          women_writers = numeric(), 
                          women_producers = numeric(), 
                          women_sound = numeric(),
                          women_music = numeric(),
                          women_art = numeric(),
                          women_makeup = numeric(),
                          women_costume = numeric(), 
                          stringsAsFactors = FALSE)

for (y in seq_along(movies_list)){
  
  # Percentage of women directors -------------------------------------------
  
  number_male_directors <- 0
  number_female_directors <- 0
  
  for (i in seq_along(movies_list[[y]])){
    movie_m_directors <- as.integer(movies_list[[y]][[i]]$directors_gender$male)
    movie_f_directors <- as.integer(movies_list[[y]][[i]]$directors_gender$female)
    
    number_male_directors <- number_male_directors + movie_m_directors
    number_female_directors <- number_female_directors + movie_f_directors
  }
  
  percentage_women_directors <- 100 * number_female_directors / (number_male_directors+number_female_directors) 
  
  # Percentage of women writers -------------------------------------------
  
  number_male_writers <- 0
  number_female_writers <- 0
  
  for (i in seq_along(movies_list[[y]])){
    movie_m_writers <- as.integer(movies_list[[y]][[i]]$writers_gender$male)
    movie_f_writers <- as.integer(movies_list[[y]][[i]]$writers_gender$female)
    
    number_male_writers <- number_male_writers + movie_m_writers
    number_female_writers <- number_female_writers + movie_f_writers
  }
  
  percentage_women_writers <- 100 * number_female_writers / (number_male_writers+number_female_writers) 
  
  # Percentage of women producers -------------------------------------------
  
  number_male_producers <- 0
  number_female_producers <- 0
  
  for (i in seq_along(movies_list[[y]])){
    movie_m_producers <- as.integer(movies_list[[y]][[i]]$producers_gender$male)
    movie_f_producers <- as.integer(movies_list[[y]][[i]]$producers_gender$female)
    
    number_male_producers <- number_male_producers + movie_m_producers
    number_female_producers <- number_female_producers + movie_f_producers
  }
  
  percentage_women_producers <- 100 * number_female_producers / (number_male_producers+number_female_producers) 
  
  # Percentage of women sound -------------------------------------------
  
  number_male_sound <- 0
  number_female_sound <- 0
  
  for (i in seq_along(movies_list[[y]])){
    movie_m_sound <- as.integer(movies_list[[y]][[i]]$sound_gender$male)
    movie_f_sound <- as.integer(movies_list[[y]][[i]]$sound_gender$female)
    
    number_male_sound <- number_male_sound + movie_m_sound
    number_female_sound <- number_female_sound + movie_f_sound
  }
  
  percentage_women_sound <- 100 * number_female_sound / (number_male_sound+number_female_sound) 
  
  # Percentage of women music -------------------------------------------
  
  number_male_music <- 0
  number_female_music <- 0
  
  for (i in seq_along(movies_list[[y]])){
    movie_m_music <- as.integer(movies_list[[y]][[i]]$music_gender$male)
    movie_f_music <- as.integer(movies_list[[y]][[i]]$music_gender$female)
    
    number_male_music <- number_male_music + movie_m_music
    number_female_music <- number_female_music + movie_f_music
  }
  
  percentage_women_music <- 100 * number_female_music / (number_male_music+number_female_music) 
  
  # Percentage of women art -------------------------------------------
  
  number_male_art <- 0
  number_female_art <- 0
  
  for (i in seq_along(movies_list[[y]])){
    movie_m_art <- as.integer(movies_list[[y]][[i]]$art_gender$male)
    movie_f_art <- as.integer(movies_list[[y]][[i]]$art_gender$female)
    
    number_male_art <- number_male_art + movie_m_art
    number_female_art <- number_female_art + movie_f_art
  }
  
  percentage_women_art <- 100 * number_female_art / (number_male_art+number_female_art) 
  
  # Percentage of women makeup -------------------------------------------
  
  number_male_makeup <- 0
  number_female_makeup <- 0
  
  for (i in seq_along(movies_list[[y]])){
    movie_m_makeup <- as.integer(movies_list[[y]][[i]]$makeup_gender$male)
    movie_f_makeup <- as.integer(movies_list[[y]][[i]]$makeup_gender$female)
    
    number_male_makeup <- number_male_makeup + movie_m_makeup
    number_female_makeup <- number_female_makeup + movie_f_makeup
  }
  
  percentage_women_makeup <- 100 * number_female_makeup / (number_male_makeup+number_female_makeup) 
  
  # Percentage of women costume -------------------------------------------
  
  number_male_costume <- 0
  number_female_costume <- 0
  
  for (i in seq_along(movies_list[[y]])){
    movie_m_costume <- as.integer(movies_list[[y]][[i]]$costume_gender$male)
    movie_f_costume <- as.integer(movies_list[[y]][[i]]$costume_gender$female)
    
    number_male_costume <- number_male_costume + movie_m_costume
    number_female_costume <- number_female_costume + movie_f_costume
  }
  
  percentage_women_costume <- 100 * number_female_costume / (number_male_costume+number_female_costume) 
  
  # Create a dataset percentages_y for each year ---------------------------
  
  percentages_y <- data.frame(year = 2018-y, 
                              women_directors = percentage_women_directors, 
                              women_writers = percentage_women_writers, 
                              women_producers = percentage_women_producers, 
                              women_sound = percentage_women_sound,
                              women_music = percentage_women_music,
                              women_art = percentage_women_art,
                              women_makeup = percentage_women_makeup,
                              women_costume = percentage_women_costume, 
                              stringsAsFactors = FALSE)
  
  # Combine percentages_y and percentages -------------------------------------
  
  percentages <- bind_rows(percentages, percentages_y)
  
}