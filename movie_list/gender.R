library(plyr)
library(genderizeR)

### Genderize our lists of names

# for each year
for (y in seq_along(movies_list)){ 
  # for each movie
  for (i in seq_along(movies_list[[y]])){
    
    # Genderize directors -----------------------------------------------------
    directors <- movies_list[[y]][[i]]$directors
    
    if (directors == ""){
      directors_gender <- list()
      directors_gender$male <- 0
      directors_gender$female <- 0
      movies_list[[y]][[i]]$directors_gender <- directors_gender
    }
    
    else{
      # Split the firstnames and the lastnames
      # Keep the firstnames
      directors <- strsplit(directors, " ")
      l <- c()
      for (j in seq_along(directors)){
        l <- c(l, directors[[j]][1])
      }
      
      directors <- l
      movie_directors_male <- 0
      movie_directors_female <- 0
      
      # Genderize every firstname and count the number of males and females 
      for (p in seq_along(directors)){
        directors_gender <- genderizeAPI(x = directors[p], apikey = "233b284134ae754d9fc56717fec4164e")
        gender <- directors_gender$response$gender
        if (length(gender)>0 && gender == "male"){
          movie_directors_male <- movie_directors_male + 1
        }
        if (length(gender)>0 && gender == "female"){
          movie_directors_female <- movie_directors_female + 1
        }
      }
      
      # Put the number of males and females in movies_list
      directors_gender <- list()
      directors_gender$male <- movie_directors_male
      directors_gender$female <- movie_directors_female
      movies_list[[y]][[i]]$directors_gender <- directors_gender
    }  
    
    
    # Genderize writers -----------------------------------------------------
    writers <- movies_list[[y]][[i]]$writers
    
    if (writers == ""){
      writers_gender <- list()
      writers_gender$male <- 0
      writers_gender$female <- 0
      movies_list[[y]][[i]]$writers_gender <- writers_gender
    }
    
    else{
      # Split the firstnames and the lastnames
      # Keep the firstnames
      writers <- strsplit(writers, " ")
      l <- c()
      for (j in seq_along(writers)){
        l <- c(l, writers[[j]][1])
      }
      
      writers <- l
      movie_writers_male <- 0
      movie_writers_female <- 0
      
      # Genderize every firstname and count the number of males and females 
      for (p in seq_along(writers)){
        writers_gender <- genderizeAPI(x = writers[p], apikey = "233b284134ae754d9fc56717fec4164e")
        gender <- writers_gender$response$gender
        if (length(gender)>0 && gender == "male"){
          movie_writers_male <- movie_writers_male + 1
        }
        if (length(gender)>0 && gender == "female"){
          movie_writers_female <- movie_writers_female + 1
        }
      }
      
      # Put the number of males and females in movies_list
      writers_gender <- list()
      writers_gender$male <- movie_writers_male
      writers_gender$female <- movie_writers_female
      movies_list[[y]][[i]]$writers_gender <- writers_gender
    }  
    
    # Genderize producers -----------------------------------------------------
    producers <- movies_list[[y]][[i]]$producers
    
    if (producers == ""){
      producers_gender <- list()
      producers_gender$male <- 0
      producers_gender$female <- 0
      movies_list[[y]][[i]]$producers_gender <- producers_gender
    }
    
    else{
      # Split the firstnames and the lastnames
      # Keep the firstnames
      producers <- strsplit(producers, " ")
      l <- c()
      for (j in seq_along(producers)){
        l <- c(l, producers[[j]][1])
      }
      
      producers <- l
      movie_producers_male <- 0
      movie_producers_female <- 0
      
      # Genderize every firstname and count the number of males and females 
      for (p in seq_along(producers)){
        producers_gender <- genderizeAPI(x = producers[p], apikey = "233b284134ae754d9fc56717fec4164e")
        gender <- producers_gender$response$gender
        if (length(gender)>0 && gender == "male"){
          movie_producers_male <- movie_producers_male + 1
        }
        if (length(gender)>0 && gender == "female"){
          movie_producers_female <- movie_producers_female + 1
        }
      }
      
      # Put the number of males and females in movies_list
      producers_gender <- list()
      producers_gender$male <- movie_producers_male
      producers_gender$female <- movie_producers_female
      movies_list[[y]][[i]]$producers_gender <- producers_gender
    }  
    
    # Genderize sound -----------------------------------------------------
    sound <- movies_list[[y]][[i]]$sound
    
    if (sound == ""){
      sound_gender <- list()
      sound_gender$male <- 0
      sound_gender$female <- 0
      movies_list[[y]][[i]]$sound_gender <- sound_gender
    }
    
    else{
      # Split the firstnames and the lastnames
      # Keep the firstnames
      sound <- strsplit(sound, " ")
      l <- c()
      for (j in seq_along(sound)){
        l <- c(l, sound[[j]][1])
      }
      
      sound <- l
      movie_sound_male <- 0
      movie_sound_female <- 0
      
      # Genderize every firstname and count the number of males and females 
      for (p in seq_along(sound)){
        sound_gender <- genderizeAPI(x = sound[p], apikey = "233b284134ae754d9fc56717fec4164e")
        gender <- sound_gender$response$gender
        if (length(gender)>0 && gender == "male"){
          movie_sound_male <- movie_sound_male + 1
        }
        if (length(gender)>0 && gender == "female"){
          movie_sound_female <- movie_sound_female + 1
        }
      }
      
      # Put the number of males and females in movies_list
      sound_gender <- list()
      sound_gender$male <- movie_sound_male
      sound_gender$female <- movie_sound_female
      movies_list[[y]][[i]]$sound_gender <- sound_gender
    }  
    
    # Genderize music -----------------------------------------------------
    music <- movies_list[[y]][[i]]$music
    
    if (music == ""){
      music_gender <- list()
      music_gender$male <- 0
      music_gender$female <- 0
      movies_list[[y]][[i]]$music_gender <- music_gender
    }
    
    else{
      # Split the firstnames and the lastnames
      # Keep the firstnames
      music <- strsplit(music, " ")
      l <- c()
      for (j in seq_along(music)){
        l <- c(l, music[[j]][1])
      }
      
      music <- l
      movie_music_male <- 0
      movie_music_female <- 0
      
      # Genderize every firstname and count the number of males and females 
      for (p in seq_along(music)){
        music_gender <- genderizeAPI(x = music[p], apikey = "233b284134ae754d9fc56717fec4164e")
        gender <- music_gender$response$gender
        if (length(gender)>0 && gender == "male"){
          movie_music_male <- movie_music_male + 1
        }
        if (length(gender)>0 && gender == "female"){
          movie_music_female <- movie_music_female + 1
        }
      }
      
      # Put the number of males and females in movies_list
      music_gender <- list()
      music_gender$male <- movie_music_male
      music_gender$female <- movie_music_female
      movies_list[[y]][[i]]$music_gender <- music_gender
    }  
    
    # Genderize art -----------------------------------------------------
    art <- movies_list[[y]][[i]]$art
    
    if (art == ""){
      art_gender <- list()
      art_gender$male <- 0
      art_gender$female <- 0
      movies_list[[y]][[i]]$art_gender <- art_gender
    }
    
    else{
      # Split the firstnames and the lastnames
      # Keep the firstnames
      art <- strsplit(art, " ")
      l <- c()
      for (j in seq_along(art)){
        l <- c(l, art[[j]][1])
      }
      
      art <- l
      movie_art_male <- 0
      movie_art_female <- 0
      
      # Genderize every firstname and count the number of males and females 
      for (p in seq_along(art)){
        art_gender <- genderizeAPI(x = art[p], apikey = "233b284134ae754d9fc56717fec4164e")
        gender <- art_gender$response$gender
        if (length(gender)>0 && gender == "male"){
          movie_art_male <- movie_art_male + 1
        }
        if (length(gender)>0 && gender == "female"){
          movie_art_female <- movie_art_female + 1
        }
      }
      
      # Put the number of males and females in movies_list
      art_gender <- list()
      art_gender$male <- movie_art_male
      art_gender$female <- movie_art_female
      movies_list[[y]][[i]]$art_gender <- art_gender
    }  
    
    # Genderize makeup -----------------------------------------------------
    makeup <- movies_list[[y]][[i]]$makeup
    
    if (makeup == ""){
      makeup_gender <- list()
      makeup_gender$male <- 0
      makeup_gender$female <- 0
      movies_list[[y]][[i]]$makeup_gender <- makeup_gender
    }
    
    else{
      # Split the firstnames and the lastnames
      # Keep the firstnames
      makeup <- strsplit(makeup, " ")
      l <- c()
      for (j in seq_along(makeup)){
        l <- c(l, makeup[[j]][1])
      }
      
      makeup <- l
      movie_makeup_male <- 0
      movie_makeup_female <- 0
      
      # Genderize every firstname and count the number of males and females 
      for (p in seq_along(makeup)){
        makeup_gender <- genderizeAPI(x = makeup[p], apikey = "233b284134ae754d9fc56717fec4164e")
        gender <- makeup_gender$response$gender
        if (length(gender)>0 && gender == "male"){
          movie_makeup_male <- movie_makeup_male + 1
        }
        if (length(gender)>0 && gender == "female"){
          movie_makeup_female <- movie_makeup_female + 1
        }
      }
      
      # Put the number of males and females in movies_list
      makeup_gender <- list()
      makeup_gender$male <- movie_makeup_male
      makeup_gender$female <- movie_makeup_female
      movies_list[[y]][[i]]$makeup_gender <- makeup_gender
    }  
    
    # Genderize costume -----------------------------------------------------
    costume <- movies_list[[y]][[i]]$costume
    
    if (costume == ""){
      costume_gender <- list()
      costume_gender$male <- 0
      costume_gender$female <- 0
      movies_list[[y]][[i]]$costume_gender <- costume_gender
    }
    
    else{
      # Split the firstnames and the lastnames
      # Keep the firstnames
      costume <- strsplit(costume, " ")
      l <- c()
      for (j in seq_along(costume)){
        l <- c(l, costume[[j]][1])
      }
      
      costume <- l
      movie_costume_male <- 0
      movie_costume_female <- 0
      
      # Genderize every firstname and count the number of males and females 
      for (p in seq_along(costume)){
        costume_gender <- genderizeAPI(x = costume[p], apikey = "233b284134ae754d9fc56717fec4164e")
        gender <- costume_gender$response$gender
        if (length(gender)>0 && gender == "male"){
          movie_costume_male <- movie_costume_male + 1
        }
        if (length(gender)>0 && gender == "female"){
          movie_costume_female <- movie_costume_female + 1
        }
      }
      
      # Put the number of males and females in movies_list
      costume_gender <- list()
      costume_gender$male <- movie_costume_male
      costume_gender$female <- movie_costume_female
      movies_list[[y]][[i]]$costume_gender <- costume_gender
    } 
  }
}