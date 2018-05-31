library("xml2")
library("rvest")
library("magrittr")

### movies_list creation

movies_list <- list()

for (r in seq_len(nrow(top_movies))) {
  
  # FOCUS ON EACH MOVIE -----------------------------------------------------------------
  movie_name <- top_movies[r, "movie_name"]
  movie_year <- as.character(top_movies[r, "movie_year"])
  page <- read_html(as.character(top_movies[r, "movie_crewlink"]))
  
  # GATHER THE CREW NAMES FOR THIS MOVIE ------------------------------------------------
  movie_allcrew <- html_nodes(page, '.name , .dataHeaderWithBorder') %>%
    html_text()
  movie_allcrew <- gsub("[\n]", "", movie_allcrew) %>%
    trimws() #Remove white spaces 
  
  # SPLIT THE CREW NAMES BY CATEGORY ----------------------------------------------------
  movie_categories <- html_nodes(page, '.dataHeaderWithBorder') %>%
    html_text()
  movie_categories <- gsub("[\n]", "", movie_categories) %>%
    trimws() #Remove white spaces
  
  ## DIRECTORS --------------------------------------------------------------------------
  movie_directors <- c()
  for (i in 1:(length(movie_allcrew)-1)){ # I Can use `1:length(vec)` as I now movie_allcrew is not empty
    if (grepl("Directed by", movie_allcrew[i])){
      j <- 1
      k <- i+1
      while (! grepl("Writing Credits", movie_allcrew[k]) && ! grepl("Cast", movie_allcrew[k])){ # 1 movie with no writing credits
        movie_directors <- c(movie_directors, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  if (length(movie_directors) == 0){
    movie_directors <- c("")
  }
  

  ## WRITERS ---------------------------------------------------------------
  movie_writers <- c()
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Writing Credits", movie_allcrew[i])){
      j <- 1
      while (! grepl("Writing Credits", movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+2], movie_allcrew[k])){ #j+2 --> problem with grepl(cast)
        movie_writers <- c(movie_writers, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  if (length(movie_writers) == 0){
    movie_writers <- c("")
  } else{
    movie_writers <- head(movie_writers, -1) #Remove the last element (cast)
  }
  
  ## PRODUCERS ---------------------------------------------------------------
  movie_producers <- c()
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Produced by", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_producers <- c(movie_producers, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  if (length(movie_producers) == 0){
    movie_producers <- c("")
  }
  
  ## SOUND DEPARTMENT ------------------------------------------------------------------ 
  movie_sound <- c()
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Sound Department", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_sound <- c(movie_sound, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  if (length(movie_sound) == 0){
    movie_sound <- c("")
  }
  
  ## MUSIC DEPARTMENT -------------------------------------------------------------------
  movie_music <- c()
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Music by", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_music <- c(movie_music, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Music Department", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_music <- c(movie_music, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  if (length(movie_music) == 0){
    movie_music <- c("")
  }
  
  ## ART DEPARTMENT ----------------------------------------------------------------------
  movie_art <- c()
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Art Direction by", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_art <- c(movie_art, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Art Department", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_art <- c(movie_art, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  if (length(movie_art) == 0){
    movie_art <- c("")
  }
  
  ## MAKEUP DEPARTMENT ------------------------------------------------------------------
  movie_makeup <- c()
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Makeup Department", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_makeup <- c(movie_makeup, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  if (length(movie_makeup) == 0){
    movie_makeup <- c("")
  }
  
  ## COSTUME DEPARTMENT ------------------------------------------------------------------
  movie_costume <- c()
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Costume Design by", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_costume <- c(movie_costume, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  for (i in 1:(length(movie_allcrew)-1)){
    if (grepl("Costume and Wardrobe Department", movie_allcrew[i])){
      j <- 1
      while (! grepl(movie_allcrew[i], movie_categories[j])){
        j <- j+1
      }
      k <- i+1
      while (! grepl(movie_categories[j+1], movie_allcrew[k])){
        movie_costume <- c(movie_costume, movie_allcrew[k])
        k <- k+1
      }
    }
  }
  if (length(movie_costume) == 0){
    movie_costume <- c("")
  }
  

  ## MOVIE_INFO CONTAINS THE MOVIE CREW NAMES ORDERED BY CATEGORY ----------------------
  movie_info <- list()
  movie_info$directors <- movie_directors
  movie_info$writers <- movie_writers
  movie_info$producers <- movie_producers
  movie_info$sound <- movie_sound
  movie_info$music <- movie_music
  movie_info$art <- movie_art
  movie_info$makeup <- movie_makeup
  movie_info$costume <- movie_costume
  
  ## MOVIES_LIST GATHERS THE INFORMATION FOR EVERY YEAR AND EVERY MOVIE ----------------
  movies_list[[movie_year]][[movie_name]] <- movie_info
  
}