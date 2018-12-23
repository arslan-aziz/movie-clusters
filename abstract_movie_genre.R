library(tidyverse)
library(jsonlite)
movie_genre <- read_csv('movies_metadata.csv')
movie_genre <- movie_genre %>% select(id, original_title, genres)
movie_genre$genres <- gsub("'", '"', movie_genre$genres)
movie_genre$genres <- purrr::map(movie_genre$genres, jsonlite::fromJSON)
movie_genre$tmplen <- unlist(lapply(movie_genre$genres, length))

movie_genre <- movie_genre %>% filter(tmplen>0) %>% unnest()

movie_genre <- movie_genre %>% select(-tmplen)

names(movie_genre) <- c('movieId', 'original_title', 'genre_id', 'genre_name')

save(movie_genre, file="movie_genreID_genre.Rdata")

ratings <- read_csv('the-movies-dataset/ratings.csv')
