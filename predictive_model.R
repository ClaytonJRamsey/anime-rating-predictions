require(tidyverse)
require(ggplot2)
require(rayshader)
require(stringr)

anime <- read_csv("anime.csv")

head(anime)

hist(anime$rating)
hist(anime$members)


# The below is to separate the genres from the entries in the data frame
# into discrete names.
genre_split <- function(x){
  return(str_split(x, ", "))
}
genres <- apply(matrix(anime$genre), 2, genre_split)
names(genres[[1]]) <- 1:length(genres[[1]])

# Making a variable to indicate how many genres each anime lists.
genre_tag_number <- lapply(genres[[1]], length)
genre_tag_number <- as.data.frame(do.call(rbind, genre_tag_number))

# Extract the nth genre of anime r using x <- genres[[1]][[r]][n]