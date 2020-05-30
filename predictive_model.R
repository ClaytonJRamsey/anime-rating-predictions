require(tidyverse)
require(ggplot2)
require(rayshader)
require(stringr)

anime <- read_csv("anime.csv")

# Separating the genres from the entries in the data frame
# into discrete names.
genre_split <- function(x){
  return(str_split(x, ", "))
}
genres <- apply(matrix(anime$genre), 2, genre_split)
names(genres[[1]]) <- 1:length(genres[[1]])

# Making a variable to indicate how many genres each anime lists.
genre_tag_number <- lapply(genres[[1]], length)
genre_tag_number <- as.data.frame(do.call(rbind, genre_tag_number))
names(genre_tag_number) = "genre_tag_number"
anime <- cbind(anime, genre_tag_number)
rm(genre_tag_number)

#Making a comprehensive list of all listed genres:

# Extract the nth genre of anime r using x <- genres[[1]][[r]][n]

# Some visualizations:
hist(anime$rating)
hist(anime$members)
hist(anime$genre_tag_number)

plot(anime$genre_tag_number, anime$rating)
rating_vs_genre_no <- lm(rating ~ genre_tag_number, data = anime)
summary(rating_vs_genre_no)
hist(rating_vs_genre_no$residuals)
plot(rating_vs_genre_no$fitted.values, rating_vs_genre_no$residuals)

plot(log(anime$members), anime$rating)
g <- ggplot(data = anime, aes(x = genre_tag_number, y = log(members), color = rating))
(g <- g + scale_color_gradient(low = "red", high = "green") + geom_point())
