require(tidyverse)
require(ggplot2)
require(rayshader)
require(stringr)

################## Data Cleaning and Preparing Variables ##########################

anime_raw <- read_csv("anime.csv")

anime_raw <- anime_raw %>% mutate(episodes = as.integer(episodes))

anime <- anime_raw[which(complete.cases(anime_raw)),]
rm(anime_raw)

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

# This transformed variable
anime <- anime %>% mutate(genre_member_ratio = 10 - 5*anime$genre_tag_number/log(anime$members))

# Making a comprehensive list of all listed genres:
# Extract the nth genre of anime r using x <- genres[[1]][[r]][n]
all_genres <- character()
for(i in 1:length(genres[[1]])){
  for(j in 1:length(genres[[1]][[i]])){
    # Append the genre if it isn't there yet.
    if(!(genres[[1]][[i]][j] %in% all_genres)){
      all_genres <- append(all_genres, genres[[1]][[i]][j])
    }
  }
}

# This is to make factor variables indicating the presence of each genre for each anime.
rating_by_genre <- tibble(anime$rating)
for(i in 1:length(all_genres)){
  #create a variable for that genre:
  x <- integer(length(anime$genre))
  #populate the variable:
  for (j in 1:length(anime$genre)){
    x[j] <- all_genres[i] %in% genres[[1]][[j]]
  }
  # Attach the newly created variable to the ratings:
  x <- x %>% as_factor() %>% as_tibble()
  names(x) <- all_genres[i]
  rating_by_genre <- cbind(rating_by_genre, x)
}

rm(i); rm(j);


######################### Visualizations #####################################

#How many genre tags the entry contains:
gt <- ggplot(data = anime, aes(genre_tag_number)) + geom_bar()
gt

# Rating is the response variable
r <- ggplot(data = anime, aes(rating)) + geom_histogram()
r

# Members is the number of members of the group
m <- ggplot(data = anime, aes(members)) + geom_histogram()
m

# Rating vs. type
type_table <- anime %>% group_by(type) %>% summarize(ave_rating = mean(rating, na.rm = TRUE))

g <- ggplot(data = anime, aes(x = type, y = rating)) + geom_boxplot()
g 
type_table

# How many episodes:
e <- ggplot(data = anime, aes(episodes)) + geom_histogram()
e

# There is some connection between rating and the number of genres present.
means_table <- anime %>% group_by(genre_tag_number) %>% summarize(ave_rating = mean(rating, na.rm = TRUE))
mt <- ggplot(data = means_table, aes(x = genre_tag_number, y = ave_rating)) + geom_point()
mt

# Rating vs the log of members.
mb <- ggplot(data = anime, aes(x = log(members), y = rating)) + geom_point()
mb
