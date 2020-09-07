# I'm working on maintaining my skills by modeling this anime data and making predictive models.

require(tidyverse)
require(ggplot2)
require(stringr)
require(GGally)
require(Metrics)

################## Data Cleaning and Preparing Variables ##########################

anime_raw <- read_csv("anime.csv")

# This variable is for the number of episodes, so it should be coded numeric.
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
rating_by_genre <- tibble(rating = anime$rating)
# anime_name <- tibble(anime_name = anime$name)
# rating_by_genre <- cbind(rating_by_genre, anime_name)
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

#Checking the correlations of numeric variables
ggpairs(data = anime, columns = 5:8)

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

er <- ggplot(data = anime, aes(x = log(episodes), y = rating)) + geom_point() + geom_smooth()
er

# There is some connection between rating and the number of genres present.
means_table <- anime %>% group_by(genre_tag_number) %>% summarize(ave_rating = mean(rating, na.rm = TRUE))
mt <- ggplot(data = means_table, aes(x = genre_tag_number, y = ave_rating)) + geom_point() + geom_smooth()
mt

mt2 <- ggplot(data = anime, aes(x = genre_tag_number, y = rating)) + geom_point() + geom_smooth()
mt2

# Rating vs the log of members.
mb <- ggplot(data = anime, aes(x = log(members), y = rating)) + geom_point()
mb <- mb + geom_smooth()
mb

#################### Analyses ###########################

# Models:
# genre only
# other show characteristics
# hybrid

# Dividing the data set into training and testing
numrows <- length(anime$rating)
training_indices <- sample(1:numrows, round(0.7*numrows), replace = FALSE)
testing_indices <- setdiff(1:numrows, training_indices)

# Starting with a high-dimension model estimating the rating from the presence or absense of genres.
genremodel <- glm(rating ~ ., data = rating_by_genre[training_indices,])
summary(genremodel)
# Many of the genres show significance.
genremodel <- step(genremodel)
summary(genremodel)

#genre-based model predictions
ratings_predict <- predict(genremodel, rating_by_genre[testing_indices,])
ratings_actual <- rating_by_genre$rating[testing_indices]

# a model based on other characteristics
model_2 <- glm(rating ~ genre_tag_number + I(genre_tag_number^2) + I(log(episodes)) + type,
               data = anime[training_indices,])
summary(model_2)
ratings_predict_2 <- predict(model_2, anime[testing_indices,])
rating_summary <- tibble(actual = ratings_actual, pred1 = ratings_predict, pred2 = ratings_predict_2)

rating_summary

# Now to see if we can improve the genre-based model by including variables from the second model.

# This is to unify the data frames to have a source for prediction.
anime_allvars <- cbind(rating_by_genre[,-1], anime)

model_3 <- update(genremodel, . ~ . + genre_tag_number + I(genre_tag_number^2) + type,
                  data = anime_allvars[training_indices,])
ratings_predict_3 <- predict(model_3, anime_allvars[testing_indices,])

rmse(ratings_actual, ratings_predict)
rmse(ratings_actual, ratings_predict_2)
rmse(ratings_actual, ratings_predict_3)

mape(ratings_actual, ratings_predict)
mape(ratings_actual, ratings_predict_2)
mape(ratings_actual, ratings_predict_3)
