require(tidyverse)
require(ggplot2)
require(rayshader)

anime <- read_csv("anime.csv")

head(anime)

hist(anime$rating)
hist(anime$members)

