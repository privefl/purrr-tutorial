library(repurrrsive)

str(sw_people, max.level = 1)
length(sw_people)

sw_people[[1]][["name"]]

library(purrr)

length(sw_people[[1]][["starships"]])
map(sw_people, ~length(.x[["starships"]]))

sw_people[[1]][["starships"]]

## find the names of the starships
ind <- match(sw_people[[1]][["starships"]], 
      map(sw_starships, ~.x[["url"]]))

map(sw_starships[ind], ~.x[["name"]])


planet_lookup <- map_chr(sw_planets, "name") %>%
  set_names(map_chr(sw_planets, "url"))

luke <- sw_people[[1]]
planet_lookup[match(luke[["homeworld"]], map_chr(sw_planets, "url"))]
planet_lookup[match(map_chr(sw_people, "homeworld"), 
                    map_chr(sw_planets, "url"))]

## But, planet_lookup is named!
planet_lookup[luke$homeworld]
planet_lookup[map_chr(sw_people, "homeworld")]


## Use walk if you want to return nothing, just side effect

sw_people <- sw_people %>% set_names(map_chr(sw_people, "name"))
map_int(sw_people, ~length(.x[["starships"]]))
map(sw_people, "starships") %>% map_int(length)
map_chr(sw_people, ~.x[["hair_color"]])
map_chr(sw_people, "hair_color")
map_lgl(sw_people, ~.x[["gender"]] == "male")
map_chr(sw_people, "mass") %>%
  readr::parse_number(na = "unknown")


map(sw_films, "characters") %>% 
  # map_int(length) %>%
  lengths() %>%
  set_names(map_chr(sw_films, "title")) %>%
  sort(decreasing = TRUE)

map_chr(sw_species, "eye_colors") %>%
  stringr::str_split(stringr::fixed(", ")) %>%
  map_int(length) %>% 
  which.max()
  
map_int(sw_planets, ~map_lgl(.x, ~identical(.x, "unknown")) %>%
  sum()) # TODO: verify




# map2

gap_split_small <- gap_split[1:10]
countries <- names(gap_split_small)

c1 <- gap_split_small[[1]]
ggplot(c1, aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle(countries[[1]])

map2(gap_split_small, countries, ~{
  ggplot(.x, aes(year, lifeExp)) + 
    geom_line() + 
    ggtitle(.y)
})

walk2(gap_split_small, countries, ~{
  p <- ggplot(.x, aes(year, lifeExp)) + 
    geom_line() + 
    ggtitle(.y)
  ggsave(paste0(.y, ".pdf"))
})

file.remove(paste0(countries, ".pdf"))


library(tidyverse)
library(repurrrsive)
# A useful lookup table -----------------------------------------------
film_number_lookup <- map_chr(sw_films, "url") %>%
  map(~ stringr::str_split_fixed(.x, "/", 7)[, 6]) %>%
  as.numeric() %>%
  set_names(map_chr(sw_films, "url"))
people_tbl <- tibble(
  name
  = sw_people %>% map_chr("name"),
  films
  = sw_people %>% map("films"),
  height = sw_people %>% map_chr("height") %>%
    readr::parse_number(na = "unknown"),
  species = sw_people %>% map_chr("species", .null = NA_character_)
)
# Turning parts of our list to a tibble ---------------------------------
people_tbl$films
# Use map with mutate to manipulate list columns
people_tbl <- people_tbl %>%
  mutate(
    film_numbers = map(films,
                       ~ film_number_lookup[.x]),
    n_films = map_int(films, length)
  )
people_tbl %>% select(name, film_numbers, n_films)


people_tbl %>%
  mutate(films_squashed = map_chr(film_numbers, 
                                  ~paste(sort(.x), collapse = ", "))) %>%
  select(name, films_squashed)

