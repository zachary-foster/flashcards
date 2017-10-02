## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
comment = "#>",
collapse = TRUE,
warning = FALSE,
message = FALSE,
eval = TRUE
)

## ----eval = FALSE--------------------------------------------------------
#  library(flashcards)
#  user_home_path <- tempfile() # replace with a path of your choice if you want
#  init_user_dir(user_home_path)

## ----eval = FALSE--------------------------------------------------------
#  my_deck_path <- file.path(user_home_path, "decks", "solar_system")
#  init_deck(my_deck_path, name = "Solar system",
#            description = "Planets in our solar system")

## ----eval = FALSE--------------------------------------------------------
#  add_card(my_deck_path,front = "Sun", back = "https://upload.wikimedia.org/wikipedia/commons/f/fb/Sun_in_February_%28black_version%29.jpg", source = "HalloweenNight")
#  add_card(my_deck_path,front = "Sun", back = "Center of the solar system")
#  add_card(my_deck_path,front = "Sun", back = "Hydrogen fuses into helium here")
#  add_card(my_deck_path,front = "Mercury", back = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Mercury_in_color_-_Prockter07-edit1.jpg/600px-Mercury_in_color_-_Prockter07-edit1.jpg", source = "NASA/JPL")
#  add_card(my_deck_path,front = "Mercury", back = "First planet from the Sun")
#  add_card(my_deck_path,front = "Mercury", back = "Smallest planet in the solar system")
#  add_card(my_deck_path,front = "Venus", back = "https://upload.wikimedia.org/wikipedia/commons/e/e5/Venus-real_color.jpg", source = "Ricardo Nunes")
#  add_card(my_deck_path,front = "Venus", back = "Second planet from the Sun")
#  add_card(my_deck_path,front = "Venus", back = "Rotates in the opposite direction to most other planets")
#  add_card(my_deck_path,front = "Earth", back = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/97/The_Earth_seen_from_Apollo_17.jpg/599px-The_Earth_seen_from_Apollo_17.jpg", source = "NASA")
#  add_card(my_deck_path,front = "Earth", back = "Third planet from the Sun")
#  add_card(my_deck_path,front = "Earth", back = "The only object in the Universe known to harbor life")
#  add_card(my_deck_path,front = "Mars", back = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/02/OSIRIS_Mars_true_color.jpg/600px-OSIRIS_Mars_true_color.jpg", source = "ESA")
#  add_card(my_deck_path,front = "Mars", back = "Fourth planet from the Sun")
#  add_card(my_deck_path,front = "Mars", back = "Reddish iron oxide gives it a reddish appearance")
#  add_card(my_deck_path,front = "Jupiter", back = "https://upload.wikimedia.org/wikipedia/commons/2/2b/Jupiter_and_its_shrunken_Great_Red_Spot.jpg")
#  add_card(my_deck_path,front = "Jupiter", back = "Fifth planet from the Sun")
#  add_card(my_deck_path,front = "Jupiter", back = "Mass one-thousandth that of the Sun")
#  add_card(my_deck_path,front = "Saturn", back = "https://upload.wikimedia.org/wikipedia/commons/c/c7/Saturn_during_Equinox.jpg", source = "NASA")
#  add_card(my_deck_path,front = "Saturn", back = "Sixth planet from the Sun")
#  add_card(my_deck_path,front = "Saturn", back = "Second-largest planet in the Solar System")
#  add_card(my_deck_path,front = "Uranus", back = "https://upload.wikimedia.org/wikipedia/commons/3/3d/Uranus2.jpg", source = "NASA")
#  add_card(my_deck_path,front = "Uranus", back = "Seventh planet from the Sun")
#  add_card(my_deck_path,front = "Uranus", back = "Sideways axis of rotation")
#  add_card(my_deck_path,front = "Neptune", back = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Neptune_Full_%28cropped%29.jpg/1024px-Neptune_Full_%28cropped%29.jpg")
#  add_card(my_deck_path,front = "Neptune", back = "Eighth and farthest known planet from the Sun")
#  add_card(my_deck_path,front = "Neptune", back = "Coldest planet in the Solar System")

## ----eval = FALSE--------------------------------------------------------
#  practice(user_dir = user_home_path)

## ----eval = FALSE--------------------------------------------------------
#  plot_progress(user_dir = user_home_path)

