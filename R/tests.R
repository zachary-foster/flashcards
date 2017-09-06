#' Get names of available tests
#'
#' Get names of available tests
#'
#' @export
test_names <- function() {
  names(available_tests())
}

#' List of available test
#'
#' Return the list of available tests.
#'
#' @keywords internal
available_tests <- function() {
  list("review" = test_review,
       "choose" = test_choose)
}


#' Review a card
#'
#' Review a card by flipping it over until moving to the next test.
#'
#' @param card The index of the card to use
#' @param deck The table containing the deck information.
#'
#' @keywords internal
test_review <- function(card, deck) {
  deck_path <- deck$deck_path[card]
  front <- deck$front[card]
  back <- deck$back[card]

  my_print("Press [enter] to flip card. Type 'c' to finish.")
  input = ""
  count = 0
  while (input != "c") {
    plot(plot_card_side(c(front, back)[count %% 2 + 1], deck_path = deck_path))
    input <- readline()
    count <- count + 1
  }

  # Return results
  data.frame(front_hash = deck$front_hash[card],
             back_hash = deck$back_hash[card],
             right = 0.1,
             wrong = 0,
             updated = date(),
             deck_name = basename(deck_path),
             test_name = "review")

}


#' Multiple choice test
#'
#' Display one side of multiple cards and ask which matches the other side of
#' another card shown.
#'
#' @param card The index of the card to use
#' @param deck The table containing the deck information.
#' @param max_choices The maximum number of cards to show.
#'
#' @keywords internal
test_choose <- function(card, deck, max_choices = 4) {
  # Pick side and card to test
  side_index <- sample.int(2, 1)
  key_side <- deck[[c("front", "back")[side_index]]]
  test_side <- deck[[c("front", "back")[- side_index]]]
  key_card <- key_side[card]

  # Pick some choices
  wrong_indexes <- sample.int(nrow(deck))
  wrong_indexes <- wrong_indexes[! duplicated(test_side[wrong_indexes])]
  wrong_indexes <- wrong_indexes[card != wrong_indexes]
  if (length(wrong_indexes) > max_choices - 1) {
    wrong_indexes <- wrong_indexes[sample.int(max_choices - 1)]
  }
  test_indexes <- c(card, wrong_indexes)
  test_indexes <- test_indexes[sample.int(length(test_indexes))]
  test_cards <- test_side[test_indexes]

  # Present test
  test_plots <- lapply(test_indexes,
                       function(i) plot_card_side(test_side[i], deck_path = deck$deck_path[i]))
  test_selection <- cowplot::plot_grid(plotlist = test_plots,
                                       labels = seq_along(test_plots),
                                       label_size = 30,
                                       label_colour = "#777777")
  key_plot <- plot_card_side(key_card, deck_path = deck$deck_path[card])
  print(cowplot::plot_grid(key_plot, test_selection, ncol = 1))

  # Get user input
  my_print("Enter the number that corresponds to the match:")
  input = ""
  count = 0
  while (! input %in% seq_along(test_cards)) {
    input <- readline()
    count <- count + 1
  }

  # Score test
  if (input == which(card == test_indexes)) { # Right!
    return(data.frame(front_hash = deck$front_hash[card],
                      back_hash = deck$back_hash[card],
                      right = 1,
                      wrong = 0,
                      updated = date(),
                      deck_name = basename(deck$deck_path[card]),
                      test_name = "choice"))
  } else { # Wrong!
    return(data.frame(front_hash = deck$front_hash[c(card, wrong_indexes)],
                      back_hash = deck$back_hash[c(card, wrong_indexes)],
                      right = 0,
                      wrong = c(-1, rep(-0.1, length(wrong_indexes))),
                      updated = date(),
                      deck_name = basename(deck$deck_path[c(card, wrong_indexes)]),
                      test_name = "choice"))

  }

}


#' Plot an image
#'
#' Plot an image from a file path
#'
#' @param path The path to the image to plot
#'
#' @keywords internal
plot_image <- function(path) {
  plot(magick::image_read(path))
  my_plot <- recordPlot()
  plot.new() ## clean up device
  return(my_plot)
}


#' Plot text
#'
#' Plot text
#'
#' @param text The text to plot
#'
#' @keywords internal
plot_text <- function(text) {
  ggplot2::ggplot(data = data.frame(label = text)) +
    ggfittext::geom_fit_text(label = text, xmin = 0, xmax = 1, ymin = 0, ymax = 1, grow = TRUE) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
}


#' Plot a card
#'
#' Plot a card. If it is a image file path, the contents of file is plotted.
#'
#' @param card The content of the side of a card
#'
#' @keywords internal
plot_card_side <- function(card, deck_path) {
  img_path <- file.path(deck_path, "images", card)

  if (file.exists(img_path)) {
    return(plot_image(img_path))
  } else {
    return(plot_text(card))
  }
}
