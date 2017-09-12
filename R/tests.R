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
  data.frame(front = deck$front[card],
             back = deck$back[card],
             front_hash = deck$front_hash[card],
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
#' @param pick_multiple If \code{TRUE}, allow multiple correct answers
#'
#' @keywords internal
test_choose <- function(card, deck, max_choices = 4, pick_multiple = TRUE) {
  # Pick side and card to test
  sides <- c("front", "back")[sample.int(2)]
  side_hashes <- paste0(sides[1], "_hash")
  answer_side <- deck[[sides[1]]]
  answer_hashes <- deck[[side_hashes[1]]]
  option_side <- deck[[sides[2]]]
  option_hashes <- deck[[side_hashes[2]]]
  answer_card <- answer_side[card]

  # Pick some choices
  wrong_indexes <- sample.int(nrow(deck))
  # wrong_indexes <- wrong_indexes[! duplicated(option_side[wrong_indexes])]
  wrong_indexes <- wrong_indexes[card != wrong_indexes]
  if (length(wrong_indexes) > max_choices - 1) {
    wrong_indexes <- wrong_indexes[sample.int(max_choices - 1)]
  }
  option_indexes <- c(card, wrong_indexes)
  option_indexes <- option_indexes[sample.int(length(option_indexes))]
  test_cards <- option_side[option_indexes]

  # Present test
  option_plots <- lapply(option_indexes,
                         function(i) plot_card_side(option_side[i], deck_path = deck$deck_path[i]))
  options <- cowplot::plot_grid(plotlist = option_plots,
                                labels = seq_along(option_plots),
                                label_size = 30,
                                label_colour = "#777777")
  answer_plot <- plot_card_side(answer_card, deck_path = deck$deck_path[card])
  print(cowplot::plot_grid(answer_plot, options, ncol = 1))

  # Get user input
  if (pick_multiple && sum(answer_hashes[option_indexes] == answer_hashes[card]) > 1) {
    my_print("Enter the numbers that apply to the card on top, separated by commas:")
    input = ""
    count = 0
    while (! input %in% seq_along(test_cards)) {
      if (count != 0) {
        my_print("Invalid input. Must be a number between 1 and ", length(test_cards), ".")
      }
      input <- readline()
      count <- count + 1
    }

  } else {
    my_print("Enter the number that applies to the card on top:")
    input = ""
    count = 0
    while (length(input) ==0 || ! all(input %in% seq_along(test_cards))) {
      if (count != 0) {
        my_print("Invalid input. Must be one or more numbes between 1 and ", length(test_cards), " separated by commas.")
      }
      input <- strsplit(readline(), ", *")[[1]]
      count <- count + 1
    }
  }
  input <- as.numeric(input)

  # Score test
  output <- lapply(option_indexes[input],
                   function(answer_index) {
                     if (answer_hashes[answer_index] == answer_hashes[card]) { # Right!
                       return(data.frame(front = deck$front[answer_index],
                                         back = deck$back[answer_index],
                                         front_hash = deck$front_hash[answer_index],
                                         back_hash = deck$back_hash[answer_index],
                                         right = 1,
                                         wrong = 0,
                                         updated = date(),
                                         deck_name = basename(deck$deck_path[answer_index]),
                                         test_name = "choice"))
                     } else { # Wrong!
                       return(data.frame(front = deck$front[answer_index],
                                         back = deck$back[answer_index],
                                         front_hash = deck$front_hash[answer_index],
                                         back_hash = deck$back_hash[answer_index],
                                         right = 0,
                                         wrong = -1,
                                         updated = date(),
                                         deck_name = basename(deck$deck_path[answer_index]),
                                         test_name = "choice"))


                     }
                   })


  return(do.call(rbind, output))

}


#' Plot an image
#'
#' Plot an image from a file path
#'
#' @param path The path to the image to plot
#'
#' @keywords internal
plot_image <- function(path) {
  if (endsWith(tolower(path), "png")) {
    image <- png::readPNG(path)
  } else if (endsWith(tolower(path), "jpg") || endsWith(tolower(path), "jpeg")) {
    image <- jpeg::readJPEG(path)
  } else {
    stop(paste0('Not an accepted file format: "', path, '".'))
  }
  y_max <- dim(image)[1] / dim(image)[2]
  ggplot2::ggplot() +
    ggplot2::annotation_raster(image, ymin = 0, xmin = 0, xmax = 1, ymax = y_max) +
    ggplot2::ylim(0, y_max) +
    ggplot2::xlim(0, 1) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()
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
    ggfittext::geom_fit_text(label = text, xmin = 0, xmax = 1, ymin = 0, ymax = 1, grow = TRUE, reflow = TRUE) +
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
