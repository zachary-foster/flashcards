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
       "choose" = test_choose,
       "answer" = test_answer)
}


#' Review a card
#'
#' Review a card by flipping it over until moving to the next test.
#'
#' @param card The index of the card to use
#' @param deck The table containing the deck information.
#' @param progress The progress table for the user
#' @param max_score Each time this test is selected, a distribution between 1
#'   and 0 is sampled that simulates a guess at how well the user knows a card.
#'   0 means does not know and 1 means does know. Cards that get a score higher
#'   than this option value will not be used for this test and a diffent test
#'   will be tried.
#'
#' @keywords internal
test_review <- function(card, deck, progress, max_score = 0.5) {
  # Only allow this test for cards that are not well known
  is_prog_record <- progress$front_hash == deck$front_hash[card] & progress$back_hash == deck$back_hash[card]
  if (sum(is_prog_record) == 0) { # If no matches in progress table
    card_score <- sample_learn_dist(0, 0)
  } else {
    card_score <- sample_learn_dist(progress$right[is_prog_record], progress$wrong[is_prog_record])
  }
  if (card_score > max_score) {
    return(NA)
  }

  # Flip card until user types "c"
  deck_path <- deck$deck_path[card]
  front <- deck$front[card]
  back <- deck$back[card]
  my_print("Press [Enter] to flip card. Press [c] to finish.")
  input = ""
  count = 0
  while (input != "c") {
    graphics::plot(plot_card_side(c(front, back)[count %% 2 + 1], deck_path = deck_path))
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
#' @param progress The progress table for the user
#' @param max_choices The maximum number of cards to show.
#' @param pick_multiple If \code{TRUE}, allow multiple correct answers
#'
#' @keywords internal
test_choose <- function(card, deck, progress, max_choices = 4, pick_multiple = TRUE) {
  # Internal parameters
  diff_deck_penalty <- 0.1 # How likly, relative to 1, that a card from a different deck will be chosen as an option

  # Pick side and card to test
  sides <- c("front", "back")[sample.int(2)]
  side_hashes <- paste0(sides, "_hash")
  answer_side <- deck[[sides[1]]]
  answer_hashes <- deck[[side_hashes[1]]]
  option_side <- deck[[sides[2]]]
  option_hashes <- deck[[side_hashes[2]]]
  answer_card <- answer_side[card]

  # Pick some choices
  wrong_indexes <- sample.int(nrow(deck),
                              prob = ifelse(deck$deck_path[card] == deck$deck_path, 1, diff_deck_penalty))
  wrong_indexes <- wrong_indexes[! duplicated(option_side[wrong_indexes])]
  wrong_indexes <- wrong_indexes[option_side[wrong_indexes] != option_side[card]]
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
                                scale = 0.9,
                                labels = seq_along(option_plots),
                                label_size = 30,
                                label_colour = "#777777")
  answer_plot <- plot_card_side(answer_card, deck_path = deck$deck_path[card])
  print(cowplot::plot_grid(answer_plot, options, ncol = 1, rel_heights = c(1, 1.62)))

  # Get user input
  if (pick_multiple) {
    my_print("Enter the numbers that apply to the card on top, separated by commas:")
    input = ""
    count = 0
    while (length(input) == 0 || ! all(input %in% seq_along(test_cards))) {
      if (count != 0) {
        play_sound("partial.wav")
        my_print("Invalid input. Must be one or more numbes between 1 and ", length(test_cards), " separated by commas.")

      }
      input <- strsplit(readline(), ", *")[[1]]
      count <- count + 1
    }

  } else {
    my_print("Enter the number that applies to the card on top:")
    input = ""
    count = 0
    while (length(input) != 1 || ! input %in% seq_along(test_cards)) {
      if (count != 0) {
        play_sound("partial.wav")
        my_print("Invalid input. Must be a number between 1 and ", length(test_cards), ".")
      }
      input <- readline()
      count <- count + 1
    }
  }
  input <- as.numeric(input)

  # Score test
  answer_indexes <- option_indexes[input]
  correct_option_hashes <- option_hashes[answer_hashes == answer_hashes[card]]
  output <- lapply(seq_along(option_indexes),
                   function(i) {
                     option_index <- option_indexes[i]
                     if (option_hashes[option_index] %in% correct_option_hashes) { # Correct option
                       if (option_index %in% answer_indexes) { # Right!
                         right <- 1
                         wrong <- 0
                       } else { # Missing answer
                         right <- 0
                         wrong <- 0.5
                       }
                     } else {
                       if (option_index %in% answer_indexes) { # Wrong!
                         right <- 0
                         wrong <- 1
                       } else { # Missing wrong answer
                         right <- 0.05
                         wrong <- 0
                       }
                     }
                     return(data.frame(front = deck$front[option_index],
                                       back = deck$back[option_index],
                                       front_hash = deck$front_hash[option_index],
                                       back_hash = deck$back_hash[option_index],
                                       right = right,
                                       wrong = wrong,
                                       updated = date(),
                                       deck_name = basename(deck$deck_path[option_index]),
                                       test_name = "choice"))
                   })

  # Report right answers
  is_right <- option_hashes[option_indexes[input]] %in% correct_option_hashes
  if (sum(is_right) == 1) {
    my_print(input[is_right], " is right!")
  } else if (sum(is_right) == 2) {
    my_print(paste0(input[is_right], collapse = " and "), " are right!")
  } else if (sum(is_right) > 2) {
    my_print(paste0(input[is_right], collapse = ", "), " are right!")
  }

  # Report wrong answers
  if (sum(! is_right) == 1) {
    my_print(input[! is_right], " is WRONG!")
  } else if (sum(! is_right) == 2) {
    my_print(paste0(input[! is_right], collapse = " and "), " are WRONG!")
  } else if (sum(! is_right) > 2) {
    my_print(paste0(input[! is_right], collapse = ", "), " are WRONG!")
  }

  # Report missing answers
  is_missing <- option_hashes[option_indexes] %in% correct_option_hashes & ! seq_along(option_indexes) %in% input
  missing_indexes <- seq_along(option_indexes)[is_missing]
  if (length(missing_indexes) == 1) {
    my_print(missing_indexes, " is a correct answer!")
  } else if (length(missing_indexes) == 2) {
    my_print(paste0(missing_indexes, collapse = " and "), " are correct answers!")
  } else if (length(missing_indexes) > 2) {
    my_print(paste0(missing_indexes, collapse = ", "), " are correct answers!")
  }

  # Play sound
  if (all(is_right) && length(missing_indexes) == 0) {
    play_sound("correct.wav")
  } else if (all(!is_right)) {
    play_sound("wrong.wav")
  } else {
    play_sound("partial.wav")
  }

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



#' Typing test
#'
#' Display one side of a card and ask which matches the other side of another
#' card shown. The user must type in the other side of the card.
#'
#' @param card The index of the card to use
#' @param deck The table containing the deck information.
#' @param progress The progress table for the user
#' @param max_chars The maximum number of characters the answer side of a card
#'   can have.
#' @param max_dist The maximum proportion of differences the answer can be to
#'   the correct answer while still being considered correct.
#'
#' @keywords internal
test_answer <- function(card, deck, progress, max_chars = 20, max_dist = 0.90) {
  # Check that this test can be used with the card

  # Pick a side to present

  # Present card

  # Get user input

  # Score test

  # Report results

  # Play sound


  return(NA)
}

