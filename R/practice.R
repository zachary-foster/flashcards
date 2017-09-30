#' Start a practice session
#'
#' Start practicing one or more decks.
#'
#' @param user_dir The path to the folder containing the deck library, progress
#'   file, and history file. If the folder was created using
#'   \code{\link{init_user_dir}}, this is the only argument needed. By default,
#'   this is the current working directory.
#' @param decks The names or folder paths of one or more decks to practice on.
#'   If "library" is supplied, then these paths are relative to it. By default,
#'   all decks in "library" are used.
#' @param library The path to the deck library. This is where the user stores
#'   their decks and practice history. By default, this is a directory called
#'   "decks" in the "user_dir" folder.  If "user_dir" is supplied, then this paths is
#'   relative to it.
#' @param progress The file used to store a user's progress for one or more
#'   decks. By default, this is a file called "progress.tsv".  If "user_dir" is
#'   supplied, then this paths is relative to it. If \code{NULL}, no progress
#'   file is used.
#' @param history The file used to store a user's practice history for one or
#'   more decks. By default, this is a file called "history.tsv".  If "user_dir" is
#'   supplied, then this paths is relative to it. If \code{NULL}, no history
#'   file is used.
#' @param tests The names of test types to use in this practice session. By
#'   default, the practice types specified by the decks are used.
#' @param record If \code{TRUE}, update the user's practice history with
#'   the results of this practice session.
#' @param focus A number between 0 amd 1. Low numbers means less well known
#'   cards will be practiced, whereas higher number means more well known cards
#'   will be practiced.
#' @param max_tests The number of cards to present.
#'
#' @export
practice <- function(user_dir = getwd(), decks = NULL, progress = "progress.tsv",
                     library = "decks", history = "history.tsv",
                     tests = test_names(), record = TRUE, focus = 0.5,
                     max_tests = 10) {

  # Load decks
  deck_data <- load_decks(decks = decks, library = library, user_dir = user_dir)

  # Load the progress
  progress <- get_project_file(progress, user_dir = user_dir)
  history <-  get_project_file(history, user_dir = user_dir)
  progress_data <- load_progress(progress = progress, user_dir = user_dir, complain = TRUE)

  # Main loop
  done = FALSE
  cards_tested <- 0
  all_changes <- NULL
  while (! done) {
    # Pick a fact to test
    card <- pick_card(deck = deck_data, progress = progress_data, focus = focus)

    # Present a test
    test_results <- present_test(card = card, deck = deck_data, tests = tests, progress = progress_data)

    if (record) {
      # Update the progress
      progress_data <- update_progress(changes = test_results, progress = progress_data)
      all_changes <- rbind(all_changes, test_results)

      # Save progress
      save_progress(progress = progress_data, path = progress)

      # Save history
      save_history(changes = test_results, history_path = history)

      # Print result
      score <- sum(test_results$right) - sum(test_results$wrong)
      my_print(ifelse(score > 0, "+", "-"), " ", round(abs(score), digits = 2), " points")
    }

    # Update count
    cards_tested <- cards_tested + 1

    # Wait for user to press enter
    my_print("Press [Enter] to continue. Press [q] to stop practice.")
    input <- readline()
    done <- tolower(input) == "q" # Stop practice if TRUE

    # Check if the session is done
    if (cards_tested >= max_tests) {
      done <- TRUE
    }

    # Clear plot
    grDevices::dev.off()
  }

  # Report results
  my_print("Practice complete!")
  if (record) {
    total <- sum(all_changes$right) - sum(all_changes$wrong)
    accuracy <- sum(all_changes$right) / (sum(all_changes$right) + sum(all_changes$wrong))
    my_print("Total score: ", ifelse(total >= 0, "+", "-"), " ", round(abs(total), digits = 2), " points\n",
             "Accuracy:    ",  as.integer(accuracy * 100), "%")
  }
}



#' Pick a card to practice
#'
#' Pick a card to practice based on a user's practice progress.
#'
#' @param deck A table containing deck information.
#' @param progress A table containing a users practice progress.
#' @param focus A number between 0 amd 1. Low numbers means less well known
#'   cards will be practiced, whereas higher number means more well known cards
#'   will be practiced.
#'
#' @return The row index in the deck of the card to practice
#'
#' @keywords internal
pick_card <- function(deck, progress, focus = 0.5) {
  # Apply users progress to deck
  deck$right <- 0
  deck$wrong <- 0
  for (i in seq_len(nrow(progress))) {
    matching <- deck$front_hash == progress$front_hash[i] & deck$back_hash == progress$back_hash[i]
    deck[matching, "right"] <- progress$right[i]
    deck[matching, "wrong"] <- progress$wrong[i]
  }

  # Pick a card
  score <- sample_learn_dist(deck$right, deck$wrong)
  score_diff <- abs(score - focus) * deck$difficulty
  result <- which.min(score_diff)

  return(result)
}


#' Sample the learning distribution
#'
#' Uses the number of right and wrong answers and the time of the last practice
#' to sample from a beta distribution simulating the user's comprehension of the
#' card.
#'
#' @param right The number or right answers/points.
#' @param wrong The number or wrong answers/points.
#' @param last_practiced TODO
#'
#' @return Integers between 1 and 0 for each input.
#'
#' @keywords internal
sample_learn_dist <- function(right, wrong, last_practiced = NULL) {
  vapply(seq_len(length(right)), FUN.VALUE = numeric(1), function(i) {
    stats::rbeta(n = 1, shape1 = right[i] + 1, shape2 = wrong[i] + 1)
  })
}

#' Calculate a hash of a card
#'
#' Calculate a hash of the contents of a card.
#'
#' @param content The content of either the front or the back of one or more cards.
#'
#' @return A hash string
#'
#' @keywords internal
card_hash <- function(content) {
  vapply(content, FUN.VALUE = character(1), function(x) {
    if (file.exists(x)) {
      result <- digest::digest(file = x)
    } else {
      result <- digest::digest(object = x)
    }
    return(result)
  })
}


#' Present a test for a card
#'
#' Pick a test ad present it to the user for a given card
#'
#' @param card The index of a card in "deck"
#' @param deck The data.frame containing the deck information
#' @param tests The name of tests to try to use
#' @param progress The progress table for the user
#'
#' @return The changes in scores resulting from the test
#'
#' @keywords internal
present_test <- function(card, deck, tests = test_names(), progress = NULL) {
  deck_path <- deck$deck_path[card]
  # deck_settings <- get_deck_settings(deck_path)

  successful_test <- FALSE
  test_result <- NULL
  while(! successful_test) {
    # Choose a test
    if (length(tests) == 0) {
      stop("No applicable tests found.")
    }
    test_name <- tests[sample(seq_len(length(tests)), size = 1)]
    tests <- tests[tests != test_name] # Dont try the same test twice
    test_to_try <- available_tests()[[test_name]]

    # Present a test
    # test_args <- c(list("card" = card, "deck" = deck), test_settings)
    test_args <- list("card" = card, "deck" = deck, "progress" = progress)
    test_result <- do.call(test_to_try, test_args)

    # Check if the test worked
    if (length(test_result) != 1 && ! is.na(test_result)) {
      successful_test <- TRUE
    }
  }

  return(test_result)
}


#' Get the path to a project file
#'
#' Get a file that might be a absolute path, in the current working directory, or in the user directory.
#'
#' @param file_path File to find.
#' @param user_dir The path to the folder containing the deck library, progress
#'   file, and history file.
#'
#' @return vector
#' @keywords internal
get_project_file <- function(file_path, user_dir = user_dir) {
  # Append user directory to library if supplied
  if (!is.null(user_dir) && !is.null(file_path) && !R.utils::isAbsolutePath(file_path)) {
    file_path <- file.path(user_dir, file_path)
  }
  return(file_path)
}
