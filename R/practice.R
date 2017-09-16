#' Start a practice session
#'
#' Start practicing one or more decks.
#'
#' @param home The path to the folder containing the deck library, progress
#'   file, and history file. If the folder was created using
#'   \code{\link{init_home_dir}}, this is the only argument needed. By default,
#'   this is the current working directory.
#' @param decks The names or folder paths of one or more decks to practice on.
#'   If "library" is supplied, then these paths are relative to it. By default,
#'   all decks in "library" are used.
#' @param library The path to the deck library. This is where the user stores
#'   their decks and practice history. By default, this is a directory called
#'   "decks" in the "home" folder.  If "home" is supplied, then this paths is
#'   relative to it.
#' @param progress The file used to store a user's progress for one or more
#'   decks. By default, this is a file called "progress.tsv".  If "home" is
#'   supplied, then this paths is relative to it. If \code{NULL}, no progress
#'   file is used.
#' @param history The file used to store a user's practice history for one or
#'   more decks. By default, this is a file called "history.tsv".  If "home" is
#'   supplied, then this paths is relative to it. If \code{NULL}, no history
#'   file is used.
#' @param tests The names of test types to use in this practice session. By
#'   default, the practice types specified by the decks are used.
#' @param update_history If \code{TRUE}, update the user's practice history with
#'   the results of this practice session.
#' @param focus A number between 0 amd 1. Low numbers means less well known
#'   cards will be practiced, whereas higher number means more well known cards
#'   will be practiced.
#' @param max_tests The number of cards to present.
#'
#' @export
practice <- function(home = getwd(), decks = NULL, progress = "progress.tsv",
                     library = "decks", history = "history.tsv",
                     tests = test_names(), update_history = TRUE, focus = 0.5,
                     max_tests = 20) {

  # Load decks
  deck_data <- load_decks(decks = decks, library = library, home = home)

  # Load the progress
  progress <- get_project_file(progress, home = home)
  history <-  get_project_file(history, home = home)
  progress_data <- load_progress(progress = progress, home = home, complain = TRUE)

  # Main loop
  done = FALSE
  cards_tested <- 0
  all_changes <- NULL
  while (! done) {
    # Pick a fact to test
    card <- pick_card(deck = deck_data, progress = progress_data, focus = focus)

    # Present a test
    test_results <- present_test(card = card, deck = deck_data, tests = tests, progress = progress_data)
    grDevices::dev.off() # clear plot

    # Update the progress
    progress_data <- update_progress(changes = test_results, progress = progress_data)
    all_changes <- rbind(all_changes, test_results)

    # Save progress
    save_progress(progress = progress_data, path = progress)

    # Save history
    save_history(changes = test_results, history_path = history)

    # Update count
    cards_tested <- cards_tested + 1

    # Check if the session is done
    if (cards_tested >= max_tests) {
      done <- TRUE
    }
  }

  # Report results
  my_print("Practice complete!")
}


#' Checks if decks folders are formatted right
#'
#' Checks if paths to putative decks point to decks formatted correctly.
#'
#' @param paths The paths to putative decks
#' @param complain If \code{TRUE}, issue warnings for any paths that do not
#'   point to valid decks.
#'
#' @return A \code{TRUE/FALSE} vector corresponding to "paths".
#'
#' @keywords internal
is_deck <- function(paths, complain = TRUE) {
  required_files <- c("settings.yml", "deck.tsv")

  # Check the deck paths
  result <- vapply(paths, FUN.VALUE = logical(1), function(path) {
    if (! file.exists(path)) {
      return(FALSE)
    } else if (! file.info(path)$isdir) {
      return(FALSE)
    } else if (! all(file.exists(file.path(path, required_files)))) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  })

  # Issue warning if invalid decks were found
  if (complain && any(! result)) {
    warning(paste0('The following paths do not contain correctly formatted decks:\n',
                   limited_print(prefix = "  ", paths[! result], type = "silent"), "\n",
                   'Decks must be folders with the following files:\n',
                   limited_print(prefix = "  ", required_files, type = "silent")),
            call. = FALSE, immediate. = TRUE)
  }

  return(result)
}


#' Checks if deck data are formatted right
#'
#' Checks if deck data is formatted correctly
#'
#' @param decks The paths to putative decks. Named by their file paths
#' @param complain If \code{TRUE}, issue warnings for any paths that do not
#'   point to valid decks.
#'
#' @return A \code{TRUE/FALSE} vector corresponding to "paths".
#'
#' @keywords internal
check_deck_format <- function(decks, complain = TRUE) {
  required_cols <- c("front",	"back",	"source")

  # Check the deck paths
  result <- vapply(decks, FUN.VALUE = logical(1), function(deck) {
    all(required_cols %in% colnames(deck))
  })

  # Issue warning if invalid decks were found
  if (complain && any(! result)) {
    warning(paste0('The following paths do not contain correctly formatted decks:\n',
                   limited_print(prefix = "  ", names(decks)[! result], type = "silent"), "\n",
                   'Decks must have the folllowing columns:\n',
                   limited_print(prefix = "  ", required_cols, type = "silent")),
            call. = FALSE, immediate. = TRUE)
  }

  return(invisible(result))
}


#' Load one or more decks
#'
#' Load one or more decks
#'
#' @param decks The names or folder paths of one or more decks to practice on.
#'   By default, all decks in "library" are used.
#' @param library The path to the deck library. This is where the user stores
#'   their decks and practice history. By default, this is a directory called
#'   "deck_library" in the current working directory. If \code{NULL}, it will be
#'   the current working directory.
#' @param add_hash If \code{TRUE}, add "front_hash" and "back_hash" columns
#'   containing the md5 hash of the contents of the cards.
#'
#' @return A \code{data.frame}
#'
#' @keywords internal
load_decks <- function(decks, library, home, add_hash = TRUE) {
  # Check that either decks or library are specified
  if (is.null(decks) && is.null(library)) {
    stop('Either "decks" or "library" must be specified.')
  }

  # Check that home exists and is a directory
  if (! is.null(home)) {
    if (! file.exists(home)) {
      stop(paste0('The users home directory "', home, '" does not exist.'), call. = FALSE)
    } else if (! file.info(home)$isdir) {
      stop(paste0('The users home directory "', home, '" must be a folder.'), call. = FALSE)
    }
  }

  # Append home to library if supplied
  if (!is.null(home) && !is.null(library) && !R.utils::isAbsolutePath(library)) {
    library <- file.path(home, library)
  }

  # Check that library exists and is a directory
  if (! is.null(library)) {
    if (! file.exists(library)) {
      stop(paste0('The deck library "', library, '" does not exist.'), call. = FALSE)
    } else if (! file.info(library)$isdir) {
      stop(paste0('The deck library "', library, '" must be a folder.'), call. = FALSE)
    }
  }

  # Get deck list
  if (is.null(decks)) {
    decks <- list.dirs(library, recursive = FALSE)
    decks <- decks[is_deck(decks, complain = FALSE)]
    message(paste0('Using the following decks:\n',
                   limited_print(prefix = "  ", basename(decks), type = "silent")))
  } else {
    if (!is.null(library)) {
      decks <- ifelse(R.utils::isAbsolutePath(decks),
                      decks, file.path(library, decks))
    }
    decks <- decks[is_deck(decks, complain = TRUE)]
    if (length(decks) == 0) {
      stop("No valid decks supplied.", call. = FALSE)
    }
  }

  # Load and combine the decks used
  deck_tsv_paths <- file.path(decks, "deck.tsv")
  deck_data <- lapply(deck_tsv_paths, utils::read.table, header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE, quote = "")
  deck_data <- deck_data[check_deck_format(deck_data, complain = TRUE)]
  if (length(deck_data) == 0) {
    stop("No valid decks supplied.")
  }

  # Combine into a single table
  deck_data <- lapply(seq_len(length(deck_data)), function(i) {
    deck_data[[i]]$deck_path <- decks[i]
    return(deck_data[[i]])
  })
  result <- do.call(rbind, deck_data)

  # Remove any incomplete cards
  incomplete <- result$front == "" | result$back == "" | is.na(result$front) | is.na(result$back)

  if (sum(incomplete) > 0) {
    problem_decks <- unique(result$deck_path[incomplete])
    warning(paste0("There were ", sum(incomplete),
                   " incomplete cards found in the given decks:\n",
                   limited_print(basename(problem_decks), type = "silent")),
            call. = FALSE, immediate. = TRUE)
    result <- result[! incomplete, ]
  }

  # Check that there are cards in the deck
  if (nrow(result) == 0) {
    stop("Could not find any valid cards.")
  }

  # Add card content hashes
  if (add_hash) {
    result$front_hash <- card_hash(result$front)
    result$back_hash <- card_hash(result$back)
  }

  return(result)
}


#' Load practice progress
#'
#' Load practice progress
#'
#' @param progress The file used to store a user's practice progress for one or
#'   more decks. By default, this is a file called "progress.csv" in the current
#'   working directory.
#' @param complain If \code{TRUE}, issue warnings for any paths that do not
#'   point to valid decks.
#'
#' @return A \code{data.frame}
#'
#' @keywords internal
load_progress <- function(progress, home = NULL, complain = TRUE) {
  required_cols <- progress_cols()

  # Check file can be found
  if (is.null(progress) || ! file.exists(progress)) {
    if (complain) {
      warning(paste0('Can not find the users progress file:\n  "', progress, '"'),
              call. = FALSE, immediate. = TRUE)
    }
    result <- matrix(nrow = 0, ncol = length(required_cols))
    colnames(result) <- required_cols
    return(as.data.frame(result))
  }

  # Load file
  result <- utils::read.table(file = progress, header = TRUE, sep = "\t")

  # Check that the table is formatted correctly
  if (any(! required_cols %in% colnames(result))) {
    warning(paste0('The following file is not a correctly formatted user progress: "',
                   progress, '"\n',
                   'User histories must have the folllowing columns:\n',
                   limited_print(prefix = "  ", required_cols, type = "silent")),
            call. = FALSE, immediate. = TRUE)
  }

  return(result)
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


#' Get standard progress columns
#'
#' Get standard progress columns
#'
#' @keywords internal
progress_cols <- function() {
  c("front", "back", "front_hash", "back_hash", "right", "wrong", "updated")
}


#' Get standard history columns
#'
#' Get standard history columns
#'
#' @keywords internal
history_cols <- function() {
  c("front", "back", "front_hash", "back_hash", "right", "wrong", "updated", "deck_name", "test_name")
}


#' Get standard deck columns
#'
#' Get standard deck columns
#'
#' @keywords internal
deck_cols <- function() {
  c("front", "back", "difficulty", "source", "source_url")
}


#' Get the path to a project file
#'
#' Get a file that might be a absolute path, in the current working directory, or in the home directory.
#'
#' @param file_path File to find.
#' @param home The path to the folder containing the deck library, progress
#'   file, and history file.
#'
#' @return vector
#' @keywords internal
get_project_file <- function(file_path, home = home) {
  # Append home to library if supplied
  if (!is.null(home) && !is.null(file_path) && !R.utils::isAbsolutePath(file_path)) {
    file_path <- file.path(home, file_path)
  }
  return(file_path)
}


#' Apply changes to the progress record
#'
#' Apply changes to the progress record
#'
#' @param changes A table containing the results of a practice session
#' @param progress The current progress table
#'
#' @return data.frame
#' @keywords internal
update_progress <- function(changes, progress) {
  # Add new rows to progress for first occurances of cards
  match_index <- match(paste(changes$front_hash, changes$back_hash),
                       paste(progress$front_hash, progress$back_hash))
  progress <- rbind(progress, changes[is.na(match_index), progress_cols()])

  # Update cards practiced in the past
  changes <- changes[!is.na(match_index), ]
  updated_index <- match_index[!is.na(match_index)]
  progress[updated_index, "right"] <- progress[updated_index, "right"] + changes$right
  progress[updated_index, "wrong"] <- progress[updated_index, "wrong"] + changes$wrong

  return(progress)
}

#' Save changes to the user's practice history
#'
#' Save changes to the user's practice history
#'
#' @param changes A table containing the results of a practice session
#' @param history_path The path to the users history file
#'
#' @keywords internal
save_history <- function(changes, history_path) {
  utils::write.table(changes[, history_cols()], file = history_path, row.names = FALSE,
              col.names = ! file.exists(history_path), sep = "\t", quote = FALSE,
              append = file.exists(history_path))
}


#' Save the progress file
#'
#' Save the progress file
#'
#' @param progress The current progress table
#' @param path Where to save the file
#'
#' @keywords internal
save_progress <- function(progress, path) {
  utils::write.table(progress[, progress_cols()], file = path, row.names = FALSE,
              sep = "\t", quote = FALSE)
}
