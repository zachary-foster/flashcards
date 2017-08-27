#' Start a practice session
#'
#' Start practicing one or more decks.
#'
#' @param decks The names or folder paths of one or more decks to practice on.
#'   By default, all decks in "library" are used.
#' @param library The path to the deck library. This is where the user stores
#'   their decks and practice history. By default, this is a directory called
#'   "deck_library" in the current working directory. If \code{NULL}, it will be
#'   the current working directory.
#' @param history The file used to store a user's practice history for one or
#'   more decks. By default, this is a file called "history.csv" in the current
#'   working directory.
#' @param tests The names of test types to use in this practice session. By
#'   default, the practice types specified by the decks are used.
#' @param update_history If \code{TRUE}, update the user's practice history with
#'   the results of this practice session.
#' @param focus A number between 0 amd 1. Low numbers means less well known
#'   cards will be practiced, whereas higher number means more well known cards
#'   will be practiced.
#'
#' @export
practice <- function(decks = NULL, library = "deck_library",
                     history = "history.tsv", tests = NULL,
                     update_history = TRUE, focus = 0.5) {

  # Load decks
  deck_data <- load_decks(decks = decks, library = library)

  # Load the history
  history_data <- load_history(history = history, complain = TRUE)

  # Main loop
  done = FALSE
  while (! done) {
    # Pick a fact to test
    card <- pick_card(deck = deck_data, history = history_data, focus = focus)

    # Pick a test to use
    test <- pick_test(deck = deck_data, card = card)

    # Present a test
    present_test(deck = deck_data, history = history_data, tests = tests)

    # Update the history
  }

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
            call. = FALSE)
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
  required_cols <- c("front",	"back",	"difficulty",	"source")

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
            call. = FALSE)
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
#'
#' @return A \code{data.frame}
#'
#' @keywords internal
load_decks <- function(decks, library) {
  # Check that library can be found
  if (is.null(library)) {
    library <- getwd()
  } else {
    if (! file.exists(library)) {
      stop(paste0('The deck library "', library, '" does not exist.'))
    } else if (! file.info(library)$isdir) {
      stop(paste0('The deck library "', library, '" must be a folder.'))
    }
  }

  # Get deck list
  if (is.null(decks)) {
    decks <- list.dirs(library, recursive = FALSE)
    decks <- decks[is_deck(decks, complain = FALSE)]
    message(paste0('Using the following decks:\n',
                   limited_print(prefix = "  ", paths[! decks], type = "silent")))
  } else {
    decks <- decks[is_deck(decks, complain = TRUE)]
    if (length(decks) == 0) {
      stop("No valid decks supplied.")
    }
  }

  # Load and combine the decks used
  deck_tsv_paths <- file.path(decks, "deck.tsv")
  deck_data <- lapply(deck_tsv_paths, read.table, header = TRUE, sep = "\t")
  deck_data <- deck_data[check_deck_format(deck_data, complain = TRUE)]
  if (length(decks) == 0) {
    stop("No valid decks supplied.")
  }

  # Combine and return
  do.call(rbind, deck_data)
}


#' Load practice history
#'
#' Load practice history
#'
#' @param history The file used to store a user's practice history for one or
#'   more decks. By default, this is a file called "history.csv" in the current
#'   working directory.
#' @param complain If \code{TRUE}, issue warnings for any paths that do not
#'   point to valid decks.
#'
#' @return A \code{data.frame}
#'
#' @keywords internal
load_history <- function(history, complain = TRUE) {
  required_cols <- c("front_hash", "back_hash", "right", "wrong", "updated")

  # Check file can be found
  if (! file.exists(history)) {
    if (complain) {
      warning(paste0('Can not find the users history file:\n  "', history, '"'))
    }
    result <- matrix(nrow = 0, ncol = length(required_cols))
    colnames(result) <- required_cols
    return(as.data.frame(result))
  }

  # Load file
  result <- read.table(file = history, header = TRUE, sep = "\t")

  # Check that the table is formatted correctly
  if (any(! required_cols %in% colnames(result))) {
    warning(paste0('The following file is not a correctly formatted user history: "',
                   history, '"\n',
                   'User histories must have the folllowing columns:\n',
                   limited_print(prefix = "  ", required_cols, type = "silent")),
            call. = FALSE)
  }

  return(result)
}
