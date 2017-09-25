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
load_decks <- function(decks, library, user_dir, add_hash = TRUE) {
  # Check that either decks or library are specified
  if (is.null(decks) && is.null(library)) {
    stop('Either "decks" or "library" must be specified.')
  }

  # Check that user_dir exists and is a directory
  if (! is.null(user_dir)) {
    if (! file.exists(user_dir)) {
      stop(paste0('The users home directory "', user_dir, '" does not exist.'), call. = FALSE)
    } else if (! file.info(user_dir)$isdir) {
      stop(paste0('The users home directory "', user_dir, '" must be a folder.'), call. = FALSE)
    }
  }

  # Append user directory to library if supplied
  if (!is.null(user_dir) && !is.null(library) && !R.utils::isAbsolutePath(library)) {
    library <- file.path(user_dir, library)
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



#' Get standard deck columns
#'
#' Get standard deck columns
#'
#' @keywords internal
deck_cols <- function() {
  c("front", "back", "difficulty", "source", "source_url")
}

