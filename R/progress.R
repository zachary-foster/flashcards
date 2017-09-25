#' Load practice progress
#'
#' Load practice progress
#'
#' @param progress The file used to store a user's practice progress for one or
#'   more decks. By default, this is a file called "progress.csv" in the current
#'   working directory.
#' @param complain If \code{TRUE}, issue warnings for any paths that do not
#'   point to valid decks.
#' @param restrict_to_deck If a deck table is supplied, only return progress
#'   data for that deck
#'
#' @return A \code{data.frame}
#'
#' @keywords internal
load_progress <- function(progress, user_dir = NULL, complain = TRUE,
                          restrict_to_deck = NULL) {
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

  # Only return cards in a deck table
  if (! is.null(restrict_to_deck)) {
    result <- result[combined_hash(result) %in% combined_hash(restrict_to_deck), ]
  }

  return(result)
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


#' Get standard progress columns
#'
#' Get standard progress columns
#'
#' @keywords internal
progress_cols <- function() {
  c("front", "back", "front_hash", "back_hash", "right", "wrong", "updated")
}
