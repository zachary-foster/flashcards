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



#' Get standard history columns
#'
#' Get standard history columns
#'
#' @keywords internal
history_cols <- function() {
  c("front", "back", "front_hash", "back_hash", "right", "wrong", "updated", "deck_name", "test_name")
}

