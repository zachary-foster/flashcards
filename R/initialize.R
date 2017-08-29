#' Create a users home directory
#'
#' Create a home directory with a deck library, history file, and progress file.
#' A user would generally have a single deck library and add decks to it. This
#' function creates a new deck library with an example deck to get started.
#'
#' @param path The name/location of the deck library folder.
#'
#' @return NULL
#'
#' @export
init_home_dir <- function(path) {
  # Make directory
  if (file.exists(path)) {
    stop(paste0('A file/folder already exists at "', path, '"'))
  }
  dir.create(path, recursive = TRUE)

  # Add example deck
  from_deck_path <- file.path(system.file(package = "flashcards"),
                            "extdata", "example_decks", "solar_system")
  to_deck_path <- file.path(path, "decks")
  dir.create(to_deck_path, recursive = TRUE)
  file.copy(from_deck_path, to_deck_path, recursive = TRUE)

  # Add empty progress file
  writeLines(paste(progress_cols(), collapse = "\t"), file.path(path, "progress.tsv"))

  # Add empty history file
  writeLines(paste(history_cols(), collapse = "\t"), file.path(path, "history.tsv"))

  # Make into an R project
  rproj_path <- file.path(path, paste0(basename(path), ".Rproj"))
  template_path <- system.file("templates/template.Rproj", package = "devtools")
  file.copy(template_path, rproj_path)
}
