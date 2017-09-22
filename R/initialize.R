#' Create a user's home directory
#'
#' Create a user's home directory with a deck library, history file, and progress file.
#' A user would generally have a single deck library and add decks to it. This
#' function creates a new deck library with an example deck to get started.
#'
#' @param path The name/location of the deck library folder.
#'
#' @return NULL
#'
#' @export
init_user_dir <- function(path) {
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

  return(path)
}


#' Create a deck template
#'
#' Create a new empty deck
#'
#' @param path The path to new deck folder
#' @param name The name of the deck in a foramt that looks nice.
#' @param description The description of the deck. This would typically be a sentance or two.
#'
#' @return The path where the deck was created
#'
#' @export
init_deck <- function(path, name = basename(path), description = "") {
  # internal params
  deck_file_name <- "deck.tsv"
  deck_info_name <- "info.yml"
  deck_settings_name <- "settings.yml"
  image_dir_name <- "images"

  # Check that the path does not already exist
  if (file.exists(path)) {
    stop(paste0('Something already exists at "', path, '".'))
  }

  # Create folder
  dir.create(path, recursive = TRUE)

  # Create empty image folder
  dir.create(file.path(path, image_dir_name), recursive = TRUE)


  # Add empty deck.tsv
  writeLines(paste(deck_cols(), collapse = "\t"), file.path(path, deck_file_name))

  # Add empty settings file
  file.create(file.path(path, deck_settings_name))

  # Add info file
  writeChar(yaml::as.yaml(list(name = name, description = description)),
            file.path(path, deck_info_name), eos = NULL)

  return(path)
}
