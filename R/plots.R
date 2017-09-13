#' Plot users progress
#'
#' Plot the progress on one or more decks for a user's home directory.
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
#'
#' @return a ggplot object
#'
#' @export
plot_progress <- function(home = getwd(), decks = NULL,
                          progress = "progress.tsv", library = "decks") {
  # Load decks
  deck_data <- load_decks(decks = decks, library = library, home = home)

  # Load the progress
  progress <- get_project_file(progress, home = home)
  progress_data <- load_progress(progress = progress, home = home, complain = TRUE)

  # Combine untested cards with progress
  default_deck_data <- deck_data[, c("front", "back", "front_hash", "back_hash")]
  default_deck_data$right <- 0
  default_deck_data$wrong <- 0
  default_deck_data$updated <- ""
  progress_data <- update_progress(changes = default_deck_data,
                                   progress = progress_data)
  progress_data$total <- progress_data$right + abs(progress_data$wrong) + 2
  progress_data$score <- (progress_data$right + 1) / progress_data$total
  match_index <- match(paste(deck_data$front_hash, deck_data$back_hash),
                       paste(progress_data$front_hash, progress_data$back_hash))
  progress_data$deck_path <- deck_data$deck_path[match_index]

  # Make color scale
  score_color_count <- 5
  total_color_count <- 5
  score_color_range <- grDevices::colorRampPalette(c("red", "yellow", "green"))(score_color_count)
  color_key <- do.call(rbind, lapply(score_color_range, function(x) {
    grDevices::colorRampPalette(c("#EEEEEE", x))(total_color_count)
  }))


  # Assign colors to cards
  total_group_size = 2
  progress_data$score_group <- as.numeric(cut(progress_data$score,
                                breaks = 0:score_color_count / score_color_count,
                                labels = 1:score_color_count))
  progress_data$total_group <- as.numeric(cut(progress_data$total,
                                breaks = 0:total_color_count * total_group_size,
                                labels = 1:total_color_count))
  progress_data$total_group[is.na(progress_data$total_group)] <- total_color_count
  progress_data$card_color <- vapply(seq_along(progress_data$total_group), function(i) color_key[progress_data$score_group[i], progress_data$total_group[i]], character(1))
  progress_data$card_color <- progress_data$card_color[order(progress_data$score_group, progress_data$total_group)]

  # Plot graph
  plot_height <- 4
  max_width <- ceiling(max(table(progress_data$deck_path)) / plot_height)
  deck_plots <- lapply(split(progress_data, progress_data$deck_path), function(x) {
    color <- table(as.character(x$card_color))[rev(unique(x$card_color))]
    plot_width <- ceiling(nrow(x) / plot_height)
    ploy_title <- get_deck_name(unique(x$deck_path))
    waffle::waffle(as.vector(color), colors = names(color), legend_pos = "", title = ploy_title, rows = plot_height, pad = max_width - plot_width)
  })
  do.call(waffle::iron, deck_plots)
}



#' Get deck name
#'
#' Get deck name from its path
#'
#' @param deck_path The path to the deck folder
#'
#' @keywords internal
get_deck_name <- function(deck_path) {
  basename(deck_path)
}
