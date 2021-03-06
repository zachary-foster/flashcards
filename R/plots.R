#' Plot users progress
#'
#' Plot the progress on one or more decks for a user's home directory.
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
#'   "decks" in the user folder.  If "user_dir" is supplied, then this paths is
#'   relative to it.
#' @param progress The file used to store a user's progress for one or more
#'   decks. By default, this is a file called "progress.tsv".  If "user_dir" is
#'   supplied, then this paths is relative to it. If \code{NULL}, no progress
#'   file is used.
#' @param min_cards Decks with fewer cards than this will be compressed into a
#'   single deck.
#'
#' @return a ggplot object
#'
#' @export
plot_progress <- function(user_dir = getwd(), decks = NULL,
                          progress = "progress.tsv", library = "decks",
                          min_cards = 10) {
  # Internal parameters
  score_color_breaks = c(-.1, .4, .6, .8, .9, 1.1) # The limits of ranges that determines the color of cards
  score_color_count <- length(score_color_breaks) - 1
  total_color_breaks <- c(-1, 1, 5, 10, 30, 100000000) # The limits of ranges that determines the intensity of the color of cards
  total_color_count <- length(total_color_breaks) - 1
  max_width <- 50

  # Load decks
  deck_data <- load_decks(decks = decks, library = library, user_dir = user_dir)

  # Load the progress
  progress <- get_project_file(progress, user_dir = user_dir)
  progress_data <- load_progress(progress = progress, user_dir = user_dir,
                                 complain = TRUE, restrict_to_deck = deck_data)

  # Combine untested cards with progress
  default_deck_data <- deck_data[, c("front", "back", "front_hash", "back_hash")]
  default_deck_data$right <- 0
  default_deck_data$wrong <- 0
  default_deck_data$updated <- ""
  progress_data <- update_progress(changes = default_deck_data,
                                   progress = progress_data)
  progress_data$total <- progress_data$right + progress_data$wrong
  progress_data$score <- progress_data$right / progress_data$total
  progress_data$score[is.nan(progress_data$score)] <- 0
  match_index <- match(paste(progress_data$front_hash, progress_data$back_hash),
                       paste(deck_data$front_hash, deck_data$back_hash))
  progress_data$deck_path <- deck_data$deck_path[match_index]

  # Make color scale
  score_color_range <- grDevices::colorRampPalette(c("#c51b7d", "#4d9221"))(score_color_count)
  color_key <- do.call(rbind, lapply(score_color_range, function(x) {
    grDevices::colorRampPalette(c("#EEEEEE", x))(length(total_color_breaks))
  }))

  # Assign colors to cards
  progress_data$score_group <- as.numeric(cut(progress_data$score,
                                              breaks = score_color_breaks,
                                              labels = 1:score_color_count))
  progress_data$total_group <- as.numeric(cut(progress_data$total,
                                              breaks = total_color_breaks,
                                              labels = 1:total_color_count))
  progress_data$total_group[is.na(progress_data$total_group)] <- length(total_color_breaks)
  progress_data$card_color <- vapply(seq_along(progress_data$total_group),
                                     function(i) color_key[progress_data$score_group[i], progress_data$total_group[i]], character(1))
  progress_data <- progress_data[order(progress_data$total_group, progress_data$score_group), ]

  # Add deck name to card info
  progress_data$deck_name <- get_deck_name(progress_data$deck_path)

  # Combine minor decks
  is_small <- table(progress_data$deck_name) < min_cards
  progress_data$deck_name[is_small[progress_data$deck_name]] <- "Small decks"

  # Plot graph
  deck_plots <- lapply(split(progress_data, progress_data$deck_name), function(x) {
    color <- table(as.character(x$card_color))[rev(unique(x$card_color))]
    plot_height <- ceiling(nrow(x) / max_width)
    plot_width <- ceiling(nrow(x) / plot_height)
    plot_title <- x$deck_name[1]
    waffle::waffle(as.vector(color),
                   colors = names(color),
                   legend_pos = "",
                   title = plot_title,
                   rows = plot_height,
                   pad = max_width - plot_width)
  })
  do.call(waffle::iron, deck_plots)
}
