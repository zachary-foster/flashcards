#' Add cards to a deck
#'
#' Add cards to a deck
#'
#' @param deck_path The path to the deck folder
#' @param front The front of the cards to add
#' @param back The back of the cards to add
#' @param difficulty A positive number, typically between 1 and 10, indicating
#'   the difficulty of the card. Lower difficulties will typically be presented
#'   first.
#' @param source Where the information came from the copyright holder of images
#' @param source_url The location on the internet the infromation came from
#' @param rename_img If \code{TRUE}, rename the image files based on the
#'   other side of the card.
#' @param max_pixels The maximum number of pixels an image can be. Larger images
#'   will be shrunk to this size and saved as a jpeg.
#'
#' @param return NULL
#'
#' @export
add_card <- function(deck_path, front, back, difficulty = 1, source = "",
                     source_url = "", rename_img = TRUE, max_pixels = 300000) {
  # Check if the front or back of the cards are images
  image_formats <- c("jpg", "jpeg", "png", "bmp")
  process_if_image <- function(path, other_side) {
    if (is_image_path(path)) {
      if (! is_image_path(other_side) && rename_img) {
        img_name <- paste0(gsub(pattern = " ", replacement = "_", tolower(other_side)), ".jpg")
      } else {
        img_name <- basename(path)
      }
      add_image(path, deck_path, img_name, max_pixels = max_pixels)
    }
  }
  process_if_image(front, back)
  process_if_image(back, front)

  # Create card data row
  output <- data.frame(front = front, back = back, difficulty = difficulty,
                       source = source, source_url = source_url)

  # Add cards to deck
  tsv_path <- file.path(deck_path, "deck.tsv")
}


is_image_path <- function(path) {
  tolower(tools::file_ext(path)) %in% image_formats && (file.exists(path) || RCurl::url.exists(path))
}


add_image <- function(img_path, deck_path, name = basename(img_path), max_pixels = 300000) {

}

