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
        img_name <- paste0(other_side, ".jpg")
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

#' Check if a string is a image path
#'
#' Check if a string is a local or remote image path
#'
#' @param path The putative image path
#'
#' @keywords internal
is_image_path <- function(path) {
  tolower(tools::file_ext(path)) %in% image_formats && (file.exists(path) || RCurl::url.exists(path))
}


#' Add an image to a deck
#'
#' Add an image to a deck
#'
#' @param img_path The path to the source image, either local or remote
#' @param name The name to save the image as. If the name already exists, a counter is added to the end. Spaces and capitals will be replaced.
#' @param deck_path The path to the deck directory
#' @param max_pixels The maximum number of pixels an image can be. Larger images
#'   will be shrunk to this size and saved as a jpeg.
add_image <- function(img_path, name = basename(img_path), deck_path, max_pixels = 300000) {
  # Internal params
  max_char <- 100 # The maximum number of characters in a file name


  # Sanitize file name
  # name <- paste0(gsub(pattern = "[\/:*?<>|' ]", replacement = "_", tolower(other_side)))

  # Apply length limit to file name
  if (nchar(name) > max_char) {
    name <- paste0(tools::file_path_sans_ext(substr(name, 1, max_char)), ".", tools::file_ext(name))
  }

  # Make image destination path
  img_dir_path <- file.path(deck_path, "images")
  img_dest_path <- file.path(img_dir_path, name)

  # Make image destination path unique if not already
  if (file.exists(img_dest_path)) {
    name_suffix <- stringr::str_match(tools::file_path_sans_ext(name), ".*_([0-9]+)$")[,2]
    if (is.na(name_suffix)) {
      name_suffix <- 1
    }
    img_dest_path <- paste0(tools::file_path_sans_ext(img_dest_path),
                            "_", name_suffix, tools::file_ext(img_dest_path))
  }

  # Load image
  img <- imager::load.image(img_path)

  # Shink image if needed
  if (dim(img)[1] * dim(img)[2] > max_pixels) {
    width <- round(sqrt((dim(img)[1] * max_pixels) /  dim(img)[2]))
    height <- round(sqrt((dim(img)[2] * max_pixels) /  dim(img)[1]))
    img <- imager::resize(img, size_x = width, size_y = height)
  }

  # Save image
  imager::save.image(img, img_dest_path)
}
