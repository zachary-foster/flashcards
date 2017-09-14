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
#' @return NULL
#'
#' @export
add_card <- function(deck_path, front, back, difficulty = 1, source = "",
                     source_url = "", rename_img = TRUE, max_pixels = 300000) {
  # Check if the front or back of the cards are images
  process_if_image <- function(path, other_side) {
    if (is_image_path(path)) {
      if (! is_image_path(other_side) && rename_img) {
        img_name <- paste0(other_side, ".jpg")
      } else {
        img_name <- basename(path)
      }
      output <- basename(add_image(img_path = path, deck_path = deck_path,
                                   name = img_name, max_pixels = max_pixels))
    } else {
      output <- path
    }
    return(output)
  }
  front <- process_if_image(front, back)
  back <- process_if_image(back, front)

  # If source_url is not specified and the front or back is a url, set source_url
  if (missing(source_url) && source_url == "") {
    if (RCurl::url.exists(front)) {
      source_url <- front
    } else if (RCurl::url.exists(back)) {
      source_url <- back
    }
  }

  # Create card data row
  output <- data.frame(front = front, back = back, difficulty = difficulty,
                       source = source, source_url = source_url)

  # Add cards to deck
  tsv_path <- file.path(deck_path, "deck.tsv")
  deck_data <- utils::read.table(tsv_path, header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE)
  new_deck <- rbind(deck_data, output)

  # Save deck data
  utils::write.table(new_deck, file = tsv_path, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)

  return(NULL)
}

#' Check if a string is a image path
#'
#' Check if a string is a local or remote image path
#'
#' @param path The putative image path
#'
#' @keywords internal
is_image_path <- function(path) {
  image_formats <- c("jpg", "jpeg", "png", "bmp")
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
#'
#' @return The path to where the image was saved
#'
add_image <- function(img_path, name = basename(img_path), deck_path, max_pixels = 300000) {
  # Internal params
  max_char <- 100 # The maximum number of characters in a file name

  # Sanitize file name
  name <- paste0(gsub(pattern = "[\\/:*?<>|' ]", replacement = "_", tolower(name)))

  # Apply length limit to file name
  if (nchar(name) > max_char) {
    name <- paste0(tools::file_path_sans_ext(substr(name, 1, max_char)), ".", tools::file_ext(name))
  }

  # Make image destination path
  img_dir_path <- file.path(deck_path, "images")
  img_dest_path <- file.path(img_dir_path, name)

  # Make image destination path unique if not already
  if (file.exists(img_dest_path)) {
    all_suffixes <- as.numeric(stringr::str_match(list.files(img_dir_path), ".*_([0-9]+)\\..+$")[,2])
    if (all(is.na(all_suffixes))) {
      name_suffix <- 2
    } else {
      name_suffix <- max(all_suffixes, na.rm = TRUE) + 1
    }
    img_dest_path <- paste0(tools::file_path_sans_ext(img_dest_path),
                            "_", name_suffix, ".", tools::file_ext(img_dest_path))
  }

  # Load image
  img <- imager::load.image(img_path)

  # Shink image if needed
  if (dim(img)[1] * dim(img)[2] > max_pixels) {
    width <- round(sqrt((dim(img)[1] * max_pixels) /  dim(img)[2]))
    height <- round(sqrt((dim(img)[2] * max_pixels) /  dim(img)[1]))
    img <- imager::resize(img, size_x = width, size_y = height)
    my_print("Reducing image size to ", height, " by ", width, ".")
  }

  # Save image
  imager::save.image(img, img_dest_path)

  # Return the path where the image was save
  return(img_dest_path)
}
