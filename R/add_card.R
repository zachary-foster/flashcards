#' Add cards to a deck
#'
#' Add cards to a deck
#'
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
add_card <- function(front, back, difficulty = 1, source = NULL,
                     source_url = NULL, rename_img = TRUE, max_pixels = 300000) {

}
