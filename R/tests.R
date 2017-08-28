#' Get names of available tests
#'
#' Get names of available tests
#'
#' @export
test_names <- function() {
  names(available_tests())
}

#' List of available test
#'
#' Return the list of available tests.
#'
#' @keywords internal
available_tests <- function() {
  list("review" = test_review,
       "choose" = test_choose)
}


#' test under construction
#'
#' test under construction
#'
#' @keywords internal
test_review <- function(card, deck) {
  deck_path <- deck$deck_path[card]

  show_side <- function(content) {
    img_path <- file.path(deck_path, "images", content)

    if (file.exists(img_path)) {
      plot(magick::image_read(img_path))
    } else {
      print(ggplot(data = data.frame(label = content)) +
              geom_fit_text(label = content, xmin = 0, xmax = 1, ymin = 0, ymax = 1, grow = TRUE) +
              xlim(0, 1) +
              ylim(0, 1) +
              theme_void())
    }
  }

  front <- deck$front[card]
  back <- deck$back[card]

  message("Press [enter] to flip card. Type 'c' to finish.")
  input = ""
  count = 0
  while (input != "c") {
    show_side(c(front, back)[count %% 2 + 1])
    input <- readline()
    count <- count + 1
  }

  # Return results
  data.frame(front_hash = deck$front_hash[card],
             back_hash = deck$back_hash[card],
             right = 0.1,
             wrong = 0,
             updated = date(),
             deck_name = basename(deck_path),
             test_name = "review")

}


#' test under construction
#'
#' test under construction
#'
#' @keywords internal
test_choose <- function() {

}
