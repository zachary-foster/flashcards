#' Print a subset of a character vector
#'
#' Prints the start and end values for a character vector. The number of values
#' printed depend on the width of the screen by default.
#'
#' @param chars (`character`) What to print.
#' @param prefix (`character` of length 1) What to print before
#'   `chars`, on the same line.
#' @param max_chars (`numeric` of length 1) The maximum number of
#'   characters to print.
#' @param type (`"error"`, `"warning"`, `"message"`, `"cat"`, `"print"`, `"silent"``)
#'
#' @return `NULL`
#'
#' @keywords internal
limited_print <- function(chars, prefix = "",
                          max_chars = getOption("width") - nchar(prefix) - 5,
                          type = "message") {

  if (length(chars) == 0) {
    cat(prefix)
    return(invisible(NULL))
  }


  # https://stat.ethz.ch/pipermail/r-help/2006-March/101023.html
  interleave <- function(v1,v2) {
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
  }

  q = "'"
  interleaved <- interleave(chars[1:(length(chars) / 2)],
                            rev(chars[(length(chars) / 2 + 1):length(chars)]))
  is_greater_than_max <- cumsum(nchar(interleaved) + 2) + 10 > max_chars
  if (all(! is_greater_than_max)) {
    max_printed <- length(chars)
  } else {
    max_printed <- which.max(is_greater_than_max)
  }
  if (max_printed < length(chars)) {
    first_part <-  chars[1:as.integer(max_printed / 2 - 0.5)]
    second_part <-
      chars[as.integer(length(chars) - (max_printed / 2) + 1.5):length(chars)]
    output <- paste0(paste0(collapse = ", ", first_part),
                     " ... ",
                     paste0(collapse = ", ", second_part),
                     "\n")
  } else {
    output <- paste0(paste0(collapse = ", ", chars), "\n")
  }
  output <- paste(prefix, output, collapse = "")

  if (type == "error") {
    stop(output, call. = FALSE)
  } else if (type == "warning") {
    warning(output, call. = FALSE, immediate. = TRUE)
  } else if (type == "message") {
    message(output)
  } else if (type == "cat") {
    cat(output)
  } else if (type == "print") {
    print(output)
  } else if (type != "silent") {
    stop("invalid type option")
  }
  return(invisible(output))
}


#' Print something
#'
#' The standard print function for this package. This is a wrapper to make
#' package-wide changes easier.
#'
#' @param ... Something to print
#'
#' @keywords internal
my_print <- function(...) {
  text <- paste0(as.character(list(...)), collapse = "")
  message(text)
}


#' Read deck info
#'
#' Parse the information in a deck information YAML file
#'
#' @param path The file path to the deck folder
#'
#' @keywords internal
get_deck_info <- function(path) {
  info_path <- file.path(path, "info.yml")
  if (file.exists(info_path)) {
    data <- yaml::yaml.load_file(info_path)
  } else {
    data <- list()
  }
  if (! "deck_name" %in% names(data)) {
    data$deck_name <- Hmisc::capitalize(gsub(pattern = "_",
                                             replacement = " ",
                                             basename(path)))
  }
  return(data)
}


#' Get the combined hash for each card in a table
#'
#' Get the combined hash for each card in a table
#'
#' @param card_data A table with columns "front_hash" and "back_hash"
#'
#' @return A character vector of combined hashs
#'
#' @keywords internal
combined_hash <- function(card_data) {
  paste0(card_data$front_hash, card_data$back_hash)
}


#' Check for urls
#'
#' Check if a vector contains urls
#'
#' @param text The vector to test
#' @param test If \code{TRUE}, check that the url exists. This might take a second.
#'
is_url <- function(text, test = FALSE) {
  if (test) {
    return(grepl(text, pattern = "^(http|https)://") && RCurl::url.exists(text))
  } else {
    return(grepl(text, pattern = "^(http|https)://"))
  }
}


#' Check if a string is a image path
#'
#' Check if a string is a local or remote image path
#'
#' @param path The putative image path
#' @param test If \code{TRUE}, check that urls exists. This might take a second.
#'
#' @keywords internal
is_image_path <- function(path, test_url = curl::has_internet(), warn = TRUE) {
  image_formats <- c("jpg", "jpeg", "png", "bmp")
  ends_with_image_ext <- tolower(tools::file_ext(path)) %in% image_formats
  is_image <- ends_with_image_ext && (file.exists(path) || is_url(path, test = test_url))

  # Warn if it looks like an image path, but cannot be found
  if (warn && ends_with_image_ext && ! is_image) {
    warning(paste0('"', path, '" looks like a path to an image, but it cannot be found. If it is a URL to an image, check your internet connection. If it is a local file, check that it actually exists.'))
  }

  return(is_image)
}
