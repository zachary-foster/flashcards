#' Play a sound
#'
#' Plays a sounds included with the pacakge in "inst/extdata/sounds"
#'
#' @references
#'
#' Sound files used:
#'
#' "wrong.wav": freesound.org user "Bertrof"
#'   https://freesound.org/people/Bertrof/sounds/131657/
#' "partial.wav: freesound.org user "JavierZumer"
#'   https://freesound.org/people/JavierZumer/sounds/257226/
#' "correct.wav: freesound.org user "LittleRainySeasons"
#'   https://freesound.org/people/LittleRainySeasons/sounds/335908/
#'
#' @param The name of the sound file. Must be a wav file.
#'
#' @keywords internal
play_sound <- function(name) {
  path <- system.file(package = "flashcards",
                      file.path("extdata", "sounds", name))
  beepr::beep(path)
}
