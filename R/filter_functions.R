

#' @export
include_letters <- function(words, good_letters) {
  if (length(good_letters) == 1) {
    good_letters <- strsplit(good_letters, "") |> unlist()
  }
  for(s in good_letters) {
    words <- words[grepl(s, words)]
  }
  words
}

#' @export
exclude_letters <- function(words, bad_letters) {
  if (length(bad_letters) == 1) {
    bad_letters <- strsplit(bad_letters, "") |> unlist()
  }
  for(s in bad_letters) {
    words <- words[! grepl(s, words)]
  }
  words
}

#' @export
regex_filter <- function(words, r) {
  words[grep(r, words)]
}
