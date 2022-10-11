
#' Load a word list from a disctionary
#'
#' @description
#' Loads a list of words from the unix file '/usr/share/dict/words' and filters
#' to words with 5 letters.
#'
#' @param file The file path to a dictionary file. Defaults to /usr/share/dict/words
#'
#' @return A character vector of words loaded from the file
#' @export
#'
#' @examples
load_dictionary <- function(file = "/usr/share/dict/words") {
  words <- readLines(file)
  words[grepl("^[a-z]{5}$", words)]
}

