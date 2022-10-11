

#' Filter word list by matching letters
#'
#' @description
#' Reduce word list to those words containing specific letters. These would be
#' the yellow and green letters generated after a guess. The list of letters can
#' either be passed as a character vector c("a", "b", "c")
#'
#' @usage
#' include_letters(words, good_letters)
#'
#' @param words a character vector of the current word list or object retured by load_dictionary()
#' @param good_letters either a string or a character vector of letters to be included
#'
#' @return
#' @export
#'
#' @examples
#' # Find words that contain the letters "abc"
#' load_dictionary() |> include_letters("abc")
#'
#' # Or as a character vector
#' load_dictionary() |> include_letters(c("a", "b", "c"))
include_letters <- function(words, good_letters) {
  if (length(good_letters) == 1) {
    good_letters <- strsplit(good_letters, "") |> unlist()
  }
  for(s in good_letters) {
    words <- words[grepl(s, words)]
  }
  words
}


#' Filter word list by excluded letters
#'
#' @description
#' Reduce word list by removing words containing letters that have been excluded.
#' These would be the grey letters generated after a guess. The list of letters
#' can either be passed as a character vector c("a", "b", "c").
#'
#' @param words a character vector of the current word list or object retured by load_dictionary()
#' @param bad_letters either a string or a character vector of letters to be excluded
#'
#' @return
#' @export
#'
#' @examples
#' load_dictionary() |> exclude_letters("abcdefghijklmorts")
exclude_letters <- function(words, bad_letters) {
  if (length(bad_letters) == 1) {
    bad_letters <- strsplit(bad_letters, "") |> unlist()
  }
  for(s in bad_letters) {
    words <- words[! grepl(s, words)]
  }
  words
}



#' Reduce word list using a regex
#'
#' @description
#' If you are familiar with regular expressions, you can use this simple wrapper
#' function to filter the list of words. Useful when filtering words by position.
#' For example, if you know the letter i is in the third position (green letter),
#' you can use the regex '^..i' to limit to those letters. Or if you know the
#' middle letter cannot be a "t" or a "w" you can use this regex '^..[^tw]'
#'
#' @param words a character vector of the current word list or object retured by load_dictionary()
#' @param r the regular expression (regex) to use for filtering
#'
#' @return
#' @export
#'
#' @examples
#' # Find words that start with "abo"
#' load_dictionary() |> regex_filter("^abo")
#'
#' # Resuce a word list by those that do not have an a in the third position
#' load_dictionary() |> include_letters("abc") |> regex_filter("^..[^a]")
#'
#' # Reduce a word list by those that do not have an a in the third position byt start with a c
#' load_dictionary() |> include_letters("abc") |> regex_filter("^c.[^a]")
regex_filter <- function(words, r) {
  words[grep(r, words)]
}

