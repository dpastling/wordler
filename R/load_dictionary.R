
#' @export
load_dictionary <- function() {
  read.delim("/usr/share/dict/words", col.names = "words") |>
  #read.delim("ospd.txt", col.names = "words") |>
    dplyr::filter(
      nchar(words) == 5,
      grepl("^[a-z]{5}$", words)
    ) |>
    purrr::pluck("words")
}

# idea: could be extended to filter dictionary based on usage
#       or load a corpus that has already been curated

