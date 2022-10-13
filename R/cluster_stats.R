
#' @export
get_common_cluster_letters <- function(.df) {
  .df |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(common = get_conserved_letters(word), n = n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(n))
}



get_conserved_letters <- function(words) {
  words <- strsplit(words, "")
  words <- matrix(unlist(words), ncol = 5, byrow = TRUE)
  words <- apply(words, 2, unique)
  lapply(words, function(.x) { ifelse(length(.x) > 1, ".", .x)}) |>
    unlist() |>
    paste(collapse = "")
}

