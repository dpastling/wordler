

#' @export
count_letters <- function(.x) {
  strsplit(.x, "") |>
    unlist() |>
    table() |>
    sort(decreasing = TRUE)
}

#' @export
rank_words_weight <- function(words, threshold = 0.05) {
  letter_count <- count_letters(words)
  letter_count <- letter_count / sum(letter_count)
  # exclude rare letters from the count
  #letter_count <- letter_count[letter_count >= threshold]
  word_stats <- purrr::map_dbl(strsplit(words, ""), ~ sum(letter_count[unique(.x)], na.rm = TRUE))
  data.frame(words = words, stat = word_stats) |> dplyr::arrange(desc(stat))
}

#' @export
rank_words_count <- function(words, desired_letters) {
  if (length(desired_letters) == 1) {
    desired_letters <- strsplit(desired_letters, "") |> unlist()
  }
  word_stats <- purrr::map_int(strsplit(words, ""), ~ sum(unique(.x) %in% desired_letters, na.rm = TRUE))
  data.frame(words = words, stat = word_stats) |> dplyr::arrange(desc(stat))
}

#' @export
count_pairs <- function(words) {
  all_letters <- strsplit(words, "") |> unlist() |> unique() |> sort()
  #result <- combn(all_letters, 2) |> t() |> as.data.frame()
  result <- expand.grid(all_letters, all_letters) |> dplyr::filter(Var1 != Var2)
  result$total <- NA
  result$both  <- NA
  for(i in 1:nrow(result)) {
    result[i, "total"] <- sum(grepl(result[i, 1], words))
    result[i, "both"]  <- sum(grepl(result[i, 1], words) & grepl(result[i, 2], words))
    result[i, "both"]  <- result[i, "both"]
  }
  result <- tidyr::pivot_wider(result, names_from = "Var2", values_from = "both") |>
    arrange(desc(total))
  result[, c("Var1", "total", as.character(result$Var1))]
}

#' @export
edit_distance <- function(s1, s2)
{
  s1 <- unlist(strsplit(s1, ""))
  s2 <- unlist(strsplit(s2, ""))
  length(which(s1 == s2)) / length(s1)
}


