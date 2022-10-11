


#' Count the frequency of letters in a word list
#'
#' Count the number of times a letter is found at least once in each word. Note
#' that duplicate letters (for example the letter o in "books") are only counted
#' once. Often we are interested in presence or absence of letters
#'
#' @param words a character vector of words
#'
#' @return returns a contingency table, an object of class "table", an named array of integer values.
#' @export
#'
#' @examples
#' load_dictionary() |> count_letters()
count_letters <- function(words) {
  strsplit(words, "") |>
    # ignore repeated letters in words like "doors"
    purrr::map(unique) |>
    unlist() |>
    table() |>
    sort(decreasing = TRUE)
}

#' @export
letter_freq_table <- function(words) {
  words <- strsplit(words, "")
  words <- matrix(unlist(words), ncol = 5, byrow = TRUE)
  .x <- apply(words, 2, function(.x) {
    .x <- table(.x)
    .x <- .x[sort(names(.x))] / sum(.x)
    #.x <- .x[sort(names(.x))]
    tidyr::pivot_wider(as.data.frame(.x), names_from = ".x", values_from = "Freq", values_fill = 0)
  }) |>
    dplyr::bind_rows() |>
    as.matrix()
  .x[is.na(.x)] <- 0
  .x
}


#' Rank words by letter frequency
#'
#' Suggest words using letter frequency. You can increase your chances of finding
#' a match using letters with high frequency letters. Words like 'arose' use the
#' top five most common letters in the dictionary.
#'
#' @param words
#'
#' @return
#' @export
#'
#' @examples
#' # Get the top ten words based on letter frequency
#' load_dictionary() |> rank_words_weight() |> head(n = 10)
rank_words_weight <- function(words) {
  letter_count <- count_letters(words)
  letter_count <- letter_count / sum(letter_count)
  word_stats <- purrr::map_dbl(strsplit(words, ""), ~ sum(letter_count[unique(.x)], na.rm = TRUE))
  data.frame(words = words, stat = word_stats) |> dplyr::arrange(desc(stat))
}


#' Rank words by count of specific letters
#'
#' Given a word list and a set of specific letters, count how many of those letters
#' appear in each word. For example find words that use the most vowels. No five
#' letter word uses them all, but this function will rank words that use the most
#' of the given list.
#'
#' @param words
#' @param desired_letters
#'
#' @return a data frame with two columns, the words and the count
#' @export
#'
#' @examples
#' # Find words that use the most vowels
#' load_dictionary() |> rank_words_count("aeiou") |> head(n = 10)
rank_words_count <- function(words, desired_letters) {
  if (length(desired_letters) == 1) {
    desired_letters <- strsplit(desired_letters, "") |> unlist()
  }
  word_stats <- purrr::map_int(strsplit(words, ""), ~ sum(unique(.x) %in% desired_letters, na.rm = TRUE))
  data.frame(words = words, count = word_stats) |> dplyr::arrange(desc(count))
}

#' @export
rank_words_position <- function(words) {
  freq <- letter_freq_table(words)
  word_stats <- purrr::map_dbl(strsplit(words, ""), ~ sum(freq[, .x] * diag(1, nrow = 5, ncol = 5)))
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



# rank_words_best <- function(answer, guess = NULL, hard_mode = FALSE,
#                             metric = "length", cluster_threshold = 0.4001) {
#   stopifnot(metric %in% c("length", "cluster"))
#   if (is.null(guess) & hard_mode) {
#     guess <- answer
#   } else if (is.null(guess)) {
#     guess <- load_dictionary()
#   }
#   #result <- matrix(0, nrow = length(guess), ncol = length(answer))
#   result <- rep(0, length(guess))
#   for (i in seq_along(guess)) {
#     for (j in seq_along(answer)) {
#       if (guess[i] == answer[j]) next
#       x1 <- strsplit(guess[i], "") |> unlist()
#       x2 <- strsplit(answer[j], "") |> unlist()
#       matching_letters <- x1[x1 %in% x2]
#       grey_letters     <- x1[! x1 %in% x2]
#       green_regex      <- ifelse(x1 == x2, x1, ".") |> paste(collapse = "")
#       yellow_regex     <- ifelse(x1 %in% x2 & x1 != x2, paste0("[^", x1, "]"), ".") |>
#         paste(collapse = "")
#       remaining_words <- answer |>
#           exclude_letters(grey_letters) |>
#           include_letters(matching_letters) |>
#           regex_filter(green_regex) |>
#           regex_filter(yellow_regex)
#       if (metric == "length") {
#         result[i] <- result[i] + (length(remaining_words) / length(answer))
#       } else if (metric == "cluster") {
#         mean_size <- find_clusters(remaining_words, threshold = cluster_threshold) |>
#           cluster_stats() |>
#           purrr::pluck("mean_size")
#         result[i] <- result[i] + (mean_size / length(guess))
#       } else {
#         stop(paste("metric", metric, "is not valid"))
#       }
#     }
#   }
#   data.frame(word = guess, value = result) |>
#     dplyr::arrange(value)
# }

