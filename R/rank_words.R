
#' Rank words by those best at solving the puzzle
#'
#' Compare each word against a dictionary and estimate the number of remaining
#' words if that word is guessed. Words are ranked by the mean number of
#' remaining words with the best candidate ranked first. You can supply your own
#' dictionary with the parameter `dict`, or if left as NULL (the default)
#' the function will load in the built in unix dictionary. You can also set `dict` to
#' the reduced words list or set `hard_mode` to TRUE. This function can be slow if the set of words to
#' choose from is large, so to speed things up it makes use of the `foreach`
#' package. You can use the `n_cores` parameter.
#'
#' @param remain a vector of remaining words
#' @param dict a vector of words to choose from. If NULL words chosen from the full dictionary
#' @param metric how the metric is calculated, either "size" for total size, or "percent"
#' @param hard_mode logical. If TRUE use the remaining set of words as the dictionary
#' @param n_cores integer. The number of cores to use for parallelization
#'
#' @return a tibble of the provided words with the mean, min, and max cluster
#' sizes ranked from best to worse
#' @export
#'
#' @examples
#' @importFrom foreach %dopar% %:%
#' @export
rank_words <- function(remain, dict = NULL, metric = "size", hard_mode = FALSE,
                            n_cores = 2) {
  if (is.null(dict) | hard_mode) {
    dict <- remain
  } else if (is.null(dict)) {
    dict <- load_dictionary()
  }

  doParallel::registerDoParallel(cores = n_cores)

  patterns <- foreach::foreach(i = dict, .combine = rbind)  %dopar% {
      purrr::map_chr(remain, ~wordle_pattern(dict = i, remain = .x))
    }
  result <- foreach::foreach(i = 1:nrow(patterns), .combine = rbind) %dopar% {
    .x <- table(patterns[i, ]) |> as.numeric()
    if (metric == "percent") {
      data.frame(
        mean_size = sum(.x^2 / length(remain)),
        min_size  = min(.x^2 / length(remain)),
        max_size  = max(.x^2 / length(remain))
      )
    } else if (metric == "size") {
      data.frame(
        mean_size = mean(.x),
        min_size  = min(.x),
        max_size  = max(.x)
      )
    }

  }
  doParallel::stopImplicitCluster()
  result[["word"]] <- dict
  tibble::as_tibble(result) |>
    dplyr::select(word, tidyselect::everything()) |>
    dplyr::arrange(mean_size)
  #tibble::tibble(word = dict, stat = result) |>
  #  dplyr::arrange(stat)
}


#' Generate a wordle-style pattern for two words
#'
#' Given a guess and an answer, generate the wordle-style pattern of green, yellow
#' and grey coded letters.
#'
#' @param .guess a single character value with the word to be guessed
#' @param .answer a single character value with theoretical answer
#'
#' @return a single character value with "g" for green perfect match, "y" for
#' partial yellow matches, and "x" for grey non-matching letters.
#' @export
#'
#' @examples
#'
#' #' wordle_pattern("lever", "eaten")
#' returns "xyxgx"
#'
#' wordle_pattern("knoll", "lever")
#' # returns "xxxyx"
#'
#' wordle_pattern("lever", "knoll")
#' # returns "yxxxx"
wordle_pattern <- function(guess, answer) {
  if (guess == answer) return("ggggg")
  guess  <- strsplit(guess, "") |> unlist()
  answer <- strsplit(answer, "") |> unlist()
  pattern <- rep("x", 5)
  pattern[guess == answer] <- "g"
  answer[answer == guess] <- "."
  for (i in seq_along(guess)) {
    if (guess[i] %in% answer & pattern[i] != "g") {
      pattern[i] <- "y"
      # deal with repeated letters
      answer[answer %in% guess[i] & ! duplicated(answer)] <- "."
    }
  }
  paste(pattern, collapse = "")
}



