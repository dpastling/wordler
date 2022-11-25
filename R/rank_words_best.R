

#' @importFrom foreach %dopar% %:%
#' @export
rank_words_best <- function(answer, guess = NULL, metric = "cluster", hard_mode = FALSE,
                            n_cores = 2, cluster_type = "FORK") {
  if (is.null(guess) & hard_mode) {
    guess <- answer
  } else if (is.null(guess)) {
    guess <- load_dictionary()
  }

  doParallel::registerDoParallel(cores = n_cores)

  patterns <- foreach::foreach(i = guess, .combine = rbind)  %dopar% {
      purrr::map_chr(answer, ~wordle_pattern(guess = i, answer = .x))
    }
  result <- foreach::foreach(i = 1:nrow(patterns), .combine = rbind) %dopar% {
    .x <- table(patterns[i, ]) |> as.numeric()
    if (metric == "size") {
      data.frame(
        mean_size = sum(.x^2 / length(answer)),
        min_size  = min(.x^2 / length(answer)),
        max_size  = max(.x^2 / length(answer))
      )
    } else if (metric == "cluster") {
      data.frame(
        mean_size = mean(.x),
        min_size  = min(.x),
        max_size  = max(.x)
      )
    }

  }
  doParallel::stopImplicitCluster()
  result[["word"]] <- guess
  tibble::as_tibble(result) |>
    dplyr::select(word, tidyselect::everything()) |>
    dplyr::arrange(mean_size)
  #tibble::tibble(word = guess, stat = result) |>
  #  dplyr::arrange(stat)
}


mean_word_size <- function(.guess, .answer, words) {
  if (.guess == .answer) return(1 / length(words))
  remaining_words <- reduce_dictionary(.guess, .answer, words)
  length(remaining_words) / length(words)
}


#' Generate a wordle pattern for two words
#'
#' Given a guess and an answer, generate the wordle pattern with green, yellow
#' and grey squares.
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



