

#' @importFrom foreach %dopar% %:%
#' @export
rank_words_best <- function(answer, guess = NULL, metric = "size", hard_mode = FALSE,
                            n_cores = 10, cluster_type = "FORK") {
  if (is.null(guess) & hard_mode) {
    guess <- answer
  } else if (is.null(guess)) {
    guess <- load_dictionary()
  }

  doParallel::registerDoParallel(cores = n_cores)
  # parallel::stopCluster(cl)
  # doParallel::stopImplicitCluster()
  #patterns <- foreach::foreach(i = guess, .combine = rbind) %:%
  #  foreach::foreach(j = answer, .combine = c) %dopar% {
  #    wordle_pattern(guess = i, answer = j)
  #  }
  patterns <- foreach::foreach(i = guess, .combine = rbind)  %dopar% {
      purrr::map_chr(answer, ~wordle_pattern(guess = i, answer = .x))
    }
  result <- foreach::foreach(i = 1:nrow(patterns), .combine = c) %dopar% {
    .x <- table(patterns[i, ]) |> as.numeric()
    if (metric == "size") {
      sum(.x^2 / length(answer))
    } else if (metric == "cluster") {
      mean(.x)
    }

  }
  # possible_patterns <- lapply(patterns, names) |> unlist() |> unique()
  #  result <- foreach::foreach(i = 1:length(patterns), .combine = c) %:%
  #    foreach::foreach(j = possible_patterns, .combine = sum) %dopar% {
  #      if (! j %in% names(patterns[[i]])) return(0)
  #      remaining_words <- reduce_dictionary(guess[i], pattern = j, answer)
  #      (length(remaining_words) / length(answer)) * patterns[[i]][j]
  #    }
  doParallel::stopImplicitCluster()

  data.frame(word = guess, stat = result) |>
    dplyr::arrange(stat)
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



#' @export
reduce_dictionary <- function(guess, pattern, dictionary) {
  if (pattern == "ggggg") return(guess)
  guess <- strsplit(guess, "") |> unlist()
  pattern <- strsplit(pattern, "") |> unlist()
  grey_letters     <- guess[pattern %in% "x"] |> unique()
  matching_letters <- guess[! pattern %in% "x"] |> unique()
  # deal with multiple letters in guess, one colored the other grey
  if (length(grey_letters) > 0 & length(matching_letters) > 0) {
    grey_letters <- grey_letters[! grey_letters %in% matching_letters]
  }
  position_regex <- rep(".", 5)
  for (i in seq_along(pattern)) {
    if (pattern[i] == "g")  {
      position_regex[i]  <- guess[i]
    } else {
      position_regex[i] <- paste0("[^", guess[i], "]")
    }
  }
  position_regex  <- paste(position_regex, collapse = "")
  if (length(grey_letters) > 0) {
    dictionary <- exclude_letters(dictionary, grey_letters)
  }
  if (length(matching_letters) > 0) {
    dictionary <- include_letters(dictionary, matching_letters)
  }
  if (position_regex != ".....") {
    dictionary <- regex_filter(dictionary, position_regex)
  }
  dictionary
}


