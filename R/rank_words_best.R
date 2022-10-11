

#' @importFrom foreach %dopar% %:%
#' @export
rank_words_best <- function(answer, guess = NULL, hard_mode = FALSE,
                            n_cores = 10, cluster_type = "FORK") {
  if (is.null(guess) & hard_mode) {
    guess <- answer
  } else if (is.null(guess)) {
    guess <- load_dictionary()
  }

  #doParallel::registerDoParallel(cores = n_cores)

  result <- foreach::foreach(i = guess, .combine = c) %:%
    foreach::foreach(j = answer, .combine = sum) %dopar% {
        mean_word_size(.guess = i, .answer = j, words = answer)
    }
  #parallel::stopCluster(cl)
  data.frame(word = guess, value = result) |>
    dplyr::arrange(value)
}


mean_word_size <- function(.guess, .answer, words) {
  if (.guess == .answer) return(1 / length(words))
  remaining_words <- reduce_dictionary(.guess, .answer, words)
  length(remaining_words) / length(words)
}



reduce_dictionary <- function(.guess, .answer, dictionary) {
  if (.guess == .answer) return(.guess)
  .guess <- strsplit(.guess, "") |> unlist()
  .answer <- strsplit(.answer, "") |> unlist()
  matching_letters <- .guess[.guess %in% .answer]
  grey_letters     <- .guess[! .guess %in% .answer]
  green_regex      <- ifelse(.guess == .answer, .guess, ".") |> paste(collapse = "")
  yellow_regex     <- ifelse(.guess %in% .answer & .guess != .answer, paste0("[^", .guess, "]"), ".") |>
    paste(collapse = "")
  if (length(grey_letters) > 0) {
    dictionary <- exclude_letters(dictionary, grey_letters)
  }
  if (length(matching_letters) > 0) {
    dictionary <- include_letters(dictionary, matching_letters)
  }
  if (green_regex != ".....") {
    dictionary <- regex_filter(dictionary, green_regex)
  }
  if (yellow_regex != ".....") {
    dictionary <- regex_filter(dictionary, yellow_regex)
  }
  dictionary
}


