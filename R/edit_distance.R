
# edit distance of remaining words in the dictionary
# which guesses prunes the tree into fewer groups?
# for example, guessing one word leaves a group of 5 related words
#              or a different guess splits into smaller groups of two

#' @export
distance_lv <- function(words)
{
  n <- length(words)
  d <- matrix(NA, ncol = n, nrow = n)
  #d <- as.data.frame(d)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      if (j >= i) next
      atomic.i <- (j - 1) * n + i
      d[atomic.i] <- adist(words[i], words[j], cost = c(1000, 1000, NULL))
      d[atomic.i] <- d[atomic.i] / max(nchar(words[c(i,j)]))
      if (nchar(words[i]) != nchar(words[j])) d[atomic.i] <- 1
    }
  }
  as.dist(d)
}

#' @export
find_clusters <- function(.x, threshold = 0.4001)
{
  #.x <- unlist(.x)
  #if (length(.x) < 2) return(c(1))
  d  <- distance_pi(.x)
  d  <- as.dist(d)
  hc <- hclust(d)
  hc.clusters <- cutree(hc, h = threshold)
  hc.clusters <- as.numeric(hc.clusters)
  data.frame(word = .x, cluster = hc.clusters)
}


#' @export
count_clusters <- function(.x, min.clust = 2)
{
  x.summary <- table(.x)
  # remove singletons, as they are not considered a cluster
  x.summary <- x.summary[x.summary >= min.clust]
  length(x.summary)
}

#' @export
cluster_stats <- function(.x, min.clust = 2) {
  counts <- table(.x$cluster)
  .x <- .x[counts >= min.clust, ]
  counts <- counts[counts >= min.clust]
  data.frame(
    n           = length(unique(.x$cluster)),
    mean_size   = mean(counts),
    median_size = median(counts),
    max_size    = max(counts)
  )
}

fast_pi <- function(s1, s2)
{
  s1 <- unlist(strsplit(s1, ""))
  s2 <- unlist(strsplit(s2, ""))
  # the following is not necessary if words are the same length
  # if (nchar(x[i]) != nchar(x[j])) d[atomic.i] <- 1
  #if (length(s1) != length(s2)) return(0)
  return((length(which(s1 == s2)) / length(s1)) * 100)
}

distance_pi <- function(.x)
{
  n <- length(.x)
  d <- matrix(NA, ncol = n, nrow = n)
  #d <- as.data.frame(d)
  for (i in seq_along(.x))
  {
    for (j in seq_along(.x))
    {
      if (j >= i) next
      atomic.i <- (j - 1) * n + i
      d[atomic.i] <- (100 - fast_pi(.x[i], .x[j])) / 100
      # the following is not necessary if words are the same length
      # if (nchar(.x[i]) != nchar(.x[j])) d[atomic.i] <- 1
    }
  }
  return(d)
}


# common_cluster_letters <- function(.df) {
# for each cluster which letters are found in all
#
# return common letter combinations
# return letter counts
#
# or return unique/wild card letters that differ in each (will these help break clusters)
# }



