##########################
#### Score functions #####
##########################

#' F1 Micro Score
#'
#' Calculates the micro-averaged F1 score between ground truth and predictions.
#'
#' @param truth Numeric vector of ground truth assignments (0 = negative, 1-N = singlet, 1000 = doublet)
#' @param pred Numeric vector of predicted assignments (0 = negative, 1-N = singlet, 1000 = doublet)
#' @return A numeric F1 micro score between 0 and 1
#' @export
#' @examples
#' truth <- c(1, 2, 3, 0, 1000)
#' pred <- c(1, 2, 0, 0, 1000)
#' f1_micro(truth, pred)
f1_micro <- function(truth, pred) {

  prec <- sum(pred == truth) / sum(pred > 0)
  rec <- sum(pred == truth) / length(truth > 0)

  score <- 2 * prec * rec / (prec + rec)
  return(score)
}

#' F1 Macro Score
#'
#' Calculates the macro-averaged F1 score between ground truth and predictions,
#' averaging equally across all samples.
#'
#' @param truth Numeric vector of ground truth assignments
#' @param pred Numeric vector of predicted assignments
#' @param bars Numeric vector of sample indices (e.g. 1:8 for 8 samples)
#' @return A numeric F1 macro score between 0 and 1
#' @export
#' @examples
#' truth <- c(1, 2, 3, 0, 1000)
#' pred <- c(1, 2, 0, 0, 1000)
#' f1_macro(truth, pred, bars = 1:3)
f1_macro <- function(truth, pred, bars) {

  precs <- c()
  recs <- c()
  for (i in 1:length(bars)) {
    bar_i <- bars[i]
    precs[i] <- sum(pred == bar_i & truth == bar_i) / sum(pred == bar_i)
    recs[i] <- sum(pred == bar_i & truth == bar_i) / sum(truth == bar_i)
  }

  scores <- 2 * precs * recs / (precs + recs)
  scores[is.na(scores)] <- 0
  score <- mean(scores)
  return(score)
}

#' F1 Weighted Score
#'
#' Calculates the weighted F1 score between ground truth and predictions,
#' weighting each sample by its proportion in the ground truth.
#'
#' @param truth Numeric vector of ground truth assignments
#' @param pred Numeric vector of predicted assignments
#' @param bars Numeric vector of sample indices (e.g. 1:8 for 8 samples)
#' @param weights Numeric vector of per-sample weights (should sum to 1)
#' @return A numeric F1 weighted score between 0 and 1
#' @export
#' @examples
#' truth <- c(1, 2, 3, 0, 1000)
#' pred <- c(1, 2, 0, 0, 1000)
#' f1_weighted(truth, pred, bars = 1:3, weights = c(1/3, 1/3, 1/3))
f1_weighted <- function(truth, pred, bars, weights) {

  precs <- c()
  recs <- c()
  for (i in 1:length(bars)) {
    bar_i <- bars[i]
    precs[i] <- sum(pred == bar_i & truth == bar_i) / sum(pred == bar_i)
    recs[i] <- sum(pred == bar_i & truth == bar_i) / sum(truth == bar_i)
  }

  scores <- 2 * precs * recs / (precs + recs)
  scores[is.na(scores)] <- 0
  score <- sum(weights * scores)
  return(score)
}

#' Matthews Correlation Coefficient
#'
#' Calculates the multiclass Matthews Correlation Coefficient (MCC) between
#' ground truth and predictions.
#'
#' @param truth Numeric vector of ground truth assignments
#' @param pred Numeric vector of predicted assignments
#' @param bars Numeric vector of sample indices (e.g. 1:8 for 8 samples)
#' @return A numeric MCC score between -1 and 1
#' @export
#' @examples
#' truth <- c(1, 2, 3, 0, 1000)
#' pred <- c(1, 2, 0, 0, 1000)
#' mcc(truth, pred, bars = 1:3)
mcc <- function(truth, pred, bars) {

  classes <- unique(c(0, bars))
  truth <- factor(truth, levels = classes)
  pred <- factor(pred, levels = classes)

  conf <- table(truth, pred)

  t_k <- rowSums(conf)
  p_k <- colSums(conf)
  c <- sum(diag(conf))
  s <- sum(conf)

  denom <- sqrt(
    (s^2 - sum(p_k^2)) *
      (s^2 - sum(t_k^2))
  )

  if (denom == 0) {
    return(NA)
  }

  mcc <- (c * s - sum(t_k * p_k)) / denom
  return(mcc)
}

#' Concordance
#'
#' Calculates the concordance (overall accuracy) between ground truth and predictions.
#'
#' @param truth Numeric vector of ground truth assignments
#' @param pred Numeric vector of predicted assignments
#' @return A numeric concordance score between 0 and 1
#' @export
#' @examples
#' truth <- c(1, 2, 3, 0, 1000)
#' pred <- c(1, 2, 0, 0, 1000)
#' concordance(truth, pred)
concordance <- function(truth, pred) {
  con <- sum(truth == pred) / length(truth)
  return(con)
}

#' Per-Sample F1 Scores
#'
#' Calculates F1 scores for each individual sample.
#'
#' @param truth Numeric vector of ground truth assignments
#' @param pred Numeric vector of predicted assignments
#' @param bars Numeric vector of sample indices (e.g. 1:8 for 8 samples)
#' @return A numeric vector of per-sample F1 scores
#' @export
#' @examples
#' truth <- c(1, 2, 3, 0, 1000)
#' pred <- c(1, 2, 0, 0, 1000)
#' f1_scores(truth, pred, bars = 1:3)
f1_scores <- function(truth, pred, bars) {

  precs <- c()
  recs <- c()
  for (i in 1:length(bars)) {
    bar_i <- bars[i]
    precs[i] <- sum(pred == bar_i & truth == bar_i) / sum(pred == bar_i)
    recs[i] <- sum(pred == bar_i & truth == bar_i) / sum(truth == bar_i)
  }

  scores <- 2 * precs * recs / (precs + recs)
  scores[is.na(scores)] <- 0
  return(scores)
}