#' Attractiveness of Self-Resembling Faces.
#'
#' Attractiveness judgements of same-sex and other-sex self-resembling faces from DeBruine (2004).
#'
#' @format A data frame with 108 rows and 16 variables:
#' \describe{
#'   \item{col1}{col1 description}
#'   \item{col2}{col2 description}
#' }
#' @source \url{https://osf.io/3c5s4/}
"self_res_att"

round0 <- function(x, digits = 0) {
  fmt <- paste0("%.", digits, "f")
  x0 <- sprintf(fmt, x)

  return(x0)
}

#' APA text for Paired-Samples T-Test
#'
#' @description
#' Create APA-formatted text for the results of an independent-samples t-test in the following format:
#'
#' A paired-samples t-test was conducted to compare \{dv\} between \{level1\} (M = \{mean1\}, SD = \{sd1\}) and \{level2\} (M = \{mean2\}, SD = \{sd2\}). There was a \{non\}significant difference; t(\{df\}) = \{t_value\}, p = \{p_value\}.
#' @param x A vector of the values for level 1.
#' @param y A vector of the values for level 2.
#' @param dv The text describing the DV in the output statement.
#' @param level1 The text describing level 1 in the output statement.
#' @param level2 The text describing level 2 in the output statement.
#' @return A character string.
#' @export
#' @examples
#' # use generic text
#' apa_t_pair(x = self_res_att$f_self,
#'            y = self_res_att$f_non)
#'
#' # specify the text for dv and levels
#' apa_t_pair(x = self_res_att$f_self,
#'            y = self_res_att$f_non,
#'            dv = "preferences for female faces",
#'            level1 = "participants who resembled those faces",
#'            level2 = "non-self participants")
apa_t_pair <- function(x, y,
                       dv = "the DV",
                       level1 = "level 1",
                       level2 = "level 2") {
  # warn about identical values
  if (all(x == y)) {
    stop("x and y cannot be identical")
  }

  t_results <- t.test(x, y, paired = TRUE)

  template <- "A paired-samples t-test was conducted to compare {dv} between {level1} (M = {mean1}, SD = {sd1}) and {level2} (M = {mean2}, SD = {sd2}). There was a {non}significant difference; t({df}) = {t_value}, p = {p_value}."

  glue::glue(
    template,
    mean1   = round0(mean(x), 1),
    sd1     = round0(sd(x), 1),
    mean2   = round0(mean(y), 1),
    sd2     = round0(sd(y), 1),
    non     = ifelse(t_results$p.value < .05, "", "non-"),
    df      = round0(t_results$parameter, 0),
    t_value = round0(t_results$statistic,2),
    p_value = round0(t_results$p.value, 3)
  )
}
