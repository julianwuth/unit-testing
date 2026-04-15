#' Column summaries
#'
#' @param df A data.frame, array or matrix.
#' @param fun The summary to be applied. One of "mean", "sd", "min", "max".
#'
#' @returns Named vector with per column summaries.
#' @export
#'
col_summary <-  function(df, fun = c("mean", "sd", "min", "max")) {
  if (!any(is.data.frame(df), is.matrix(df), is.array(df)))
    stop("df needs to be one of data.frame, array or matrix.")

  if (anyNA(df))
    stop("Please remove NAs from the data before using this function.")

  summary_fun <- match.arg(fun)

  if (summary_fun == "sd") {
    unique_elements <- apply(df, 2, get_unique)

    if(any(unique_elements < 2))
      stop("SD cannot be computed because one column has less than two unique elements.")
  }

  res <- apply(df, 2, match.fun(summary_fun))

  return(res)
}

get_unique <- function(col) {
  return(length(unique(col)))
}
