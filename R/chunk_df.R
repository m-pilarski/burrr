#' Title
#'
#' @param .df ...
#' @param .n_rows ...
#' @param .n_chunks ...
#'
#' @returns ...
#' @export
#'
#' @examples
#' "example"
chunk_df <- function(.df, .n_rows=NULL, .n_chunks=NULL){

  stopifnot(is.data.frame(.df))
  stopifnot(sum(sapply(list(.n_rows, .n_chunks), is.null)) == 1)
  stopifnot(sum(sapply(list(.n_rows, .n_chunks), is.numeric)) == 1)

  .df_row_seq <- seq_len(nrow(.df)) - 1

  if(is.numeric(.n_rows)){
    stopifnot(length(.n_rows) == 1 & .n_rows > 0 & .n_rows %% 1 == 0)
    .df_split_id <- .df_row_seq %/% .n_rows + 1

  }
  if(is.numeric(.n_chunks)){
    stopifnot(length(.n_chunks) == 1 & .n_chunks > 0 & .n_chunks %% 1 == 0)
    .df_split_id <- ((.df_row_seq / nrow(.df)) * .n_chunks) %/% 1 + 1
  }

  unname(split.data.frame(.df, .df_split_id))

}

#' Title
#'
#' @param .vec ...
#' @param .n_elems ...
#' @param .n_chunks ...
#'
#' @returns ...
#' @export
#'
#' @examples
#' "example"
chunk_vector <- function(.vec, .n_elems=NULL, .n_chunks=NULL){

  stopifnot(rlang::is_vector(.vec))
  stopifnot(sum(sapply(list(.n_elems, .n_chunks), is.null)) == 1)
  stopifnot(sum(sapply(list(.n_elems, .n_chunks), is.numeric)) == 1)

  .vec_elems_seq <- seq_along(.vec) - 1

  if(is.numeric(.n_elems)){
    stopifnot(length(.n_elems) == 1 & .n_elems > 0 & .n_elems %% 1 == 0)
    .vec_split_id <- .vec_elems_seq %/% .n_elems + 1
  }
  if(is.numeric(.n_chunks)){
    stopifnot(length(.n_chunks) == 1 & .n_chunks > 0 & .n_chunks %% 1 == 0)
    .vec_split_id <- ((.vec_elems_seq / length(.vec)) * .n_chunks) %/% 1 + 1
  }

  unname(split.default(.vec, .vec_split_id))

}

# ggplot2::cut_number() >> ggplot2:::breaks()
# function (x, equal, nbins = NULL, binwidth = NULL)
# {
#   equal <- arg_match0(equal, c("numbers", "width"))
#   if ((!is.null(nbins) && !is.null(binwidth)) || (is.null(nbins) &&
#                                                   is.null(binwidth))) {
#     cli::cli_abort("Specify exactly one of {.arg n} and {.arg width}.")
#   }
#   rng <- range(x, na.rm = TRUE, finite = TRUE)
#   if (equal == "width") {
#     if (!is.null(binwidth)) {
#       fullseq(rng, binwidth)
#     }
#     else {
#       seq(rng[1], rng[2], length.out = nbins + 1)
#     }
#   }
#   else {
#     if (!is.null(binwidth)) {
#       probs <- seq(0, 1, by = binwidth)
#     }
#     else {
#       probs <- seq(0, 1, length.out = nbins + 1)
#     }
#     stats::quantile(x, probs, na.rm = TRUE)
#   }
# }
