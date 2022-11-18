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

  unname(split.default(.vec, .df_split_id))

}
