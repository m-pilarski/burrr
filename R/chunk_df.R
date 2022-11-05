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

  split.data.frame(.df, .df_split_id)

}
