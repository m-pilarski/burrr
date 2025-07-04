set_stategy <- function(.workers){

  .linux_is <- identical(
    purrr::pluck(purrr::possibly(Sys.info)(), "sysname"), "Linux"
  )

  if(.workers %% 1 != 0 | .workers < 0){
    stop(".workers needs to be a natural number")
  }else if(.workers %in% 0:1){
    future::plan(future::sequential, gc=TRUE, split=.workers == 0)
  }else if(.workers > 1){
    .workers <- min(.workers, future::availableCores())
    future::plan(future::sequential, .skip=FALSE, .cleanup=TRUE)
    gc()
    if(.linux_is){
      future::plan(future::multicore, workers=.workers, gc=TRUE)
    }else{
      future::plan(future::multisession, workers=.workers, gc=TRUE)
    }
  }else{
    stop("unable to set future strategy", call.=FALSE)
  }

  return(invisible(NULL))

}

set_options <- function(.pkg, .globals, .silent){

  .furrr_opts_pars <- list(packages=.pkg, seed=TRUE)

  if(future::nbrOfWorkers() > 1){
    .furrr_opts_pars <- c(.furrr_opts_pars, list(globals=.globals))
    if(.silent){
      .furrr_opts_pars <- c(
        .furrr_opts_pars, list(conditions=character(0), stdout=FALSE)
      )
    }
  }

  .furrr_opts <- rlang::exec(furrr::furrr_options, !!!.furrr_opts_pars)

  return(.furrr_opts)

}

#' Title
#'
#' @param .x ...
#' @param .f ...
#' @param ... ...
#' @param .workers ...
#' @param .scheduling ...
#' @param .silent ...
#' @param .show_progress ...
#' @param .globals ...
#' @param .pkg ...
#'
#' @returns ...
#' @export
#'
#' @examples
#' "example"
best_map <- function(
  .x, .f, ..., .workers=NULL, .scheduling=1, .silent=FALSE, .show_progress=TRUE,
  .globals=TRUE, .pkg=NULL
){

  .f <- purrr::as_mapper(.f)
  .dots <- list(...)

  if(!is.null(.workers)){
    .strategy_backup <- future::plan()
    on.exit(future::plan(.strategy_backup))
    set_stategy(.workers=.workers)
  }

  .furrr_opts_pars <- list(packages=.pkg)
  if(future::nbrOfWorkers() > 1){
    .furrr_opts_pars <- c(.furrr_opts_pars, list(seed=TRUE, globals=.globals))
    if(.silent){
      .furrr_opts_pars <- c(
        .furrr_opts_pars, list(conditions=character(0), stdout=FALSE)
      )
    }
  }
  .furrr_opts <- rlang::exec(furrr::furrr_options, !!!.furrr_opts_pars)

  progressr::with_progress(
    expr={
      .p <- progressr::progressor(steps=length(.x))
      .r <- furrr::future_map(.x=.x, .f=function(..x_i){
        ..r_i <- rlang::exec(.fn=.f, ..x_i, !!!.dots)
        .p()
        return(..r_i)
      }, .options=.furrr_opts); .p(amount=Inf)
    },
    handlers=progressr::handler_progress(
      format=":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width=60,
      complete="="
    ),
    interval=1,
    enable=.show_progress
  )

  return(.r)

}

################################################################################

#' Title
#'
#' @param .x ...
#' @param .y ...
#' @param .f ...
#' @param ... ...
#' @param .workers ...
#' @param .scheduling ...
#' @param .silent ...
#' @param .show_progress ...
#' @param .globals ...
#' @param .pkg ...
#'
#' @returns ...
#' @export
#'
#' @examples
#' "example"
best_map2 <- function(
  .x, .y, .f, ..., .workers=NULL, .scheduling=1, .silent=FALSE,
  .show_progress=TRUE, .globals=TRUE, .pkg=NULL
){

  stopifnot((length(.x) == length(.y)) | any(c(length(.x), length(.y)) == 1))

  .f <- purrr::as_mapper(.f)
  .dots <- list(...)

  .linux_is <- identical(
    purrr::pluck(purrr::possibly(Sys.info)(), "sysname"), "Linux"
  )

  if(!is.null(.workers)){
    .strategy_backup <- future::plan()
    on.exit(future::plan(strategy=.strategy_backup))
    set_stategy(.workers=.workers)
  }

  .furrr_opts <- set_options(.pkg=.pkg, .globals=.globals, .silent=.silent)

  progressr::with_progress(
    expr={
      .p <- progressr::progressor(steps=max(length(.x), length(.y)))
      .r <- furrr::future_map2(.x=.x, .y=.y, .f=function(..x_i, ..y_i){
        ..r_i <- rlang::exec(.fn=.f, ..x_i, ..y_i, !!!.dots)
        .p()
        return(..r_i)
      }, .options=.furrr_opts); .p(amount=Inf)
    },
    handlers=progressr::handler_progress(
      format=":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width=60,
      complete="="
    ),
    interval=1,
    enable=.show_progress
  )

  return(.r)

}

