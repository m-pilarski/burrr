best_map <- function(
  .x, .f, ..., .workers=NULL, .scheduling=1, .silent=FALSE, .show_progress=TRUE,
  .globals=TRUE, .pkg=NULL
){

  .f <- purrr::as_mapper(.f)
  .dots <- list(...)

  if(!is.null(.workers)){
    .strategy_backup <- future::plan()
    if(.workers %% 1 != 0 | .workers < 0){
      stop(".workers needs to be a natural number")
    }else if(.workers %in% 0:1){
      if(.workers == 0){
        future::plan(future::transparent, gc=TRUE)
      }else{
        future::plan(future::sequential, gc=TRUE)
      }
    }else if(.workers >= 2){
      .workers <- min(.workers, future::availableCores())
      future::plan(future::sequential, .skip=FALSE, .cleanup=TRUE)
      gc()
      future::plan(future::multisession, workers=.workers, gc=TRUE)
    }else{
      stop("unable to set future strategy", call.=FALSE)
    }
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

  tryCatch({
    progressr::with_progress(
      expr={
        .p <- progressr::progressor(steps=length(.x))
        .r <- furrr::future_map(.x, function(..x_i){
          ..r_i <- rlang::exec(.f, ..x_i, !!!.dots); .p(); return(..r_i)
        }, .options=.furrr_opts); .p(amount=Inf)
      },
      handlers=progressr::handler_progress(
        format=":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
        width=60,
        complete="="
      ),
      interval=1, enable=.show_progress
    )
  }, interrupt=function(...){
    stop("Interrupted by the user")
  }, finally={
    if(!is.null(.workers)){future::plan(.strategy_backup)}
  })

  return(.r)

}

################################################################################

best_map2 <- function(
  .x, .y, .f, ..., .workers=NULL, .scheduling=1, .silent=FALSE,
  .show_progress=TRUE, .globals=TRUE, .pkg=NULL
){

  stopifnot((length(.x) == length(.y)) | any(c(length(.x), length(.y)) == 1))

  .f <- purrr::as_mapper(.f)
  .dots <- list(...)

  if(!is.null(.workers)){
    .strategy_backup <- future::plan()
    if(.workers %% 1 != 0 | .workers < 0){
      stop(".workers needs to be a natural number")
    }else if(.workers %in% 0:1){
      if(.workers == 0){
        future::plan(future::transparent, gc=TRUE)
      }else{
        future::plan(future::sequential, gc=TRUE)
      }
    }else if(.workers >= 2){
      .workers <- min(.workers, future::availableCores())
      future::plan(future::sequential, .skip=FALSE, .cleanup=TRUE)
      gc()
      future::plan(future::multisession, workers=.workers, gc=TRUE)
    }else{
      stop("unable to set future strategy", call.=FALSE)
    }
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

  tryCatch({
    progressr::with_progress(
      expr={
        .p <- progressr::progressor(steps=max(length(.x), length(.y)))
        .r <- furrr::future_map2(.x, .y, function(..x_i, ..y_i){
          ..r_i <- rlang::exec(.f, ..x_i, ..y_i, !!!.dots); .p(); return(..r_i)
        }, .options=.furrr_opts); .p(amount=Inf)
      },
      handlers=progressr::handler_progress(
        format=":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
        width=60,
        complete="="
      ),
      interval=1, enable=.show_progress
    )
  }, interrupt=function(...){
    stop("Interrupted by the user")
  }, finally={
    if(!is.null(.workers)){future::plan(.strategy_backup)}
  })

  return(.r)

}

