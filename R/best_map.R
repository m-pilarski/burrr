
best_map <- function(
  .x, .f, ..., .workers=1, .scheduling=1, .silent=FALSE
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
      future::plan(future::multisession, workers=.workers, gc=TRUE)
    }else{
      stop("unable to set future strategy", call.=FALSE)
    }
  }

  .furrr_opts_pars <- list()
  if(future::nbrOfWorkers() > 1){
    .furrr_opts_pars <- c(.furrr_opts_pars, list(seed=TRUE))
    if(.silent){
      .furrr_opts_pars <- c(
        .furrr_opts_pars, list(conditions=character(0), stdout=FALSE)
      )
    }
  }
  .furrr_opts <- rlang::exec(furrr::furrr_options, !!!.furrr_opts_pars)

  tryCatch({
    progressr::with_progress({
      .p <- progressr::progressor(steps=length(.x))
      .r <- furrr::future_map(.x, function(..x_i){
        ..r_i <- rlang::exec(.f, ..x_i, !!!.dots); .p(); return(..r_i)
      }, .options=.furrr_opts); .p(amount=Inf)
    })
  }, finally={
    if(!is.null(.workers)){future::plan(.strategy_backup)}
  })

  return(.r)

}

################################################################################

best_map2 <- function(
  .x, .y, .f, ..., .workers=NULL, .scheduling=1, .silent=FALSE
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
      future::plan(future::multisession, workers=.workers, gc=TRUE)
    }else{
      stop("unable to set future strategy", call.=FALSE)
    }
  }

  .furrr_opts_pars <- list()
  if(future::nbrOfWorkers() > 1){
    .furrr_opts_pars <- c(.furrr_opts_pars, list(seed=TRUE))
    if(.silent){
      .furrr_opts_pars <- c(
        .furrr_opts_pars, list(conditions=character(0), stdout=FALSE)
      )
    }
  }

  .furrr_opts <- rlang::exec(furrr::furrr_options, !!!.furrr_opts_pars)

  tryCatch({
    progressr::with_progress({
      .p <- progressr::progressor(steps=max(length(.x), length(.y)))
      .r <- furrr::future_map2(.x, .y, function(..x_i, ..y_i){
        ..r_i <- rlang::exec(.f, ..x_i, ..y_i, !!!.dots); .p(); return(..r_i)
      }, .options=.furrr_opts); .p(amount=Inf)
    })
  }, finally={
    if(!is.null(.workers)){future::plan(.strategy_backup)}
  })

  return(.r)

}

