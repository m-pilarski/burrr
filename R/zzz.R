.onLoad <- function(libname, pkgname) {

  progressr::handlers(progressr::handler_progress(
    format=":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
    width=60,
    complete="="
  ))

}

.onAttach <- function(libname, pkgname) {
}

