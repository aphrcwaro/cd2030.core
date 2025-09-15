# package init hooks live here

# package-level state env
.cd2030_state <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # defaults for runtime state
  .cd2030_state$selected_group <- NULL
  .cd2030_state$overrides <- list()

  # lock immutable defaults if present
  ns <- asNamespace(pkgname)
  if (exists(".cd2030_indicator_groups", envir = ns, inherits = FALSE)) {
    lockBinding(".cd2030_indicator_groups", ns)
  }
}

