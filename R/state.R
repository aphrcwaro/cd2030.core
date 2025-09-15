# runtime config helpers

#' Set selected group
#' @param group One of "vaccine","rmncah"
#' @export
set_selected_group <- function(group) {
  group <- arg_match0(group, names(.cd2030_indicator_groups))
  .cd2030_state$selected_group <- group
  options(cd2030.selected_group = group)
  invisible(group)
}

#' Get selected group
#' @export
get_selected_group <- function() {
  val <- .cd2030_state$selected_group
  if (is.null(val)) {
    val <- getOption("cd2030.selected_group", "rmncah")
    .cd2030_state$selected_group <- val
  }
  val
}

#' Register or replace an indicator-group override
#' @export
register_indicator_group <- function(name, value) {
  stopifnot(is.character(name), length(name) == 1L)
  stopifnot(is.list(value), !is.null(names(value)))
  .cd2030_state$overrides[[name]] <- value
  invisible(name)
}

#' Reset an override
#' @export
reset_indicator_group <- function(name) {
  .cd2030_state$overrides[[name]] <- NULL
  invisible(name)
}
