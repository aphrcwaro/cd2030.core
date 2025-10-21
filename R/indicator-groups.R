#' Built-in indicator groups (defaults)
#'
#' `.cd2030_indicator_groups` defines the built-in indicator groups (a.k.a. *profiles*)
#' shipped with the package. Users can **override or extend** these via
#' [register_indicator_group()]; the live view used throughout the package is the
#' merged result returned by `.get_all_groups()`.
#'
#' Each group is a **named list of categories**, where each category contains a
#' character vector of indicator codes.
#'
#' @format A named list. Top-level names are group names (e.g. `"rmncah"`, `"vaccine"`).
#'   Each value is a named list of categories -> character vector of indicators.
#'
#' @examples
#' names(.cd2030_indicator_groups)              # built-in groups
#' .cd2030_indicator_groups$rmncah$anc          # RMNCAH ANC indicators
#'
#' @keywords internal
.cd2030_indicator_groups <- list(
  vaccine = list(
    anc   = c('anc1'),
    idelv = c('instdeliveries','instlivebirths'),
    vacc  = c('bcg','ipv1','ipv2','measles1','measles2','opv1','opv2','opv3',
              'penta1','penta2','penta3','pcv1','pcv2','pcv3','rota1','rota2')
  ),
  rmncah = list(
    anc   = c('anc1','anc_1trimester','anc4','ipt2','ipt3','syphilis_test','ifa90','hiv_test'),
    idelv = c('sba','instdeliveries','instlivebirths','csection','low_bweight','pnc48h',
              'total_stillbirth','stillbirth_f','stillbirth_m','maternal_deaths','neonatal_deaths'),
    vacc  = c('penta1','penta3','measles1','measles2','bcg'),
    opd   = c('opd_total','opd_under5'),
    ipd   = c('ipd_total','ipd_under5')
  )
)

#' Combine built-ins with user overrides
#'
#' Returns the **current** set of indicator groups after applying any overrides
#' registered with [register_indicator_group()]. Overrides can replace or merge
#' with built-ins by **group name**.
#'
#' @return A named list of groups (built-ins + overrides applied).
#' @noRd
.get_all_groups <- function() {
  list_merge(.cd2030_indicator_groups, .cd2030_state$overrides)
}

#' Set the selected indicator group globally
#'
#' Persists the selected group in session state and as an option
#' (`options(cd2030.selected_group = <name>)`). Downstream helpers can retrieve
#' it with [get_selected_group()].
#'
#' @param group Group name. Must exist in `.get_all_groups()`.
#'
#' @return Invisibly returns the resolved group name.
#'
#' @examples
#' set_selected_group("rmncah")
#' get_selected_group()
#'
#' @export
set_selected_group <- function(group) {
  all_names <- names(.get_all_groups())
  group <- arg_match(group, values = all_names)
  .cd2030_state$selected_group <- group
  options(cd2030.selected_group = group)
  invisible(group)
}

#' Get the globally selected indicator group
#'
#' Returns the last group set via [set_selected_group()]. If none is set for the
#' session, falls back to `getOption("cd2030.selected_group", "rmncah")` and caches it.
#'
#' @return A length-1 character vector (group name).
#'
#' @examples
#' get_selected_group()
#'
#' @export
get_selected_group <- function() {
  val <- .cd2030_state$selected_group %||% getOption('cd2030.selected_group', 'rmncah')
  .cd2030_state$selected_group <- val
  val
}

#' Register or override an indicator group (profile)
#'
#' Adds or modifies a group definition by **name**. If the name already exists,
#' use `on_conflict = "replace"` to replace the entire definition, or
#' `on_conflict = "merge"` to **union** indicators *per category*.
#'
#' Reserved names `"auto"` and `"custom"` cannot be registered.
#'
#' @param name A non-empty string: the group/profile name to create or modify.
#' @param value A **named list**: each element is a category containing a character
#'   vector of indicator codes.
#' @param on_conflict One of `"replace"`, `"merge"`, or `"error"`. `"merge"` performs
#'   a union per category (`sort(unique(c(old, new)))`).
#'
#' @return Invisibly returns `name`.
#'
#' @details
#' - If `name` matches a built-in (e.g. `"rmncah"`), the override will be layered
#'   on top of the built-in according to `on_conflict`.
#' - The merged, live view is available via `.get_all_groups()` or
#'   [get_indicator_groups()].
#'
#' @examples
#' # Replace RMNCAH completely
#' register_indicator_group(
#'   "rmncah",
#'   list(anc = c("anc1","anc4"), idelv = c("sba")),
#'   on_conflict = "replace"
#' )
#'
#' # Extend RMNCAH by merging into existing categories (union)
#' register_indicator_group(
#'   "rmncah",
#'   list(hiv = c("hiv_test","pmtct1")),
#'   on_conflict = "merge"
#' )
#'
#' @export
register_indicator_group <- function(name, value, on_conflict = c('replace', 'merge', 'error')) {
  on_conflict <- arg_match(on_conflict)

  if (!is_string(name) || !nzchar(name)) {
    cd_abort(c('x' = '{.arg name} must be a non-empty string'))
  }

  if (!is.list(value) || !is_named(value)) {
    cd_abort(c('x' = '{.arg value} must be a named list of categories with character vectors'))
  }

  bad <- purrr::map_lgl(value, ~ !is.character(.x) || anyNA(.x))
  if (any(bad)) {
    cd_abort(c('x' = 'Each category must be a character vector of indicators'))
  }

  exists_now <- name %in% names(.get_all_groups())
  if (exists_now && identical(on_conflict, 'error')) {
    cd_abort(c('x' = 'Indicator group {.arg name} already exists'))
  }

  if (identical(on_conflict, 'replace') || !exists_now) {
    .cd2030_state$overrides[[name]] <- value
  } else {
    # merge
    base <- .get_all_groups()[[name]] %||% list()
    cats <- union(names(base), names(value))
    merged <- cats %>%
      set_names(cats) %>%
      map(~ {
        lhs <- base[[.x]] %||% character()
        rhs <- value[[.x]] %||% character()
        sort(unique(c(lhs, rhs)))
      })
    .cd2030_state$overrides[[name]] <- merged
  }

  invisible(name)
}

#' Reset (remove) a group override
#'
#' Removes a user override for `name`. If `name` matches a built-in group, the
#' built-in remains intact; only the override layer is removed.
#'
#' @param name Group name whose override should be removed.
#'
#' @return Invisibly returns `name`.
#'
#' @examples
#' reset_indicator_group("rmncah")
#'
#' @export
reset_indicator_group <- function(name) {
  if (!is_string(name) || !nzchar(name)) {
    cd_abort(c('x' = 'name must be a non-empty string'))
  }

  .cd2030_state$overrides[[name]] <- NULL
  invisible(name)
}

#' List available indicator group names
#'
#' Returns all **current** groups (built-ins + any overrides).
#'
#' @return A sorted character vector of group names.
#'
#' @examples
#' list_indicator_groups()
#'
#' @export
list_indicator_groups <- function() {
  sort(names(.get_all_groups()))
}

#' Get Indicator Group Names
#' @return Character vector of indicator group names
#' @export
get_indicator_group_names <- function() names(get_indicator_groups())

#' Get the current indicator group definition
#'
#' Returns the **merged** definition for a group (built-in overlaid with any
#' override). If `group` is omitted, uses [get_selected_group()].
#'
#' @param group Group name. Defaults to the globally selected group.
#'
#' @return A named list of categories -> character vector of indicators.
#'
#' @examples
#' get_indicator_groups("rmncah")
#'
#' @export
get_indicator_groups <- function(group = get_selected_group()) {
  choices <- list_indicator_groups()
  group <- arg_match(group, values = choices)

  def <- .get_all_groups()[[group]]
  if (is.null(def)) {
    cd_abort(c('x' = 'Unknown indicator group {.val name}. Available: {paste(choices, collapse = ', '}'))
  }

  def
}

#' Auto-detect the best-matching indicator group from a vector of indicators
#'
#' Given a character vector of indicator codes present in the dataset, selects
#' the group whose **entire universe** is present in the data (strict coverage:
#' `group ⊆ data`). If multiple groups are fully covered, the tie is broken by
#' choosing the **largest** universe (most specific), then alphabetically.
#'
#' @param indicators Character vector of indicator codes present in the data.
#'
#' @return The detected group name (character scalar).
#'
#' @details
#' Detection considers the merged view from `.get_all_groups()` (i.e., built-ins
#' plus any user overrides registered via [register_indicator_group()]).
#'
#' @examples
#' \dontrun{
#' inds <- c("anc1","penta1","penta3","measles1","measles2","bcg")
#' detect_indicator_group(inds)
#' }
#'
#' @export
detect_indicator_group <- function(indicators) {
  if (!is.character(indicators) || !length(indicators)) {
    cd_abort(c('x' = '{.arg indicators} must be a non-empty character vector'))
  }

  indicators <- unique(indicators)

  # flatten each group's universe
  all_groups <- .get_all_groups()
  flat <- imap(all_groups, ~ unique(list_c(.x)))

  # Candidate groups whose entire universe is present in the dataset
  covers <- keep(names(flat), ~ {
    grp <- flat[[.x]]
    length(grp) > 0L && all(grp %in% indicators)
  })

  if (!length(covers)) {
    cd_abort(c(
      "x" = "No indicator group is fully covered by the dataset indicators.",
      "i" = "Register or extend a group via {.fun register_indicator_group}."
    ))
  }

  # Prefer the most specific covered group: largest universe size
  grp_sizes <- set_names(map_int(covers, ~ length(flat[[.x]])), covers)
  best <- names(grp_sizes)[grp_sizes == max(grp_sizes)]

  # Stable final tie-break
  if (length(best) > 1L) {
    cd_info(c('i' = paste0("Auto-detect tie resolved alphabetically among: ", paste(best, collapse = ", "))))
    best <- sort(best)[1]
  }

  # Log final decision and candidate sizes
  cd_info(c('i' = paste0(
    "Auto-detect candidates (|group|): ",
    paste(sprintf("%s=%d", names(grp_sizes), grp_sizes), collapse = ", "),
    " -> selected ", best
  )))

  best
}

#' Resolve a group name from input and available indicators
#'
#' Helper to turn `"auto"` or `"custom"` into a **concrete** group name given the
#' indicator vector from the data. For `"auto"`, uses [detect_indicator_group()].
#' For `"custom"`, `profile` must be a known group name (built-in or registered).
#'
#' @param indicators Character vector of indicators present in the dataset.
#' @param group One of `"auto"`, `"rmncah"`, `"vaccine"`, or `"custom"`.
#' @param profile For `"custom"`, the group name to use. Ignored otherwise.
#'
#' @return A concrete group name (character scalar).
#'
#' @examples
#' \dontrun{
#' resolve_indicator_group(c("anc1","penta1"), group = "rmncah")
#' }
#'
#' @noRd
resolve_indicator_group <- function(indicators, group = c('auto', 'rmncah', 'vaccine', 'custom'), profile = NULL) {
  group <- arg_match(group)

  if (group == 'custom' && is.null(profile)) {
    cd_abort(c('x' = '{.arg profile} must be provided in custom group.'))
  }

  resolved <- switch (
    group,
    auto = detect_indicator_group(indicators),
    custom = profile,
    group
  )

  resolved
}

#' @title Get All Indicators
#' @description Flatten all indicators from all groups
#' @return Character vector of all indicators
#' @export
get_all_indicators <- function() sort(list_c(get_indicator_groups()))

#' @title Get Indicators excluding indicators without denominator
#' @description Flatten all indicators from all groups
#' @return Character vector of all indicators
#' @export
get_indicator_without_opd_ipd <- function()  {
  groups <- get_indicator_groups()
  indicators <- sort(list_c(groups[!names(groups) %in% c("ipd", "opd")]))
  indicators[!indicators %in% c('sba', "total_stillbirth", "stillbirth_f", "stillbirth_m", "maternal_deaths", "neonatal_deaths", 'under5_deaths', 'total_deaths')]
}

#' @title Get Named Indicator Vector
#' @description Each indicator is named by its group
#' @return Named character vector of indicators
#' @export
get_named_indicators <- function() {
  groups <- get_indicator_groups()
  out <- list_c(groups)
  names(out) <- rep(names(groups), lengths(groups))
  out
}
