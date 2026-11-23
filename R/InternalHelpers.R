# quiets NOTE from R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c("."))

#' Print a message (info) to the user, checking option if package should be verbose or not
#'
#' @param ... message components
#'
#' @noRd
.TFL_message <- function(...) {
	if (getOption("TFLMonolixSuite.verbose", FALSE)) {
		message(...)
	}
}


#' Resolve the path of the R/Quarto/Rmd script that produced the current table
#'
#' Determines the file path of the script that called the current helper so it
#' can be stored as a data-frame attribute (`r_source`) and later rendered as
#' an auto-footnote by [styleTFL()].
#'
#' Detection order (first non-empty result wins):
#' 1. **Innermost `source()`'d file** — the deepest `ofile` on the call stack.
#'    When a meta-script `source()`s a table script (or a Quarto chunk does
#'    the same), this returns the *table* script, i.e. the one that actually
#'    built the data frame.
#' 2. **`knitr::current_input(dir = TRUE)`** — the `.Rmd`/`.qmd` being rendered
#'    when no nested `source()` is in flight.
#' 3. **`--file=` in `commandArgs()`** — set when launched via `Rscript`.
#' 4. **`rstudioapi::getSourceEditorContext()$path`** — the active editor tab
#'    in an interactive RStudio session.
#'
#' @return A normalised character path, or `NULL` when no source script can be
#'   determined (e.g. a plain interactive R console with no editor).
#'
#' @keywords internal
#' @noRd
.getRScriptPath <- function() {
	# 1. Innermost source()'d script: walk frames from deepest to outermost so
	# meta -> table -> helper resolves to "table.R", not "meta.R".
	frame_count <- sys.nframe()
	if (frame_count > 0) {
		for (n in rev(seq_len(frame_count))) {
			frame <- tryCatch(sys.frame(n), error = function(e) NULL)
			if (is.null(frame)) next
			ofile <- tryCatch(
				get0("ofile", envir = frame, inherits = FALSE),
				error = function(e) NULL
			)
			if (!is.null(ofile) && is.character(ofile) &&
					length(ofile) == 1 && nzchar(ofile)) {
				return(tryCatch(normalizePath(ofile, winslash = "/", mustWork = FALSE),
												error = function(e) ofile))
			}
		}
	}

	# 2. knitr / Quarto rendering context (no source() in flight).
	if (requireNamespace("knitr", quietly = TRUE)) {
		knitr_input <- tryCatch(
			knitr::current_input(dir = TRUE),
			error = function(e) NULL
		)
		if (!is.null(knitr_input) && is.character(knitr_input) && nzchar(knitr_input)) {
			return(knitr_input)
		}
	}

	# 3. Rscript invocation (no source() chain).
	cmd_args <- commandArgs(trailingOnly = FALSE)
	file_arg <- grep("^--file=", cmd_args, value = TRUE)
	if (length(file_arg) > 0) {
		path <- sub("^--file=", "", file_arg[1])
		if (nzchar(path)) {
			return(tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE),
											error = function(e) path))
		}
	}

	# 4. Interactive RStudio: fall back to the active editor tab.
	if (requireNamespace("rstudioapi", quietly = TRUE)) {
		rs_avail <- tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)
		if (isTRUE(rs_avail)) {
			rs_path <- tryCatch(
				rstudioapi::getSourceEditorContext()$path,
				error = function(e) NULL
			)
			if (!is.null(rs_path) && is.character(rs_path) && nzchar(rs_path)) {
				return(tryCatch(normalizePath(rs_path, winslash = "/", mustWork = FALSE),
												error = function(e) rs_path))
			}
		}
	}

	return(NULL)
}


# Function to load specified project, which will do nothing in the case that the project
# is already loaded
#

#' Check if project is loaded, and loads project if not
#'
#' Function to load specified project, which will do nothing in the case that the project
#' is already loaded. Will switch software for lixoftConnectors if necessary.
#'
#' @param projectFile Project file
#'
#'@noRd
.ensureProjectLoaded <- function(projectFile = NULL) {

	.checkLixoftConnectors()

	# suppress lixoftConnector messages (i.e., ERROR, WARNING, and INFO) in the case of connectors not initialized,
	# no project loaded, etc., the functions return NULL in these cases
	# Note: directory and seed are only project settings common to all 3 software
	current_prj_info  <- suppressMessages(lixoftConnectors::getProjectSettings())
	connector_state <- suppressMessages(lixoftConnectors::getLixoftConnectorsState())

	if (is.null(projectFile)) {
		# CASE 1: No project argument →  use current

		if (is.null(current_prj_info) || is.null(current_prj_info$directory)) {
			stop("No project is currently loaded and no project was provided.",
					 call. = FALSE)
		}
		.TFL_message("No project specified, using project results from: ",
						current_prj_info$directory)
	}
	else {
		# CASE 2: Project argument provided

		project_software <- .getSoftwareFromProjectFile(projectFile)
		if (project_software == "unknown") {
			stop("Unknown file type: ", projectFile)
		}

		# Get information on currently loaded project, if any
		# Note: only Monolix provides getProjectInfo()$project, so the only one we can verify which project loaded
		if (is.null(current_prj_info) || is.null(current_prj_info$project) || project_software != "monolix") {
			same_project <- FALSE

			if (is.null(connector_state)) {
				# connectors not initialized
				same_software <- FALSE
			} else{
				current_software <- connector_state$software
				same_software <- project_software == current_software
			}
		}
		else {
			# Project already loaded
			project_path <- normalizePath(projectFile, winslash = "/", mustWork = FALSE)
			current_project <- normalizePath(current_prj_info$project, winslash = "/", mustWork = FALSE)
			current_software <- connector_state$software

			same_project <- project_path == current_project
			same_software <- project_software == current_software
		}

		# Load project only if it differs or none loaded. Both connector calls report
		# failure by returning FALSE (not an R error), which would otherwise leave the
		# previously loaded project in place and silently return its results.
		if (!same_project) {
			if (!same_software) {
				# When the connectors were never initialized, a failure here is most likely
				# a wrong MonolixSuite installation folder, so explain how to set it.
				.connectorCallOrStop(
					.initializeConnectors(software = project_software, force = TRUE),
					paste0("initialize lixoftConnectors for ", project_software),
					hint = if (is.null(connector_state)) .connectorInitHelp() else NULL)
			}
			.TFL_message("Loading new project: ", projectFile)
			.connectorCallOrStop(
				lixoftConnectors::loadProject(projectFile = projectFile),
				paste0("load project '", projectFile, "'"))
		}
		else {
			.TFL_message("Project already loaded: ", current_project)
		}
	}
}

#' Run a lixoftConnectors call that reports failure through its return value
#'
#' `loadProject()` and `initializeLixoftConnectors()` do not raise R errors: on
#' failure they return `FALSE` and report the reason as an `"[ERROR] ..."`
#' message (e.g. project file not found, file not a valid project, data file
#' missing, unknown software). This helper evaluates such a call and turns a
#' failure into an R error that includes the connector's error text.
#'
#' A call counts as failed when it returns `FALSE`, or when it returns anything
#' other than `TRUE` together with an `"[ERROR]"` message. The return value is
#' the primary signal; the message text is only used to explain the error.
#' Messages are not muffled, so the connector output is still shown (or hidden
#' by an outer `suppressMessages()`).
#'
#' @param expr The connector call (evaluated inside the function).
#' @param what Short description for the error, e.g. `"load project 'x.pkx'"`.
#' @param hint Optional text appended to the error on a new line, e.g. how to fix it.
#' @return `invisible(TRUE)` on success; otherwise an error.
#' @noRd
.connectorCallOrStop <- function(expr, what, hint = NULL) {
	errs <- character()
	ok <- withCallingHandlers(expr, message = function(m) {
		txt <- trimws(conditionMessage(m))
		if (startsWith(txt, "[ERROR]")) errs <<- c(errs, trimws(sub("^\\[ERROR\\]", "", txt)))
	})
	if (isFALSE(ok) || (!isTRUE(ok) && length(errs) > 0)) {
		stop("Could not ", what,
				 if (length(errs)) paste0(": ", paste(errs, collapse = " ")) else ".",
				 if (!is.null(hint)) paste0("\n", hint),
				 call. = FALSE)
	}
	invisible(TRUE)
}

#' Gets the software corresponding to the project file extension
#'
#' Note this only includes the three applications supported by the lixoftConnectors
#'
#' @param projectFile Project file
#'
#' @returns "pkanalix", "monolix", or "simulx" (or "unknown" for unrecognized extension)
#' @noRd
.getSoftwareFromProjectFile = function(projectFile)
{
	ext <- tolower(tools::file_ext(projectFile))
	software <- switch(ext,
										 pkx = "pkanalix",
										 mlxtran = "monolix",
										 smlx = "simulx",
										 "unknown")
	return(software)
}

#' Gets the extension corresponding to the specified software application
#'
#' @param software Software application
#'
#' @returns File extension (or "unknown" for unrecognized application)
#' @noRd
.getExtentionFromSoftware = function(software)
{
	software <- tolower(software)
	ext <- switch(software,
								pkanalix = "pkx",
								monolix = "mlxtran",
								simulx = "smlx",
								sycomore = "syc",
								"unknown")
	return(ext)
}

#-------------------------------------------------------------------------------
#' Checks the project file matches the specified software
#'
#' Checks that project file is for the specified software application via the file extension.
#' If the project file is NULL, checks that there is a currently loaded project of the
#' specified software application.
#'
#' @param projectFile Project file
#' @param software Software application required (multiple can be specified)
#'
#'@noRd
.checkProjectFile = function(projectFile, software)
{
	# if project file is NULL, check that currently loaded project is correct software
	if (is.null(projectFile)) {
		# The currently loaded project is read through the connectors; check them
		# first so a missing or uninitialized lixoftConnectors is reported as such
		# rather than as "no project loaded" (the tryCatch below would mask it).
		# Not needed for an explicit file (e.g. tableRunHistory() Sycomore files).
		.checkLixoftConnectors(requireInitialized = TRUE)

		# check if currently loaded project is the specified software application
		current_prj_info  <- tryCatch(lixoftConnectors::getProjectSettings(), error = function(e) NULL)

		if (is.null(current_prj_info) || is.null(current_prj_info$directory)) {
			stop("No project is currently loaded and no project was provided.",
					 call. = FALSE)
		}
		else {
			connector_state <- tryCatch(lixoftConnectors::getLixoftConnectorsState(), error = function(e) NULL)

			if (!(connector_state$software %in% software)) {
				stop("This function is only available for ", software,
						 " projects,  while the current project uses ", connector_state$software, ".",
						 call. = FALSE)
			}
		}
	}
	# otherwise verify file extension of to be loaded project
	else {
		ext <- sapply(software, .getExtentionFromSoftware)
		if (!any(endsWith(projectFile, ext)))
			stop("This function is only available for ", software, " projects, while the specified project is ", ext, ".",
					 call. = FALSE)
	}
}

.checkFlextable <- function() {
	if (!requireNamespace("flextable", quietly = TRUE))
	{
		stop("The flextable package is necessary for this function. Please install it from CRAN.",
				 call. = FALSE)
	}
}

#-------------------------------------------------------------------------------
# lixoftConnectors initialization
#
# lixoftConnectors is in Suggests: it is installed with MonolixSuite rather than
# from CRAN, and styleTFL() works without it. Initializing the connectors loads
# the MonolixSuite library (lib/lixoftConnectors.dll on Windows,
# lib/liblixoftConnectors.so on Linux and macOS) from the installation folder, which is either passed as `path` or read from the
# `monolixSuite=` line of ~/lixoft/lixoft.ini (the most recently launched
# MonolixSuite version). Constraints imposed by lixoftConnectors:
#   * the library can only be loaded once per R session, so the installation
#     folder cannot be changed without restarting R;
#   * the library fails to load ("LoadLibrary failure") once the `zip` package
#     is loaded, e.g. by flextable or officer.
# The `TFLMonolixSuite.lixoftPath` option lets users point to a MonolixSuite
# installation that is not the one recorded in lixoft.ini.

# Package-level state; `connectorInitError` holds the reason the initialization
# in .onLoad() failed (NULL if it succeeded or was not attempted).
.tflState <- new.env(parent = emptyenv())

#' Whether the lixoftConnectors MonolixSuite library is loaded in this session
#'
#' Same test that `lixoftConnectors::initializeLixoftConnectors()` uses: whether
#' the MonolixSuite library is among the loaded DLLs (the R package itself has no
#' DLL). The DLL name is the library file name without extension, which depends
#' on the OS: "lixoftConnectors" (lixoftConnectors.dll) on Windows, and
#' "liblixoftConnectors" (liblixoftConnectors.so) on Linux and macOS.
#' @param dlls Names of the loaded DLLs.
#' @noRd
.connectorsInitialized <- function(dlls = names(getLoadedDLLs())) {
	any(c("lixoftConnectors", "liblixoftConnectors") %in% dlls)
}

#' Whether the zip package is loaded, after which the connectors cannot be initialized
#' @noRd
.zipLoaded <- function() {
	"zip" %in% loadedNamespaces()
}

#' MonolixSuite installation folder from the `TFLMonolixSuite.lixoftPath` option
#'
#' @return The folder, or `""` (lixoftConnectors then uses lixoft.ini) if unset.
#' @noRd
.lixoftPathOption <- function() {
	path <- getOption("TFLMonolixSuite.lixoftPath")
	if (is.null(path) || identical(path, "")) return("")
	if (!is.character(path) || length(path) != 1 || is.na(path)) {
		stop("The option 'TFLMonolixSuite.lixoftPath' must be a single string giving ",
				 "the MonolixSuite installation folder.", call. = FALSE)
	}
	path.expand(path)
}

#' Explain how to point the connectors to a MonolixSuite installation
#'
#' @param restart If `TRUE`, the connectors can no longer be initialized in this
#'   session, so only instructions that start with restarting R are given.
#'   Defaults to whether `zip` is loaded (e.g. flextable or officer loaded before
#'   this package), since initializing the connectors then fails.
#' @noRd
.connectorInitHelp <- function(restart = .zipLoaded()) {
	optPath <- tryCatch(.lixoftPathOption(), error = function(e) "")
	cause <- if (nzchar(optPath)) {
		paste0("The MonolixSuite installation folder is taken from the option ",
					 "TFLMonolixSuite.lixoftPath (currently \"", optPath, "\").\n")
	} else {
		paste0("The MonolixSuite installation folder is read from lixoft.ini, which ",
					 "points to the most recently launched MonolixSuite version.\n")
	}
	# The option is only read when the connectors are initialized (at package
	# load, or when a function reads a project file), not when it is set; so
	# setting it in a session where the package is already loaded and then using
	# styleTFL() (which loads zip) would still require a restart. Hence: the
	# option goes before library(), and within a loaded session the connectors
	# must be initialized explicitly before styleTFL().
	optionStep <- paste0(
		"set options(TFLMonolixSuite.lixoftPath = \"<MonolixSuite folder>\") before ",
		"library(MonolixSuiteTFL) (add it to your .Rprofile to keep it)")
	initStep <- paste0(
		"call lixoftConnectors::initializeLixoftConnectors(path = \"<MonolixSuite folder>\") ",
		"before using styleTFL() or loading flextable or officer")
	if (restart) {
		# zip may have been loaded before this package, in which case the folder
		# can be right and the fix is only the load order
		paste0(
			"Restart R, and load MonolixSuiteTFL before flextable, officer, or other ",
			"packages that load zip.\n",
			cause,
			"To use the MonolixSuite installation in another folder, after restarting R either:\n",
			"  - ", optionStep, ", or\n",
			"  - ", initStep, ".")
	} else {
		paste0(
			cause,
			"To use the MonolixSuite installation in another folder, either:\n",
			"  - restart R and ", optionStep, ", or\n",
			"  - in this session, ", initStep, " (after that, R must be restarted first).")
	}
}

#' Initialize the lixoftConnectors, using the `TFLMonolixSuite.lixoftPath` option
#'
#' The folder from the option is only passed when the connectors are not yet
#' initialized: once the library is loaded, lixoftConnectors only accepts
#' `path = ""` (or the exact library path) to switch software.
#'
#' @param software "monolix", "pkanalix", or "simulx".
#' @param force Passed to `initializeLixoftConnectors()`; `TRUE` switches
#'   software without asking for confirmation.
#' @return The (invisible) return value of `initializeLixoftConnectors()`.
#' @noRd
.initializeConnectors <- function(software = "monolix", force = FALSE) {
	path <- if (.connectorsInitialized()) "" else .lixoftPathOption()
	lixoftConnectors::initializeLixoftConnectors(software = software, path = path, force = force)
}

#' Initialize the connectors when the package is loaded
#'
#' Called from .onLoad(). Does nothing if lixoftConnectors is not installed, or
#' if the connectors are already initialized (e.g. the user called
#' `initializeLixoftConnectors(path = ...)` before loading this package; calling
#' it again with another software would ask to confirm a software switch).
#' Never fails: the reason for a failed initialization is stored in
#' `.tflState$connectorInitError` and reported by .onAttach() and
#' .checkLixoftConnectors().
#' @noRd
.initializeConnectorsOnLoad <- function() {
	.tflState$connectorInitError <- NULL
	if (!requireNamespace("lixoftConnectors", quietly = TRUE) || .connectorsInitialized()) {
		return(invisible(NULL))
	}
	.tflState$connectorInitError <- tryCatch({
		suppressMessages(.connectorCallOrStop(.initializeConnectors(), "initialize lixoftConnectors"))
		NULL
	}, error = function(e) conditionMessage(e))
	invisible(NULL)
}

#' Check that lixoftConnectors can be used
#'
#' Call this at the entry of every code path that reads project results through
#' the connectors. Errors if lixoftConnectors is not installed, or if the
#' connectors are not initialized and cannot be anymore because `zip` is loaded.
#'
#' @param requireInitialized If `TRUE`, also error when the connectors are not
#'   initialized (for functions that use the currently loaded project and so
#'   cannot initialize the connectors themselves).
#' @noRd
.checkLixoftConnectors <- function(requireInitialized = FALSE) {
	if (!requireNamespace("lixoftConnectors", quietly = TRUE))
	{
		stop("The lixoftConnectors package is necessary for this function. ",
				 "It is installed with MonolixSuite; see the MonolixSuite documentation ",
				 "for installation instructions.",
				 call. = FALSE)
	}
	if (.connectorsInitialized()) return(invisible(TRUE))

	loadError <- .tflState$connectorInitError
	reason <- if (is.null(loadError)) "" else paste0(loadError, "\n")
	if (.zipLoaded()) {
		stop("lixoftConnectors is not initialized, and can no longer be initialized in ",
				 "this R session because the zip package is already loaded (for example by ",
				 "styleTFL(), flextable, or officer).\n", reason,
				 .connectorInitHelp(restart = TRUE),
				 call. = FALSE)
	}
	if (requireInitialized) {
		stop("lixoftConnectors is not initialized, so no project is loaded.\n", reason,
				 .connectorInitHelp(),
				 call. = FALSE)
	}
	invisible(TRUE)
}
#-------------------------------------------------------------------------------
#' Rounds numbers to specified number of digits
#'
#' Three rounding methods are supported: "monolixSuiteRounding" which mirrors the behavior of
#' MonolixSuite applications - values with |x| >= 1 are rounded to `digits` decimal places and
#' values with |x| < 1 to `digits` significant figures; "significantFigures" where the number of significant
#' digits are specified using the standard/textbook definition (zeroes non-significant digits
#' before the decimal for large numbers); and "significantFiguresPreserved" which is a modified
#' significant-figures definition that never zeroes digits before the decimal - once the integer
#' part already has as many digits as requested, the value is simply rounded to the nearest whole
#' number. To retain the original values and not round, use "none".
#'
#' @param to_round number, vector or data frame to round
#' @param digits number of digits to round to
#' @param exclude names of columns to exclude
#' @param rounding_method "monolixSuiteRounding" (default), "significantFigures",
#'   "significantFiguresPreserved", or "none"
#'
#' @returns rounded number
#' @noRd
.round_num = function(to_round, digits, exclude = character(0), rounding_method = c("monolixSuiteRounding", "significantFigures", "significantFiguresPreserved", "none")) {

	rounding_method <- match.arg(rounding_method)

	if (rounding_method == "none") {
		return(to_round)
	}

	if (is.data.frame(to_round)) {
		rr <- to_round %>%
			dplyr::mutate(dplyr::across(tidyselect::where(is.double) & !dplyr::any_of(exclude),
																	.fns = ~.round_num(.x, digits, rounding_method = rounding_method))) %>%
			# Character-to-numeric conversion if needed
			dplyr::mutate(dplyr::across(
				tidyselect::where(is.character) & !dplyr::any_of(exclude),
				.fns = ~{
					num_converted <- suppressWarnings(as.numeric(.x))
					if (any(!is.na(num_converted))) {
						.round_num(num_converted, digits, rounding_method = rounding_method)
					} else {
						.x
					}
				}
			))
	}else if(is.numeric(to_round)) {
		# Track NaN positions BEFORE rounding
		nan_positions <- is.nan(to_round)

		if (rounding_method == "monolixSuiteRounding") {
			# MonolixSuite behavior: |x| >= 1 -> round to `digits` decimal places;
			# |x| < 1 -> round to `digits` significant figures.
			rr <- ifelse(abs(to_round) >= 1,
									 round(to_round, digits = digits),
									 signif(to_round, digits = digits))
		} else if (rounding_method == "significantFigures") {
			rr <- signif(to_round, digits = digits)
		} else if (rounding_method == "significantFiguresPreserved") {
			# Nb of digits before the decimal point (0 for |x| < 1).
			# Computed with trunc()+nchar() rather than floor(log10(.))+1 to avoid
			# floating-point edge cases (e.g. log10(1000) evaluating fractionally below 3).
			int_digits <- ifelse(abs(to_round) < 1, 0L,
													 nchar(format(trunc(abs(to_round)), scientific = FALSE, trim = TRUE)))

			rr <- ifelse(int_digits >= digits,
									 round(to_round, digits = 0),
									 signif(to_round, digits = digits))
		}

		# Restore NaN values AFTER rounding
		rr[nan_positions] <- NaN
	}else{
		rr <- to_round
	}

	return(rr)
}
#-------------------------------------------------------------------------------
#' Round a numeric vector and format it as a display string
#'
#' Display-layer companion to `.round_num()`. Rounds `x` with the given
#' `digits`/`method` (so the result is canonical and matches `.round_num()`),
#' then renders each value as a character string at the precision that
#' `digits`/`method` imply. This is where trailing-zero display is handled: a
#' numeric cannot carry trailing zeros, so the padded form (`1.20`, `0.010`) only
#' exists as a string.
#'
#' Keep this separate from `.round_num()` / `.applyRounding()`, which must stay
#' numeric for mid-pipeline work (ordering, merging, combined metrics). Apply
#' `.formatRounded()` only at the display boundary, once per (digits, method)
#' group — for per-parameter overrides, call it with each parameter's own config.
#'
#' @param x Numeric vector (character-numeric is coerced; genuinely non-numeric
#'   input is returned via `as.character()` unchanged).
#' @param digits Number of digits (n). Interpreted per `method`.
#' @param method One of "monolixSuiteRounding", "significantFigures",
#'   "significantFiguresPreserved", "none".
#' @param trailingZeros Logical. If `TRUE`, pad each value to the precision
#'   implied by `digits`/`method` (`1.2 -> "1.20"` for n = 3 sig figs). If
#'   `FALSE` (default), show the minimal faithful form with no padding
#'   (`"1.2"`), which also avoids the uniform-decimal padding that a numeric
#'   column would otherwise get when rendered.
#'
#' @details
#' The number of decimals `d` is derived per value from the rounded magnitude:
#' \itemize{
#'   \item significantFigures: `d = max(0, n - floor(log10(|x|)) - 1)`
#'   \item significantFiguresPreserved: as above, but `d = 0` once the integer
#'     part already has >= n digits
#'   \item monolixSuiteRounding: `d = n` for |x| >= 1 (n decimal places), else
#'     `n - floor(log10(|x|)) - 1` (n significant figures)
#' }
#' Zero gets the decimals of a value of 1 (`"0.000"` for monolixSuiteRounding
#' and n = 3, `"0.00"` for the significant-figure methods). Values with
#' |x| >= 1e9 or 0 < |x| < 1e-6 are shown in scientific notation with n
#' significant figures (e.g. `"2.92e+53"`), for every method except "none".
#' `NA` is preserved as `NA_character_` and `NaN` as `"NaN"` so flextable's
#' `na_str`/`nan_str` defaults still apply downstream.
#'
#' @returns Character vector the same length as `x`.
#' @noRd
.formatRounded <- function(x,
													 digits,
													 method = c("monolixSuiteRounding", "significantFigures",
																			"significantFiguresPreserved", "none"),
													 trailingZeros = FALSE) {
	method <- match.arg(method)

	# Coerce character-numeric; return genuinely non-numeric input untouched.
	if (!is.numeric(x)) {
		xn <- suppressWarnings(as.numeric(x))
		if (!any(!is.na(xn))) return(as.character(x))
		x <- xn
	}

	# "none": no rounding, minimal stringification. Preserve the NA/NaN
	# distinction (is.na() is TRUE for NaN, so restore NaN after blanking NA).
	if (method == "none") {
		out <- as.character(x)
		out[is.na(x)]  <- NA_character_
		out[is.nan(x)] <- "NaN"
		return(out)
	}

	# Round to the canonical value first so d is computed from the rounded
	# magnitude (e.g. 9.96 -> 10 -> "10", not "10.0").
	xr <- .round_num(x, digits = digits, rounding_method = method)

	ax   <- abs(xr)
	mag  <- floor(log10(ax))            # position of the first significant digit

	d <- switch(method,
		"significantFigures" = pmax(0, digits - mag - 1),
		"significantFiguresPreserved" = {
			int_digits <- ifelse(ax < 1, 0, mag + 1)
			ifelse(int_digits >= digits, 0, pmax(0, digits - mag - 1))
		},
		# |x| >= 1: `digits` decimal places; |x| < 1: `digits` significant figures
		"monolixSuiteRounding" = ifelse(ax >= 1, digits, pmax(0, digits - mag - 1))
	)

	# Guard non-finite magnitudes (NA, Inf).
	d[!is.finite(d)]   <- 0
	d[!is.finite(ax)]  <- 0
	# Zero has no magnitude: give it the decimals a value of 1 gets, so it pads
	# like its neighbours ("(0.000; 1.000)" rather than "(0; 1.000)").
	is_zero    <- which(ax == 0)
	d[is_zero] <- if (method == "monolixSuiteRounding") digits else max(0, digits - 1)
	xr[is_zero] <- 0   # drop the sign of -0 (e.g. round(-0.0001, 3)) so it never shows as "-0.000"
	d <- as.integer(d)

	# sprintf vectorises over both the precision (.*) and the value.
	out <- sprintf("%.*f", d, xr)

	# Very large / very small magnitudes: scientific notation with `digits`
	# significant figures. Fixed notation is unreadable there (e.g. an RSE of
	# 2.92e+53 printed as a 54-digit number) and beyond ~1e15 shows
	# floating-point noise digits.
	is_sci <- which(is.finite(ax) & ax != 0 & (ax >= 1e9 | ax < 1e-6))
	if (length(is_sci)) {
		out[is_sci] <- sprintf("%.*e", as.integer(max(0, digits - 1)), xr[is_sci])
	}

	# Minimal form: strip trailing zeros and any dangling decimal point.
	if (!isTRUE(trailingZeros)) {
		is_fixed       <- !seq_along(out) %in% is_sci
		has_dot        <- is_fixed & grepl(".", out, fixed = TRUE)
		out[has_dot]   <- sub("0+$", "", out[has_dot])
		out[has_dot]   <- sub("\\.$", "", out[has_dot])
		# scientific: strip trailing zeros of the mantissa only ("1.20e+05" -> "1.2e+05")
		out[is_sci]    <- sub("\\.?0+e", "e", out[is_sci])
	}

	# Preserve missing/NaN for downstream na_str / nan_str handling.
	out[is.na(xr)]  <- NA_character_
	out[is.nan(xr)] <- "NaN"
	out
}
#-------------------------------------------------------------------------------
#' Round a data frame with optional per-parameter overrides
#'
#' Wraps `.round_num()`. When `overrides` is NULL it is identical to
#' `.round_num()`, so it is a drop-in replacement at existing call sites.
#' Otherwise every parameter listed in `overrides` is rounded with its own
#' digits/method and everything else keeps the global `digits`/`method`.
#'
#' @param df data frame to round
#' @param digits global number of digits
#' @param method global rounding method (see `.round_num()`)
#' @param exclude descriptor columns never rounded
#' @param overrides canonical data.frame(parameter, digits, method), or NULL
#' @param paramCol for `paramAxis = "row"`, the column holding the parameter id
#' @param paramAxis "row" (parameter is a value in `paramCol`; summary/pop/ratio/
#'   BE tables) or "column" (parameter is a column name; listings)
#' @param format Logical. If `FALSE` (default) the non-excluded numeric columns
#'   are rounded and returned **numeric** (original behaviour, for mid-pipeline
#'   use). If `TRUE` they are rounded and rendered as **character** display
#'   strings via `.formatRounded()` - use only at the display boundary.
#' @param trailingZeros Logical, passed to `.formatRounded()` when
#'   `format = TRUE` (ignored otherwise). `TRUE` (default) pads each value to the
#'   precision its `digits`/`method` imply; `FALSE` shows the minimal form.
#'
#' @returns the data frame with the non-excluded numeric columns rounded, row
#'   order preserved. Those columns are numeric when `format = FALSE` and
#'   character when `format = TRUE`.
#' @noRd
.applyRounding <- function(df, digits, method,
													 exclude       = character(0),
													 overrides     = NULL,
													 paramCol      = NULL,
													 paramAxis     = c("row", "column"),
													 format        = FALSE,
													 trailingZeros = TRUE) {

	# Round (numeric, default) or format (character) the non-excluded columns for a
	# single (digits, method). In format mode all numeric columns return character
	# (doubles via .formatRounded(); integer counts via as.character()), so value
	# columns never mix types.
	.apply <- function(d, digits, method, exclude) {
		if (!isTRUE(format)) {
			return(.round_num(d, digits = digits, exclude = exclude,
												rounding_method = method))
		}
		dplyr::mutate(d, dplyr::across(
			tidyselect::where(is.numeric) & !dplyr::any_of(exclude),
			.fns = ~ if (is.double(.x)) {
				.formatRounded(.x, digits = digits, method = method, trailingZeros = trailingZeros)
			} else {
				as.character(.x)
			}))
	}
	# No overrides -> single global pass.
	if (is.null(overrides) || !nrow(overrides)) {
		return(.apply(df, digits = digits, method = method, exclude = exclude))
	}
	paramAxis <- match.arg(paramAxis)

	if (paramAxis == "column") {
		# Parameters are columns: process everything globally EXCEPT the overridden
		# columns, then each overridden column with its own config.
		over_cols <- intersect(overrides$parameter, names(df))
		df <- .apply(df, digits = digits, method = method,
								 exclude = union(exclude, over_cols))
		for (col in over_cols) {
			cfg <- overrides[overrides$parameter == col, , drop = FALSE][1, ]
			if (isTRUE(format)) {
				df[[col]] <- .formatRounded(df[[col]], digits = cfg$digits,
																		method = cfg$method, trailingZeros = trailingZeros)
			} else {
				df[[col]] <- .round_num(df[[col]], digits = cfg$digits,
																rounding_method = cfg$method)
			}
		}
		return(df)
	}

	# paramAxis == "row": parameter identity lives in a key column.
	if (is.null(paramCol) || !paramCol %in% names(df)) {
		return(.apply(df, digits = digits, method = method, exclude = exclude))
	}
	df$.orig_order <- seq_len(nrow(df))
	# Protect the ordering key in both modes: in format mode it must NOT become a
	# character, or order() below would sort it lexicographically (1,10,11,...,2).
	excl     <- union(exclude, ".orig_order")
	is_over  <- df[[paramCol]] %in% overrides$parameter

	glob_rows <- df[!is_over, , drop = FALSE]
	if (nrow(glob_rows)) {
		glob_rows <- .apply(glob_rows, digits = digits, method = method, exclude = excl)
	}
	over_rows <- df[is_over, , drop = FALSE]
	if (nrow(over_rows)) {
		parts <- lapply(split(over_rows, over_rows[[paramCol]]), function(g) {
			cfg <- overrides[overrides$parameter == g[[paramCol]][1], , drop = FALSE][1, ]
			.apply(g, digits = cfg$digits, method = cfg$method, exclude = excl)
		})
		over_rows <- do.call(rbind, parts)
	}
	out <- rbind(glob_rows, over_rows)
	out <- out[order(out$.orig_order), , drop = FALSE]
	out$.orig_order <- NULL
	rownames(out) <- NULL
	out
}

#' PrettyLabels
#'
#' Converts statistical parameter column names to human-readable, formatted labels
#' for presentation in tables and reports. Maps common statistical abbreviations
#' to their full, properly formatted names.
#'
#' @param df A data frame containing statistical columns to be relabeled.
#'
#' @return A data frame with column names converted to pretty, human-readable labels.
.prettyLabels <- function(df) {
	# Helper function to extract CI level from column name
	extract_ci_level <- function(col_name) {
		# Match pattern like IIV_CI_2.5_97.5 (with prefix)
		matches <- regmatches(col_name, regexec("_CI_([0-9.]+)_([0-9.]+)$", col_name))
		if (length(matches[[1]]) == 3) {
			lower_p <- as.numeric(matches[[1]][2])
			upper_p <- as.numeric(matches[[1]][3])
			ci_level <- round(upper_p - lower_p)
			return(ci_level)
		}
		# Match pattern like CI_2.5_97.5 (standalone, no prefix)
		matches <- regmatches(col_name, regexec("^CI_([0-9.]+)_([0-9.]+)$", col_name))
		if (length(matches[[1]]) == 3) {
			lower_p <- as.numeric(matches[[1]][2])
			upper_p <- as.numeric(matches[[1]][3])
			ci_level <- round(upper_p - lower_p)
			return(ci_level)
		}
		return(NULL)
	}

	# Static labels for exact matches
	stat_labels <- c(
		"parameter"           = "Parameter",
		"mean"                = "Mean",
		"sd"                  = "SD",
		"se"                  = "SE",
		"CV"                  = "CV (%)",
		"min"                 = "Minimum",
		"max"                 = "Maximum",
		"median"              = "Median",
		"geoMean"             = "Geometric\nMean",
		"GeoMean"             = "Geometric\nMean",
		"geoSD"               = "Geometric\nSD",
		"geoCV"               = "Geometric\nCV (%)",
		"GeomCV"              = "Geometric\nMean (CV %)",
		"GeomSD"              = "Geometric\nMean (SD)",
		"MeanSD"              = "Arithmetic\nMean (SD)",
		"MeanCV"              = "Arithmetic\nMean (CV %)",
		"harmMean"            = "Harmonic\nMean",
		"P05"                 = "5th Percentile",
		"P95"                 = "95th Percentile",
		"N"                   = "N",
		"Ntot"                = "N Total",
		"Nobs"                = "n",
		"Nmiss"               = "N Missing",
		"p.value"             = "p-value",
		"RSE"                 = "RSE (%)",
		"CI"                  = "Confidence \nInterval",
		"FE_Estimate"         = "Fixed Effect \nEstimate",
		"FE_RSE"              = "RSE (%)",
		"IIV_SD"              = "IIV\nSD",
		"IIV_CV"              = "IIV\nCV (%)",
		"IIV_RSE"             = "IIV\nRSE (%)",
		"IOV_SD"              = "IOV\nSD",
		"IOV_CV"              = "IOV\nCV (%)",
		"IOV_RSE"             = "IOV\nRSE (%)",
		"Dosing.range"        = "Dosing Range",
		"Point.estimate"      = "Point Estimate",
		"Criteria.interval"   = "Criteria\nInterval",
		"Ratio"               = "Geometric Mean\nRatio (%)",
		"Parent"              = "Parent Run",
		"Path"                = "Project Path",
		"LLlin"               = "LL\n(Linearization)",
		"LLis"                = "LL\n(Imp. Sampling)",
		"BICclin"             = "BICc\n(Linearization)",
		"BICcis"              = "BICc\n(Imp. Sampling)",
		"dLLlin"              = "ΔLL\n(Linearization)",
		"dLLis"               = "ΔLL\n(Imp. Sampling)",
		"dBICclin"            = "ΔBICc\n(Linearization)",
		"dBICcis"             = "ΔBICc\n(Imp. Sampling)",
		"Run"                 = "Run",
		"Reference"           = "Reference\nModel",
		"Tested"              = "Tested\nRelationship",
		"Step"                = "Step",
		"CovariateModel"      = "Covariate\nModel",
		"LL"                  = "OFV\n(-2LL)",
		"BICc"                = "BICc",
		"dLL"                 = "ΔOFV",
		"dBICc"               = "ΔBICc",
		"Significant"         = "Significant",
		"Improved"            = "BICc\nImproved",
		"BestModel"           = "Best\nModel",
		"ActualTime"          = "Actual Time",
		"NominalTime"         = "Nominal Time",
		"UsedConc"            = "Used Concentration",
		"nbSbj"               = "Nb. of Subjects",
		"totalNbObs"          = "Total Nb. of Obs.",
		"meanNbObsPerSbj"     = "Mean Nb. of Obs./Subject",
		"minNbObsPerSbj"      = "Min. Nb. of Obs./Subject",
		"maxNbObsPerSbj"      = "Max. Nb. of Obs./Subject")

	# Process each column name
	col_names <- names(df)
	new_names <- sapply(col_names, function(col) {
		# First check for dynamic CI columns (pattern matching for any confidence level)
		# Matches both prefixed (e.g., IIV_CI_2.5_97.5) and standalone (e.g., CI_2.5_97.5)
		if (grepl("(^|_)CI_[0-9.]+_[0-9.]+$", col)) {
			ci_level <- extract_ci_level(col)

			if (!is.null(ci_level)) {
				# Determine prefix and format accordingly (check most specific first)
				if (grepl("^Boot_IOV_CI", col)) {
					return(paste0("Bootstrap IOV\n", ci_level, "% CI"))
				} else if (grepl("^Boot_IIV_CI", col)) {
					return(paste0("Bootstrap IIV\n", ci_level, "% CI"))
				} else if (grepl("^Boot_CI", col)) {
					return(paste0("Bootstrap\n", ci_level, "% CI"))
				} else if (grepl("^IOV_CI", col)) {
					return(paste0("IOV\n", ci_level, "% CI"))
				} else if (grepl("^IIV_CI", col)) {
					return(paste0("IIV\n", ci_level, "% CI"))
				} else if (grepl("^FE_CI", col)) {
					return(paste0(ci_level, "% CI"))
				} else if (grepl("^CI_", col)) {
					return(paste0(ci_level, "% CI"))
				}
			}
		}

		# Handle Bootstrap IOV columns (non-CI)
		if (grepl("^Boot_IOV_", col)) {
			suffix <- sub("^Boot_IOV_", "", col)
			if (suffix == "Median") return("Bootstrap IOV\nMedian")
			if (suffix == "RSE") return("Bootstrap IOV\nRSE (%)")
			return(paste0("Bootstrap IOV\n", suffix))
		}

		# Handle Bootstrap IIV columns (non-CI)
		if (grepl("^Boot_IIV_", col)) {
			suffix <- sub("^Boot_IIV_", "", col)
			if (suffix == "Median") return("Bootstrap IIV\nMedian")
			if (suffix == "RSE") return("Bootstrap IIV\nRSE (%)")
			return(paste0("Bootstrap IIV\n", suffix))
		}

		# Handle Bootstrap FE columns (non-CI)
		if (grepl("^Boot_", col)) {
			suffix <- sub("^Boot_", "", col)
			if (suffix == "Median") return("Bootstrap\nMedian")
			if (suffix == "RSE") return("Bootstrap\nRSE (%)")
			return(paste0("Bootstrap\n", suffix))
		}

		# Check static labels for exact match
		if (col %in% names(stat_labels)) {
			return(unname(stat_labels[col]))
		}

		# Default: keep original name
		return(col)
	}, USE.NAMES = FALSE)

	names(df) <- new_names
	return(df)
}

#' Filter run history table to show only selected criteria columns
#'
#' @description
#' Removes LL or BICc columns (including deltas) based on user selection.
#' - filterCriteria = "LL": removes BICclin, BICcis, dBICclin, dBICcis
#' - filterCriteria = "BICc": removes LLlin, LLis, dLLlin, dLLis
#' - filterCriteria = "none": keeps all columns (default)
#'
#' @param df Data frame with run history
#' @param filterCriteria Which criteria to filter: "none", "LL", or "BICc"
#' @return Data frame with filtered columns
#' @keywords internal
.filterCriteriaColumns <- function(df, filterCriteria = c("none", "LL", "BICc")) {
	filterCriteria <- match.arg(filterCriteria)

	if (filterCriteria == "none") {
		return(df)
	}

	# Define columns to remove (base + delta columns)
	if (filterCriteria == "LL") {
		# Keep LL columns, remove all BICc columns
		cols_to_remove <- c("BICclin", "BICcis", "dBICclin", "dBICcis")
	} else {
		# Keep BICc columns, remove all LL columns
		cols_to_remove <- c("LLlin", "LLis", "dLLlin", "dLLis")
	}

	# Remove only columns that exist
	cols_to_remove <- intersect(cols_to_remove, names(df))
	df <- df[, !names(df) %in% cols_to_remove, drop = FALSE]

	return(df)
}

#' Validate sorting and filtering criteria compatibility
#'
#' @description
#' Ensures that sortByCriteria and filterCriteria are compatible:
#' - If sorting by a criteria, you can only filter by "none" or that same criteria
#' - Cannot sort by LL but filter to show only BICc (and vice versa)
#'
#' @param sortByCriteria Sorting criteria: "none", "LL", or "BICc"
#' @param filterCriteria Filter criteria: "none", "LL", or "BICc"
#' @return NULL (invisibly) if valid, otherwise stops with error
#' @keywords internal
.validateCriteriaArgs <- function(sortByCriteria, filterCriteria) {
	# If not sorting, any filterCriteria is valid
	if (sortByCriteria == "none") {
		return(invisible(NULL))
	}

	# If sorting by a criteria, filterCriteria must be "none" or match sortByCriteria
	if (filterCriteria != "none" && filterCriteria != sortByCriteria) {
		stop(sprintf(
			"Incompatible arguments: sortByCriteria = '%s' but filterCriteria = '%s'.\n%s",
			sortByCriteria, filterCriteria,
			"When sorting by a criteria, filterCriteria must be 'none' or match the sorting criteria."
		), call. = FALSE)
	}

	return(invisible(NULL))
}

#' Detect which estimation method to use for sorting
#'
#' @description
#' When both linearization and importance sampling results are available,
#' determines which method to use based on the parent/root project.
#'
#' @param df Data frame with run history
#' @param col_lin Column name for linearization values
#' @param col_is Column name for importance sampling values
#' @return Column name to use for sorting
#' @keywords internal
.detectSortingMethod <- function(df, col_lin, col_is) {
	# Find root project(s) - those without a parent
	root_rows <- which(is.na(df$Parent) | df$Parent == "")


	if (length(root_rows) == 0) {
		# No root found, prefer importance sampling if available
		if (any(!is.na(df[[col_is]]))) return(col_is)
		return(col_lin)
	}

	# Check which method the root project used (prefer IS over lin)
	root_is <- df[[col_is]][root_rows[1]]
	root_lin <- df[[col_lin]][root_rows[1]]

	if (!is.na(root_is)) {
		return(col_is)
	} else if (!is.na(root_lin)) {
		return(col_lin)
	} else {
		# Root has neither - check what's available globally
		if (any(!is.na(df[[col_is]]))) return(col_is)
		return(col_lin)
	}
}

#' Sort run history table by model selection criteria
#'
#' @description
#' Sorts the run history table by LL (log-likelihood/OFV) or BICc.
#' Automatically detects whether linearization or importance sampling
#' results are available and chooses appropriately.
#'
#' @param df Data frame with run history (must contain LL/BICc columns)
#' @param sortByCriteria Sorting criteria: "none", "LL", or "BICc"
#' @return Sorted data frame (ascending order - best model first)
#' @keywords internal
.sortRunHistory <- function(df, sortByCriteria = c("none", "LL", "BICc")) {
	sortByCriteria <- match.arg(sortByCriteria)

	if (sortByCriteria == "none") {
		return(df)
	}

	# Map criteria to column names
	if (sortByCriteria == "LL") {
		col_lin <- "LLlin"
		col_is  <- "LLis"
	} else {
		col_lin <- "BICclin"
		col_is  <- "BICcis"
	}

	# Check column availability
	has_lin <- col_lin %in% names(df) && any(!is.na(df[[col_lin]]))
	has_is  <- col_is %in% names(df) && any(!is.na(df[[col_is]]))

	if (!has_lin && !has_is) {
		warning(sprintf("No %s results available for sorting. Returning unsorted.", sortByCriteria),
						call. = FALSE)
		return(df)
	}

	# Determine which column to sort by
	if (has_lin && has_is) {
		# Both methods present - detect based on parent/root project
		sort_col <- .detectSortingMethod(df, col_lin, col_is)
	} else if (has_is) {
		sort_col <- col_is
	} else {
		sort_col <- col_lin
	}

	# Sort ascending (lowest = best for both OFV and BICc)
	df <- df[order(df[[sort_col]], na.last = TRUE), ]
	rownames(df) <- NULL

	return(df)
}


################################################################################
# Helpers for tableCovariateModelBuilding()
################################################################################

#' Compact string describing the covariate model of a single built model
#'
#' Turns the logical `individualModels` matrix returned by
#' [lixoftConnectors::getModelBuildingResults()] into a human readable string
#' such as `"Cl~lw70, V~tSex"`.
#'
#' @param im data.frame of logical values (rows = parameters, cols = covariates).
#' @return A single character string; `"(no covariates)"` when no covariate is used.
#' @keywords internal
.covStructureString <- function(im) {
	parts <- character(0)
	for (p in rownames(im)) {
		covs <- colnames(im)[which(as.logical(unlist(im[p, ])))]
		if (length(covs)) {
			parts <- c(parts, paste0(p, "~", paste(covs, collapse = "+")))
		}
	}
	if (!length(parts)) return("(no covariates)")
	paste(parts, collapse = ", ")
}

#' Canonical key for a covariate-model matrix
#'
#' Flattens the logical `individualModels` matrix into a fixed-order string so
#' two models can be compared for structural equality (used to find the nested
#' parent of a tested model).
#'
#' @param im data.frame of logical values (rows = parameters, cols = covariates).
#' @return A character key.
#' @keywords internal
.imKey <- function(im) {
	im <- im[order(rownames(im)), order(colnames(im)), drop = FALSE]
	paste(as.integer(as.matrix(im)), collapse = "")
}

#' Degrees of freedom added by a covariate relationship
#'
#' Continuous covariates add one coefficient (df = 1); categorical covariates add
#' one coefficient per non-reference category (df = number of categories - 1).
#'
#' @param covName Covariate name (as used in the individual model).
#' @param covInfo Result of [lixoftConnectors::getCovariateInformation()].
#' @return Integer degrees of freedom, or `1L` when the covariate type cannot be
#'   determined.
#' @keywords internal
.covDf <- function(covName, covInfo) {
	if (is.null(covInfo) || is.null(covInfo$covariate)) return(1L)
	info <- covInfo$covariate
	idx  <- match(covName, info$name)
	if (is.na(idx)) return(1L)
	type <- info$type[idx]
	if (!is.null(type) && grepl("categorical", type, ignore.case = TRUE)) {
		# number of categories - 1
		modalities <- if (!is.null(covInfo$modalityNumber)) covInfo$modalityNumber[idx] else NA
		if (is.na(modalities)) {
			# fall back to counting distinct observed values
			vals <- info[[covName]]
			modalities <- if (!is.null(vals)) length(unique(stats::na.omit(vals))) else 2L
		}
		return(max(1L, as.integer(modalities) - 1L))
	}
	1L
}

#' Likelihood-ratio-test p-value between two nested models
#'
#' Computes the LRT p-value for the single relationship that differs between a
#' *reduced* model (fewer parameters, the null hypothesis) and a *full* model
#' (more parameters). The LRT statistic is the difference in -2*log-likelihood:
#' \deqn{\Delta(-2LL) = (-2LL_{reduced}) - (-2LL_{full})}
#' compared to a chi-squared distribution with `dof` degrees of freedom (the
#' number of coefficients the relationship contributes).
#'
#' The caller identifies which model is reduced/full from the step direction
#' (forward: reference = reduced, tested = full; backward: tested = reduced,
#' reference = full), **not** from the likelihood values. With exact maximum
#' likelihood the full model has the smaller -2LL, but MonolixSuite estimates the
#' likelihood stochastically (SAEM) and, when linearization is used, only
#' approximately, so the fuller model can occasionally have a *larger* -2LL. In
#' that case the statistic is clamped to 0 (no evidence the extra parameter
#' improves the fit), giving a p-value of 1.
#'
#' @param llReduced Numeric -2*log-likelihood (OFV) of the reduced model.
#' @param llFull Numeric -2*log-likelihood (OFV) of the full model.
#' @param dof Integer degrees of freedom (see [.covDf()]).
#' @return The upper-tail chi-squared p-value, or `NA_real_` if any input is
#'   missing.
#' @keywords internal
.lrtPValue <- function(llReduced, llFull, dof) {
	if (is.na(llReduced) || is.na(llFull) || is.na(dof)) return(NA_real_)
	lrtStat <- max(0, llReduced - llFull)   # clamp: fuller model never fits worse under H1
	stats::pchisq(lrtStat, df = dof, lower.tail = FALSE)
}

#' Contiguous step-group ids for a stepwise model-building history
#'
#' Stepwise model building (COSSAC/SCM) proceeds in *rounds*: within a round the
#' algorithm tests candidate relationships against the same current-best model
#' (the `Reference`) in the same direction, then keeps the winner and starts the
#' next round against it. A round is a maximal run of consecutive models (in
#' algorithm order) sharing the same `(step, reference)` pair, which separates a
#' 2nd forward round from the 1st and backward phases from forward ones.
#'
#' The returned id increments at each round boundary, so callers can sort *within*
#' a round (`order(group, criterion)`) while the rounds keep their algorithm order.
#'
#' @param step Character vector of step labels (e.g. "Base"/"Forward"/"Backward").
#' @param reference Character vector of reference-model labels, same length.
#' @return Integer vector of 1-based contiguous group ids (`integer(0)` if empty).
#' @keywords internal
.modelBuildingStepGroups <- function(step, reference) {
	n <- length(step)
	if (n == 0) return(integer(0))
	step <- as.character(step)
	reference <- as.character(reference)
	# new group at row 1 and wherever step or reference changes vs the previous row
	changed <- c(TRUE, step[-1] != step[-n] | reference[-1] != reference[-n])
	cumsum(changed)
}

#' Accepted best model per step for a stepwise model-building table
#'
#' Given the assembled COSSAC/SCM table, returns the `Run` labels of the single
#' *accepted* winner of each step group (round) - i.e. the actual model-building
#' path. Within a round the winner is the model the algorithm would pick:
#' forward = most significant (smallest LRT p-value), backward = least
#' significant (largest p-value, the covariate safe to eliminate); under the
#' BICc criterion the lowest-BICc model in both directions. The winner is kept
#' only if that step was actually accepted - forward accepts a *significant*
#' addition (`Significant == "Yes"`), backward accepts a *non-significant*
#' removal (`Significant == "No"`), and under the BICc criterion a step is
#' accepted when BICc improved (`Improved == "Yes"`). Rounds that ended in no
#' change (dead-ends) contribute no winner. The Base (start) model is never a
#' step winner and is excluded.
#'
#' Rounds are the contiguous `(Step, Reference)` groups from
#' [.modelBuildingStepGroups()], so results are independent of row order.
#'
#' @param df Assembled model-building data.frame with a `Run` column and, for
#'   stepwise strategies, `Step` / `Reference` plus either `p.value` +
#'   `Significant` (LRT) or `BICc` + `Improved` (BICc criterion).
#' @return Character vector of `Run` labels of the accepted per-step winners;
#'   `character(0)` when there is no `Step` column (e.g. SAMBA) or no winner.
#' @keywords internal
.modelBuildingBestPerStep <- function(df) {
	if (!("Step" %in% names(df)) || nrow(df) == 0) return(character(0))
	grp <- .modelBuildingStepGroups(df$Step, df$Reference)
	winners <- lapply(split(seq_len(nrow(df)), grp), function(idx) {
		# A round shares one (Step, Reference) pair, so Step is constant here.
		step <- df$Step[idx[1]]
		if (step == "Base") return(character(0))            # Base is not a step winner

		# Winner: the model the algorithm would choose in this round.
		if ("p.value" %in% names(df)) {
			metric <- df$p.value[idx]
			if (all(is.na(metric))) return(character(0))
			# forward -> most significant (min p); backward -> least significant (max p)
			pick <- if (step == "Forward") which.min(metric) else which.max(metric)
		} else {
			metric <- df$BICc[idx]
			if (all(is.na(metric))) return(character(0))
			pick <- which.min(metric)                         # BICc criterion: lowest BICc
		}
		row <- idx[pick]

		# Keep the winner only if that step was actually accepted; otherwise the
		# whole round was a dead-end (nothing added/removed) and contributes none.
		accepted <- if ("Significant" %in% names(df)) {
			if (step == "Forward") isTRUE(df$Significant[row] == "Yes")
			else                   isTRUE(df$Significant[row] == "No")
		} else if ("Improved" %in% names(df)) {
			isTRUE(df$Improved[row] == "Yes")                 # BICc criterion, both directions
		} else {
			FALSE
		}
		if (accepted) df$Run[row] else character(0)
	})
	unlist(winners, use.names = FALSE)
}


################################################################################
#' .applyCustomAliasesUnits
#'
#' Software-independent helper to apply custom aliases and/or units to a parameter
#' column in a dataframe. This function does NOT fetch aliases/units from any
#' project - it only applies the custom mappings provided.
#'
#' @param df A dataframe containing a parameter column.
#' @param param_col Character string specifying the name of the parameter column.
#'   Default is "parameter".
#' @param aliases_df Optional dataframe with columns "parameter" and "aliases".
#'   If provided, parameter values are replaced with their aliases.
#' @param units_df Optional dataframe with columns "parameter" and "units".
#'   If provided, units are appended to parameter names in parentheses.
#' @param orig_param_col Optional character string specifying a column containing
#'   original parameter names for unit lookup (useful when aliases have already
#'   been applied). If NULL, uses param_col for lookup.
#'
#' @return A dataframe with the parameter column updated with aliases and/or units.
#'
#' @details
#' The function applies transformations in order:
#' 1. Aliases are applied first (parameter values are replaced)
#' 2. Units are appended second (in parentheses after the parameter name)
#'
#' If both aliases and units are provided, the final format is: "alias (unit)"
#'
#' @noRd
.applyCustomAliasesUnits <- function(df,
																		 param_col      = "parameter",
																		 aliases_df     = NULL,
																		 units_df       = NULL,
																		 orig_param_col = NULL) {

	# Nothing to do if both are NULL
	if (is.null(aliases_df) && is.null(units_df)) {
		return(df)
	}

	# Determine lookup column for units (before aliases change param_col values)
	if (is.null(orig_param_col)) {
		# Store original parameter values for unit lookup
		orig_params <- df[[param_col]]
	} else {
		orig_params <- df[[orig_param_col]]
	}

	# Apply aliases (replace parameter values)
	if (!is.null(aliases_df)) {
		alias_map <- setNames(aliases_df$aliases, aliases_df$parameter)
		# Apply mapping, keeping original value if no alias defined
		df[[param_col]] <- sapply(df[[param_col]], function(p) {
			if (p %in% names(alias_map) && !is.na(alias_map[p])) {
				alias_map[p]
			} else {
				p
			}
		}, USE.NAMES = FALSE)
	}

	# Apply units (append to parameter names)
	if (!is.null(units_df)) {
		units_map <- setNames(units_df$units, units_df$parameter)
		# Append units using original parameter names for lookup
		df[[param_col]] <- sapply(seq_len(nrow(df)), function(i) {
			orig_p <- orig_params[i]
			current_p <- df[[param_col]][i]
			unit <- units_map[orig_p]

			if (!is.na(unit) && nchar(trimws(unit)) > 0) {
				paste0(current_p, " (", unit, ")")
			} else {
				current_p
			}
		}, USE.NAMES = FALSE)
	}

	# Remove orig_param_col if it was provided and exists
	if (!is.null(orig_param_col) && orig_param_col %in% names(df) && orig_param_col != param_col) {
		df[[orig_param_col]] <- NULL
	}

	return(df)
}

#' Get IOV variability parameter names from the currently loaded Monolix project
#'
#' Returns the names of inter-occasion variability parameters for parameters
#' with variability enabled at any non-id level in the individual parameter
#' model. Supports multiple nested IOV levels: the first non-id level maps to
#' "gamma_<param>", the second to "gamma2_<param>", and so on (matching
#' Monolix's naming convention for nested occasion variability).
#'
#' Returns an empty character vector for projects without IOV.
#'
#' Querying the model directly avoids misclassifying user-defined parameters
#' whose names happen to start with "gamma_" (e.g. a gamma_pop shape parameter)
#' as IOV variability parameters.
#'
#' @noRd
.getIOVParameters <- function() {
	ipm <- try(lixoftConnectors::getIndividualParameterModel(), silent = TRUE)
	if (inherits(ipm, "try-error") || is.null(ipm$variability)) {
		return(character(0))
	}

	iov_levels <- setdiff(names(ipm$variability), "id")

	iov_params <- character(0)
	for (j in seq_along(iov_levels)) {
		v <- ipm$variability[[iov_levels[[j]]]]
		bases <- names(v)[is.logical(v) & v]
		if (length(bases) == 0) next
		prefix <- if (j == 1) "gamma_" else paste0("gamma", j, "_")
		iov_params <- c(iov_params, paste0(prefix, bases))
	}

	iov_params
}

#' Get residual error parameter names from the currently loaded Monolix project
#'
#' Returns the names of all residual error parameters across continuous outputs,
#' including autocorrelation parameters (r, r1, r2, ...) for outputs with
#' autocorrelation enabled. Returns an empty character vector for projects with
#' no continuous observations (e.g. pure RTTE models).
#'
#' @noRd
.getResidualErrorParameters <- function() {
	obs_model <- try(lixoftConnectors::getContinuousObservationModel(), silent = TRUE)
	if (inherits(obs_model, "try-error") || is.null(obs_model$parameters)) {
		return(character(0))
	}

	res_err_params <- unlist(obs_model$parameters, use.names = FALSE)

	# Add the autocorrelation parameter r<suffix> for each output where
	# autocorrelation is enabled. The suffix matches that of the output's existing
	# residual error parameters (e.g. "a1" -> "r1", "a" -> "r").
	if (!is.null(obs_model$autocorrelation)) {
		autocorr_outputs <- names(obs_model$autocorrelation)[
			vapply(obs_model$autocorrelation, isTRUE, logical(1))
		]
		for (output_name in autocorr_outputs) {
			output_params <- obs_model$parameters[[output_name]]
			if (length(output_params) > 0) {
				suffix <- sub("^[a-zA-Z]+", "", output_params[[1]])
				res_err_params <- c(res_err_params, paste0("r", suffix))
			}
		}
	}

	res_err_params
}

#' Check (non-fatally) whether standard error estimates are available
#'
#' Like the `"rse"` branch of `.validateProjectResultsAvailability()` but returns
#' a status list instead of throwing, so callers can drop RSE/CI columns and warn
#' when the standard error estimation task has not been run.
#'
#' @param method Optional SE method name; `""` (default) accepts any method.
#' @return List with `available` (logical) and `reason` (character or `NULL`).
#' @noRd
.standardErrorsAvailable <- function(method = "") {
	all_rse <- tryCatch(lixoftConnectors::getEstimatedStandardErrors(), error = function(e) NULL)

	# No SE results at all. The connector cannot tell a task that was not run
	# from one that ran without producing results (getLaunchedTasks() reports
	# both as not launched), so the message covers both.
	if (is.null(all_rse) || length(all_rse) == 0) {
		return(list(
			available = FALSE,
			reason    = "No standard error estimates available (the standard error estimation task was not run or did not produce results)."))
	}

	# Requested method not among the available ones
	if (method != "" && !(method %in% names(all_rse))) {
		return(list(
			available = FALSE,
			reason    = paste0(
				"Standard errors for method '", method, "' not available. ",
				"Available methods: ", paste(names(all_rse), collapse = ", "), ".")))
	}

	list(available = TRUE, reason = NULL)
}

#' Read the raw (unformatted) data file of a project
#'
#' Reads the data file of a project as stored on disk: CSV/TXT, Excel
#' (XLSX/XLS, via \pkg{readxl}) or SAS (`.sas7bdat` / `.xpt`, via \pkg{haven}).
#' With Data Formatting a dataset can have several header lines (e.g. a units
#' row under the column names); `headerLines` gives their 1-based line numbers,
#' as returned by `lixoftConnectors::getFormatting()$headerLines`. Column names
#' are taken from the first header line and the data are read from the line
#' after the last header line, so the extra header rows are not read as data
#' (which would turn every column into character). With a single header line
#' the file is read exactly as before.
#'
#' @param dataFile Path to the data file.
#' @param headerLines Integer vector of header line numbers. Default `1`.
#'   Ignored for SAS files.
#' @param sheet Excel sheet used by the project (`getData()$sheet` or
#'   `getFormatting()$sheet`). `NULL` or `""` reads the first sheet. Ignored for
#'   other formats.
#' @return A data.frame with one row per data line of the file.
#' @noRd
.readRawDataFile <- function(dataFile, headerLines = 1, sheet = NULL) {
	headerLines <- suppressWarnings(as.integer(headerLines))
	headerLines <- sort(unique(headerLines[!is.na(headerLines) & headerLines >= 1L]))
	if (length(headerLines) == 0) headerLines <- 1L
	nameLine   <- headerLines[1]
	lastHeader <- max(headerLines)
	if (length(sheet) != 1 || is.na(sheet) || !nzchar(sheet)) sheet <- NULL

	ext <- tolower(tools::file_ext(dataFile))

	if (ext %in% c("sas7bdat", "xpt")) {
		if (!requireNamespace("haven", quietly = TRUE)) {
			stop("Reading SAS data files (.sas7bdat/.xpt) requires the 'haven' package. Install it with install.packages('haven').")
		}
		df <- if (ext == "xpt") haven::read_xpt(dataFile) else haven::read_sas(dataFile)
		# drop SAS labels/formats so columns are plain numeric/character vectors
		df <- haven::zap_formats(haven::zap_labels(df))
		return(as.data.frame(df, stringsAsFactors = FALSE))
	}

	if (ext %in% c("csv", "txt")) {
		sep <- .getDelim(dataFile)
		if (lastHeader == 1L) {
			return(utils::read.csv(dataFile, header = TRUE, sep = sep))
		}
		colNames <- names(utils::read.csv(dataFile, header = TRUE, sep = sep,
																			skip = nameLine - 1L, nrows = 1))
		return(utils::read.csv(dataFile, header = FALSE, sep = sep,
													 skip = lastHeader, col.names = colNames))
	}

	if (ext %in% c("xlsx", "xls")) {
		if (!requireNamespace("readxl", quietly = TRUE)) {
			stop("Reading Excel data files requires the 'readxl' package. Install it with install.packages('readxl').")
		}
		# Read the sheet the project uses (the first one only when none is recorded).
		# as.data.frame() so downstream row/column indexing matches the read.csv path
		if (lastHeader == 1L) {
			return(as.data.frame(readxl::read_excel(dataFile, sheet = sheet), stringsAsFactors = FALSE))
		}
		colNames <- names(readxl::read_excel(dataFile, sheet = sheet, skip = nameLine - 1L, n_max = 0))
		return(as.data.frame(readxl::read_excel(dataFile, sheet = sheet, skip = lastHeader, col_names = colNames),
												 stringsAsFactors = FALSE))
	}

	stop("Only CSV, TXT, Excel (XLSX/XLS) or SAS (SAS7BDAT/XPT) format is currently supported.")
}
