#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @noRd
.find_pandoc <- function() {
    p <- Sys.which("pandoc")
    if (nzchar(p)) return(p)
    ## In conda/mamba envs, pandoc sits alongside R in the env bin dir
    conda_bin <- file.path(dirname(dirname(R.home())), "bin", "pandoc")
    if (file.exists(conda_bin)) return(conda_bin)
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
        rmarkdown::find_pandoc()
        p <- rmarkdown::pandoc_exec()
        if (nzchar(p) && file.exists(p)) return(p)
    }
    stop("pandoc not found. Install pandoc or ensure it is on the PATH.")
}

#' @noRd
.check_package <- function(pkg, reason = "") {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        msg <- sprintf("Package '%s' is required", pkg)
        if (nzchar(reason)) msg <- paste0(msg, " ", reason)
        msg <- paste0(msg, ". Install it with BiocManager::install('", pkg, "').")
        stop(msg, call. = FALSE)
    }
}
