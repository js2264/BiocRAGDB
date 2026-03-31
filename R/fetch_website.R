#' Fetch Bioconductor Website Content
#'
#' Clones the \url{https://github.com/Bioconductor/bioconductor.org}
#' repository and collects Markdown and HTML content files from the
#' `content/` directory for downstream chunking and ingestion into the
#' knowledge base.
#'
#' @param base_dir Character string. Directory where the repository will
#'   be cloned and files collected.
#' @param branch Character string. Git branch to clone.
#'   Default: `"devel"`.
#' @param remove_clone Logical. If `TRUE`, the cloned repository is deleted
#'   after collecting files. Default: `FALSE`.
#' @param force Logical. If `TRUE`, skip the interactive confirmation when
#'   `base_dir` already exists. Default: `FALSE`.
#'
#' @return A character vector of paths to the collected files (invisibly).
#'
#' @examples
#' \dontrun{
#' files <- fetch_bioc_website(tempdir())
#' head(files)
#' }
#'
#' @export
fetch_bioc_website <- function(
    base_dir,
    branch = "devel",
    remove_clone = FALSE,
    force = FALSE
) {
    repo_url <- "https://github.com/Bioconductor/bioconductor.org"
    dest <- file.path(base_dir, "bioconductor.org")

    if (dir.exists(base_dir) && !force) {
        if (interactive()) {
            message(sprintf("Base directory %s already exists.", base_dir))
            ask <- readline(prompt = "Do you want to continue? (y/n): ")
            if (tolower(ask) != "y") stop("Operation cancelled by user.")
        } else {
            stop("Base directory already exists. Use force = TRUE to overwrite.", call. = FALSE)
        }
    }
    dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)

    ## Clone the website repo (shallow)
    if (!dir.exists(dest)) {
        result <- tryCatch(
            {
                system2(
                    "git", c(
                        "clone", "--depth", "1",
                        "--branch", branch,
                        "--single-branch",
                        repo_url, dest
                    ),
                    stdout = TRUE, stderr = TRUE
                )
                dest
            },
            error = function(e) {
                warning(sprintf("Failed to clone bioconductor.org: %s", e$message))
                return(NULL)
            }
        )
        if (is.null(result)) return(invisible(character(0)))
    }

    ## Collect MD and HTML files from content/
    content_dir <- file.path(dest, "content")
    if (!dir.exists(content_dir)) {
        warning("content/ directory not found in bioconductor.org repo")
        return(invisible(character(0)))
    }

    files <- list.files(
        content_dir,
        pattern = "\\.(md|html|markdown)$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
    )

    ## Copy to a flat output directory preserving relative paths
    out_dir <- file.path(base_dir, "all_files", "bioconductor.org")
    collected <- vapply(files, function(f) {
        rel <- sub(paste0(content_dir, "/"), "", f, fixed = TRUE)
        dest_path <- file.path(out_dir, rel)
        dir.create(dirname(dest_path), showWarnings = FALSE, recursive = TRUE)
        file.copy(f, dest_path, overwrite = TRUE)
        dest_path
    }, character(1))

    if (remove_clone) {
        unlink(dest, recursive = TRUE)
    }

    message(sprintf("Collected %d website files from bioconductor.org", length(collected)))
    invisible(unname(collected))
}
