#' Fetch Bioconductor Website Content
#'
#' Clones the \url{https://github.com/Bioconductor/pkgrevdocs/}
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
#' files <- fetch_bioc_contributions(tempdir())
#' head(files)
#' }
#'
#' @export
fetch_bioc_contributions <- function(
    base_dir,
    branch = "devel",
    remove_clone = FALSE,
    force = FALSE, 
    BPPARAM = BiocParallel::bpparam()
) {
    .fetch_github(
        base_dir = base_dir,
        repo = "Bioconductor/pkgrevdocs",
        branch = branch,
        files_from = ".",
        extensions = c("Rmd"),
        remove_clone = remove_clone,
        force = force,
        BPPARAM = BPPARAM
    )
}
