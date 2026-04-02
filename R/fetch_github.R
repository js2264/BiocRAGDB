#' Fetch content from Github repo
#'
#' @param base_dir Character string. Directory where the repository will
#'   be cloned and files collected.
#' @param repo Character string. GitHub repository to clone in the format "owner/repo".
#' @param branch Character string. Git branch to clone.
#'   Default: `"devel"`.
#' @param files_from Character string. Directory from the repository to collect files from. Default: `"."`.
#' @param extensions Character vector. File extensions to collect (e.g. `c("md", "html")`).
#'   Default: `c("md", "html", "markdown", "Rmd")`.
#' @param remove_clone Logical. If `TRUE`, the cloned repository is deleted
#'   after collecting files. Default: `FALSE`.
#' @param force Logical. If `TRUE`, skip the interactive confirmation when
#'   `base_dir` already exists. Default: `FALSE`.
#'
#' @return A character vector of paths to the collected files (invisibly).
.fetch_github <- function(
    base_dir,
    repo,
    branch = "devel",
    files_from = ".",
    extensions = c("md", "html", "markdown", "Rmd"),
    remove_clone = TRUE,
    force = FALSE, 
    BPPARAM = BiocParallel::bpparam()
) {
    repo_url <- paste0("https://github.com/", repo, "/")
    dest_dir <- path.expand(base_dir) |> 
        file.path(tools::file_path_sans_ext(basename(repo_url)))

    if (dir.exists(base_dir) && !force) {
        if (interactive()) {
            message(sprintf("Base directory %s already exists.", base_dir))
            ask <- readline(prompt = "Do you want to continue? (y/n): ")
            if (tolower(ask) != "y") stop("Operation cancelled by user.")
        } else {
            stop("Base directory already exists. Use force = TRUE to overwrite.", call. = FALSE)
        }
    }

    ## Clone the website repo (shallow)
    result <- tryCatch(
        {
            system2(
                "git", c(
                    "clone", "--depth", "1",
                    "--branch", branch,
                    "--single-branch",
                    repo_url, dest_dir
                ),
                stdout = TRUE, stderr = TRUE
            )
            dest_dir
        },
        error = function(e) {
            warning(sprintf("Failed to clone %s: %s", repo, e$message))
            return(NULL)
        }
    )
    if (is.null(result)) return(invisible(character(0)))

    ## Collect files with specified extensions from content/
    content_dir <- file.path(dest_dir, files_from)
    files <- list.files(
        content_dir,
        pattern = paste0("\\.(", paste(extensions, collapse = "|"), ")$"),
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
    )

    ## Copy to a flat output directory preserving relative paths
    out_dir <- file.path(base_dir, "all_files", tools::file_path_sans_ext(basename(repo_url)))
    collected <- BiocParallel::bplapply(files, function(f) {
        rel <- sub(paste0(content_dir, "/"), "", f, fixed = TRUE)
        dest_path <- file.path(out_dir, rel)
        dir.create(dirname(dest_path), showWarnings = FALSE, recursive = TRUE)
        file.copy(f, dest_path, overwrite = TRUE)
        dest_path
    }, BPPARAM = BPPARAM)

    if (remove_clone) {
        unlink(dest_dir, recursive = TRUE)
    }

    cli::cli_alert_success(sprintf("Collected %d website files from %s", length(collected), repo))
    invisible(unname(collected))
}
