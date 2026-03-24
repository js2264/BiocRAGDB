
#' Fetch Bioconductor Package Sources
#'
#' Clones Bioconductor packages from \url{https://git.bioconductor.org}
#' and collects their R source files, vignettes, `DESCRIPTION`, and
#' `NAMESPACE` into a single output directory for downstream processing.
#'
#' @param base_dir Character string. Path to the directory where packages
#'   will be cloned and files will be collected. Created if it does not
#'   exist.
#' @param pkgs Character vector of package names to fetch. When `NULL`
#'   (the default), the full list of Bioconductor packages is obtained via
#'   \code{biocPkgList()}.
#' @param branch Character string. Git branch to clone.
#'   Default: `"devel"`.
#' @param BPPARAM A \code{\link[BiocParallel]{BiocParallelParam}} object
#'   controlling parallelisation. Default: \code{BiocParallel::bpparam()}.
#' @param remove_clones Logical. If `TRUE`, the cloned repositories are
#'   deleted after collecting files to save disk space.
#'   Default: `FALSE`.
#'
#' @return A character vector of paths to the collected files (invisibly).
#'
#' @examples
#' \dontrun{
#' files <- fetch_pkgs_resources(
#'     base_dir = tempdir(),
#'     pkgs = c("GenomicRanges", "IRanges"),
#'     branch = "devel"
#' )
#' head(files)
#' }
#'
#' @export
fetch_pkgs_resources <- function(base_dir, pkgs = NULL, branch = "devel", BPPARAM = BiocParallel::bpparam(), remove_clones = FALSE) {


    # Ensure the base directory exists
    if (dir.exists(base_dir)) {
        message(sprintf("Base directory %s already exists. Existing files may be overwritten.", base_dir))
        ask <- readline(prompt = "Do you want to continue? (y/n): ")
        if (tolower(ask) != "y") {
            stop("Operation cancelled by user.")
        }
    }
    dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)
    
    # If no specific packages are provided, fetch the list of all Bioconductor packages
    if (is.null(pkgs)) {
        pkg_tbl <- biocPkgList()
        pkgs <- pkg_tbl$Package
    }
    
    # Clone each package in parallel using BiocParallel
    pkg_dirs <- BiocParallel::bplapply(
        pkgs, 
        .clone_package, 
        base_dir = base_dir, 
        branch = branch,
        BPPARAM = BPPARAM
    ) |> unlist() |> na.omit()

    # After cloning, collect relevant files from each package
    new_files <- BiocParallel::bplapply(
        pkg_dirs, 
        .collect_package_files, 
        out_dir = file.path(base_dir, "all_files"), 
        BPPARAM = BPPARAM
    ) |> unlist() |> na.omit()

    # Then remove the cloned package directories to save space
    if (remove_clones) {
        unlink(pkg_dirs, recursive = TRUE)
    }

    # List all files 
    return(new_files)

}

#' @noRd
.collect_package_files <- function(pkg_dir, out_dir) {
    pkg_name <- basename(pkg_dir)
    files <- character(0)

    # R/ source files
    r_dir <- file.path(pkg_dir, "R")
    if (dir.exists(r_dir)) {
        r_files <- list.files(
            r_dir, pattern = "\\.[Rr]$",
            full.names = TRUE, recursive = FALSE
        )
        files <- c(files, r_files)
    }

    # Vignettes
    vig_dir <- file.path(pkg_dir, "vignettes")
    if (dir.exists(vig_dir)) {
        vig_pattern <- paste0(
            "\\.(",
            paste(c("Rmd", "rmd", "qmd", "Rnw", "rnw", "md"), collapse = "|"),
            ")$"
        )
        vig_files <- list.files(
            vig_dir, pattern = vig_pattern,
            full.names = TRUE, recursive = TRUE
        )
        files <- c(files, vig_files)
    }

    # DESCRIPTION
    desc_file <- file.path(pkg_dir, "DESCRIPTION")
    if (file.exists(desc_file)) files <- c(files, desc_file)

    # NAMESPACE
    ns_file <- file.path(pkg_dir, "NAMESPACE")
    if (file.exists(ns_file)) files <- c(files, ns_file)

    # Now copy these files to the output directory, preserving relative paths
    if (length(files) == 0L) {
        warning(sprintf("No relevant files found for %s", pkg_name))
    } else {
        new_files <- lapply(files, function(f) {
            rel_path <- sub(
                paste0(".*?/", pkg_name, "/"),
                paste0(pkg_name, "/"),
                f
            )
            dest_path <- file.path(out_dir, rel_path)
            dir.create(dirname(dest_path), showWarnings = FALSE, recursive = TRUE)
            file.copy(f, dest_path, overwrite = TRUE)
            return(dest_path)
        }) |> unlist()
    }

    return(new_files)
}



#' Clone a Bioconductor Package Repository
#'
#' Performs a shallow clone of a Bioconductor package from
#' \url{https://git.bioconductor.org} into a local directory.
#'
#' If the destination directory already exists, the clone is skipped and the
#' existing path is returned immediately. Otherwise, `git clone` is called
#' with `--depth 1 --single-branch` to minimise download size.
#'
#' @param pkg Character string. Name of the Bioconductor package to clone.
#' @param base_dir Character string. Path to the parent directory where the
#'   package repository will be cloned. The repository is placed in
#'   `file.path(base_dir, pkg)`.
#' @param branch Character string. Git branch to clone. Default: `"devel"`.
#'
#' @return The path to the cloned repository (character string), or
#'   `NA_character_` if the clone failed.
#'
#' @keywords internal
.clone_package <- function(pkg, base_dir, branch = "devel") {
    repo_url <- sprintf("https://git.bioconductor.org/packages/%s", pkg)
    dest <- file.path(base_dir, pkg)
    if (dir.exists(dest)) return(dest)
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
            warning(sprintf("Failed to clone %s: %s", pkg, e$message))
            NA_character_
        }
    )
    return(result)
}

