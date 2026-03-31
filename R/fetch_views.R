#' Fetch and Parse biocViews from the Bioconductor VIEWS File
#'
#' Downloads the `VIEWS` file for a given Bioconductor release and
#' parses it into a structured data frame containing package metadata,
#' dependency relationships, and biocViews ontology terms.
#'
#' The `VIEWS` file is a DCF-formatted file available at
#' `https://bioconductor.org/packages/<version>/bioc/VIEWS`.
#'
#' @param version Character string. Bioconductor version (e.g., `"3.21"`).
#'   Default: current devel version obtained from BiocManager.
#' @param type Character string. Repository type: `"bioc"`, `"data/annotation"`,
#'   or `"data/experiment"`. Default: `"bioc"`.
#'
#' @return A data frame with one row per package and columns including
#'   `Package`, `Version`, `Title`, `Description`, `Depends`, `Imports`,
#'   `Suggests`, `biocViews`, `Author`, `Maintainer`, and others present
#'   in the VIEWS file.
#'
#' @examples
#' \dontrun{
#' views <- fetch_bioc_views("3.21")
#' head(views[, c("Package", "biocViews")])
#' }
#'
#' @export
fetch_bioc_views <- function(
    version = NULL,
    type = c("bioc", "data/annotation", "data/experiment")
) {
    type <- match.arg(type)

    if (is.null(version)) {
        version <- tryCatch(
            as.character(BiocManager::version()),
            error = function(e) "3.21"
        )
    }

    url <- sprintf(
        "https://bioconductor.org/packages/%s/%s/VIEWS",
        version, type
    )

    tmp <- tempfile(fileext = ".dcf")
    on.exit(unlink(tmp), add = TRUE)

    download_status <- tryCatch(
        utils::download.file(url, tmp, quiet = TRUE),
        error = function(e) {
            stop(sprintf("Failed to download VIEWS from %s: %s", url, e$message),
                 call. = FALSE)
        }
    )

    ## Parse the DCF file
    views <- read.dcf(tmp)
    views <- as.data.frame(views, stringsAsFactors = FALSE)

    message(sprintf(
        "Parsed %d packages from Bioconductor %s (%s)",
        nrow(views), version, type
    ))

    views
}


#' Ingest biocViews Ontology and Package Relationships into BiocKB
#'
#' Fetches the `VIEWS` file and creates structured knowledge base chunks
#' for each package's biocViews classification and dependency graph. This
#' captures the ontology relationships (which biocViews each package belongs
#' to) and inter-package dependency links.
#'
#' @param base_dir Character string. Directory where the generated Markdown
#'   files will be saved.
#' @param version Character string. Bioconductor version. Passed to
#'   [fetch_bioc_views()].
#' @param type Character string. Repository type. Passed to
#'   [fetch_bioc_views()].
#' @param force Logical. If `TRUE`, skip interactive confirmation.
#'   Default: `FALSE`.
#' @param BPPARAM A [BiocParallel::BiocParallelParam-class] instance controlling
#'   parallelisation. Defaults to [BiocParallel::bpparam()] (the registered
#'   default). Use [BiocParallel::SerialParam()] to disable parallelism.
#'
#' @return A character vector of paths to the created Markdown files
#'   (invisibly).
#'
#' @details
#' Two types of documents are generated:
#' \describe{
#'   \item{Package metadata}{One file per package containing its
#'     biocViews terms, dependencies, title, and description.}
#'   \item{biocViews index}{One file per biocViews term listing all
#'     packages belonging to that term.}
#' }
#'
#' @examples
#' \dontrun{
#' files <- ingest_bioc_views(tempdir(), version = "3.21")
#' head(files)
#' }
#'
#' @export
ingest_bioc_views <- function(
    base_dir,
    version = NULL,
    type = c("bioc", "data/annotation", "data/experiment"),
    force = FALSE,
    BPPARAM = BiocParallel::bpparam()
) {
    type <- match.arg(type)

    if (dir.exists(base_dir) && !force) {
        if (interactive()) {
            message(sprintf("Base directory %s already exists.", base_dir))
            ask <- readline(prompt = "Do you want to continue? (y/n): ")
            if (tolower(ask) != "y") stop("Operation cancelled by user.")
        } else {
            stop("Base directory already exists. Use force = TRUE to overwrite.", call. = FALSE)
        }
    }

    views <- fetch_bioc_views(version = version, type = type)
    pkg_dir <- file.path(base_dir, "all_files", "biocviews", "packages")
    idx_dir <- file.path(base_dir, "all_files", "biocviews", "terms")
    dir.create(pkg_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(idx_dir, showWarnings = FALSE, recursive = TRUE)

    ## ── Per-package metadata files (parallel) ──
    pkg_files <- BiocParallel::bplapply(seq_len(nrow(views)), function(i) {
        row <- views[i, ]
        pkg <- row$Package
        md <- paste0(
            "# ", pkg, "\n\n",
            "- **Title**: ", trimws(row$Title %||% ""), "\n",
            "- **Version**: ", trimws(row$Version %||% ""), "\n",
            "- **biocViews**: ", trimws(row$biocViews %||% ""), "\n",
            "- **Depends**: ", trimws(row$Depends %||% ""), "\n",
            "- **Imports**: ", trimws(row$Imports %||% ""), "\n",
            "- **Suggests**: ", trimws(row$Suggests %||% ""), "\n",
            "\n## Description\n\n",
            gsub("\\s+", " ", trimws(row$Description %||% "")), "\n"
        )
        fpath <- file.path(pkg_dir, paste0(pkg, ".md"))
        writeLines(md, fpath)
        fpath
    }, BPPARAM = BPPARAM)
    pkg_files <- unlist(pkg_files)

    ## ── Per-biocViews term index files ──
    ## Build an inverted index: term -> list of packages
    term_map <- list()
    for (i in seq_len(nrow(views))) {
        pkg <- views$Package[i]
        bv <- views$biocViews[i]
        if (is.na(bv) || !nzchar(trimws(bv))) next
        terms <- trimws(strsplit(bv, ",\\s*")[[1]])
        for (term in terms) {
            term_map[[term]] <- c(term_map[[term]], pkg)
        }
    }

    term_files <- BiocParallel::bplapply(names(term_map), function(term) {
        pkgs <- sort(unique(term_map[[term]]))
        md <- paste0(
            "# biocViews: ", term, "\n\n",
            "Packages classified under the **", term, "** biocViews term ",
            "(", length(pkgs), " packages):\n\n",
            paste0("- ", pkgs, collapse = "\n"), "\n"
        )
        safe_name <- gsub("[^a-zA-Z0-9_-]", "_", term)
        fpath <- file.path(idx_dir, paste0(safe_name, ".md"))
        writeLines(md, fpath)
        fpath
    }, BPPARAM = BPPARAM)
    term_files <- unlist(term_files)

    message(sprintf(
        "Generated %d package files and %d biocViews term files",
        length(pkg_files), length(term_files)
    ))
    invisible(c(pkg_files, term_files))
}
