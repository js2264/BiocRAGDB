#' Get the Local Path to the BiocRAGDB Database
#'
#' Returns the local filesystem path to the cached BiocRAGDB DuckDB
#' database, managed by \pkg{BiocFileCache}.
#'
#' The path is resolved from (in order of priority):
#'
#' 1. The `BiocRAGDB.path` option (user override)
#' 2. The `BIOCAGENT_RAGDB_PATH` environment variable (user override)
#' 3. The BiocFileCache entry for the database
#'
#' @param check Logical. If `TRUE` (default), only returns the path if the
#'   file actually exists. If `FALSE`, returns the expected path regardless.
#'
#' @return A character string with the path to the DuckDB file, or `NULL`
#'   if `check = TRUE` and the file does not exist.
#'
#' @examples
#' # Where would the database be stored?
#' ragdb_path(check = FALSE)
#'
#' # Does it exist?
#' ragdb_path(check = TRUE)
#'
#' @export
ragdb_path <- function(check = TRUE) {
    # 1. Check option (user override)
    path <- getOption("BiocRAGDB.path")
    if (!is.null(path)) {
        if (!check || file.exists(path)) return(path)
        return(NULL)
    }

    # 2. Check environment variable (user override)
    env_path <- Sys.getenv("BIOCAGENT_RAGDB_PATH", unset = NA)
    if (!is.na(env_path)) {
        if (!check || file.exists(env_path)) return(env_path)
        return(NULL)
    }

    # 3. Check BiocFileCache
    bfc <- .ragdb_cache()
    entry <- BiocFileCache::bfcquery(bfc, "BiocRAGDB", field = "rname")
    if (nrow(entry) > 0L) {
        cached_path <- entry$rpath[1L]
        if (!check || file.exists(cached_path)) return(cached_path)
    }

    NULL
}


#' Fetch the BiocRAGDB from ExperimentHub
#'
#' Downloads (or retrieves from cache) the BiocRAGDB DuckDB database
#' from ExperimentHub and stores it in a local \pkg{BiocFileCache}.
#'
#' @param force Logical. If `TRUE`, re-downloads even if a local copy exists.
#'   Default: `FALSE`.
#'
#' @return The file path to the local DuckDB database (invisibly).
#'
#' @details
#' The database is hosted on Bioconductor ExperimentHub (resource ID: EHXXX).
#' On first call, it is downloaded from ExperimentHub and added to a local
#' \pkg{BiocFileCache}. On subsequent calls, the cached copy is returned
#' unless `force = TRUE`.
#'
#' The BiocFileCache used is located at
#' `tools::R_user_dir("BiocRAGDB", "cache")`.
#'
#' @examples
#' \dontrun{
#' # First-time download
#' db_path <- ragdb_fetch()
#'
#' # Force re-download
#' db_path <- ragdb_fetch(force = TRUE)
#' }
#'
#' @export
ragdb_fetch <- function(force = FALSE) {
    bfc <- .ragdb_cache()
    rname <- "BiocRAGDB"

    # Check if already in cache
    entry <- BiocFileCache::bfcquery(bfc, rname, field = "rname")

    if (nrow(entry) > 0L && !force) {
        cached_path <- entry$rpath[1L]
        if (file.exists(cached_path)) {
            message("BiocRAGDB already cached at: ", cached_path)
            return(invisible(cached_path))
        }
        # Stale entry — remove and re-fetch
        BiocFileCache::bfcremove(bfc, entry$rid)
    }

    if (nrow(entry) > 0L && force) {
        BiocFileCache::bfcremove(bfc, entry$rid)
    }

    # Fetch from ExperimentHub
    eh_path <- .fetch_from_experimenthub()

    # Add to our cache (copy into BFC-managed location)
    cached_path <- BiocFileCache::bfcadd(
        bfc, rname = rname, fpath = eh_path, action = "copy"
    )
    message("BiocRAGDB cached at: ", cached_path)

    invisible(as.character(cached_path))
}


#' Update the Local BiocRAGDB
#'
#' Re-downloads the RAG database from ExperimentHub and updates
#' the local \pkg{BiocFileCache} entry.
#'
#' @return The file path to the updated local DuckDB database (invisibly).
#'
#' @examples
#' \dontrun{
#' ragdb_update()
#' }
#'
#' @export
ragdb_update <- function() {
    ragdb_fetch(force = TRUE)
}


#' Information About the Local BiocRAGDB
#'
#' Prints summary information about the locally cached RAG database,
#' including its path, file size, and modification time.
#'
#' @return A named list with `path`, `size_mb`, and `modified` fields
#'   (invisibly). Prints a human-readable summary.
#'
#' @examples
#' \dontrun{
#' ragdb_info()
#' }
#'
#' @export
ragdb_info <- function() {
    path <- ragdb_path(check = TRUE)

    if (is.null(path)) {
        message(
            "BiocRAGDB is not cached locally.\n",
            "Run ragdb_fetch() to download it."
        )
        return(invisible(NULL))
    }

    fi <- file.info(path)
    info <- list(
        path = path,
        size_mb = round(fi$size / 1024^2, 1),
        modified = fi$mtime
    )

    message(
        "BiocRAGDB\n",
        "  Path:     ", info$path, "\n",
        "  Size:     ", info$size_mb, " MB\n",
        "  Modified: ", format(info$modified, "%Y-%m-%d %H:%M:%S")
    )

    invisible(info)
}


# ---- Internal helpers ----

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' Get or create the BiocFileCache for BiocRAGDB
#' @return A BiocFileCache object
#' @noRd
.ragdb_cache <- function() {
    cache_dir <- tools::R_user_dir("BiocRAGDB", "cache")
    BiocFileCache::BiocFileCache(cache_dir, ask = FALSE)
}

#' Fetch the database file from ExperimentHub
#'
#' ExperimentHub itself uses BiocFileCache internally; this fetches the
#' resource and returns the local file path.
#'
#' @return Path to the downloaded file.
#' @noRd
.fetch_from_experimenthub <- function() {
    eh <- ExperimentHub::ExperimentHub()

    # Placeholder ExperimentHub ID — to be replaced with actual ID after
    # submission to ExperimentHub
    eh_id <- "EHXXX"

    eh[[eh_id]]
}
