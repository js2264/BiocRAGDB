#' BiocKB S7 Class
#'
#' An S7 class representing a Bioconductor knowledge base backed by a
#' DuckDB ragnar store. The constructor can either create a new store or
#' connect to an existing one.
#'
#' @param db_path Character string. Path to the DuckDB file.
#' @param embedding_model Character string. Name of the Ollama embedding
#'   model to use when creating a new store. Default: `"nomic-embed-text"`.
#' @param overwrite Logical. If `TRUE`, any existing store at `db_path` is
#'   replaced when creating. Default: `FALSE`.
#' @param read_only Logical. If `TRUE`, connect to an existing store in
#'   read-only mode. If `FALSE` (the default), create a new store.
#'
#' @return A `BiocKB` S7 object.
#'
#' @seealso [insert_bioc_kb()], [build_bioc_index()], [retrieve_bioc_kb()], [kb_path()]
#'
#' @examples
#' \dontrun{
#' # Create a new store
#' kb <- BiocKB(tempfile(fileext = ".duckdb"))
#'
#' # Connect to an existing store
#' kb <- BiocKB("biockb.duckdb", read_only = TRUE)
#' }
#'
#' @export
BiocKB <- S7::new_class(
    "BiocKB",
    properties = list(
        db_path = S7::class_character,
        embedding_model = S7::class_character,
        read_only = S7::class_logical,
        store = S7::class_any
    ),
    constructor = function(
        db_path,
        embedding_model = "nomic-embed-text",
        overwrite = FALSE,
        read_only = FALSE
    ) {
        if (read_only) {
            store <- ragnar::ragnar_store_connect(db_path, read_only = TRUE)
        } else {
            .ollama_running() || .ollama_start()
            store <- ragnar::ragnar_store_create(
                db_path,
                embed = function(x) {
                    ragnar::embed_ollama(x, model = embedding_model)
                },
                embedding_size = 768L,
                overwrite = overwrite,
                name = "BiocKB",
                title = "BiocKB"
            )
        }
        S7::new_object(
            S7::S7_object(),
            db_path = db_path,
            embedding_model = embedding_model,
            read_only = read_only,
            store = store
        )
    }
)

#' Insert Chunks into a BiocKB Store
#'
#' Embeds and inserts pre-chunked documents into a BiocKB store.
#' A progress message is printed showing the origin, number of chunks,
#' and elapsed time.
#'
#' @param kb A `BiocKB` object.
#' @param chunks A data frame of chunks as returned by [chunk_bioc_file()].
#'
#' @return The `BiocKB` object invisibly on success, or `NULL` if insertion
#'   failed (with a warning).
#'
#' @seealso [BiocKB], [chunk_bioc_file()]
#'
#' @examples
#' \dontrun{
#' kb <- BiocKB(tempfile(fileext = ".duckdb"))
#' chunks <- chunk_bioc_file("path/to/file.R")
#' insert(kb, chunks)
#' }
#'
#' @export
insert_bioc_kb <- S7::new_generic("insert_bioc_kb", "kb")

S7::method(insert_bioc_kb, BiocKB) <- function(kb, chunks) {
    .ollama_running() || .ollama_start()
    start_time <- Sys.time()
    tryCatch(
        ragnar::ragnar_store_insert(kb@store, chunks),
        error = function(e) {
            warning(sprintf("Error inserting chunks into store: %s", e$message))
            return(NULL)
        }
    )
    end_time <- Sys.time()
    time_taken <- as.numeric(end_time - start_time, units = "secs")
    message(sprintf(
        "%s | %d chunks | %.2fs",
        chunks@document@origin, nrow(chunks), time_taken
    ))
    invisible(kb)
}

#' Build the Search Index of a BiocKB Store
#'
#' Finalises the store by building the vector-similarity search index.
#' This should be called after all chunks have been inserted with
#' [insert_bioc_kb()].
#'
#' @param kb A `BiocKB` object.
#'
#' @return The `BiocKB` object (invisibly).
#'
#' @seealso [BiocKB], [insert_bioc_kb()]
#'
#' @examples
#' \dontrun{
#' kb <- BiocKB(tempfile(fileext = ".duckdb"))
#' # ... insert chunks ...
#' build_bioc_index(kb)
#' }
#'
#' @export
build_bioc_index <- S7::new_generic("build_bioc_index", "kb")

S7::method(build_bioc_index, BiocKB) <- function(kb) {
    .ollama_running() || .ollama_start()
    ragnar::ragnar_store_build_index(kb@store)
    invisible(kb)
}

#' Retrieve Relevant Chunks from a BiocKB Store
#'
#' Performs semantic search over the knowledge base to find chunks
#' relevant to a query.
#'
#' @param kb A `BiocKB` object.
#' @param query Character string. The search query.
#' @param top_k Integer. Number of results to return. Default: 5.
#'
#' @return A data frame of matching chunks with relevance scores.
#'
#' @seealso [BiocKB], [build_bioc_index()]
#'
#' @examples
#' \dontrun{
#' kb <- BiocKB("biockb.duckdb", read_only = TRUE)
#' results <- retrieve_bioc_kb(kb, "DESeq2 differential expression")
#' }
#'
#' @export
retrieve_bioc_kb <- S7::new_generic("retrieve_bioc_kb", "kb")

S7::method(retrieve_bioc_kb, BiocKB) <- function(kb, query, top_k = 5L) {
    ragnar::ragnar_retrieve(kb@store, query, top_k = as.integer(top_k))
}

S7::method(format, BiocKB) <- function(x, ...) {
    status <- if (x@read_only) "connected (read-only)" else "writable"
    c(
        sprintf("<BiocKB> [%s]", status),
        sprintf("  Path: %s", x@db_path),
        sprintf("  Embedding model: %s", x@embedding_model)
    )
}

S7::method(print, BiocKB) <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    invisible(x)
}


#' Path to the BiocKB Database
#'
#' Returns the local file path to the BiocKB DuckDB database, downloading
#' it from ExperimentHub on first use and caching it via BiocFileCache.
#'
#' @param check Logical. If `TRUE` (the default), verifies the file exists
#'   and returns `NULL` with a message if it does not.
#'
#' @return A character string with the database path, or `NULL` if not
#'   available.
#'
#' @examples
#' \dontrun{
#' kb_path()
#' }
#'
#' @export
kb_path <- function(check = TRUE) {
    .check_package("ExperimentHub", "for downloading the knowledge base")
    .check_package("BiocFileCache", "for caching the knowledge base")
    eh <- ExperimentHub::ExperimentHub()
    ## TODO: replace with actual ExperimentHub ID once resource is submitted
    db_path <- tryCatch(
        eh[["EH_BIOCKB_PLACEHOLDER"]],
        error = function(e) NULL
    )
    if (check && (is.null(db_path) || !file.exists(db_path))) {
        message("BiocKB database not found. See ?kb_path for details.")
        return(NULL)
    }
    db_path
}
