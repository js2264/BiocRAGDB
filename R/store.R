#' Create a BiocRAGDB Store
#'
#' Initialises a new \pkg{ragnar} RAG store backed by a DuckDB
#' database. Embeddings are generated locally by Ollama using the
#' specified model.
#'
#' An Ollama server must be reachable (it will be started automatically
#' if the `ollama` executable is found on the `PATH`).
#'
#' @param db_path Character string. Path for the new DuckDB file.
#' @param embedding_model Character string. Name of the Ollama embedding
#'   model to use. Default: `"nomic-embed-text"`.
#' @param overwrite Logical. If `TRUE`, any existing store at `db_path` is
#'   replaced. Default: `FALSE`.
#'
#' @return A ragnar store object (invisibly), as returned by
#'   \code{\link[ragnar]{ragnar_store_create}}.
#'
#' @seealso [biocragdb_store_insert()], [biocragdb_store_index()],
#'   [biocragdb_store_connect()]
#'
#' @examples
#' \dontrun{
#' store <- biocragdb_store_create(tempfile(fileext = ".duckdb"))
#' }
#'
#' @export
biocragdb_store_create <- function(db_path, embedding_model = "nomic-embed-text", overwrite = FALSE) {

    # Check that ollama server is running 
    .ollama_running() || .ollama_start()

    ragnar::ragnar_store_create(
        db_path,
        embed = function(x) {ragnar::embed_ollama(x, model = embedding_model)}, 
        embedding_size = 768L,
        overwrite = overwrite, 
        name = "BiocRAGDB", 
        title = "BiocRAGDB"
    )
}

#' Insert Chunks into a BiocRAGDB Store
#'
#' Embeds and inserts pre-chunked documents into an existing ragnar store.
#' A progress message is printed for each batch showing the origin, number
#' of chunks, and elapsed time.
#'
#' @param store A ragnar store object created by
#'   [biocragdb_store_create()] or opened by [biocragdb_store_connect()].
#' @param chunks A data frame of chunks as returned by [chunk_bioc_file()].
#'
#' @return `TRUE` invisibly on success, or `NULL` if insertion failed
#'   (with a warning).
#'
#' @seealso [biocragdb_store_create()], [chunk_bioc_file()]
#'
#' @export
biocragdb_store_insert <- function(store, chunks) {

    # Check that ollama server is running 
    .ollama_running() || .ollama_start()

    # logging time 
    start_time <- Sys.time()

    tryCatch(
        {
            ragnar::ragnar_store_insert(store, chunks)
        },
        error = function(e) {
            warning(sprintf("Error inserting chunks into store: %s", e$message))
            return(NULL)
        }
    )

    # log time taken for insertion
    end_time <- Sys.time()
    time_taken <- end_time - start_time
    # log as "ORIGIN | NUM_CHUNKS | TIME_TAKEN"
    message(sprintf("%s | %d chunks | %.2fs", chunks@document@origin, nrow(chunks), as.numeric(time_taken, units = "secs")))

    return(invisible(TRUE))
}

#' Build the Search Index of a BiocRAGDB Store
#'
#' Finalises the store by building the vector-similarity search index.
#' This should be called after all chunks have been inserted with
#' [biocragdb_store_insert()].
#'
#' @param store A ragnar store object.
#'
#' @return The result of \code{\link[ragnar]{ragnar_store_build_index}}
#'   (invisibly).
#'
#' @seealso [biocragdb_store_create()], [biocragdb_store_insert()]
#'
#' @export
biocragdb_store_index <- function(store) {
    .ollama_running() || .ollama_start()
    ragnar::ragnar_store_build_index(store)
}

#' Connect to an Existing BiocRAGDB Store
#'
#' Opens a previously created ragnar store for querying.
#'
#' @param db_path Character string. Path to the DuckDB file.
#'
#' @return A ragnar store connection object.
#'
#' @seealso [biocragdb_store_create()]
#'
#' @export
biocragdb_store_connect <- function(db_path) {
    ragnar::ragnar_store_connect(db_path, read_only = TRUE)
}
