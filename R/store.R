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

biocragdb_store_index <- function(store) {
    ragnar::ragnar_store_build_index(store)
}

biocragdb_store_connect <- function(db_path) {
    ragnar::ragnar_store_connect(db_path)
}
