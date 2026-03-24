biocragdb_retrieve_excerpts <- function(store) {
    retrieved_chunk_ids <- integer()
    function(text) {
        chunks <- ragnar::ragnar_retrieve(
            store,
            text,
            top_k = 10,
            filter = !.data$chunk_id %in% retrieved_chunk_ids
        )
        retrieved_chunk_ids <<- unique(unlist(c(retrieved_chunk_ids, chunks$chunk_id)))
        chunks
    }
}
