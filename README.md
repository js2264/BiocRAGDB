# Overview

This notebook documents the complete pipeline for building the BiocRAGDB, 
a DuckDB-backed RAG (retrieval-augmented generation) database containing
the source code, vignettes, and metadata from all Bioconductor packages.

The DuckDB database is built using the `ragnar` package using the 
`nomic-embed-text` embedding model, a general-purpose text embedding
model that performs well on a wide range of tasks.

The resulting database file will be available from ExperimentHub for distribution via
the `BiocRAGDB` package.

## Fetch package resources

```{r setup-clone-dir}
devtools::load_all()
base_dir <- "/pasteur/appa/scratch/jaseriza/BiocRAGDB"
collected_files <- fetch_pkgs_resources(
    base_dir, 
    pkgs = c('DESeq2', 'tidyCoverage'), 
    BPPARAM = BiocParallel::MulticoreParam(workers = 12, progressbar = TRUE)
)
```

## Chunk Bioc files into chunks 

```{r build-store}
db_path <- file.path(base_dir, "BiocRAGDB.duckdb")
store <- biocragdb_store_create(db_path, embedding_model = "nomic-embed-text", overwrite = TRUE)
chunks_list <- lapply(collected_files, chunk_bioc_file)
```

## Ingest chunks into the database

```{r}
lapply(chunks_list, biocragdb_store_insert, store = store)
biocragdb_store_index(store)
```

## Validate the database

Run some sanity checks on the built database.

```{r validate}
store <- biocragdb_store_connect(db_path)

results <- ragnar::ragnar_retrieve(
    store,
    "how can I perform DE analysis?",
    top_k = 5
)
print(results[, c("origin", "doc_id", "text")])

results2 <- ragnar::ragnar_retrieve(
    store,
    "how to create an plot to show coverage tracks over TSSs?",
    top_k = 5
)
print(results2[, c("origin", "doc_id", "text")])
```

