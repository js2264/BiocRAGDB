# BiocKB

RAG knowledge base for Bioconductor, backed by
[ragnar](https://github.com/tidyverse/ragnar) + DuckDB. Harvests, chunks, and
indexes Bioconductor package sources, website content, support site Q&A, and
biocViews metadata for semantic retrieval.

## Installation

```r
BiocManager::install("BiocKB")
```

## Quick start

```r
library(BiocKB)

## Connect to a specific version of the pre-built knowledge base 
## (downloads from ExperimentHub if needed)
kb <- BiocKB(kb_path(bioc_version = "3.20"), read_only = TRUE)

## Retrieve relevant chunks
retrieve_bioc_kb(kb, "differential expression with DESeq2", top_k = 5)
```

BiocKB maintains **versioned knowledge bases** aligned with Bioconductor release cycles, 
ensuring your RAG retrieval is anchored to a specific version of Bioconductor packages, 
website content, and documentation.

## Building versioned knowledge bases

BiocKB supports **versioned knowledge bases** tied to specific Bioconductor releases 
(e.g., 3.19, 3.20). Each version maintains a snapshot of package sources, 
vignettes, documentation, and support site posts for reproducibility and 
version-specific retrieval.

The typical workflow follows: **fetch → chunk → insert → index → retrieve**.

### 1. Fetch Bioconductor resources

```r
library(BiocKB)

base_dir <- "biockb_resources"

## Clone and collect Bioconductor package sources (R/, vignettes, DESCRIPTION, NAMESPACE)
pkgs_files <- fetch_pkgs_resources(base_dir, pkgs = c("DESeq2", "scran"), branch = "devel")

## Clone bioconductor.org website content
pages <- fetch_bioc_website(base_dir)

## Download biocViews ontology + per-package metadata
views <- fetch_bioc_views(base_dir)

## Clone the Package Developer book
contribs <- fetch_bioc_contributions(base_dir)

## Scrape support.bioconductor.org Q&A posts
posts <- fetch_support_posts(base_dir, max_pages = 10)

all_files <- c(pkgs_files, unlist(pages), views, unlist(contribs), posts)
```

### 2. Chunk and insert into the knowledge base

```r
## AST-aware chunking of all collected files
chunks <- lapply(all_files, chunk_bioc_file, base_path = base_dir)

## Create a new versioned knowledge base (requires Ollama for embeddings)
## Specify bioc_version to tie this KB to a Bioconductor release
kb <- BiocKB(
    "biockb.duckdb", 
    bioc_version = "3.23",
    overwrite = TRUE
)

## Insert chunks and build the search index
purrr::map(chunks, insert_bioc_kb, kb = kb)
build_bioc_index(kb)
```

### 3. Retrieve

```r
## Connect to a versioned knowledge base for querying
kb <- BiocKB("biockb-3.23.duckdb", bioc_version = "3.23", read_only = TRUE)

## Semantic search returns results anchored to the KB's Bioconductor version
retrieve_bioc_kb(kb, "How to perform differential expression analysis")
retrieve_bioc_kb(kb, "How to submit a package to Bioconductor")
```

## Key Functions

| Function | Description |
|---|---|
| `BiocKB()` | S7 class: create or connect to a DuckDB ragnar store |
| `insert_bioc_kb()` | Embed and insert chunks into a BiocKB store |
| `build_bioc_index()` | Build the vector-similarity search index |
| `retrieve_bioc_kb()` | Semantic search over the knowledge base |
| `kb_path()` | Locate or download the pre-built knowledge base |
| `chunk_bioc_file()` | AST-aware chunking of R, Rmd, Rnw, DESCRIPTION files |
| `fetch_pkgs_resources()` | Clone and collect Bioconductor package sources |
| `fetch_bioc_website()` | Clone bioconductor.org content |
| `fetch_bioc_views()` | Fetch biocViews ontology + per-package metadata |
| `fetch_bioc_contributions()` | Clone Bioconductor Package Developer book |
