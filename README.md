# BiocKB

RAG knowledge base for Bioconductor, backed by
[ragnar](https://github.com/tidyverse/ragnar) + DuckDB. Harvests, chunks, and
indexes Bioconductor package sources, website content, support site Q&A, and
biocViews metadata for semantic retrieval.

## Installation

```r
BiocManager::install("BiocKB")
```

## Quick Start

```r
library(BiocKB)

## Connect to a pre-built knowledge base (downloads from ExperimentHub if needed)
store <- biockb_store_connect(kb_path())

## Retrieve relevant chunks
ragnar::ragnar_retrieve(store, "differential expression with DESeq2", top_k = 5)
```

## Building a Knowledge Base from Scratch

```r
library(BiocKB)

base_dir <- tempdir()

## 1. Fetch sources
files <- fetch_pkgs_resources(base_dir, pkgs = c("DESeq2", "scran"))
fetch_bioc_website(base_dir)
fetch_support_posts(base_dir, max_pages = 10)
ingest_bioc_views(base_dir)

## 2. Chunk
chunks <- lapply(files, chunk_bioc_file)

## 3. Create, insert, and index the store
db_path <- file.path(base_dir, "BiocKB.duckdb")
store <- biockb_store_create(db_path)
lapply(chunks, biockb_store_insert, store = store)
biockb_store_index(store)
```

## Key Functions

| Function | Description |
|---|---|
| `biockb_store_create()` | Create a new DuckDB ragnar store |
| `biockb_store_insert()` | Insert chunks into a store |
| `biockb_store_index()` | Build the embedding index |
| `biockb_store_connect()` | Connect to an existing store |
| `chunk_bioc_file()` | AST-aware chunking of R, Rmd, Rnw files |
| `fetch_pkgs_resources()` | Clone and collect Bioconductor package sources |
| `fetch_bioc_website()` | Clone bioconductor.org content |
| `fetch_support_posts()` | Scrape support.bioconductor.org Q&A |
| `fetch_bioc_views()` | Fetch biocViews ontology |
| `ingest_bioc_views()` | Ingest biocViews + package metadata |
| `kb_path()` | Locate or download the pre-built knowledge base |

## Part of BiocAI

BiocKB is part of the [BiocAI](https://github.com/BiocAI/BiocAI) project —
a modular AI stack for the Bioconductor ecosystem.
