# Overview

This notebook documents the complete pipeline for building BiocRAGDB, 
a DuckDB-backed RAG (retrieval-augmented generation) database containing
relevant content sourced from a range of Bioconductor resources. 

Resources include: 

- [x] Bioconductor packages (DESCRIPTION, NAMESPACE, vignettes, source code)
- [ ] Bioconductor website (https://github.com/Bioconductor/bioconductor.org)
- [ ] Bioconductor contribution documentation (https://github.com/Bioconductor/pkgrevdocs/)
- [ ] Bioconductor Q&A sites (support, stackoverflow, biostars?)

The DuckDB database is built using the `ragnar` package using the 
`nomic-embed-text` embedding model, a general-purpose text embedding
model that performs well on a wide range of tasks.

The resulting database file will be available from ExperimentHub for distribution via
the `BiocRAGDB` package.

## Fetch package resources

```R
devtools::load_all()
base_dir <- "/pasteur/appa/scratch/jaseriza/BiocRAGDB"
collected_files <- fetch_pkgs_resources(
    base_dir, 
    pkgs = c('monocle', 'scran'), 
    force = TRUE,
    BPPARAM = BiocParallel::MulticoreParam(workers = 12, progressbar = TRUE)
)
```

## Chunk Bioc files into chunks 

```R
chunks_list <- lapply(collected_files, chunk_bioc_file)
```

## Ingest chunks into the database

```R
db_path <- file.path(base_dir, "BiocRAGDB.duckdb")
store <- biocragdb_store_create(db_path, embedding_model = "nomic-embed-text", overwrite = TRUE)
lapply(chunks_list, biocragdb_store_insert, store = store)
biocragdb_store_index(store)
DBI::dbDisconnect(store@con)
```

## Check the database

Run some sanity checks on the built database.

```R
store <- biocragdb_store_connect(db_path)

results1 <- ragnar::ragnar_retrieve(
    store,
    "how do I do differential expression analysis specifically with monocle?",
    top_k = 10
)

results2 <- ragnar::ragnar_retrieve(
    store,
    "how do I do differential expression analysis specifically with scran?",
    top_k = 10
)
```

```R
print(results1[, c("origin", "doc_id", "text")])
## # A tibble: 16 × 3
##    origin                              doc_id text                              
##    <chr>                                <int> <chr>                             
##  1 monocle/DESCRIPTION                     18 "Package: monocle\nType: Package\…
##  2 monocle/R/BEAM.R                         1 "### BEAM\n\n```r\n#' Branched ex…
##  3 monocle/R/CellDataSet.R                  3 "### setClass: CellDataSet\n\n```…
##  4 monocle/R/RcppExports.R                 15 "### jaccard_coeff\n\n```r\njacca…
##  5 monocle/R/cds_conversion.R               2 "### exportCDS\n\n```r\n#' Export…
##  6 monocle/R/differential_expression.R      8 "### differentialGeneTest\n\n```r…
##  7 monocle/R/expr_models.R                  9 "### fitModel\n\n```r\n#' Fits a …
##  8 monocle/R/order_cells.R                 13 "### reduceDimension\n\n```r\n#' …
##  9 scran/R/convertTo.R                     28 "### convertTo\n\n```r\n#' Conver…
## 10 scran/R/convertTo.R                     28 "        pd <- colData(x)\n    } …
## 11 scran/R/denoisePCA.R                    35 "### expression_1\n\n```r\n#' Den…
## 12 scran/R/findMarkers.R                   38 "### expression_1\n\n```r\n#' Fin…
## 13 scran/R/pairwiseWilcox.R                56 "### expression_1\n\n```r\n#' Per…
## 14 scran/R/pseudoBulkDGE.R                 57 "### expression_1\n\n```r\n#' Qui…
## 15 scran/R/pseudoBulkDGE.R                 57 "#' If \\code{include.intermediat…
## 16 scran/R/pseudoBulkDGE.R                 57 "#' Batch effects and the effecti…

print(results2[, c("origin", "doc_id", "text")])
### A tibble: 15 × 3
##   origin                              doc_id text                              
##   <chr>                                <int> <chr>                             
## 1 monocle/DESCRIPTION                     18 "Package: monocle\nType: Package\…
## 2 monocle/R/BEAM.R                         1 "### BEAM\n\n```r\n#' Branched ex…
## 3 monocle/R/differential_expression.R      8 "### diff_test_helper\n\n```r\n#'…
## 4 monocle/R/differential_expression.R      8 "### differentialGeneTest\n\n```r…
## 5 scran/DESCRIPTION                       74 "Package: scran\nVersion: 1.39.1\…
## 6 scran/R/convertTo.R                     28 "### convertTo\n\n```r\n#' Conver…
## 7 scran/R/cyclone.R                       32 "#' @param verbose A logical scal…
## 8 scran/R/modelGeneVar.R                  49 "### expression_1\n\n```r\n#' Mod…
## 9 scran/R/namespace.R                     53 "### expression_1\n\n```r\n#' @im…
##10 scran/R/pairwiseTTests.R                55 "#' @param log.p A logical scalar…
##11 scran/R/pairwiseTTests.R                55 "#' This contains the fields \\co…
##12 scran/R/pseudoBulkDGE.R                 57 "#' If \\code{include.intermediat…
##13 scran/R/pseudoBulkDGE.R                 57 "#' Batch effects and the effecti…
##14 scran/vignettes/scran.Rmd               73 "---\ntitle: Using scran to analy…
##15 scran/vignettes/scran.Rmd               73 "# Introduction\n\nSingle-cell RN…```
```

## Query an LLM with retrieved results as context

```R
library(ellmer)
chat <- ellmer::chat_ollama(
    model = "qwen3-coder:30b",
    system = stringr::str_squish("
        You are a helpful assistant for answering questions about Bioconductor 
        packages and resources.
    "),
    api_args = list(text = list(verbosity = "low"))
)
ragnar::ragnar_register_tool_retrieve(chat, store)
chat$chat("What do you know about scran, monocle, and glmGamPoi, only based on provided BiocRAGDB database?")
```

```
◯ [tool call] search_BiocRAGDB(text = c("scran bioconductor package", "scran single cell RNA sequencing"...)
● #> [               
  #>  {
  #>  "origin": "scran/DESCRIPTION",
  #>  "doc_id": 74,
  #>  "chunk_id": 1470,
  #> …
◯ [tool call] search_BiocRAGDB(text = c("monocle bioconductor package", "monocle single cell RNA sequencing"...)
● #> [               
  #>  {
  #>  "origin": "monocle/DESCRIPTION",
  #>  "doc_id": 18,
  #>  "chunk_id": 648,
  #> …
◯ [tool call] search_BiocRAGDB(text = c("glmGamPoi bioconductor package", "glmGamPoi single cell RNA sequencing"...)
● #> [               
  #>  {
  #>  "origin": "monocle/R/BEAM.R",
  #>  "doc_id": 1,
  #>  "chunk_id": 77,
  #> …
Based on the provided BiocRAGDB database, here's what I know about scran, monocle, and glmGamPoi:

## scran

scran is a Bioconductor package designed for single-cell RNA-seq data analysis. It provides methods for various routine tasks in 
single-cell analysis workflows, including:

- Cell cycle phase assignment
- Detection of highly variable and significantly correlated genes
- Identification of marker genes
- Normalization of single-cell RNA-seq data through deconvolution of size factors from cell pools
- Variance modeling and testing for marker genes and gene-gene correlations

The package depends on several Bioconductor packages including SingleCellExperiment, scuttle, edgeR, and limma. It also provides 
functions for pseudo-bulk differential expression analysis and clustering. scran supports both standard and ranked expression values for 
clustering, and includes functions for subclustering and normalization. The package is maintained by Aaron Lun and has extensive 
documentation in vignettes, including a comprehensive guide for analyzing scRNA-seq data.

## monocle

monocle is a Bioconductor package specialized for clustering, differential expression, and trajectory analysis in single-cell RNA-seq 
experiments. Key features include:

- Ordering cells according to progress through a biological process without predefined gene markers
- Performing differential expression analysis and time-series analysis
- Clustering, visualization, and other useful tasks on single-cell expression data
- Working with RNA-Seq and qPCR data
- Supporting trajectory inference and branching point detection (using BEAM)

The package provides functionalities for converting between different single-cell analysis toolkits (exporting to Seurat and importing 
from Seurat), handling cell datasets through a CellDataSet class, and implementing various clustering approaches. It also includes 
utilities for size factor calculation and dataset loading (including the HSMMSingleCell package).

## glmGamPoi

While I don't have specific information about glmGamPoi in the provided database, based on the package name and context, it's likely a 
Bioconductor package related to generalized linear model (GLM) analysis of single-cell sequencing data. Given the context of the other 
packages mentioned (scran and monocle), it's probably designed for modeling single-cell expression data, possibly for differential 
expression analysis using GAMs or similar methods, though I can't provide specific details based only on the database information.
```
