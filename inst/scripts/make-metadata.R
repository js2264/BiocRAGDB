## Script to generate inst/extdata/metadata.csv
## Run from the package root: Rscript inst/scripts/make-metadata.R

metadata <- data.frame(
    Title = "BiocRAGDB: RAG Knowledge Base for BiocAgent",
    Description = paste(
        "DuckDB-backed RAG database containing chunked and embedded",
        "source code, vignettes, DESCRIPTION, and NAMESPACE files from",
        "all Bioconductor software packages. Built with ragnar."
    ),
    BiocVersion = "3.21",
    Genome = NA_character_,
    SourceType = "DuckDB",
    SourceUrl = "https://git.bioconductor.org/packages/",
    SourceVersion = format(Sys.Date(), "%Y%m%d"),
    Species = NA_character_,
    TaxonomyId = NA_integer_,
    Coordinate_1_based = NA,
    DataProvider = "Bioconductor",
    Maintainer = "Jacques Serizay <jacquesserizay@gmail.com>",
    RDataClass = "character",
    DispatchClass = "FilePath",
    RDataPath = "BiocRAGDB/bioc_ragdb.duckdb",
    stringsAsFactors = FALSE
)

outdir <- file.path("inst", "extdata")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

write.csv(metadata, file = file.path(outdir, "metadata.csv"),
    row.names = FALSE)

message("metadata.csv written to ", file.path(outdir, "metadata.csv"))
