.onLoad <- function(libname, pkgname) {
    env_path <- Sys.getenv("BIOCAGENT_RAGDB_PATH", unset = NA)
    if (!is.na(env_path) && is.null(getOption("BiocRAGDB.path"))) {
        options(BiocRAGDB.path = env_path)
    }
}
