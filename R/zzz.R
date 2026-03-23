.onLoad <- function(libname, pkgname) {
    env_path <- Sys.getenv("BIOCAGENT_RAGDB_PATH", unset = NA)
    if (!is.na(env_path) && is.null(getOption("BiocAgentRAGDB.path"))) {
        options(BiocAgentRAGDB.path = env_path)
    }
}
