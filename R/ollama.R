.ollama_url <- function(OLLAMA_HOST = NULL) {
    host <- OLLAMA_HOST %||% "127.0.0.1:11434"
    url <- paste0("http://", host)
    return(url)
}

.ollama_start <- function(url = .ollama_url(), path = .ollama_exists()) {
    
    # Run the bash command `ollama serve` in the background
    Sys.setenv(OLLAMA_HOST = sub("http://", "", url))
    system2(path, "serve", wait = FALSE, stdout = FALSE, stderr = FALSE)
    
    # Check that the server is running by listing models
    models <- .ollama_list(url)

}

.ollama_running <- function(url = .ollama_url()) {
    tryCatch(
        {
            httr2::request(url) |>
                httr2::req_url_path_append("api", "tags") |> 
                httr2::req_perform()
            return(TRUE)
        },
        error = function(e) {
            if (grepl("Could not connect to server", conditionMessage(e))) {
                return(FALSE)
            } else {
                warning(sprintf("Error checking Ollama server: %s", e$message))
                return(FALSE)
            }
        }
    )
}

.ollama_list <- function(url = .ollama_url()) {
    req <- httr2::request(url) |>
        httr2::req_url_path_append("api", "tags") |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    req <- req$models
    names <- vapply(req, \(x) x$name, 'chararcter')
    models <- lapply(req, \(x) {
        x[-c(which(names(x) == "name"))]
    })
    names(models) <- names
    return(models)
}

.ollama_exists <- function(path = Sys.which("ollama")) {
    if (nzchar(path) && file.exists(path)) {
        return(invisible(unname(path)))
    }
    else {
        message <- "Ollama executable not found. Please ensure Ollama is installed and on your system PATH."
        warning(message)
        return(FALSE)
    }
}
