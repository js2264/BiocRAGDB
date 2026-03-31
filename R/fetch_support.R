#' Fetch Posts from the Bioconductor Support Site
#'
#' Downloads questions and answers from
#' \url{https://support.bioconductor.org} using its public REST API,
#' and writes each post as a Markdown file suitable for chunking and
#' ingestion into the knowledge base.
#'
#' The API is documented at
#' \url{https://support.bioconductor.org/info/api/}.
#'
#' @param base_dir Character string. Directory where the fetched posts
#'   will be saved (one Markdown file per post).
#' @param max_pages Integer. Maximum number of result pages to fetch.
#'   Each page typically contains 25 posts. Default: `100`
#'   (i.e., up to 2 500 posts).
#' @param tag Character string. Optional tag to filter posts
#'   (e.g., `"deseq2"`). Default: `NULL` (all posts).
#' @param force Logical. If `TRUE`, skip the interactive confirmation
#'   when `base_dir` already exists. Default: `FALSE`.
#'
#' @return A character vector of paths to the created Markdown files
#'   (invisibly).
#'
#' @details
#' For each post, the title, body (in HTML, converted to text), accepted
#' answer (if any), tags, and metadata are written as a single Markdown
#' document. Rate limiting is respected with a brief pause between
#' paginated requests.
#'
#' @examples
#' \dontrun{
#' files <- fetch_support_posts(tempdir(), max_pages = 2)
#' head(files)
#' }
#'
#' @export
fetch_support_posts <- function(
    base_dir,
    max_pages = 100L,
    tag = NULL,
    force = FALSE
) {
    .check_package("httr2", "for fetching support site posts")
    .check_package("jsonlite", "for parsing API responses")

    if (dir.exists(base_dir) && !force) {
        if (interactive()) {
            message(sprintf("Base directory %s already exists.", base_dir))
            ask <- readline(prompt = "Do you want to continue? (y/n): ")
            if (tolower(ask) != "y") stop("Operation cancelled by user.")
        } else {
            stop("Base directory already exists. Use force = TRUE to overwrite.", call. = FALSE)
        }
    }

    out_dir <- file.path(base_dir, "all_files", "support.bioconductor.org")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    base_url <- "https://support.bioconductor.org"
    collected <- character(0)
    page <- 1L

    while (page <= max_pages) {
        ## Build the API URL for listing posts
        api_path <- if (!is.null(tag)) {
            sprintf("/api/post/?limit=25&offset=%d&tag=%s",
                    (page - 1L) * 25L, utils::URLencode(tag))
        } else {
            sprintf("/api/post/?limit=25&offset=%d", (page - 1L) * 25L)
        }

        resp <- tryCatch(
            {
                paste0(base_url, '/api/post/') |> 
                    # add query parameters
                    httr2::request() |>
                    httr2::req_url_query(limit = 25, offset = (page - 1L) * 25L, tag = tag) |>
                    httr2::req_headers(Accept = "application/json") |>
                    httr2::req_timeout(30) |>
                    httr2::req_perform()
            },
            error = function(e) {
                warning(sprintf("API request failed (page %d): %s", page, e$message))
                return(NULL)
            }
        )

        if (is.null(resp)) break

        body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
        posts <- body$results %||% body$objects %||% body

        ## If the result is a flat list of posts (not wrapped), handle it
        if (is.null(names(posts)) && length(posts) == 0) break
        if (!is.null(names(posts)) && !"id" %in% names(posts[[1]])) break

        for (post in posts) {
            md_text <- .format_support_post(post)
            post_id <- post$id %||% post$uid %||% gsub("[^a-zA-Z0-9]", "_", post$title %||% "unknown")
            fname <- sprintf("post_%s.md", post_id)
            fpath <- file.path(out_dir, fname)
            writeLines(md_text, fpath)
            collected <- c(collected, fpath)
        }

        ## Check if there are more pages
        has_next <- !is.null(body[["next"]]) || length(posts) >= 25L
        if (!has_next) break

        page <- page + 1L
        Sys.sleep(0.5)
    }

    message(sprintf("Fetched %d posts from support.bioconductor.org", length(collected)))
    invisible(collected)
}


#' Format a support site post as Markdown
#' @param post List. A single post object from the API.
#' @return Character string with Markdown content.
#' @noRd
.format_support_post <- function(post) {
    title <- post$title %||% "Untitled"
    body <- post$content %||% post$text %||% post$html %||% ""

    ## Strip HTML tags for clean text
    body <- gsub("<[^>]+>", "", body)

    tags <- paste(unlist(post$tag_val %||% post$tags %||% list()), collapse = ", ")
    post_type <- post$type_display %||% post$type %||% "Question"
    created <- post$creation_date %||% post$created %||% ""
    url <- if (!is.null(post$id)) {
        sprintf("https://support.bioconductor.org/p/%s/", post$id)
    } else ""

    ## Build answer section
    answer_text <- ""
    if (!is.null(post$answer) && length(post$answer) > 0) {
        answers <- if (is.list(post$answer)) post$answer else list(post$answer)
        answer_parts <- vapply(answers, function(a) {
            a_body <- a$content %||% a$text %||% a$html %||% as.character(a)
            gsub("<[^>]+>", "", a_body)
        }, character(1))
        answer_text <- paste0(
            "\n\n## Answer\n\n",
            paste(answer_parts, collapse = "\n\n---\n\n")
        )
    }

    ## Accepted answer (some API formats put it at top level)
    if (nzchar(answer_text) == 0 && !is.null(post$accepted_answer)) {
        acc <- post$accepted_answer
        acc_text <- if (is.character(acc)) acc else acc$content %||% acc$text %||% ""
        acc_text <- gsub("<[^>]+>", "", acc_text)
        if (nzchar(acc_text)) {
            answer_text <- paste0("\n\n## Accepted Answer\n\n", acc_text)
        }
    }

    ## Replies
    reply_text <- ""
    if (!is.null(post$replies) && length(post$replies) > 0) {
        reply_parts <- vapply(post$replies, function(r) {
            r_body <- r$content %||% r$text %||% r$html %||% as.character(r)
            gsub("<[^>]+>", "", r_body)
        }, character(1))
        reply_text <- paste0(
            "\n\n## Replies\n\n",
            paste(reply_parts, collapse = "\n\n---\n\n")
        )
    }

    paste0(
        "# ", title, "\n\n",
        "- **Type**: ", post_type, "\n",
        "- **Tags**: ", tags, "\n",
        "- **Date**: ", created, "\n",
        if (nzchar(url)) paste0("- **URL**: ", url, "\n") else "",
        "\n## Question\n\n",
        body,
        answer_text,
        reply_text
    )
}
