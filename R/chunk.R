#' @export 
chunk_bioc_file <- function(file_path) {

    ext <- tolower(tools::file_ext(file_path))
    bname <- basename(file_path)
    rel_path <- sub(paste0(base_dir, "/"), "", file_path, fixed = TRUE)
    origin <- rel_path %||% file_path

    ## ── DESCRIPTION / NAMESPACE: single chunk ──
    if (bname %in% c("DESCRIPTION", "NAMESPACE")) {
        txt <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
        md <- ragnar::MarkdownDocument(txt, origin = origin)
        return(ragnar::markdown_chunk(md, target_size = Inf))
    }

    ## ── R source files: AST-aware chunking ──
    if (ext %in% c("r")) {
        md_text <- r_to_markdown(file_path)
        md <- ragnar::MarkdownDocument(md_text, origin = origin)
        return(.safe_markdown_chunk(
            md, target_size = 1000,
            segment_by_heading_levels = 3L
        ))
    }

    ## ── Vignettes (.Rmd, .qmd, .md): section-based ──
    if (ext %in% c("rmd", "qmd", "md")) {
        md <- ragnar::read_as_markdown(file_path, origin = origin)
        return(.safe_markdown_chunk(
            md, target_size = 1000,
            segment_by_heading_levels = c(1L, 2L)
        ))
    }

    ## ── Vignettes (.Rnw): Sweave/knitr LaTeX via pandoc ──
    if (ext %in% c("rnw")) {
        md_text <- rnw_to_markdown(file_path)
        md <- ragnar::MarkdownDocument(md_text, origin = origin)
        return(.safe_markdown_chunk(
            md, target_size = 1000,
            segment_by_heading_levels = c(1L, 2L)
        ))
    }

    ## ── Fallback: generic markdown chunking ──
    md <- ragnar::read_as_markdown(file_path, origin = origin)
    .safe_markdown_chunk(md)
}

#' Wrapper around ragnar::markdown_chunk that retries after stripping
#' footnote syntax when xml2/libxml2 version mismatches cause failures.
.safe_markdown_chunk <- function(md, ...) {
    tryCatch(
        ragnar::markdown_chunk(md, ...),
        error = function(e) {
            if (!grepl("StartTag|invalid element", conditionMessage(e)))
                stop(e)
            ## Strip footnote markers that can trigger libxml2 parsing bugs
            txt <- gsub("\\[\\^[^]]+\\]", "", as.character(md))
            md_clean <- ragnar::MarkdownDocument(txt, origin = attr(md, "origin") %||% "")
            ragnar::markdown_chunk(md_clean, ...)
        }
    )
}

#' Convert R source file to markdown with one ### section per top-level expression
#' Roxygen comments are kept with their function.
r_to_markdown <- function(file_path) {
    lines <- readLines(file_path, warn = FALSE)
    if (length(lines) == 0) return("")

    parsed <- tryCatch(
        parse(file_path, keep.source = TRUE),
        error = function(e) NULL
    )

    # Unparseable → single code block
    if (is.null(parsed) || length(parsed) == 0) {
        return(paste0("```r\n", paste(lines, collapse = "\n"), "\n```"))
    }

    srcrefs <- utils::getSrcref(parsed)
    sections <- character(length(srcrefs))
    prev_end <- 0L

    for (i in seq_along(srcrefs)) {
        ref <- srcrefs[[i]]
        first_line <- ref[1L]
        last_line  <- ref[3L]

        # Walk backwards to capture roxygen block
        start_line <- first_line
        j <- first_line - 1L
        while (j > prev_end && grepl("^\\s*#'", lines[j])) {
            start_line <- j
            j <- j - 1L
        }

        # Extract a heading name from the expression
        name <- extract_r_name(lines[first_line], i)
        chunk_code <- paste(lines[start_line:last_line], collapse = "\n")
        sections[i] <- paste0("### ", name, "\n\n```r\n", chunk_code, "\n```")
        prev_end <- last_line
    }

    paste(sections, collapse = "\n\n")
}

#' Convert an Rnw (Sweave/knitr) file to markdown.
#' Extracts code chunks, pre-processes Bioconductor LaTeX macros,
#' uses pandoc for LaTeX-to-markdown conversion, then restores code chunks.
#' Falls back to regex-based conversion if pandoc fails.
rnw_to_markdown <- function(file_path) {
    lines <- readLines(file_path, warn = FALSE)

    ## 1. Extract Sweave/knitr code chunks, replace with placeholders
    chunks <- list()
    in_chunk <- FALSE
    chunk_lines <- character()
    result_lines <- character()
    for (line in lines) {
        if (grepl("^<<.*>>={1,2}\\s*$", line)) {
            in_chunk <- TRUE
            chunk_lines <- character()
            next
        }
        if (in_chunk && grepl("^@(\\s|%|$)", line)) {
            in_chunk <- FALSE
            idx <- length(chunks) + 1L
            chunks[[idx]] <- paste(chunk_lines, collapse = "\n")
            placeholder <- sprintf("RNWCODECHUNK%04dPLACEHOLDER", idx)
            result_lines <- c(result_lines,
                sprintf("\\begin{verbatim}%s\\end{verbatim}", placeholder))
            next
        }
        if (in_chunk) {
            chunk_lines <- c(chunk_lines, line)
        } else {
            result_lines <- c(result_lines, line)
        }
    }

    ## 2. Extract title before stripping preamble
    title_line <- grep("^\\\\title\\{", result_lines, value = TRUE)
    title <- if (length(title_line) > 0) {
        sub("^\\\\title\\{(.*)\\}\\s*$", "\\1", title_line[1])
    } else NULL

    ## 3. Pre-process Bioconductor-specific LaTeX macros
    bioc_macros <- c(
        "Rfunction", "Rpackage", "Rclass", "Robject", "Rmethod",
        "Rcode", "Rfunarg", "Biocpkg", "CRANpkg", "software"
    )
    for (macro in bioc_macros) {
        result_lines <- gsub(
            sprintf("\\\\%s\\{([^}]*)\\}", macro),
            "\\\\texttt{\\1}",
            result_lines
        )
    }
    result_lines <- gsub("\\\\Bioconductor\\{\\}", "Bioconductor", result_lines)

    ## 4. Strip preamble and postamble
    doc_start <- grep("\\\\begin\\{document\\}", result_lines)
    doc_end   <- grep("\\\\end\\{document\\}", result_lines)
    if (length(doc_start) > 0 && length(doc_end) > 0) {
        result_lines <- result_lines[(doc_start[1] + 1):(doc_end[1] - 1)]
    }
    result_lines <- result_lines[!grepl("^\\\\maketitle\\s*$", result_lines)]

    ## 4b. Additional pre-processing to help pandoc
    ## Remove \noindent with brace-balanced stripping
    for (i in seq_along(result_lines)) {
        if (grepl("\\\\noindent\\s*\\{", result_lines[i])) {
            result_lines[i] <- sub("\\\\noindent\\s*\\{", "", result_lines[i])
            ## Remove matching trailing } if line has balanced braces now
            n_open  <- nchar(gsub("[^{]", "", result_lines[i]))
            n_close <- nchar(gsub("[^}]", "", result_lines[i]))
            if (n_close > n_open) {
                result_lines[i] <- sub("\\}\\s*$", "", result_lines[i])
            }
        } else {
            result_lines[i] <- gsub("\\\\noindent\\b\\s*", "", result_lines[i])
        }
    }
    ## Strip thebibliography blocks (not useful for RAG)
    bib_start <- grep("\\\\begin\\{thebibliography\\}", result_lines)
    bib_end   <- grep("\\\\end\\{thebibliography\\}", result_lines)
    if (length(bib_start) > 0 && length(bib_end) > 0) {
        result_lines <- result_lines[-(bib_start[1]:bib_end[1])]
    }
    ## Neutralise \Sexpr{...} → (R code)
    result_lines <- gsub("\\\\Sexpr\\{[^}]*\\}", "(R code)", result_lines)

    ## 5. Write to temp file and run pandoc
    tmp_in  <- tempfile(fileext = ".tex")
    tmp_out <- tempfile(fileext = ".md")
    on.exit(unlink(c(tmp_in, tmp_out)), add = TRUE)
    writeLines(result_lines, tmp_in)
    pandoc <- .find_pandoc()
    exit_code <- system2(pandoc, c("-f", "latex", "-t", "markdown",
                      "--wrap=none", "-o", tmp_out, tmp_in),
            stdout = FALSE, stderr = FALSE)
    if (exit_code != 0L || !file.exists(tmp_out)) {
        md_text <- .latex_to_markdown_fallback(result_lines)
    } else {
        md_text <- paste(readLines(tmp_out, warn = FALSE), collapse = "\n")
    }

    ## 6. Assemble markdown
    if (!is.null(title) && nchar(title) > 0) {
        md_text <- paste0("# ", title, "\n\n", md_text)
    }

    ## 7. Restore code chunks as fenced R blocks
    for (i in seq_along(chunks)) {
        placeholder <- sprintf("RNWCODECHUNK%04dPLACEHOLDER", i)
        replacement <- paste0("\n```r\n", chunks[[i]], "\n```\n")
        md_text <- sub(placeholder, replacement, md_text, fixed = TRUE)
    }

    md_text
}

#' Simple regex-based LaTeX to markdown conversion (fallback when pandoc fails)
.latex_to_markdown_fallback <- function(tex_lines) {
    txt <- paste(tex_lines, collapse = "\n")
    ## Convert sectioning commands
    txt <- gsub("\\\\section\\*?\\{([^}]+)\\}", "\n## \\1\n", txt)
    txt <- gsub("\\\\subsection\\*?\\{([^}]+)\\}", "\n### \\1\n", txt)
    txt <- gsub("\\\\subsubsection\\*?\\{([^}]+)\\}", "\n#### \\1\n", txt)
    ## Convert common formatting
    txt <- gsub("\\\\texttt\\{([^}]+)\\}", "`\\1`", txt)
    txt <- gsub("\\\\textbf\\{([^}]+)\\}", "**\\1**", txt)
    txt <- gsub("\\\\emph\\{([^}]+)\\}", "*\\1*", txt)
    txt <- gsub("\\\\textit\\{([^}]+)\\}", "*\\1*", txt)
    ## Convert links
    txt <- gsub("\\\\url\\{([^}]+)\\}", "\\1", txt)
    txt <- gsub("\\\\href\\{([^}]+)\\}\\{([^}]+)\\}", "[\\2](\\1)", txt)
    ## Restore verbatim placeholders (strip the \begin/\end wrapper)
    txt <- gsub("\\\\begin\\{verbatim\\}(RNWCODECHUNK\\d+PLACEHOLDER)\\\\end\\{verbatim\\}",
                "\\1", txt)
    ## Convert itemize/enumerate
    txt <- gsub("\\\\item\\b", "- ", txt)
    ## Remove environment markers
    txt <- gsub("\\\\begin\\{[^}]+\\}(\\[[^]]*\\])?", "", txt)
    txt <- gsub("\\\\end\\{[^}]+\\}", "", txt)
    ## Strip remaining single-arg commands: \cmd{content} → content
    txt <- gsub("\\\\[a-zA-Z]+\\*?\\{([^}]*)\\}", "\\1", txt)
    ## Strip no-arg commands
    txt <- gsub("\\\\[a-zA-Z]+\\b", "", txt)
    ## Clean up stray braces and backslashes
    txt <- gsub("[{}]", "", txt)
    txt <- gsub("\\\\", "", txt)
    ## Collapse excessive blank lines
    txt <- gsub("\n{3,}", "\n\n", txt)
    trimws(txt)
}

#' Best-effort name extraction from an R expression line
extract_r_name <- function(expr_line, fallback_index) {
    # name <- ... or name = ...
    m <- regmatches(expr_line, regexpr(
        "^\\s*\"?([a-zA-Z._][a-zA-Z0-9._<>\\[\\]%]*)\"?\\s*(<-|=)\\s",
        expr_line, perl = TRUE
    ))
    if (length(m) == 1 && nchar(m) > 0) {
        return(trimws(sub("\\s*(<-|=)\\s*$", "", m)))
    }
    # setMethod("name", ...), setGeneric("name", ...), setClass("name", ...)
    m <- regmatches(expr_line, regexpr(
        "(setMethod|setGeneric|setClass|setReplaceMethod|setValidity)\\(\\s*[\"']([^\"']+)[\"']",
        expr_line, perl = TRUE
    ))
    if (length(m) == 1 && nchar(m) > 0) {
        nm <- sub(".*[\"']([^\"']+)[\"'].*", "\\1", m)
        prefix <- sub("\\(.*", "", m)
        return(paste0(prefix, ": ", nm))
    }
    paste0("expression_", fallback_index)
}

