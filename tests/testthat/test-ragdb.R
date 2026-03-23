test_that("ragdb_path returns NULL with no cache and check = TRUE", {
    withr::local_options(BiocAgentRAGDB.path = NULL)
    withr::local_envvar(BIOCAGENT_RAGDB_PATH = NA)
    # With a fresh temp cache dir, no entry exists
    tmp_cache <- tempfile("bfc_test_")
    dir.create(tmp_cache)
    on.exit(unlink(tmp_cache, recursive = TRUE))
    # Mock .ragdb_cache to use our temp dir
    mockery::stub(ragdb_path, ".ragdb_cache", function() {
        BiocFileCache::BiocFileCache(tmp_cache, ask = FALSE)
    })
    path <- ragdb_path(check = TRUE)
    expect_null(path)
})

test_that("ragdb_path respects option", {
    withr::local_options(BiocAgentRAGDB.path = "/tmp/test_ragdb.duckdb")
    path <- ragdb_path(check = FALSE)
    expect_equal(path, "/tmp/test_ragdb.duckdb")
})

test_that("ragdb_path respects option with check = TRUE", {
    withr::local_options(BiocAgentRAGDB.path = "/tmp/nonexistent_ragdb.duckdb")
    expect_null(ragdb_path(check = TRUE))
})

test_that("ragdb_path respects environment variable", {
    withr::local_options(BiocAgentRAGDB.path = NULL)
    withr::local_envvar(BIOCAGENT_RAGDB_PATH = "/tmp/env_ragdb.duckdb")
    expect_equal(ragdb_path(check = FALSE), "/tmp/env_ragdb.duckdb")
})

test_that("ragdb_path finds entry in BiocFileCache", {
    withr::local_options(BiocAgentRAGDB.path = NULL)
    withr::local_envvar(BIOCAGENT_RAGDB_PATH = NA)

    tmp_cache <- tempfile("bfc_test_")
    dir.create(tmp_cache)
    on.exit(unlink(tmp_cache, recursive = TRUE))

    # Create a fake DB file and add to BFC
    fake_db <- tempfile(fileext = ".duckdb")
    writeLines("placeholder", fake_db)
    bfc <- BiocFileCache::BiocFileCache(tmp_cache, ask = FALSE)
    BiocFileCache::bfcadd(bfc, "BiocAgentRAGDB", fpath = fake_db, action = "copy")

    mockery::stub(ragdb_path, ".ragdb_cache", function() bfc)
    path <- ragdb_path(check = TRUE)
    expect_type(path, "character")
    expect_true(file.exists(path))
})

test_that("ragdb_info handles missing database gracefully", {
    withr::local_options(BiocAgentRAGDB.path = NULL)
    withr::local_envvar(BIOCAGENT_RAGDB_PATH = NA)
    tmp_cache <- tempfile("bfc_test_")
    dir.create(tmp_cache)
    on.exit(unlink(tmp_cache, recursive = TRUE))
    mockery::stub(ragdb_info, "ragdb_path", function(check) NULL)
    expect_message(ragdb_info(), "not cached")
})

test_that("ragdb_fetch caches via BiocFileCache and returns early on repeat", {
    tmp_cache <- tempfile("bfc_test_")
    dir.create(tmp_cache)
    on.exit(unlink(tmp_cache, recursive = TRUE))

    fake_db <- tempfile(fileext = ".duckdb")
    writeLines("placeholder", fake_db)

    bfc <- BiocFileCache::BiocFileCache(tmp_cache, ask = FALSE)

    mockery::stub(ragdb_fetch, ".ragdb_cache", function() bfc)
    mockery::stub(ragdb_fetch, ".fetch_from_experimenthub", function() fake_db)

    # First call — downloads (mocked) and caches
    expect_message(result <- ragdb_fetch(force = FALSE), "cached")
    expect_type(result, "character")
    expect_true(file.exists(result))

    # Second call — returns cached entry
    expect_message(result2 <- ragdb_fetch(force = FALSE), "already cached")
    expect_true(file.exists(result2))
})

test_that("ragdb_info shows info for existing database", {
    tmp_cache <- tempfile("bfc_test_")
    dir.create(tmp_cache)
    on.exit(unlink(tmp_cache, recursive = TRUE))

    fake_db <- tempfile(fileext = ".duckdb")
    writeLines("placeholder", fake_db)
    bfc <- BiocFileCache::BiocFileCache(tmp_cache, ask = FALSE)
    cached <- BiocFileCache::bfcadd(
        bfc, "BiocAgentRAGDB", fpath = fake_db, action = "copy"
    )

    withr::local_options(BiocAgentRAGDB.path = as.character(cached))
    expect_message(info <- ragdb_info(), "BiocAgentRAGDB")
    expect_type(info, "list")
    expect_named(info, c("path", "size_mb", "modified"))
})
