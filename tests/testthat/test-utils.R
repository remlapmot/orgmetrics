test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("gh org repos", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true") # for n_per_page --> 2
    org <- "ropensci"
    repo_names <- httptest2::with_mock_dir ("utils_gh_org", {
        list_gh_org_repos (org)
    })

    expect_type (repo_names, "character")
    expect_length (repo_names, n_per_page_in_tests (100))
    expect_true (any (grepl ("\\/", repo_names))) # bare names only
})

test_that ("pkgs are r", {

    pkgs <- "ropensci-review-tools/orgmetrics"
    chk <- httptest2::with_mock_dir ("pkgs_are_r", {
        pkgs_are_r (pkgs)
    })
    expect_length (chk, length (pkgs))
    expect_type (chk, "logical")
    expect_true (all (chk))
    expect_named (chk)
})

test_that ("write pkgs_json", {

    d <- fs::path (fs::path_temp (), "tmpjson")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    repos <- c ("orgmetrics", "repometrics")
    repos <- vapply (repos, function (repo) {
        tmp <- generate_test_pkg ()
        path <- fs::dir_copy (
            tmp,
            fs::path (d, "ropensci-review-tools", repo),
            overwrite = TRUE
        )
        fs::path_rel (path, start = d)
    }, character (1L))
    pkgs <- data.frame (
        repo_path = fs::path (d, repos),
        repos = repos,
        row.names = NULL
    )

    f <- write_pkgs_json (pkgs, dir = d)
    expect_s3_class (f, "fs_path")
    expect_true (fs::file_exists (f))
    expect_equal (basename (f), "packages.json")

    fs::dir_delete (d)
})

test_that ("branch_spec_type classifies branch field values", {

    expect_equal (branch_spec_type (NULL), "none")
    expect_equal (branch_spec_type (NA_character_), "none")
    expect_equal (branch_spec_type ("main"), "branch")
    expect_equal (branch_spec_type ("feature/foo"), "branch")
    expect_equal (branch_spec_type ("v1.2.3"), "tag")
    expect_equal (branch_spec_type ("*release"), "release")
    sha40 <- paste0 (rep ("a", 40), collapse = "")
    expect_equal (branch_spec_type (sha40), "sha")
    # 39 chars is not a full SHA – treated as a branch name
    sha39 <- paste0 (rep ("a", 39), collapse = "")
    expect_equal (branch_spec_type (sha39), "branch")
})

skip_if (!test_all)

# No mocking here, just actual cloning on 1st two rOpenSci repos
test_that ("clone gh org repos", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true") # n_per_page = 2

    d <- fs::path (fs::path_temp (), "orgrepos")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    repos <- c ("orgmetrics", "repometrics")
    repos <- vapply (repos, function (repo) {
        tmp <- generate_test_pkg ()
        path <- fs::dir_copy (
            tmp,
            fs::path (d, "ropensci-review-tools", repo),
            overwrite = TRUE
        )
        fs::path_rel (path, start = d)
    }, character (1L))
    pkgs <- data.frame (
        repo_path = fs::path (d, repos),
        repos = repos,
        row.names = NULL
    )

    f <- write_pkgs_json (pkgs, dir = d)
    fs::dir_delete (pkgs$repo_path)

    clone_gh_org_repos (pkgs_json = f)

    f <- fs::dir_ls (d, type = "file")
    expect_length (f, 1L)
    expect_equal (basename (f), "packages.json")
    f <- fs::dir_ls (d, type = "directory")
    expect_length (f, 1L)
    expect_equal (basename (f), "ropensci-review-tools")

    d_org <- f
    f <- fs::dir_ls (d_org, type = "directory")
    expect_length (f, 2L)

    fs::dir_delete (d)
})

test_that ("clone gh org repos reads branch field from packages.json", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    d <- fs::path (fs::path_temp (), "orgrepos_branch")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    pkgs <- data.frame (
        path = "ropensci-review-tools/orgmetrics",
        orgrepo = "ropensci-review-tools/orgmetrics",
        root = "ropensci-review-tools/orgmetrics",
        is_r_pkg = TRUE,
        branch = "main"
    )
    pkgs_json <- fs::path (d, "packages.json")
    jsonlite::write_json (pkgs, pkgs_json, pretty = TRUE)

    clone_gh_org_repos (pkgs_json = pkgs_json)

    repo_path <- fs::path (d, "ropensci-review-tools", "orgmetrics")
    expect_true (fs::dir_exists (repo_path))

    branch <- withr::with_dir (repo_path, gert::git_branch ())
    expect_equal (branch, "main")

    fs::dir_delete (d)
})

test_that ("clone gh org repos handles *release branch field", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    d <- fs::path (fs::path_temp (), "orgrepos_release")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    pkgs <- data.frame (
        path = "ropensci-review-tools/orgmetrics",
        orgrepo = "ropensci-review-tools/orgmetrics",
        root = "ropensci-review-tools/orgmetrics",
        is_r_pkg = TRUE,
        branch = "*release"
    )
    pkgs_json <- fs::path (d, "packages.json")
    jsonlite::write_json (pkgs, pkgs_json, pretty = TRUE)

    clone_gh_org_repos (pkgs_json = pkgs_json)

    repo_path <- fs::path (d, "ropensci-review-tools", "orgmetrics")
    expect_true (fs::dir_exists (repo_path))

    fs::dir_delete (d)
})

test_that ("clone gh org repos handles commit SHA branch field", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    d <- fs::path (fs::path_temp (), "orgrepos_sha")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    # Create a local bare repo to use as the remote
    bare_path <- fs::path (d, "bare.git")
    gert::git_init (bare_path, bare = TRUE)

    # Clone the bare repo, make a commit, push, capture the SHA
    work_path <- fs::path (d, "work")
    gert::git_clone (paste0 ("file://", bare_path), path = work_path)
    writeLines ("hello", fs::path (work_path, "hello.txt"))
    withr::with_dir (work_path, {
        gert::git_add ("hello.txt")
        gert::git_commit ("initial commit", author = "Test User <test@example.com>")
        gert::git_push ()
    })
    sha <- withr::with_dir (work_path, gert::git_log (max = 1L)$commit)

    pkgs <- data.frame (
        path = "myorg/myrepo",
        orgrepo = "myorg/myrepo",
        root = "myorg/myrepo",
        is_r_pkg = TRUE,
        branch = sha
    )
    pkgs_json <- fs::path (d, "packages.json")
    jsonlite::write_json (pkgs, pkgs_json, pretty = TRUE)

    # Patch url construction to use local file:// path
    local_url <- paste0 ("file://", bare_path)
    clone_target <- fs::path (d, "myorg", "myrepo")
    fs::dir_create (fs::path (d, "myorg"))
    gert::git_clone (local_url, path = clone_target)

    # Now test updating an existing repo: fetch + SHA checkout
    withr::with_dir (clone_target, {
        gert::git_fetch (verbose = FALSE)
        checkout_ref (sha)
        head_sha <- gert::git_log (max = 1L)$commit
        expect_equal (head_sha, sha)
    })

    fs::dir_delete (d)
})

test_that ("clone gh org repos returns subdir path when subdir field is set", {

    Sys.setenv ("ORGMETRICS_TESTS" = "true")
    Sys.setenv ("REPOMETRICS_TESTS" = "true")

    d <- fs::path (fs::path_temp (), "orgrepos_subdir")
    expect_false (fs::dir_exists (d))
    fs::dir_create (d)

    # Create a local bare repo containing a package in a 'pkg' subdirectory
    bare_path <- fs::path (d, "bare.git")
    gert::git_init (bare_path, bare = TRUE)

    work_path <- fs::path (d, "work")
    gert::git_clone (paste0 ("file://", bare_path), path = work_path)
    fs::dir_create (fs::path (work_path, "pkg"))
    writeLines ("hello", fs::path (work_path, "pkg", "DESCRIPTION"))
    withr::with_dir (work_path, {
        gert::git_add ("pkg/DESCRIPTION")
        gert::git_commit ("initial commit", author = "Test User <test@example.com>")
        gert::git_push ()
    })

    # Manually clone (simulating what clone_gh_org_repos would do via GitHub)
    clone_target <- fs::path (d, "myorg", "myrepo")
    fs::dir_create (fs::path (d, "myorg"))
    gert::git_clone (paste0 ("file://", bare_path), path = clone_target)

    # Test clone_or_update_repo directly: should return path/subdir
    pkg_path <- clone_or_update_repo (
        url = paste0 ("file://", bare_path),
        path = clone_target,
        dir_org = fs::path (d, "myorg"),
        branch_field = NULL,
        orgrepo = "myorg/myrepo",
        subdir = "pkg"
    )

    expect_equal (pkg_path, fs::path (clone_target, "pkg"))
    expect_true (fs::dir_exists (pkg_path))

    fs::dir_delete (d)
})
