validate_review_labels <- function(owner = "carpentries-lab",
                                   repo = "reviews",
                                   expected = c(
                                     "1/editor-checks",
                                     "2/seeking-reviewers",
                                     "3/reviewer(s)-assigned",
                                     "4/review(s)-in-awaiting-changes",
                                     "5/awaiting-reviewer(s)-response",
                                     "6/approved"
                                   )) {

  lbls <- gh::gh(
    "GET /repos/:owner/:repo/labels",
    owner = owner,
    repo = repo
  )

  review_lbls <- lbls %>%
    purrr::map_chr("name") %>%
    grep("^[0-9]/", ., value = TRUE)

  check <- all(
    review_lbls %in% expected &
      expected %in% review_lbls
  )

  if (!check) {
    stop("The review labels on GitHub have changed: \n",
      "on GitHub: ", paste(review_lbls, collapse = ", "), "\n",
      "here: ", paste(expected, collapse = ", ")
    )
  }

  check
}


## - issues with labels that start with number/ (like 2/) means that review is
## in progress; except for 6/approved that means review process is complete

## - we create 10 more "unknown" badges so images are ready at time of
## submission (we don't have to wait for daily build to happen before it's
## available)

get_all_review_repo_issues_raw <- function(owner, repo) {

  current_res <- gh::gh(
    "GET /repos/:owner/:repo/issues",
    owner = owner,
    repo = repo,
    per_page = 100,
    state = "all"
  )

  res <- list()
  test <- TRUE
  i <- 1

  while (test) {
    message("Getting page: ", i, " for ", sQuote(paste(owner, repo, sep = "/")))
    res <- append(res, current_res)

    current_res <- tryCatch({
      gh::gh_next(current_res)
    },
    error = function(e) {
      test <<- FALSE
      NULL
    })
    i <- i +  1
  }

  res
}


get_all_review_repo_issues <- function(owner = "carpentries-lab",
                                       repo = "reviews") {
  get_all_review_repo_issues_raw(owner, repo) %>%
    purrr::map_dfr(
      function(.x) {
        list(
          issue = .x$number,
          labels = list(purrr::map_chr(.x$labels, ~ .$name))
        )
      }
    )
}

max_review_repo_number <- function(list_issues) {
  max(list_issues[["issue"]])
}

extract_review_issues <- function(list_issues) {
  max_issue_number <- max_review_repo_number(list_issues) + 1

  list_issues %>%
    dplyr::mutate(
      review_label = purrr::pmap_chr(., function(issue, labels, ...) {
        r <- grep("^[0-9]/", labels, value = TRUE)

        if (length(r) < 1) {
          return(NA_character_)
        }

        if (length(r) > 1) {
          stop(
            "more than one review label detected for issue: ", issue,
            call. = FALSE
          )
        }

        r
      })
    ) %>%
  dplyr::filter(!is.na(review_label)) %>%
  dplyr::mutate(
    review_badge = dplyr::case_when(
      .data$review_label == "6/approved" ~ "peer-reviewed",
      TRUE ~ "pending"
    )
  ) %>%
  dplyr::bind_rows(
    tibble::tibble(
      issue = max_issue_number:(max_issue_number + 9),
      labels = vector("list", 10),
      review_label = rep(NA_character_, 10),
      review_badge = "unknown"
    )
  )
}

copy_badge <- function(issue, badge, folder = "_badges") {
  img_path <- system.file("badges", package = "clbadges")
  img <- file.path(
    img_path,
    paste0("carpentries-lab-", badge, ".svg")
  )

  if (!file.exists(img)) {
    stop(sQuote(img), "does not exist.", call. = FALSE)
  }

  out <- file.path(folder, paste0(issue, "_status.svg"))
  res <- file.copy(img, out, overwrite = TRUE)

  if (!res) {
    stop("issue when generating file: ", sQuote(out), call. = FALSE)
  }
  res
}

generate_review_badges <- function(list_issues, folder = "_badges") {

  if (!dir.exists(folder)) {
    dir.create(folder)
  }

  list_issues %>%
    purrr::pwalk(
      function(issue, review_badge, ...) {
        copy_badge(issue, review_badge, folder = folder)
      }
    )


}
