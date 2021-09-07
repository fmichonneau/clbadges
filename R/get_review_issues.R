##' @importFrom rlang .data

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

get_all_review_repo_issues <- function(owner, repo) {
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

##' Extract from the Carpentries Lab repository that tracks lesson submissions,
##' issues that are assigned review labels (i.e., that have labels that start
##' with a number followed with a / such as `2/reviewer(s)-assigned`).
##'
##' @title Extract Review Issues from the repository
##' @param owner Owner of the repository
##' @param repo Name of the repository
##' @return A two-column tibble with the issue number and the review label
##'   assigned.
##' @export
extract_review_issues <- function(owner = "carpentries-lab",
                                  repo = "reviews") {

  list_issues <- get_all_review_repo_issues(owner, repo)

  max_issue_number <- max_review_repo_number(list_issues) + 1

  review_label <- purrr::pmap_chr(list_issues, function(issue, labels, ...) {
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

  list_issues %>%
    dplyr::mutate(
      review_label = review_label
    ) %>%
  dplyr::filter(!is.na(.data$review_label)) %>%
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

##' Take the output of `extract_review_issues()` and generate SVG badges to
##' include in the README of the lesson repositories that are under review or
##' have been accepted in the Carpentries Lab.
##'
##' The images used to generate the badges are stored in the `inst/badges`
##' folder.
##'
##' @title Generate Review Badges
##' @param list_issues the output of `extract_review_issues()`
##' @param folder the folder where the generated badges will be stored
##' @return nothing, use for its side effects (generates the badges)
##' @export
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
