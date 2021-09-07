##' Check that the issue labels in the GitHub repository that holds review match
##' our expectations.
##'
##' This function only checks labels that start with a number followed with a
##' slash (e.g., "1/editor-checks"). Other labels are ignored.
##'
##' @title Validate Review Labels.
##' @param owner Name of the owner of the repository.
##' @param repo Name of the repository.
##' @param expected Vector of names of expected labels in the repository.
##' @return TRUE if the names of the labels match the expected names.
##' @export
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
    purrr::map_chr("name")

  review_lbls <- grep("^[0-9]/", review_lbls, value = TRUE)

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
