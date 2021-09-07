# Carpentries Lab Review Labels Generator


## Impelmentation Design Notes

- The implementation of this package is inspired by rOpenSci's
  [badges](https://github.com/ropensci-org/badges) repository.
- Issues tagged with labels that start with `number/` (like
  `2/seeking-reviewers`) means that review is in progress; except for
  `6/approved` that means review process is complete.
- Currently there is nothing done for reviews marked with the label `pulled`.
- We create 10 more "unknown" badges so images are ready at time of submission
 (we don't have to wait for daily build to happen before it's available)


