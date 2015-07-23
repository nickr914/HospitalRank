source("rankall.R")
rankhospital <- function(state, outcome, rank = "best") {
  hospname <- rankall(outcome,rank,,state)[1]
  print(as.vector(t(hospname)))
}
