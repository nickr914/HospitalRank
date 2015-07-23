#best.R Hospital  Nick R

source("rankall.R")

best <- function(state, outcome) {
    hospname <- rankall(outcome,,,state)[1]
    print(as.vector(t(hospname)))
}

