#rankall Nick R

library(dplyr)
rankall <- function(outcome, rank ="best", file = "outcome-of-care-measures.csv", state = "all") {
  hosp <- tbl_df(read.csv(file, colClasses = "character")[,c(2,7,11,17,23)])
  names(hosp)[c(3,4,5)] <- c("heart attack","heart failure","pneumonia")
  if (outcome %in% c("heart attack","heart failure","pneumonia")) {
    colnum <- grep(outcome,names(hosp),ignore.case = TRUE)
  } else {
    stop("invalid outcome")
  }
  names(hosp)[colnum] <- "Death"

  
  hosp[c(3,4,5)] <- suppressWarnings(as.numeric(unlist(hosp[, c(3,4,5)])))

    if(rank == "best" | rank == "worst") { 
    rank1 <- 1
  } else {
    rank1 <- as.numeric(rank)
  }
  if (rank != "worst" | is.numeric(rank)) {
    if (state != "all") { 

      if (is.na(match(state, hosp$State))) { 
        stop("invalid state")	
      } 
      hosp <- (hosp %>% group_by(State) %>% filter(State == state) %>% select(Hospital.Name,State,Death) %>%  arrange(Death))
      if(rank1 > (nrow(hosp))) {
        hosp <- slice(hosp,nrow(hosp))
      } else {
        hosp <- slice(hosp,rank1)
      }
    } else {
      print ("b")
      hosp <- (hosp %>% group_by(State) %>% select(Hospital.Name,State,Death) %>%  arrange(Death))
                if(rank1 > (nrow(hosp))) {
                 hosp <- slice(hosp,nrow(hosp))
               } else {
                 hosp <- slice(hosp,rank1)
               }
    }
  }  else {
    if (state != "all") {
      if (is.na(match(state, hosp$State))) { 
        stop("invalid state")	
      } 
      print("c")
      hosp <- (hosp %>% group_by(State) %>% filter(State == state) %>% select(Hospital.Name,State,Death) %>%  arrange(desc(Death)) %>% slice(rank1))
    } else {
      print("d")
      hosp <- (hosp %>% group_by(State) %>% select(Hospital.Name,State,Death) %>%  arrange(desc(Death)) %>% slice(rank1))
    } 
  }
  View(hosp)
  data.frame(hospital = hosp$Hospital.Name, state = hosp$State)
}
