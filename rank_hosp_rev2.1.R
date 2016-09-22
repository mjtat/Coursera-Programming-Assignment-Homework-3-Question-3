## Read in the data

outcome_dat <-
    read.csv("outcome-of-care-measures.csv",
             header = TRUE,
             stringsAsFactors = FALSE)

## Select only the key variables from the original data.
outcome_dat_organized <-
    data.frame(
        as.character(outcome_dat$Hospital.Name),
        as.character(outcome_dat$State),
        outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
        outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
        outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
        stringsAsFactors = FALSE
    )

# Rename the variables
names(outcome_dat_organized) <-
    c("hospital",
      "state",
      "heart attack",
      "heart failure",
      "pneumonia")


#Remove all the NA values.
outcome_dat_organized$`heart attack`[outcome_dat_organized$`heart attack` == "Not Available"] <-
    NA
outcome_dat_organized$`heart failure`[outcome_dat_organized$`heart failure` == "Not Available"] <-
    NA
outcome_dat_organized$pneumonia[outcome_dat_organized$pneumonia == "Not Available"] <-
    NA

outcome_dat_organized <-
    outcome_dat_organized[complete.cases(outcome_dat_organized), ]


### Create the function.
rank_hosp <- function(location, outcome, num) {
    state_subset <- subset(outcome_dat_organized, location == state)
    
    len <- as.double(length(state_subset$hospital))
    

    if (is.character(num) == TRUE){
        num <- tolower(num)
    } else {
        num <- as.numeric(num)
    }
        
    if (num == "best") {
        num <- 1
        
    } else if (num == "worst") {
        num <- len 
    }
    
    
    location <- toupper(location)
    outcome <- tolower(outcome)
    
    state_list <- unique(outcome_dat_organized$state)
    outcome_list <- colnames(outcome_dat_organized)
    num_list <- c(1:9999)
    
    x <- (as.numeric(location %in% state_list))
    y <- (as.numeric(outcome %in% outcome_list))
    z <- (as.numeric(num %in% num_list))
    
    if (x == 0 && y == 0 && z == 0) {
        stop("Invalid state, outcome, and number. Please enter valid values.")
    } else if (x == 0 && y == 0) {
        stop("Invalid state and outcome. Please enter valid values.")
    } else if (x == 0) {
        stop("Invalid state. Please enter a valid value")
    } else if (y == 0) {
        stop("Invalid outcome. Please enter a valid outcome.")
    } else if (is.character(num) == TRUE)  {
            if(num != "worst" || num != "best") {
                stop("The number you entered is invalid. Please enter a valid number.") 
            }
    } else if (num > len) {
        stop("The number you entered is greater than the number of hospitals in that state. Enter a lower number.")
            } else if (z == 0) {
        stop("Invalid number. Please enter a valid number.")
    } else {
        
        state_subset$`heart attack` <-
            as.numeric(state_subset$`heart attack`)
        
        state_subset$`heart failure` <-
            as.numeric(state_subset$`heart failure`)
        
        state_subset$pneumonia <-
            as.numeric(state_subset$pneumonia)
        
        state_subset$rank_heartattack <-
            rank(state_subset$`heart attack`,
                 ties.method = "min")
        
        state_subset$rank_heartfailure <-
            rank(state_subset$`heart failure`,
                 ties.method = "min")
        
        state_subset$rank_pneumonia <-
            rank(state_subset$pneumonia,
                 ties.method = "min")
        
        if (outcome == "heart attack") {
            column_name <- c("rank_heartattack")
            
        } else if (outcome == "heart failure") {
            column_name <- c("rank_heartfailure")
            
        } else { 
            column_name <- c("rank_pneumonia")
        }
        
        rank_dat <-
            with(state_subset, state_subset[order(state_subset[[column_name]],
                                                  state_subset$hospital), ])
        
        if (is.numeric(num) == TRUE)
            print(rank_dat$hospital[num])
        
    }
        
}

## Test it a few times.

rank_hosp("TX", "heart failure", 4)
rank_hosp("MD", "heart attack", "worst")
rank_hosp("MN", "heart attack", 5000)
rank_hosp("AL", "heart attack", 5)
rank_hosp("AF", "cancer", 5)
rank_hosp("DE", "pneumonia", "best")
rank_hosp("OH", "pneumonia", "not always the best")
    