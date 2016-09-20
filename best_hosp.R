## Read in the data

outcome_dat<-read.csv("outcome-of-care-measures.csv", header = TRUE)

## Select only the key variables from the original data.
outcome_dat_organized <- data.frame(as.character(outcome_dat$Hospital.Name),as.character(outcome_dat$State), as.double(outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), as.double(outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), as.double(outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))

# Rename the variables
names(outcome_dat_organized)<-c("hospital", "state", "heart attack", "heart failure", "pneumonia")

# Remove all the NA values.
outcome_dat_organized$`heart attack`[outcome_dat_organized$`heart attack` == "Not Available"] <- NA
outcome_dat_organized$`heart failure`[outcome_dat_organized$`heart failure` == "Not Available"] <- NA
outcome_dat_organized$pneumonia[outcome_dat_organized$pneumonia == "Not Available"] <- NA

outcome_dat_organized <- outcome_dat_organized[complete.cases(outcome_dat_organized),]
outcome_dat_organized <- droplevels(outcome_dat_organized[complete.cases(outcome_dat_organized),])

## Create a function to search the lowest mortality rate based on state, and the type of death.

## Note, states must be entered as a two letter CAPITALIZED abbreviation surrounded by quotes
## e.g., "CA", "WA", "MI"

## Options for the type of death are "heart attack", "heart failure", and "pneumonia". These must
## be entered VERBATIM in quotes.

best <- function(location, outcome) {
        
        states <- outcome_dat_organized$state
        
        outcomes <- colnames(outcome_dat_organized)
        
        y <- match(location, states, nomatch = as.numeric(0))
        
        z <- match(outcome, outcomes, nomatch = as.numeric(0))
        
        if (z == 0 && y == 0) {
                
        print("Invalid State and Outcome. Please enter valid values.") 
                
        } else if (z == 0) {
          
          print("Invalid outcome. Please enter a valid outcome")
          
        } else if (y == 0) {
          
          print("Invalid State. Please enter valid State.")
        
        } else {
        
        
        state_subset <- subset(outcome_dat_organized, location == state)
        
        names(state_subset)<- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        state_subset$hospital <- as.character(state_subset$hospital)
        
        outcome <- tolower(outcome)
        
        min <- with(state_subset, state_subset[order(state_subset[[outcome]], state_subset$hospital), ])

        print(min[1,1]) 
       } 
}     
   

## Test the function a few times.

best("BB", "heart failure")

best("OH", "pneumonia")

best("ID", "heart attack")

best("AB", "cancer")
