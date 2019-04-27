library(stringr)

cleanProfileOslaua <- function (la) {
  la <- as.character(la)
  la[la == 'The Vale of Glamorgan'] <- 'Vale of Glamorgan'
  la[la == 'Rhondda, Cynon, Taff'] <- 'Rhondda Cynon Taf'
  la[la == 'Edinburgh, City of'] <- 'City of Edinburgh'
  la[la == 'None'] <- NA
  la <- sub('&', 'and', la, fixed = TRUE)
  factor(la)
}

# origin is from https://www.ibm.com/support/knowledgecenter/en/SSLVMB_24.0.0/spss/base/idh_idd_dtwz_learn.html
cleanTimestamp <- function (timestamp) {
  as.POSIXct(timestamp, origin = '1582-10-14', tz = 'Europe/London')
}

cleanApproveDisapprove <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Strongly disapprove'] <- 0
  cleanX[x == 'Disapprove'] <- 0.25
  cleanX[x == 'Neither approve nor disapprove'] <- 0.5
  cleanX[x == 'Approve'] <- 0.75
  cleanX[x == 'Strongly approve'] <- 1.0
  cleanX
}

cleanTurnout <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Very unlikely that I would vote'] <- 0
  cleanX[x == 'Fairly unlikely'] <- 0.25
  cleanX[x == 'Neither likely nor unlikely'] <- 0.5
  cleanX[x == 'Fairly likely'] <- 0.75
  cleanX[x == 'Very likely that I would vote'] <- 1.0
  cleanX
}

cleanRefVoteBoolean <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Stay/remain in the EU'] <- TRUE
  cleanX[x == 'Leave the EU'] <- FALSE
  # Treating 'I would/will not vote' or 'Don't know' as NA
  cleanX
}

cleanRefVote <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Stay/remain in the EU'] <- 1.0
  cleanX[x == 'Leave the EU'] <- 0
  # Treating 'I would/will not vote' or 'Don't know' as NA
  cleanX
}

cleanRefInterest <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Not at all interested'] <- 0
  cleanX[x == 'Not very interested'] <- 0.33
  cleanX[x == 'Somewhat interested'] <- 0.66
  cleanX[x == 'Very interested'] <- 1.0
  cleanX
}

cleanAgreement <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Strongly disagree'] <- 0
  cleanX[x == 'Disagree'] <- 0.25
  cleanX[x == 'Neither agree nor disagree'] <- 0.5
  cleanX[x == 'Agree'] <- 0.75
  cleanX[x == 'Strongly agree'] <- 1.0
  cleanX 
}

cleanCrime <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Getting a lot lower'] <- 0
  cleanX[x == 'Getting a little lower'] <- 0.25
  cleanX[x == 'Staying about the same'] <- 0.5
  cleanX[x == 'Getting a little higher'] <- 0.75
  cleanX[x == 'Getting a lot higher'] <- 1.0
  cleanX 
}

cleanTenPoint <- function (x, lo, hi) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == lo] <- 0
  for (i in 1:9) {
    cleanX[x == i] <- i/10
  }
  cleanX[x == hi] <- 1.0
  cleanX 
}

cleanSevenPoint <- function (x, lo, hi) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == lo] <- 0
  for (i in 2:6) {
    cleanX[x == i] <- (i - 1)/6
  }
  cleanX[x == hi] <- 1.0
  cleanX 
}

cleanLessMore <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'A lot less'] <- 0
  cleanX[x == 'Somewhat less'] <- 0.25
  cleanX[x == 'About the same'] <- 0.5
  cleanX[x == 'Somewhat more'] <- 0.75
  cleanX[x == 'A lot more'] <- 1.0
  cleanX  
}

cleanSatisfaction <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Very dissatisfied'] <- 0
  cleanX[x == 'A little dissatisfied'] <- 1/3
  cleanX[x == 'Fairly satisfied'] <- 2/3
  cleanX[x == 'Very satisfied'] <- 1.0
  cleanX  
}

cleanYesNo <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Yes'] <- TRUE
  cleanX[x == 'No'] <- FALSE
  cleanX
}

cleanVoted <- function (x) {
  cleanX <- rep_len(NA, length(x))
  cleanX[x == 'Yes, voted'] <- TRUE
  cleanX[x == 'No, did not vote'] <- FALSE
  # Leaving 'Don't know' as NA
  cleanX
}

cleanIncome <- function (x) {
  lower <- rep_len(NA, length(x))
  upper <- rep_len(NA, length(x))
  
  rangeRx <- '.+£(\\d+,\\d+) to .+£(\\d+,\\d+) per year'
  isRange <- grepl(rangeRx, x)
  ranges <- str_match(x[isRange], rangeRx)
  lower[isRange] <- strtoi(sub(',', '', ranges[,2]))
  upper[isRange] <- strtoi(sub(',', '', ranges[,3]))
  
  underRx <- 'under .+£(\\d+,\\d+) per year'
  isUnder <- grepl(underRx, x)
  unders <- str_match(x[isUnder], underRx)
  lower[isUnder] <- 0
  upper[isUnder] <- strtoi(sub(',', '', unders[,2]))
  
  overRx <- '.+£(\\d+,\\d+) and over'
  isOver <- grepl(overRx, x)
  overs <- str_match(x[isOver], overRx)
  lower[isOver] <- strtoi(sub(',', '', overs[,2]))
  upper[isOver] <- Inf
  
  isNa <- grepl("Prefer not to answer|Don't know", x) | is.na(x)
  stopifnot(sum(isRange | isUnder | isOver | isNa) == length(x))
  
  data.frame(lower, upper)
}
