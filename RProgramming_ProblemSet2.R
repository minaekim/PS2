# R Programming
# Problem Set 2: Functions 
# Author: Jonas Markgraf
# Contributor: Hyunjoo Oh
# =========================


### Benford's law
## 1) Calculating violations
## ========================

# generate random sample vector ------------------
prop_vote <- sample(1:1000000, size=100000) 


# create function for "m" and "d" statistic -------------------------
violations <- function(x, m = TRUE, d = TRUE) { # by default, give "m" and "d" statistic
  first.digit <- substr(as.character(x), start = 1, stop = 1) # extract first digit of prop_vote vector
  first.digit <- as.integer(first.digit)
  Xi <- table(first.digit)/length(prop_vote) # generate observed proportional frequency vector
  if(m == T & d == F) {
    m = max(Xi - log10(1 + (1/c(1:9)))) # calculating "m" statistic
    return(list("Leemis' m" = m, distribtion = Xi))  # output1: if m == T & d == F, we only see "m" statistic
  }
  if(d == T & m == F) {
    d = sqrt(sum(Xi - log10(1 + (1/c(1:9)))^2)) # calculating "d" statistic
    return(list("Cho-Gains' d" = d, distribtion = Xi)) # output2: if m == F & d == T, we only see "d" statistic
  }
  if(d == T & m == T) {
    m = max(Xi - log10(1 + (1/c(1:9)))) # calculating "m" statistic
    d = sqrt(sum(Xi - log10(1 + (1/c(1:9)))^2)) # calculating "d" statistic
    return(list("Leemis' m" = m, "Cho-Gains' d" = d, distribtion = Xi)) # output3: if m == T & d == T, we see both statisitics
  }
}

# testing function
violations(prop_vote)
violations(prop_vote, m = F)
violations(prop_vote, d = F, m = F)



## 2) Critical Values
## ===================

# create function for critical values ------------------

print.benfords <- function(x, m = TRUE, d = TRUE) { # by default, give "m" and "d" statistic
  first.digit <- substr(as.character(x), start = 1, stop = 1) # extract first digit of prop_vote vector
  first.digit <- as.integer(first.digit)
  Xi <- table(first.digit)/length(x) # generate observed proportional frequency vector
  significance <- "* 10%; ** 5%; *** 1%" # vecto explaining asterisks

  if(m == T & d == F) {
    m = max(Xi - log10(1 + (1/c(1:9)))) # calculating "m" statistic
    m <- if(m >= 1.212) { # add asterisks for critical values of "m" statistic
      paste0(m, "***")
    } else if(m >= .967) {
      paste0(m, "**")
    } else if(m >= .851) {
      paste0(m, "*")
    } else {
      m
    }
    values1 <- c(m, significance)
    rnames1 <- c("Leemis m", "Significance:")
    return(data.frame(values1, row.names = rnames1))  # output1: if m == T & d == F, we only see "m" statistic
    
  }
  if(d == T & m == F) {
    d = sqrt(sum(Xi - log10(1 + (1/c(1:9)))^2)) # calculating "d" statistic
    d <- if(d >= 1.569) { # add asterisks for critical values of "d" statistic
      paste0(d, "***")
    } else if(d >= 1.33) {
      paste0(d, "**")
    } else if(d >= 1.212) {
      paste0(d, "*")
    } else {
      d
    }
    values2 <- c(d, significance)
    rnames2 <- c("Cho-Gains d", "Significance:")
    return(data.frame(values2, row.names = rnames2)) # output2: if m == F & d == T, we only see "d" statistic
  }
  
  if(d == T & m == T) {
    m = max(Xi - log10(1 + (1/c(1:9)))) # calculating "m" statistic
    m <- if(m >= 1.212) { # add asterisks for critical values of "m" statistic
      paste0(m, "***")
    } else if(m >= .967) {
      paste0(m, "**")
    } else if(m >= .851) {
      paste0(m, "*")
    } else {
      m
    }
    
    d = sqrt(sum(Xi - log10(1 + (1/c(1:9)))^2)) # calculating "d" statistic
    d <- if(d >= 1.569) { # add asterisks for criticl values of "d" statistic
      paste0(d, "***")
    } else if(d >= 1.33) {
      paste0(d, "**")
    } else if(d >= 1.212) {
      paste0(d, "*")
    } else {
      d
    }
    values3 <- c(m, d, significance)
    rnames3 <- c("Leemis m", "Cho-Gains d", "Significance:")
    return(data.frame(values3, row.names = rnames3)) # output3: if m == T & d == T, we see both statisitics
  }
}

# testing function
print.benfords(prop_vote)
print.benfords(prop_vote, d = F)

## Create function that creates CSV file containing table -------------

<<<<<<< HEAD
getwd()
setwd("/Users/hyunjoooh/Dropbox/2017_Spring_Washu/Stat_Prog/ProblemSet")
export.benfords <- function(x){
  benfords.table <- data.frame(print.benfords(x))
  print(benfords.table)
  sink(file = "benfords_output.csv")
=======

export.benfords2 <- function(x, name = "benfords_output2.csv") {
  write.csv(print.benfords(x), paste0("~/Dropbox/Hertie School/(4) Applied Statistical Programming (WUSTL)/Repositories/PS2/", name))
} # this works...

export.benfords2(prop_vote)



export.benfords <- function(x, name = "benfords_output.csv") {
  sink(paste0("~/Dropbox/Hertie School/(4) Applied Statistical Programming (WUSTL)/Repositories/PS2/", name))
  print.benfords(x)
  sink() # prints empty csv file...
>>>>>>> origin/master
}
export.benfords(prop_vote)

export.benfords(prop_vote)
