#############################################################################################################

                    ############## Programming assignment 1 ##############

#############################################################################################################

`001` <- read.csv("C:/Users/46766/Downloads/rprog_data_specdata/specdata/001.csv")

# Adjust working directory.

setwd("C:/Users/46766/Downloads/rprog_data_specdata/specdata")
getwd()

## Pollutantmean function

pollutantmean<-function(x, y){

  df<-data.frame()
  
  for (i in seQuestion (x,y)){ 
    df <- rbind(df, read.csv(list.files(pattern="*.csv")[i]))
  }

return(list(df[, 2][is.na(df[, 2]) != TRUE], df[, 3][is.na(df[, 3]) != TRUE]))

}

#Question 1

round(mean(pollutantmean(1, 10)[[1]]), digits=3)

#Question 2

round(mean(pollutantmean(70, 72)[[2]]), digits=3)

#Question 3

round(mean(pollutantmean(34,34)[[1]]), digits=3)

#Question 4

round(mean(pollutantmean(1,332)[[2]]), digits=3)

## Complete function (Vectorized)

Complete<-function(x){

sum(is.na(read.csv(list.files(pattern="*.csv")[x])[, c(2)]) == FALSE)} # No of "complete" cases in list 1.

Complete<-Vectorize(Complete)

#Question 5

Complete(c(6, 10, 20, 34, 100, 200, 310))

#Question 6

Complete(54)

#Question 7

RNGversion("3.5.1")
set.seed(42)

Complete(332:1)[sample(332, 10)]

## Correlation function

corr<-function(x){
  
  Corr_v2<-rep(0, 332) 
  
  for (i in seQuestion (1, 332)) {
    
    df <- read.csv(list.files(pattern="*.csv")[i])
    df<-df[complete.cases(df), ]
    
    if(nrow(df)> x){ 
      Corr_v2[i]<-cor(df[, 2],df[, 3])}}
  
  return(Corr_v2[Corr_v2 !=0])
  
}

#Question 8

cr<-sort(corr(0))


RNGversion("3.5.1")
set.seed(868)

out <- round(cr[sample(length(cr), 5)], 4)
print(out)

#Question 9

cr <- corr(129)                
cr <- sort(cr)                
n <- length(cr)

RNGversion("3.5.1")
set.seed(197)

out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

#Question 10

cr <- corr(2000)                
n <- length(cr)                
cr <- corr(1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

#############################################################################################################

                        ############## Programming assignment 2 ##############

#############################################################################################################

#Question 1 

makeCacheMatrix <- function(x = matrix()) {
  
  # Input x as a matrix. Needs to be an invertible matrix to run properly.
  if(nrow(x) == ncol(x) & det(x) !=0){ #Properties for invertible matrix:
    
    inv = NULL
    set = function(y) {
      
      
      x <<- y
      inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  }
  
  else {print("matrix not invertible")}
  
}

## Question 2

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  # If such an inversion {has} been conducted before:
  
  if (!is.null(inv)){
    
    return(inv)
  }
  
  # If such an inversion {has not} been calculated:
  
  mat = x$get()
  inv = solve(mat, ...)
  
  x$setinv(inv)
  
  return(inv)
}

#############################################################################################################

           ############## Programming assignment 3 ##############

#############################################################################################################

#Cleaning data (!)

Outcome <- as.data.frame(read.csv("C:/Users/46766/Downloads/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", header=TRUE))

Outcome <-Outcome[, c(2, 7, 11, 17, 23)]

Outcome[, seQuestion (3, 5)]<-apply(Outcome[, seQuestion (3, 5)], 2, as.numeric)

colnames(Outcome)<-c("Hospital", "State", "Hrt Atk", "Hrt Failure", "Pneumia")

# best function

best<-function(sickness, state, rank, z){
 
  
  Number<-Rfast::nth(Outcome[, sickness][Outcome$State == state], k=rank, descending = z, na.rm=TRUE)
  return(Outcome$Hospital[Outcome$State == state & Outcome[, sickness] == Number][is.na(Outcome$Hospital[Outcome$State == state & Outcome[, sickness] == Number]) != TRUE])
  
    
  }

# 3= Hrt atk
# 4= Hrt failure
# 5= pneumia

#Question 1
 
best(sickness=3, "SC", 1, FALSE)
best(sickness=5, "NY", 1, FALSE)
best(sickness=5, "AK", 1, FALSE)

#Question 2

best(sickness=3, "NC", 1, TRUE)
best(sickness=3, "WA", 7, FALSE)
tail(sort(best(sickness=5, "TX", 10, FALSE), decreasing = TRUE), 1)
tail(sort(best(sickness=3, "NY", 7, FALSE), decreasing = TRUE), 1)

#Question 3

best(sickness=3, "HI", 4, FALSE)
best(sickness=5, "NJ", 1, TRUE)
best(sickness=5, "NJ", 1, TRUE)
best(sickness=4, "NV", 10, FALSE)

