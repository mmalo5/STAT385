# 1. Create a new function named 'poly_that_loop' that accepts 
## an integer vector and returns an integer vector of the 
## same length as the input vector such that:
## i. replaces each prime number with that prime number raised 
## to the power of 2
## ii. replaces each non-prime number with that non-prime 
## number raised to the power of 3. 
## For your poly_that_loop() function, it must use at least
## one explicit loop to complete the task.
## Next, create a new function named 'poly_that_vectorized' 
## that accepts an integer vector and returns an integer 
## vector of the same length as the input vector such that:
## i. replaces each prime number with that prime number 
## raised to the power of 2
## ii. replaces each non-prime number with that non-prime 
## number raised to the power of 3.
## For your poly_that_vectorized() function, it cannot use 
## any loop (explicit or implicit) to complete the task. 
## It can only use the strength of vectorization. 
## NOTE: In R Inferno by Burns they describe the apply family
## of functions as loop-hiding, which means they are implicit 
## loops.

poly_that_loop <- function(x) {
  len <- length(x)
  ans <- rep(0,len)
  seq <- seq_along(x)
  for (i in seq) {
    prime <- TRUE
    if (x[i] <=1) {
      prime <- FALSE
    } else {
      if (floor(sqrt(x[i])) >1) {
        for (j in 2:floor(sqrt(x[i]))) {
          if (x[i]%%j == 0) {
            prime <- FALSE
            break
          }
        }
      }
    }
  if (prime) {
    ans[i] <- (x[i])^2
  } else {
    ans[i] <- (x[i])^3
  }
  }
  return(as.integer(ans))
}

poly_that_vectorized <- function(x) {
  len <- length(x)
  maxn <- max(x,0)
  prime <- rep(FALSE,len)
  if (maxn>1) {
    out <- outer(x,(1:maxn),"%%")
    out0 = rowSums(out==0)
    prime <- (out0==2) & (x>1) 
  }
  ans <- rep(0,len)
  ans[prime] <- x[prime]^2
  ans[!prime] <- x[!prime]^3
  return(as.integer(ans))
}

# 2. The following R code results in an NA for the mean of the 
## z2 object. First, explain using a comment (#) what the issue 
## is in this code. Second, change the code such that the mean 
## of the z2 object is computed and closed-form.

# The issue is the 'NA' in x2, which leads to a result of 'NA' as the mean since the 'NA' carries on from x2 to y2 to z2
x2 <- c(1.52, 2.07, NA, 4.3, 5.91)
y2 <- log(x2)
z2 <- ifelse(y2 > 1, y2, 0)
z2_new <- z2[!is.na(x2)]
mean(z2_new)
#mean(z2)


# 3. Create a new function named 'debt_free_scheduler' that helps 
## users calculate how long it takes to completely pay 
## off their fixed-rate personal loan. The function 
## accepts numeric inputs and returns a data frame of a particular number of rows
## such that:
## i. The first numeric input is the current balance in 
## US dollars. This is a single number.
## ii. The second numeric input is the flat fixed monthly 
## interest rate as a percentage. This is a single number.
## iii. The third numeric input is user's fixed automatic 
## payment amount in US dollars. This is a single number.
## iv. The fourth numeric input is the month. This is a 
## single number that can be one or two digits.
## v. The fifth numeric input is the year as a four-digit
## single number.
## vi. The data frame has five columns organized and 
## named as the following: current_balance, interest_rate,
## interest_amount, payment_amount, month, year.
## vii. The number of rows of the data frame corresponds to 
## when the current_balance is less than or equal to 0.
debt_free_scheduler <- function(current_balance, monthly_interest, auto_payment, month, year) {
  final_balance <- final_rate <- final_interest <- final_payment <- final_month <- final_year <- rep(0,9999)
  pos <- 0
  while(current_balance>0) {
    pos <- pos+1
    interest <- current_balance*monthly_interest/100
    final_balance[pos] <- current_balance
    final_rate[pos] <- monthly_interest
    final_interest[pos] <- interest
    final_payment[pos] <- auto_payment
    final_month[pos] <- month
    final_year[pos] <- year
    month <- month+1
    current_balance <- current_balance+interest-auto_payment
    while(month>12) {
      month <- month-12
      year <- year+1
    }
      
  }
  df <- data.frame(
    current_balance = final_balance[1:pos],
    interest_rate = final_rate[1:pos],
    interest_amount = final_interest[1:pos], 
    payment_amount = final_payment[1:pos], 
    month = final_month[1:pos], 
    year = final_year[1:pos]
  )
  return(df)
    
}


# 4. The following R code produces errors. First, explain 
## using a comment (#) what the issue is in this code. 
## Second, change the code such that the summat() function
## computes the sum of all values in an input matrix.
## The constraints are:
## a. You cannot use any package.
## b. You cannot use any loop.

#summat <- function(mat0){
#  apply(mat0, 1, function(row) {
#    if (nrow > 1) {
#      return(sum(row))
#      } else {
#        return("not a proper matrix")
#        }
#    })
#}
#^ in this code, the issues are that the code uses a loop (apply()) and it returns the sum of the rows, not the whole matrix
summat <- function(mat0) {
  if (is.matrix(mat0)) {
    sum_matrix = sum(mat0, na.rm=TRUE)
    return(sum_matrix)
  } else {
    return("not a proper matrix")
  }
}

# 5. Improve the R code below in any manner you wish such 
## that the result is a filtered data frame for the given 
## column and threshold. The constraints are:
## a. You cannot use any package.
## b. You cannot change the name of the function my_filter.
## c. Your result must be include the use of the iris data,
## the Sepal.Length column, and the threshold value of 7.

my_filter <- function(data, col, threshold) {
  #subset(data, col > threshold)
  vector = data[[col]]
  ans <- data[vector>threshold, ]
  return(ans)
}

my_filter(iris, "Sepal.Length", 7)
