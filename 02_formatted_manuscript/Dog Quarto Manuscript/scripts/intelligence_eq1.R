intelligence <- function(R) {
  
  # Check that arguments provided are numeric
  if (class(R) != "numeric") {
    print("Error: your input should be numeric")
  } else {
    # Perform simple mean calculation and return the result
    result <- 1 / (1 + R)
    return(result)
  }
}
