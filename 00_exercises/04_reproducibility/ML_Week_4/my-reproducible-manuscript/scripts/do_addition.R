do_addition <- function(number1, number2) {
  
  # Check that arguments provided are numeric
  if (class(number1) != "numeric" | class(number2) != "numeric") {
    print("Error: one or more of your inputs are not numeric")
  } else {
    # Perform simple addition and return the result
    result <- number1 + number2
    return(result)
  }
}
