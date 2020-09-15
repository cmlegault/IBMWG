# #function to change some part of thhe input for a given scenario
# change_element <- function(x, object="catch_cv", changeto = c(0.2, 0.1)) {
#   x[[object]] <- changeto
#   return(x)
# }
put_h <- function(a, b) {
  a[1] <- b
  return(a)
}


# function to change some part of a named list
change_it <- function(object, value, name) {
  if (name == "IBM") {
    object[[name]] = get(value[[1]])
  }
  else {
    object[[name]] = value[[1]]
  }
  return(object)
}
#reduce2(aa, names(aa), .f = change_it, .init = bb)

#function to change the input for multiple elements of the input object
# currently won't work with changing dimensions (e.g. nfleets & surveys, etc.)
change_input <- function(input, change = list(catch_cv = c(0.2, 0.1))) {
  #input2 <- purrr::reduce(change$object, change$changeto, .f = change_element, .init = input)
  input2 <- purrr::reduce2(change, names(change), .f = change_it, .init = input)
  return(input2)
}
#input2 <- change_input(base_input)


