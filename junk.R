#function to change some part of thhe input for a given scenario
change_element <- function(x, object="catch_cv", changeto = c(0.2, 0.1)) {
  x[[object]] <- changeto
  return(x)
}


base <- list(animal = "cat",
             thing = "mug",
             place = "france")
base

base2 <- change_element(base, object = "animal", changeto = "mouse")
base2

bob <- list(animal = "mouse",
            thing = "plate")
xx <- accumulate(bob, .f = change_element2, .init = base)

change_element2(base, bob)

change_element2 <- function(x, changeto = list(catch_cv=c(0.2, 0.1))) {
  x[[names(changeto)]] <- changeto[[1]]
  return(x)
}

