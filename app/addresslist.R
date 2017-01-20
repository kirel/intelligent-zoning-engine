library(dplyr)

to_addresslist = function(addresses_df) {
  stopifnot(c('street', 'no') %in% colnames(addresses_df))
  addresses_df %>% group_by(street) %>%
    summarise(description=paste(first(street), describe_numbers(no))) %>%
    .$description
}

partition_by_successive = function(x, predicate) {
  predicate = purrr::as_function(predicate)
  initial = list(solution=list(), previous=NULL)
  combine = function(intermediate, elem) {
    if (is.null(intermediate$previous)) {
      intermediate$solution = list(elem)
    } else if (predicate(intermediate$previous, elem)) {
      # split when predicate is true
      intermediate$solution = c(intermediate$solution, elem)
    } else {
      intermediate$solution[[length(intermediate$solution)]] = c(intermediate$solution[[length(intermediate$solution)]], elem)
    }
    intermediate$previous = elem
    intermediate
  }
  purrr::reduce(x, combine, .init=initial)$solution
}

# partition_by_successive(c(1:10, 12:14), function(x, y) x + 1 != y)

describe_numbers = function(x) {
  partition_by_successive(sort(x), ~ .x + 1 != .y) %>%
    purrr::map(~ do.call(paste, as.list(c(unique(range(.x)), sep="-")))) %>%
    (function(x) do.call(paste, c(x, sep=', ')))
}
