normatize <- function (data) {
  result <- data %>% 
    as_tibble() %>%
    mutate_if(
      .predicate = is.numeric, 
      .funs = function(x) x/sqrt(sum(x^2))
    ) %>%
    as.matrix()
  return(result)
}