find_assign_call <- function(x) {
  if (is_call(x, "<-") && is_symbol(x[[2]])) {
    lhs <- as_string(x[[2]])
    children <- as.list(x)[-1]
  } else {
    lhs <- character()
    children <- as.list(x)
  }
  
  c(lhs, flat_map_chr(children, find_assign_rec))
}


flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

find_assign_rec <- function(x,i) {
  print(i)
  print(expr_type(x))
  switch_expr(x,
              # Base cases
              constant = ,
              symbol = character(),
              
              # Recursive cases
              pairlist = flat_map_chr(x, find_assign_rec,i=i+1),
              call = find_assign_call(x)
  )
}


find_assign <- function(x) unique(find_assign_rec(enexpr(x),i=0))

parsed_code = parse(file(file.path(dirpath,filepath),encoding = "UTF-8"))

find_assign(parsed_code)
expr_type(parsed_code)



