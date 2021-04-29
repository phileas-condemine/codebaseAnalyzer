get_sources_path = function(code,path="",verbose=F){
  sources = grep("source\\(",code,value = T)
  # x = sources[10]
  # x = sources[11]
  sources_path = sapply(sources,function(x){
    ee <- parse(text = x)[[1]]
    eefun <- ee[[1]]
    # L'expression devrait contenir "source"
    index = which(sapply(ee,function(eee){length(grep(pattern="source",eee))>0}))
    if(index>1){
      if(verbose)print("source embedded in other fun calls")
      ee = lapply(ee,function(eee)if(length(grep(pattern="source",eee))>0)eee else NULL)
      ee = purrr::compact(ee)
      x = print(ee[[1]])
      eefun = parse(text = x)[[1]]
      ee = x
    }
    
    # get params in fun-call => the fun-call should be "source" & the param is "file".
    assertthat::assert_that(eefun=="source",msg="On devrait avoir parsé la fonction source")
    cc <- match.call(match.fun(eefun), ee)
    filepath = as.list(cc)$file
    if(verbose)print(filepath)
    filepath = eval(filepath)
    if(verbose)print(filepath)
    filepath
  })
  sources_path
  
}


get_all_sources = function(parentpath = "src/00_Actualisation_pcr_sero.R",verbose=F,dirpath="../sidep/"){
  if(verbose)print(parentpath)
  code = get_code(filepath = parentpath,dirpath=dirpath)
  direct_path = unname(get_sources_path(code))
  curr_pairs = data.table(parent = parentpath,child = direct_path)
  
  if(length(direct_path)>0){
    rec_pairs = rbindlist(lapply(direct_path,get_all_sources))
    pairs = rbind(curr_pairs,rec_pairs)
    
  } else {
    pairs = data.table(parent=character(),child=character())
  }
  
  pairs
}

unnest_funs_in_script = function(filepath,
                       destfile,
                       funs_defs){
  
    OK = F  
    try({
      parsed_code <- parse(filepath)
      OK = T
    },silent = T)
    if(!OK){
      parsed_code <- parse(file(filepath,encoding = "UTF-8"))
    }
    
    code = deeply_unnest_fun_call(parsed_code,funs_defs = funs_defs)
    
    char_code = deparse(code)
    
    if(char_code[length(char_code)] != ""){
      char_code = c(char_code,"")
    }
    writeLines(text = char_code,con = destfile)
    
  
}
  
unnest_source = function(mainfile = "src/00_Actualisation_pcr_sero.R",
                         dirpath = "../sidep/",
                         final_script = NULL,
                         n = -1L,
                         path = "../sidep/",
                         verbose = F,
                         destfile = NULL) {
  if(!is.null(final_script) && mainfile == final_script){
    code = get_code(mainfile,dirpath,n=n,verbose=verbose)
  } else {
    code = get_code(mainfile,dirpath,verbose=verbose)
  }
  code = gsub("^( )*#.*","",code)#rm comments
  
  sources_loc = grep("source\\(",code)
  has_source = (length(sources_loc)>0)
  i=1
  while(has_source){
    if(verbose)print(i)
    one_loc = sources_loc[1]
    code_to_source = code[one_loc]
    path_to_source = parse(text=code_to_source)[[1]]
    while(path_to_source[[1]] != "source"){
      print("A préciser si besoin, on suppose que source est conditionné par un if(a)b où b est l'expression à récupérer, d'où .[[3]]")
      path_to_source = path_to_source[[3]]
    }
    path_to_source = match.call(match.fun(source),path_to_source)$file
    if(verbose)print(path_to_source)
    try({
      # path_to_source = eval(paste(deparse(path_to_source),collapse=""))
      path_to_source = eval(path_to_source)
    },silent = verbose)
    if(verbose)print(path_to_source)
    if (grepl(paste0("^", dirpath),path_to_source, fixed = T)) {
      path_to_source = gsub(paste("^", dirpath), "", path_to_source, fixed = T)
    }
    if(!is.null(final_script) && path_to_source == final_script){
      print(sprintf("reached current script, will stop at line %s",n))
      sub_code = get_code(path_to_source,dirpath,n=n,verbose=verbose)
      # append codes
      code = c(code[1:(one_loc-1)],sub_code)
    } else {
      sub_code = get_code(path_to_source,dirpath,verbose=verbose)
      # append codes
      code = c(code[1:(one_loc-1)],sub_code,code[(one_loc+1):length(code)])
    }
    # check for sources calls
    sources_loc = grep("source\\(",code)
    has_source = (length(sources_loc)>0)
    i=i+1
  }
  

  
  if(verbose)print(length(code))
  if (!is.null(destfile)){
    # browser()
    # f = file(destfile, encoding = "UTF-8")
    if(code[length(code)] != ""){
      code = c(code,"")
    }
    writeLines(text = code,con = destfile)
    # close.connection(f)
  }
  invisible(code)
}




find_funs_defs = function(full_code_path = "full_code.R"){
  parsed_code = parse("full_code.R")
  
  funs_loc = grep("function",parsed_code)
  funs_defs = parsed_code[funs_loc]
  
  res = purrr::map(funs_defs,find_funs_def_per_call)
  res = purrr::compact(res)
  nms = lapply(res,names)
  assertthat::assert_that(length(nms) == length(res),msg="On devrait bien récupérer 1 nom de fonction par appel")
  res = lapply(res,function(x)x[[1]])
  names(res) <- nms
  if(sum(duplicated(nms))>0){
    warning("on a trouvé plusieurs définitions de la même fonction, on suppose qu'elles sont identiques.")
    res = res[!duplicated(nms)]
  }
  names(res)
  return(res)
}

fun_def = funs_defs[[sample(length(funs_defs),1)]]
one_call = fun_def[[sample(length(fun_def),1)]]

expr_has_custom_fun = function(expressions,funs_defs){
  if(length(expressions) > 1){
    if((expressions[[1]] == "for") && (expressions[[2]] == my_var)){
      sub_item = expressions[[4]]
      if(class(sub_item)=="{"){
        sub_item = sub_item[-1]
        sub_item = sapply(1:length(sub_item), function(i) {
          sub_item[[i]]
        })
      }
      return(one_call_has_custom_fun(sub_item,funs_defs=funs_defs))
    }
    if(!is.null(funs_defs) && (class(expressions)=="call" & as.character(expressions[[1]]) %in% names(funs_defs))){
      return(T)
    }
  }
  lapply(1:length(expressions),function(i){
    if(rlang::is_symbol(expressions) & length(expressions) == 1){
      return(F)
    } else {
      sub_item = expressions[[i]]
    }
    if(rlang::is_missing(sub_item) || is.null(sub_item) || class(sub_item)%in%base_types){
      return(F)
    } else if (class(sub_item) %in% c("call","=","<-")){
      return(one_call_has_custom_fun(sub_item,funs_defs=funs_defs))
    } else if(class(sub_item)=="{"){
      sub_item = sub_item[-1]
      sub_item = sapply(1:length(sub_item), function(i) {
        sub_item[[i]]
      })
      return(one_call_has_custom_fun(sub_item,funs_defs=funs_defs))
    } else {
      tryCatch({
        return(one_call_has_custom_fun(sub_item,funs_defs=funs_defs))
      },
      error = function(e) {
        print("exception in one_call_has_custom_fun")
        browser()
      })
    }
  })
}


unnest_fun_call = function(expressions,funs_defs){
  
  if(!is.null(funs_defs) && (class(expressions)=="call" & as.character(expressions[[1]]) %in% names(funs_defs))){
    
    fun_name = as.character(expressions[[1]])
    nms = names(expressions)
    params_loc = which(nms!="")
    if(length(params_loc)>0){
      params = lapply(params_loc,function(i){
        var = nms[i]
        val = paste(deparse(expressions[[i]]),collapse="")
        tryCatch({parse(text=paste0(var," = ",val))},error=function(e){
          print("oups var = val incorrect")
          browser()
        })
      })
    } else {
      params = NULL
    }
    fun_expr = funs_defs[[fun_name]]
    new_item = c(params,fun_expr)
    assertthat::assert_that(length(params) + length(fun_expr) == length(new_item),msg="On attend F(X=x,Y=y) avec F(X,Y){expr1;expr2} => list(X=x,Y=y,expr1,expr2)")
    new_item
  } else expressions
}

deeply_unnest_fun_call = function(expressions,funs_defs){
  
  if(length(expressions) > 1){
    if((expressions[[1]] == "for") && (expressions[[2]] == my_var)){
      sub_item = expressions[[4]]
      if(class(sub_item)=="{"){
        sub_item = sub_item[-1]
        sub_item = sapply(1:length(sub_item), function(i) {
          sub_item[[i]]
        })
      }
      return(deeply_unnest_fun_call(sub_item,funs_defs=funs_defs))
    }
    if(!is.null(funs_defs) && (class(expressions)=="call" & as.character(expressions[[1]]) %in% names(funs_defs))){
      # print("Boom, unnested one fun_call !")
      return(unnest_fun_call(expressions,funs_defs=funs_defs))
    }
  }
  lapply(1:length(expressions),function(i){
    if(rlang::is_symbol(expressions) & length(expressions) == 1){
      return(expressions)
    } else {
      sub_item = expressions[[i]]
      if(class(expressions[[i]]) == "name" && expressions[[i]] == ""){
        return(expressions)
      }
    }
    if(rlang::is_missing(sub_item) || is.null(sub_item) || class(sub_item)%in%base_types){
      return(sub_item)
    } else if (class(sub_item) %in% c("call","=","<-")){
      return(deeply_unnest_fun_call(sub_item,funs_defs=funs_defs))
    } else if(class(sub_item)=="{"){
      sub_item = sub_item[-1]
      sub_item = sapply(1:length(sub_item), function(i) {
        sub_item[[i]]
      })
      return(deeply_unnest_fun_call(sub_item,funs_defs=funs_defs))
    } else {
      tryCatch({
        return(deeply_unnest_fun_call(sub_item,funs_defs=funs_defs))
      },
      error = function(e) {
        print("exception in deeply_unnest_fun_call")
        browser()
      })
    }
  })
  
}


find_funs_def_per_call = function(one_call){
  is.call(one_call)
  if(length(one_call)>=3){
    fun = one_call[[1]]
    val = one_call[[3]]
    if((fun == "=" | fun == "<-") && class(val)=="call" && val[[1]] == "function"){
      res = val[[3]]# take function definition ...
      if (res[[1]] == "{"){# ...without {}
        res = res[-1]
        res = sapply(1:length(res), function(i) {
          res[[i]]
        })
        
        # OMG il faut gérer le return ou la dernière expression pour pouvoir assigner le résultat de l'appel à une fonction
      }
      res = list("my_fun"=res)
      names(res) <- as.character(one_call[[2]])
      return(res)
    } 
  } 
}



