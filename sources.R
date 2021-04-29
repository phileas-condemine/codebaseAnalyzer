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


unnest_source = function(mainfile="src/00_Actualisation_pcr_sero.R",dirpath="../sidep/",final_script=NULL,n=-1L,path="../sidep/",verbose=F,destfile = NULL){
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
    writeLines(text = c(code, ""),#final empty line
               con = destfile)
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



