

#' For 1 reader/writer used in one_call (found in curr_script), get the path to file.s read.
#'
#' @param one_call 
#' @param io_fun 
#' @param file_arg 
#' @param curr_script 
#' @param dirpath 
#' @param missing_vars Named-List of missing variables provided by the user to avoid parsing the code looking for var definition.
#'
#' @return
#' @export
#'
#' @examples
get_io_one_call = function(one_call,io_fun,file_arg,curr_script,dirpath,missing_vars = list(),funs_defs){
  this_env = environment()
  if(length(missing_vars)>0){
    print(sprintf("Loading vars %s",paste(names(missing_vars),collapse=", ")))
    list2env(missing_vars,envir = this_env)
  }
  print(substr(one_call,1,50))
  ee = parse(text=one_call)[[1]]
  eefun <- ee[[1]]
  inline_io_fun_pattern = get_inline_io_fun_pattern(io_fun)#la fonction peut être invoquée avec une parenthèse mais aussi dans un lapply, map... et donc être suivie d'un espace, virgule, parenthèse fermante...
  i=0
  x = as.list(ee)[[1]]
  while(eefun != io_fun){
    index = which(sapply(ee,function(eee){length(grep(pattern=inline_io_fun_pattern,eee))>0}))
    
    if(any(index==1)){
      eefun = io_fun
      
    } else {
      eeprev = ee
      ee = lapply(ee,function(eee)if(length(grep(pattern=inline_io_fun_pattern,eee))>0)eee else NULL)
      ee = purrr::compact(ee)
      
      ee = parse(text = ee)[[1]]
      if(ee!=io_fun){
        eefun = ee[[1]]
      } else {
        eefun = ee
        ee = eeprev
      }
    }
  }
  
  
  assertthat::assert_that(eefun == io_fun,msg="On devrait avoir trouvé notre io_fun")
  cc <- match.call(match.fun(eefun), ee)
  cc_list = as.list(cc)
  if(!file_arg %in% names(cc_list) && eefun=="safe_fread"){
    print("arg is not parsed for safe_fread => hardcoded to 1st arg")
    iopath = cc_list[[2]]
  } else if (!file_arg %in% names(cc_list) && eefun=="saveWorkbook") {
    print("arg is not parsed for saveWorkbook => hardcoded to 2nd arg")
    iopath = cc_list[[3]]
  } else if (!file_arg %in% names(cc_list) && eefun=="load") {
    print("arg is not parsed for load => hardcoded to 1st arg")
    iopath = cc_list[[2]]
  } else if (!file_arg %in% names(cc_list) && eefun=="loadWorkbook") {
    print("arg is not parsed for loadWorkbook => hardcoded to 1st arg")
    iopath = cc_list[[2]]
  } else if (!file_arg %in% names(cc_list) && eefun=="fread") {
    print("arg is not parsed for fread => hardcoded to 1st arg")
    iopath = cc_list[[2]]
  } else if (!file_arg %in% names(cc_list) && eefun=="read_excel") {
    print("arg is not parsed for read_excel => hardcoded to 1st arg")
    iopath = cc_list[[2]]
  } else if (!file_arg %in% names(cc_list) && eefun=="read.xlsx") {
    print("arg is not parsed for read.xlsx => hardcoded to 1st arg")
    iopath = cc_list[[2]]
  } else if (!file_arg %in% names(cc_list) && eefun=="fwrite") {
    print("arg is not parsed for fwrite => hardcoded to 2nd arg")
    iopath = cc_list[[3]]
  } else if (!file_arg %in% names(cc_list) && eefun=="write.csv") {
    print("arg is not parsed for write.csv => hardcoded to 2nd arg")
    iopath = cc_list[[3]]
  } else if (!file_arg %in% names(cc_list) && eefun=="read_fst") {
    print("arg is not parsed for read_fst => hardcoded to 1st arg")
    iopath = cc_list[[2]]
  } else {
    iopath = cc_list[[file_arg]]
  }
  
  if(is.null(iopath)){
    cc_list
    names(cc_list)
    length(cc_list)
    browser()
  }
  parsed_fun = no_pkg(deparse(cc_list[[1]]))
  print(parsed_fun)
  if(parsed_fun=="::"){
    print("package::fun not handled")
    iopath=NULL
  } else if ((ee[[2]] == io_fun) & ((ee[[1]] == "<-") | (ee[[1]] == "="))) {
    print("this is the io_fun definition")
    iopath=NULL
  } else {
    if(is.null(iopath))browser()
    iopath_evaluated = NULL
    had_missing_var_error = T
    iter = 0
    while(iter < 5 && had_missing_var_error){#can handle 5 params at most
      print(sprintf("Trial n° %s to get the actual path",iter))
      print(iopath)
      # if (iter == 2)browser()
      tryCatch({
        iopath_evaluated <- eval(iopath)
        had_missing_var_error = F
      },
      error=function(e)error_get_missing_var(e,env=this_env,missing_vars=missing_vars,verbose=1,funs_defs=funs_defs),
      silent = T)
      iter = iter+1
    }
    
    if(iter == 5){
      print(one_call)
      browser()
    }
    
    if(is.null(iopath_evaluated)){
      print(one_call)
      print("no value retrieved")
    } else {
      iopath = iopath_evaluated
    }
    
    if(class(iopath)=="call"){
      iopath = paste(deparse(iopath),collapse = " ")
    }
  }
  iopath
  
}


get_inside_fun_def = function(one_call){
  is.call(one_call)
  if(length(one_call)>=3){
    fun = one_call[[1]]
    val = one_call[[3]]
    if((fun == "=" | fun == "<-") && class(val)=="call" && val[[1]] == "function"){
      res = val[[3]]# take function definition ...
      if (res[[1]] == "{"){# ...without {}
        res = res[-1]
        sapply(1:length(res), function(i) {
          res[[i]]
        })
      }
      
    } else {
      one_call
    }
  } else {
    one_call
  }
}

is_fun_def = function(one_call){
  is.call(one_call)
  if(length(one_call)>=3){
    fun = one_call[[1]]
    val = one_call[[3]]
    if((fun == "=" | fun == "<-") && class(val)=="call" && val[[1]] == "function"){
      T      
    } else {
      F
    }
  } else {
    F
  }
}

unnest_fun_call = function(one_call,funs_defs){
  if(!is.null(funs_defs) && (class(one_call)=="call" & as.character(one_call[[1]]) %in% names(funs_defs))){
    
  fun_name = as.character(one_call[[1]])
  nms = names(one_call)
  params_loc = which(nms!="")
  if(length(params_loc)>0){
    params = lapply(params_loc,function(i){
      var = nms[i]
      val = paste(deparse(one_call[[i]]),collapse="")
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
  } else one_call
}

get_io_one_fun = function(io_fun,file_arg,filepath,dirpath="../sidep/",missing_vars=list(),funs_defs){
  # io_fun = "read.xlsx"
  # if(io_fun == "saveWorkbook")browser()
  OK = F  
  try({
    parsed_code <- parse(file.path(dirpath,filepath))
    OK = T
  },silent = T)
  if(!OK){
    parsed_code <- parse(file(file.path(dirpath,filepath),encoding = "UTF-8"))
  }
  # browser()
  # parsed_code = purrr::map(parsed_code,get_inside_fun_def)
  parsed_code = purrr::discard(parsed_code,is_fun_def)
  
  #approche inexacte car on va chercher les définitions au premier degré mais elles peuvent être imbriquées
  # parsed_code = lapply(parsed_code,unnest_fun_call,funs_defs=funs_defs)
  
  code = unlist(parsed_code)
  
  inline_io_fun_pattern = get_inline_io_fun_pattern(io_fun)#la fonction peut être invoquée avec une parenthèse mais aussi dans un lapply, map... et donc être suivie d'un espace, virgule, parenthèse fermante...
  
  code = grep(inline_io_fun_pattern,code,value=T)
  # code = grep(io_fun,code,value=T)
  if(length(code)>0){
    print(sprintf("%s expressions trouvées qui utilisent la io_fun %s",length(code),io_fun))
    curr_script = filepath
    io_one_io_fun = sapply(
      code,
      get_io_one_call,
      io_fun = io_fun,
      file_arg = file_arg,
      curr_script = curr_script,
      dirpath = dirpath,
      missing_vars = missing_vars,
      funs_defs=funs_defs
    )
    
    io_one_io_fun = unname(io_one_io_fun)
    io_one_io_fun
  } else NULL
  
}

get_io_i = function(i,io_funs,filepath=filepath,dirpath=dirpath,missing_vars=list(),funs_defs){
  io_fun = names(io_funs[i])
  file_arg = unname(io_funs[i])
  get_io_one_fun(
    io_fun = io_fun,
    file_arg = file_arg,
    filepath = filepath,
    dirpath = dirpath,
    missing_vars = missing_vars,
    funs_defs=funs_defs
  )
  
}



guess_readers = function(filepath,dirpath="../sidep"){
  src = get_code(filepath,dirpath)
  readers = unique(stringr::str_extract(src,"([A-z0-9_\\.]*)((read)|(load))([A-z0-9_\\.]*)\\("))
  readers = readers[!is.na(readers)]
  readers
}


guess_writers = function(filepath,dirpath="../sidep"){
  src = get_code(filepath,dirpath)
  writers = unique(stringr::str_extract(src,"([A-z0-9_\\.]*)((write)|(save))([A-z0-9_\\.]*)\\("))
  writers = writers[!is.na(writers)]
  writers
}


#' Get path to files read/written in filepath.
#'
#' @param filepath 
#' @param io_funs readers or writers
#' @param dirpath 
#' @param missing_vars when a var is missing, the function tries to parse the code, find the def and evaluate it. You may save some time and ambiguity by providing the actual values in a named list.
#' @param mode read or write
#'
#' @return
#' @export
#'
#' @examples
get_io = function(filepath,
                  io_funs,
                  dirpath = "../sidep",
                  missing_vars = missing_vars,
                  funs_defs,
                  mode = "read") {
  
  
  print("guess readers in file")
  print(filepath)
  if(mode=="read"){
    guessed_io_fun = guess_readers(filepath,dirpath)
    print(guessed_io_fun)
  } else if(mode=="write"){
    guessed_io_fun = guess_writers(filepath,dirpath)
    print(guessed_io_fun)
  } else {
    guessed_io_fun = "mode incorrect"
  }
  
  inputs = sapply(1:length(io_funs),get_io_i,filepath=filepath,dirpath=dirpath,io_funs=io_funs,missing_vars=missing_vars,funs_defs=funs_defs)
  names(inputs) <- names(io_funs)
  inputs = unlist(inputs)
  if(length(inputs)>0 & mode[1]=="read"){
    res = data.table(parent = filepath,child = inputs,group="input",fun=names(inputs))
  } else if(length(inputs)>0 & mode[1]=="write"){
    res = data.table(parent = filepath,child = inputs,group="output",fun=names(inputs))
  }  else {
    res = NULL
  }
  list(guessed_io_fun = guessed_io_fun,res=res)
}
