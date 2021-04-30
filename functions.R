
# locate function definition in scripts
get_fun_name = function(one_call){
  is.call(one_call)
  if(length(one_call)>=3){
    fun = one_call[[1]]
    nm = as.character(one_call[[2]])
    val = one_call[[3]]
    if((fun == "=" | fun == "<-") && class(val)=="call" && val[[1]] == "function"){
      return(nm)
    } 
  } 
}

locate_custom_funs_defs = function(script,dirpath="../sidep/"){
  OK = F  
  try({
    parsed_code <- parse(file.path(dirpath,script))
    OK = T
  },silent = T)
  if(!OK){
    parsed_code <- parse(file(file.path(dirpath,script),encoding = "UTF-8"))
  }
  
  
  
  funs_loc = grep("function",parsed_code)
  funs_defs = parsed_code[funs_loc]
  
  res = purrr::map(funs_defs,get_fun_name)
  res = purrr::compact(res)
  res = unlist(res)
  res = unique(res)
  res = data.table(parent = script,child = res,group_parent="script",group_child="function",type = "def",arrows="from")
  return(res)
}


# locate function call in scripts
no_pkg = function(nm){
  gsub("^.*::","",nm)
}

locate_funs_calls_in_expr = function(expressions,names_funs_defs,return_type = "name"){
  if(length(expressions)==0){
    return()
  }
  if(length(expressions) > 1){
    if((expressions[[1]] == "for")){
      sub_item = expressions[[4]]
      if(class(sub_item)=="{"){
        sub_item = sub_item[-1]
        sub_item = sapply(1:length(sub_item), function(i) {
          sub_item[[i]]
        })
      }
      return(locate_funs_calls_in_expr(sub_item,names_funs_defs=names_funs_defs,return_type=return_type))
    }
    
    if((expressions[[1]] == "if")){
      sub_item = expressions[3:length(expressions)]
      if(class(sub_item)=="{"){
        sub_item = sub_item[-1]
        sub_item = sapply(1:length(sub_item), function(i) {
          sub_item[[i]]
        })
      }
      return(locate_funs_calls_in_expr(sub_item,names_funs_defs=names_funs_defs,return_type=return_type))
    }
    
    if(!is.null(names_funs_defs) && (class(expressions)=="call" & no_pkg(deparse(expressions[[1]])) %in% names_funs_defs)){
      if(return_type == "name"){
        res = deparse(expressions[[1]])
      } else if (return_type == "expr"){
        res = expressions
      } else {
        stop("incorrect value for return_type, should be name or expr")
      }
      return(res)
    }
  }
  lapply(1:length(expressions),function(i){
    if(rlang::is_symbol(expressions) & length(expressions) == 1){
      return()
    } else {
      if (class(expressions[[i]]) == "name" && expressions[[i]] == "") {
        return()
      }
      sub_item = expressions[[i]]
    }
    if(rlang::is_missing(sub_item) || is.null(sub_item) || class(sub_item)%in%base_types){
      return()
    } else if (class(sub_item) %in% c("call","=","<-")){
      return(locate_funs_calls_in_expr(sub_item,names_funs_defs=names_funs_defs,return_type=return_type))
    } else if(class(sub_item)=="{"){
      sub_item = sub_item[-1]
      sub_item = sapply(1:length(sub_item), function(i) {
        sub_item[[i]]
      })
      return(locate_funs_calls_in_expr(sub_item,names_funs_defs=names_funs_defs,return_type=return_type))
    } else {
      tryCatch({
        return(locate_funs_calls_in_expr(sub_item,names_funs_defs=names_funs_defs,return_type=return_type))
      },
      error = function(e) {
        print("exception in locate_funs_calls_in_expr")
        browser()
      })
    }
  })
}

locate_funs_calls_in_script = function(script,dirpath = "../sidep/",names_funs_defs,return_type="name"){
  OK = F  
  try({
    parsed_code <- parse(file.path(dirpath,script))
    OK = T
  },silent = T)
  if(!OK){
    parsed_code <- parse(file(file.path(dirpath,script),encoding = "UTF-8"))
  }
  regex = paste0("(",paste(names_funs_defs,collapse=")|("),")")
  funs_calls_loc = grep(regex,parsed_code)
  expressions = parsed_code[funs_calls_loc]
  
  res = locate_funs_calls_in_expr(expressions,names_funs_defs=names_funs_defs,return_type=return_type)
  res = unlist(res)
  res = unique(res)
  res = data.table(parent = script,child = res,group_child="function",group_parent="script",type = "call",arrows="to")
  return(res)
}



get_iopath_expr = function(one_call,io_funs){
  eefun = no_pkg(deparse(one_call[[1]]))
  if(!is.null(io_funs) && (class(one_call)=="call" & eefun %in% names(io_funs))){
    names(one_call)
    file_arg = io_funs[[eefun]]
    if(!file_arg %in% names(one_call) && eefun=="safe_fread"){
      print("arg is not parsed for safe_fread => hardcoded to 1st arg")
      iopath = one_call[[2]]
    } else if (!file_arg %in% names(one_call) && eefun=="saveWorkbook") {
      print("arg is not parsed for saveWorkbook => hardcoded to 2nd arg")
      iopath = one_call[[3]]
    } else if (!file_arg %in% names(one_call) && eefun=="load") {
      print("arg is not parsed for load => hardcoded to 1st arg")
      iopath = one_call[[2]]
    } else if (!file_arg %in% names(one_call) && eefun=="loadWorkbook") {
      print("arg is not parsed for loadWorkbook => hardcoded to 1st arg")
      iopath = one_call[[2]]
    } else if (!file_arg %in% names(one_call) && eefun=="fread") {
      print("arg is not parsed for fread => hardcoded to 1st arg")
      iopath = one_call[[2]]
    } else if (!file_arg %in% names(one_call) && eefun=="read_excel") {
      print("arg is not parsed for read_excel => hardcoded to 1st arg")
      iopath = one_call[[2]]
    } else if (!file_arg %in% names(one_call) && eefun=="read.xlsx") {
      print("arg is not parsed for read.xlsx => hardcoded to 1st arg")
      iopath = one_call[[2]]
    } else if (!file_arg %in% names(one_call) && eefun=="fwrite") {
      print("arg is not parsed for fwrite => hardcoded to 2nd arg")
      iopath = one_call[[3]]
    } else if (!file_arg %in% names(one_call) && eefun=="write.csv") {
      print("arg is not parsed for write.csv => hardcoded to 2nd arg")
      iopath = one_call[[3]]
    } else if (!file_arg %in% names(one_call) && eefun=="read_fst") {
      print("arg is not parsed for read_fst => hardcoded to 1st arg")
      iopath = one_call[[2]]
    } else {
      iopath = one_call[[file_arg]]
    }
    if(is.null(iopath)){
      one_call
      names(one_call)
      length(one_call)
      browser()
    }
    return(list(call = one_call,iopath = iopath))
  }
}

# one_call = custom_funs_call_expr[[sample(length(custom_funs_call_expr),1)]]

eval_iopath_onecall = function(one_call,io_custom_funs,iopath_from_funs){
  
  params = names(one_call)
  params = params[params!=""]
  if(length(params)>0){
    # param = sample(params,1)
    for(param in params){
      if(class(one_call[[param]]) %in% c("character")){
        param_val = paste0('"',one_call[[param]],'"')
      } else {
        print("need eval ?")
        param_val = paste0("'",deparse1(one_call[[param]],collapse = ""),"'")
        try({
          param_val = eval(one_call[[param]])
        })
        if(class(param_val) %in% c("character")){
          param_val = paste0('"',param_val,'"')
        }
      }
      tryCatch({
        eval(parse(text = paste0(param," = ",param_val)))
      },error = function(e){
        browser()
      })
    }
  }
  
  custom_fun = deparse(one_call[[1]])
  io_one_call = iopath_from_funs[[custom_fun]]
  
  io_one_call = lapply(io_one_call,function(x)x$iopath)
  
  
  lapply(io_one_call,function(x){
    res = x
    try({
      res <- eval(x)
    })
    return(res)
  })
  
}


function2io = function(io_funs){
  
  io_custom_funs = lapply(
    funs_defs,
    locate_funs_calls_in_expr,
    names_funs_defs = names(io_funs),
    return_type = "expr"
  )
  io_custom_funs = lapply(io_custom_funs,unlist)
  io_custom_funs = purrr::compact(io_custom_funs)
  
  
  iopath_from_funs = lapply(io_custom_funs,function(calls){
    lapply(calls,get_iopath_expr,io_funs=io_funs)
  })
  
  funs_defs_with_io = funs_defs[names(iopath_from_funs)]
  
  custom_funs_call_expr = lapply(codes_path,locate_funs_calls_in_script,names_funs_defs=names(funs_defs_with_io),return_type="expr")
  custom_funs_call_expr = rbindlist(custom_funs_call_expr,fill=T)
  custom_funs_call_expr = custom_funs_call_expr$child
  custom_funs_call_expr = purrr::compact(custom_funs_call_expr)
  
  assertthat::assert_that(!any(duplicated(custom_funs_call_expr)),msg = "Deux appels identiques, il faut les répérer dans le code pour vérifier si le contexte est différent !")
  
  
  custom_fun_evaluated_iopath = lapply(custom_funs_call_expr,eval_iopath_onecall,io_custom_funs=io_custom_funs,iopath_from_funs=iopath_from_funs)
  
  nms = lapply(custom_funs_call_expr,function(x)deparse1(x[[1]],collapse=""))
  nms = unlist(nms)
  assertthat::assert_that(length(nms) == length(custom_fun_evaluated_iopath),msg="on devrait avoir récupéré une liste de noms de la même taille que la liste des evaluated_iopath")
  names(custom_fun_evaluated_iopath) <- nms
  
  lapply(custom_fun_evaluated_iopath,unlist)
}



rm_uneval_expr_unlist = function(l){
  l <- lapply(l,function(x){
    unlist(purrr::map(x,deparse1,collapse=""))
  })
  l <- rbindlist(lapply(1:length(l),function(i){
    data.table(parent= names(l)[i],child=unname(l[[i]]))
  }))
  
  unique(l)
}
