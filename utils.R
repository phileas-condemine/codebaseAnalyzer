library(data.table)
library(visNetwork)
library(readxl)
# FIND DEPENDENCIES AND LOAD THE PKG TO BE ABLE TO "match.call" & "match.fun"
pkgs = readLines("../sidep/src/00_Actualisation_pcr_sero.R",encoding = "UTF-8")
pkgs = pkgs[grepl("^library\\(",pkgs)]
sapply(pkgs, function(x) {
  eval(parse(text = x))
})


reader_funs = c(
  "fread"="input",
  "safe_fread"="input",#fonction non déclarée...
  "read.csv"="file",
  "read.csv2"="file",
  "read.xlsx"="xlsxFile",
  "read_xlsx"="path",
  "readLines"="con",
  "read_delim"="file",
  "read_excel"="path",
  "st_read"="dsn",
  "read_sf"="dsn",
  "load"="file",
  "loadWorkbook"="file",
  "readRDS"="file",
  "read_fst"="path"
)


get_code = function(filepath,dirpath="../sidep/"){
  
  code = readLines(file.path(dirpath,filepath),encoding = "UTF-8")
  code = gsub("#.*","",code)#rm comments
  code = code[code!=""]
  
  code
  
}

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

get_inline_reader_pattern = function(one_reader){
  paste0("((^)|( )|(::)|(,)|(\\())",one_reader,"((\\()|(\\))|(,)|( )|($))")
}

get_input_one_call = function(one_call,one_reader,file_arg){
  print(substr(one_call,1,50))
  ee = parse(text=one_call)[[1]]
  eefun <- ee[[1]]
  inline_reader_pattern = get_inline_reader_pattern(one_reader)#la fonction peut être invoquée avec une parenthèse mais aussi dans un lapply, map... et donc être suivie d'un espace, virgule, parenthèse fermante...
  i=0
  x = as.list(ee)[[1]]
  while(eefun != one_reader){
    index = which(sapply(ee,function(eee){length(grep(pattern=inline_reader_pattern,eee))>0}))
    
    if(any(index==1)){
      eefun = one_reader
      
    } else {
      eeprev = ee
      ee = lapply(ee,function(eee)if(length(grep(pattern=inline_reader_pattern,eee))>0)eee else NULL)
      ee = purrr::compact(ee)
      
      ee = parse(text = ee)[[1]]
      if(ee!=one_reader){
        eefun = ee[[1]]
      } else {
        eefun = ee
        ee = eeprev
      }
    }
  }
  
  
  assertthat::assert_that(eefun == one_reader,msg="On devrait avoir trouvé notre reader")
  cc <- match.call(match.fun(eefun), ee)
  cc_list = as.list(cc)
  if(!file_arg %in% names(cc_list) && eefun=="safe_fread"){
    print("arg is not parsed for safe_fread => hardcoded to 1st arg")
    iopath = cc_list[[2]]
  } else {
    iopath = cc_list[[file_arg]]
  }
  parsed_fun = cc_list[[1]]
  print(parsed_fun)
  if(parsed_fun=="::"){
    print("package::fun not handled")
    iopath=NULL
  } else if ((ee[[2]] == one_reader) & ((ee[[1]] == "<-") | (ee[[1]] == "="))) {
    print("this is the reader definition")
    iopath=NULL
  } else {
    if(is.null(iopath))browser()
    try({
      iopath <- eval(iopath)
    },silent = T)
    
    iopath = deparse(iopath)
  }
  iopath
  
}
# filepath = sample(codes_path,1)
# filepath="src/01_Nettoyage_sidep_pcr - catch warning.R"
print(filepath)
tmp = get_inputs(filepath,reader_funs=reader_funs)




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

get_inputs_one_reader = function(one_reader,file_arg,filepath,dirpath="../sidep/"){
  # one_reader = "read.xlsx"
  parsed_code = parse(file(file.path(dirpath,filepath),encoding = "UTF-8"))
  parsed_code = purrr::map(parsed_code,get_inside_fun_def)
  code = unlist(parsed_code)
  
  inline_reader_pattern = get_inline_reader_pattern(one_reader)#la fonction peut être invoquée avec une parenthèse mais aussi dans un lapply, map... et donc être suivie d'un espace, virgule, parenthèse fermante...
  
  code = grep(inline_reader_pattern,code,value=T)
  # code = grep(one_reader,code,value=T)
  
  print(sprintf("%s expressions trouvées qui utilisent le reader %s",length(code),one_reader))
  
  inputs_one_reader = sapply(code,get_input_one_call,one_reader=one_reader,file_arg=file_arg)
  
  inputs_one_reader = unname(inputs_one_reader)
  
  
}

get_inputs_i = function(i,reader_funs,filepath=filepath,dirpath=dirpath){
  reader = names(reader_funs[i])
  file_arg = unname(reader_funs[i])
  get_inputs_one_reader(one_reader = reader,file_arg = file_arg,filepath=filepath,dirpath=dirpath)
  
}



guess_readers = function(filepath,dirpath="../sidep"){
  src = get_code(filepath,dirpath)
  readers = unique(stringr::str_extract(src,"([A-z0-9_\\.]*)read([A-z0-9_\\.]*)\\("))
  readers = readers[!is.na(readers)]
  readers
}


get_inputs = function(filepath,reader_funs,
                      dirpath="../sidep") {
  
  print("guess readers in file")
  guessed_readers = guess_readers(filepath,dirpath)
  print(guessed_readers)
  
  inputs = sapply(1:length(reader_funs),get_inputs_i,filepath=filepath,dirpath=dirpath,reader_funs=reader_funs)
  names(inputs) <- names(reader_funs)
  inputs = unlist(inputs)
  if(length(inputs)>0){
    res = data.table(parent = filepath,child = inputs,group="input",fun=names(inputs))
  } else {
    res = NULL
  }
  list(guessed_readers = guessed_readers,res=res)
}


code_architecture = get_all_sources(verbose=T)
dt = code_architecture

codes_path = unique(c(code_architecture$parent,code_architecture$child))

#### FIND VARS DEFINITIONS ###

get_var_def_one_script = function(script,my_var,dirpath){
  # f = "../sidep/src/01_Nettoyage_sidep_pcr - catch warning.R"
  print(script)
  f = file(file.path(dirpath,script),encoding="UTF-8")
  sc = CodeDepends::readScript(f)
  vars_definitions = sc@.Data
  vars_definitions = purrr::keep(vars_definitions, function(x) {
    if(length(x)>1){
      x[[2]] == my_var
    } else F
  })
  vars_definitions
}

get_var_def = function(scripts,my_var,dirpath="../sidep",env=parent.frame()){
  res = lapply(scripts,get_var_def_one_script,my_var=my_var,dirpath=dirpath)
  names(res) <- scripts
  res = purrr::compact(res)
  
  if(length(unique(unlist(res)))==1){
    res = res[[1]][[1]]
    eval(res,envir = env)
  } else {
    stop("Plusieurs définitions différentes, il faut choisir la bonne !")
  }
  invisible(res)
}


my_var="safe_fread"
get_var_def(scripts = codes_path,my_var = my_var)



filepath = sample(codes_path,1)

####### tmp ########
tmp = get_inputs(filepath,reader_funs=reader_funs)

inputs = lapply(codes_path,get_inputs,reader_funs=reader_funs)

guessed_readers = lapply(inputs,function(x)x$guessed_readers)
guessed_readers = unlist(guessed_readers)
guessed_readers = unique(guessed_readers)
guessed_readers


all_inputs = rbindlist(lapply(inputs,function(x)x$res))


create_network = function(dt){
  assertthat::assert_that(all(c("parent","child")%in%names(dt)))
  nodes = data.table(title = unique(c(dt$parent,dt$child)))
  nodes$id = 1:nrow(nodes)
  edges = copy(dt)
  edges[nodes,from:=i.id,on=c("parent"="title")]
  edges[nodes,to:=i.id,on=c("child"="title")]
  visNetwork(nodes, edges, width = "100%")
}



# UNNEST THE WHOLE CODE =>
# LOOK FOR DEFINITION OF THE OBJECT WE MISS =>
# GET THE LAST BEFORE SOURCE/I/O BECAUSE IT MAY CHANGE

