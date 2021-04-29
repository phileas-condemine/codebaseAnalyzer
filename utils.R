
error_get_missing_var = function(e, env, missing_vars, verbose = 0,funs_defs) {
  
  curr_script = get("curr_script",envir=env)
  dirpath = get("dirpath",envir=env)
  one_call = get("one_call",envir=env)
  # print(e)
  # print("error ! get missing var ?")
  msg = e$message
  msg = grep("^objet .* introuvable$",msg,value = T)
  if(length(msg)>0){
    missing_var = gsub("(^objet ')|(' introuvable$)","",msg)
    write(missing_var,file = "all_missing_vars.txt",append = T)
    
    message(sprintf("search missing var : %s",missing_var))
    # print("look for definition in the entire codebase")
    res_list = get_var_def(
      scripts = codes_path,
      my_var = missing_var,
      env = env,
      takelast = F,
      missing_vars = missing_vars,
      verbose = verbose >0,
      funs_defs=funs_defs
    )
    get_def = res_list[['def']]
    pos_non_autoref = res_list[['pos_non_autoref']]
    # browser()
    if(!is.null(get_def)){#multiple definitions found !
      if(length(get_def)>1 & pos_non_autoref > 0){
        # browser()
        # print("get near-call definition.")
        # n = find_one_call_line(one_call,file.path(dirpath,curr_script))
        n = find_one_call_line(one_call,"full_code_unnest_fun_calls.R")
        # if(!(length(n)==1 && is.numeric(n)))browser()
        
        unnest_source(final_script = curr_script,dirpath = dirpath,
                      n = n,#a priori on prendrait n-1 mais comme le reader peut être à l'intérieur d'une expression {} il faut récupérer tout le contenu car la définition de la variable peut se trouver dans l'expression
                      destfile = "tmp.R",verbose = verbose>1)
        unnest_funs_in_script(filepath = "tmp.R",
                    destfile = "tmp.R",
                    funs_defs = funs_defs)
        n = find_one_call_line(one_call,"tmp.R")
        
        # message(missing_var)
        res_list = get_var_def(
          scripts = "tmp.R",
          my_var = missing_var,
          env = env,
          dirpath = ".",
          takelast = T,
          missing_vars = missing_vars,
          verbose = verbose>0,
          funs_defs=funs_defs
        )
      } else if (length(get_def) == 1 | pos_non_autoref == 0) {
        # eval(get_def[[1]],envir = parent.frame())
        # tryCatch({
        missing_var_value <-  get(missing_var, envir = env)
        # }, error = QUE FAUT-IL METTRE ICI ?#function(e)error_get_missing_var(e,env=env,missing_vars=missing_vars,verbose=verbose) ?
        # function(e) {
        #   print("oups, couldn't get the missing var.")
        #   browser()
        # }
        # )
        if(length(missing_var_value)==0){
          print(e)
          assign(missing_var,missing_var,envir=env)#X = "X" so that we know there was a problem !
          print("couldn't find any value. This may happen with list.files() if not in the prod workdir.")
        } else if (length(missing_var_value==1)){
          print(missing_var_value)
          print("FIXED : missing_var is a vector of length 1 !")
          # browser()
        } else if (length(missing_var_value)>1){
          print(missing_var_value)
          print("multi-values vector, need to be handled properly !")
          browser()
        }
      }
      
    } 
  } else {
    assign("had_missing_var_error",F,envir = env)
  }
  
}



get_code = function(filepath,dirpath="../sidep/",n=-1L,verbose=F){
  assertthat::assert_that(length(n)==1 && is.numeric(n),msg=sprintf("n devrait être un entier. A la place n vaut %s.",n))
  code = readLines(file.path(dirpath,filepath),encoding = "UTF-8",n = n,warn = verbose)
  if(n>0){
    parsable = F
  } else {
    parsable= T
  }
  n_init = n
  while(!parsable){
    # print(n)
    code = readLines(file.path(dirpath,filepath),encoding = "UTF-8",n = n,warn = verbose)
    writeLines(text = c(code, ""),#final empty line
               con = file("tmp.R", encoding = "UTF-8"))
    res = NULL
    try({
      res = parse(file = file("tmp.R",encoding = "UTF-8"))
    },silent = T)
    
    if(is.null(res)){
      n = n+1
      # print(n)
    } else {
      parsable = T
    }
    
  }
  if(n>0 && n!=n_init)print(sprintf("stopped at line %s instead of %s.",n,n_init))
  
  # code = gsub("#.*","",code)#Too simple : # can be quoted in 02a
  code = gsub("^( )*#.*","",code)#rm clean comments on separate lines.
  code = code[code!=""]
  
  code
  
}




#### FIND VARS DEFINITIONS ###

base_types = c("name","character","numeric","logical","integer")

get_var_def_one_call = function(one_call,my_var,verbose=F,funs_defs){
  
  if(length(one_call) > 1){
    if(((one_call[[1]] == "=") | (one_call[[1]] == "<-") | (one_call[[1]] == "<<-")) && (one_call[[2]] == my_var)){
      if(verbose)print("found one definition !")
      
      return(one_call)
    }
    
    if((one_call[[1]] == "for") && (one_call[[2]] == my_var)){
      if(verbose)print("found one \"for\" definition !")
      
      res1 = parse(text = paste0(one_call[[2]]," = ",one_call[[3]]))#rebuild for(x in X) as x = X 
      
      sub_item = one_call[[4]]
      if(class(sub_item)=="{"){
        sub_item = sub_item[-1]
        sub_item = sapply(1:length(sub_item), function(i) {
          sub_item[[i]]
        })
      }
      res <- lapply(sub_item,get_var_def_one_call,my_var=my_var,verbose=verbose,funs_defs=funs_defs)
      
      return(list(res1,res))
    }
    
    if(!is.null(funs_defs) && (class(one_call)=="call" & as.character(one_call[[1]]) %in% names(funs_defs))){
      new_item = deeply_unnest_fun_call(one_call,funs_defs=funs_defs)
      res <- lapply(new_item,get_var_def_one_call,my_var=my_var,verbose=verbose,funs_defs=funs_defs)
      return(res)
    }
  }
  
  
  
  
  i=0
  i=i+1
  lapply(1:length(one_call),function(i){
    if(verbose){
      print(one_call)
      print(i)
      print(length(one_call))
    }
    if(rlang::is_symbol(one_call) & length(one_call) == 1){
      sub_item = one_call
    } else {
      sub_item = one_call[[i]]
    }
    
    if(rlang::is_missing(sub_item) || is.null(sub_item) || class(sub_item)%in%base_types){
      if(verbose)print("cas de base")
      res = NULL
    } else if (class(sub_item) %in% c("call","=","<-")){
      if(verbose){
        print("call case")
        print(length(sub_item))
        print(sapply(sub_item,class))
        print(class(sub_item))
      }
      
      
      tryCatch({
        res <- get_var_def_one_call(sub_item,my_var = my_var,verbose=verbose,funs_defs=funs_defs)
      },error=function(e){browser()})
      
      
      
      
    } else if(class(sub_item)=="{"){
      if(verbose)print("bracket case")
      # browser()
      sub_item = sub_item[-1]
      sub_item = sapply(1:length(sub_item), function(i) {
        sub_item[[i]]
      })
      
      # res = c()
      # for (j in 1:length(sub_item)){
      #   res = c(res,get_var_def_one_call(sub_item[[j]],my_var = my_var))
      # }
      tryCatch({
        res <- lapply(sub_item,get_var_def_one_call,my_var=my_var,verbose=verbose,funs_defs=funs_defs)
      },error=function(e){browser()})
      
      
    } else {
      if(verbose){
        print("else case")
        print(length(sub_item))
        print(class(sub_item))
        print(sapply(sub_item,class))
      }
      
      tryCatch({
        res <- get_var_def_one_call(sub_item,my_var = my_var,verbose=verbose,funs_defs=funs_defs)
        # res <- lapply(sub_item,get_var_def_one_call,my_var=my_var)
      },error=function(e){browser()})
      
      
    }
    return(unlist(res))
  })
}

get_var_def_one_script = function(script,my_var,dirpath,verbose=F,funs_defs){
  # f = "../sidep/src/01_Nettoyage_sidep_pcr - catch warning.R"
  if(verbose)print(script)
  
  #Double tentative de lecture : d'abord avec le nom du fichier puis avec une connexion UTF-8
  
  OK = F
  f = file.path(dirpath,script)
  tryCatch({
    sc <- parse(file = f)
    OK = T
  },
  error=function(e){
    print(sprintf("couldn't parse file %s with file.path",script))
    # print(length(readLines(f)))
    # browser()
  })
  if(!OK){
    f = file(file.path(dirpath,script),encoding="UTF-8")
    tryCatch({
      sc <- parse(file = f)
      OK = T
    },
    error=function(e){
      print(sprintf("couldn't parse file %s with file(file.path,UTF8)",script))
      print(length(readLines(f)))
      browser()
    })
  }
  
  
  vars_definitions = lapply(sc,get_var_def_one_call,my_var=my_var,verbose=verbose,funs_defs=funs_defs)
  vars_definitions = purrr::compact(vars_definitions)
  vars_definitions
}

get_var_def = function(scripts,
                       my_var,
                       dirpath = "../sidep",
                       env = parent.frame(),
                       takelast = T,
                       verbose = 0,
                       missing_vars = list(),
                       funs_defs=NULL) {
  res = lapply(scripts,get_var_def_one_script,my_var=my_var,dirpath=dirpath,verbose=verbose>1,funs_defs=funs_defs)
  names(res) <- scripts
  res = unlist(res)
  
  if(length(missing_vars)>0){
    print(sprintf("Loading vars %s",paste(names(missing_vars),collapse=", ")))
    list2env(missing_vars,envir = environment())
  }
  
  print(sprintf("Found %s definition for %s",length(unique(res)),my_var))
  if(length(unique(unlist(res))) >= 1 ){
    
    # if(my_var=="liste_fichiers")browser()
    
    # Attention aux définitions de X qui nécessitent une initialisation (X<-f(X))
    # => on remonte à la première définition qui ne dépend pas de X et on fait tourner les suivantes.
    # Si on a dû remonter jusqu'à la première, alors on bypass le param takelast 
    
    res
    pos_non_autoref = length(res)
    # pos_non_autoref = pos_non_autoref-1
    need_self = 1
    while(pos_non_autoref>0 & need_self == 1){
      my_expr = res[[pos_non_autoref]]
      here = environment()
      tryCatch({
        eval(my_expr)
        need_self = -1
      },error=function(e){
        msg = e$message
        msg = grep("^objet .* introuvable$",msg,value = T)
        if(length(msg)>0){
          missing_var = gsub("(^objet ')|(' introuvable$)","",msg)
          # browser()
          if(missing_var != my_var){
            message(sprintf("Need %s to eval %s",missing_var,my_var))
            assign("need_self",0,envir = here)
          }
        }
      })
      pos_non_autoref = pos_non_autoref-1
    }
    
    if(pos_non_autoref==0 & need_self == 1){
      print("on n'a pas trouvé de définition sans auto-référence")
      browser()
    } 
    
    
    if(takelast | pos_non_autoref==0){
      res = res[(pos_non_autoref+1):length(res)]
    }
    if(length(unique(res))==1){
      res = res[1]
    }
    
    
    if(takelast | pos_non_autoref==0 | length(res)==1){
      for(k in 1:length(res)){
        print(sprintf("Evaluation de l'expression %s / %s pour récupérer la variable %s.",k,length(res),my_var))
        had_error = T
        iter = 0
        while(iter < 5 && had_error){#can handle 5 params at most
          print(sprintf("Trial n° %s to get the actual path",iter))
          
          if(iter == 4){
            print(k)
            print(res[[k]])
            browser()
            debugonce(error_get_missing_var)
            debugonce(get_var_def)
          }
          
          tryCatch({
            eval(res[[k]])
            assign(my_var,get(my_var),envir = env)
            had_error = F
            
          }, 
          error = function(e)error_get_missing_var(e,env=env,missing_vars=missing_vars,verbose=verbose,funs_defs=funs_defs)
          )
          
          if(had_error){
            tryCatch({
              print(sprintf("try again to evaluate var %s",my_var))
              eval(res[[k]],envir = env)#this time the vars should be available in env !
              assign(my_var,get(my_var,envir = env),envir = here)#get it from the env to here
              had_error = F
            }, 
            error = function(e)error_get_missing_var(e,env=env,missing_vars=missing_vars,verbose=verbose,funs_defs=funs_defs)
            )
          }
          
          
          iter = iter+1
        }
        
        
        
      }
      if(verbose>1)print("=> done !")
      
    } else {
      if(verbose>1)print("Plusieurs définitions différentes, il faut choisir la bonne ! Choisir takelast = TRUE pour utiliser la dernière définition.")
    }
    # browser()
  } else {
    res=NULL
    browser()
    message(sprintf("Aucune définition trouvée pour %s",my_var))
    def = paste(text=paste0(my_var," = \"",my_var,"\""))
    eval(def,envir=env)
    # browser()
  } 
  invisible(list(def=res,pos_non_autoref=pos_non_autoref))
}


get_inline_io_fun_pattern = function(io_fun){
  paste0("((^)|( )|(::)|(,)|(\\())",io_fun,"((\\()|(\\))|(,)|( )|($))")
}


standardize_code = function(code){
  code = gsub(" ","",code)
  # code = gsub("\n","",code)
  code
}

# AJOUTER UN CONCEPT SIMILAIRE AUX SOURCES : FUNCTIONS AVEC DISTINCTION ENTRE DEFINITION ET APPEL
# 
# SOURCE1 -> SOURCE2 -> DEF/FUN1
# SOURCE1 <- CALL/FUN1
# FUN1 -> WRITE FILE1
# FUN1 <- READ FILE0
# SINON CA N'A PAS D'INTERET CAR TOUT SE PASSE DANS 00_actu

REWRITE GET_VAR_DEF WITH THE SAME PARSING APPROACH !

find_one_call_line = function(one_call,curr_script){
  code = readLines(curr_script,encoding = "UTF-8")
  std_call = standardize_code(one_call)
  std_call = strsplit(std_call,split = "\n",fixed = T)[[1]]
  code = standardize_code(code)
  res = grep(std_call[1],code,fixed = T)
  # if(length(res) == 0){
  #   matching_call = std_call[1]
  #   # compact_code = paste(code,collapse="")
  #   # res = grep(matching_call,compact_code,fixed = T)
  #   size_expr = nchar(matching_call)
  #   print(size_expr)
  #   while(length(res)==0 && size_expr>10){
  #     size_expr = size_expr-1
  #     matching_call = substr(matching_call,1,size_expr)
  #     res = grep(matching_call,code,fixed = T)
  #   }
  #   if(length(res)>0){
  #     print(sprintf("On a finalement réduit la première ligne du call aux %s premiers chars.",size_expr))
  #   }
  #   
  # } 
  if(length(res) == 0){
    print("on n'a pas réussi à retrouver one_call dans curr_script...")
    print(one_call)
    print(curr_script)
    browser()
    
  } else {
    if(length(std_call)==1 & length(res) == 1){
      #cas facile
      res
    } else if(length(res) == 1) {
      print("pour bien faire il faut vérifier que les lignes suivantes matchent !")
      res
    } else if (length(std_call) == 1){
      print("Plusieurs candidats mais on a déjà matché au mieux donc on prend la première expression. Cette méthode peut posser problème si la variable est definie/filtrée/nettoyée en plusieurs étapes !")
      res = res[1]
    } else {
      print("ici une erreur est probable, il vaut mieux affiner en matchant le one_call complet.")
      call_compact = paste(std_call,collapse="")
      ok_candidates = c()
      i=0
      i=i+1
      for (i in 1:length(res)){
        if (i == length(res)){
          endline = length(code)
        } else {
          endline = res[i+1]-1L
        }
        startline = res[i]
        
        sub_code_compact = paste(code[startline:endline],collapse="")
        
        if(grepl(call_compact,sub_code_compact,fixed = T)){
          ok_candidates = c(ok_candidates,res[i])
        }
        
      }
      
      if(length(ok_candidates)==0){
        print("aucun ne matche...")
        browser()
      } else if (length(ok_candidates)==1){
        print("on a trouvé le bon !")
        res = res[ok_candidates]
      } else {
        print("il y a toujours plusieurs candidats !")
        browser()
      }
      
      # code[res:(res+length(std_call)-1)]
      # res = res[1]
    }
  }
  res
}



