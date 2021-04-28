# options(error=browser)

library(data.table)
library(visNetwork)
library(readxl)
# FIND DEPENDENCIES AND LOAD THE PKG TO BE ABLE TO "match.call" & "match.fun"
pkgs = readLines("../sidep/src/00_Actualisation_pcr_sero.R",encoding = "UTF-8")
pkgs = pkgs[grepl("^library\\(",pkgs)]
sapply(pkgs, function(x) {
  eval(parse(text = x))
})

source("utils.R",local = T,encoding = "UTF-8")
source("sources.R",local = T,encoding = "UTF-8")
source("inputs.R",local = T,encoding = "UTF-8")
source("outputs.R",local = T,encoding = "UTF-8")
source("viz.R",local = T,encoding = "UTF-8")


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
  "read_fst"="path",
  "yaml.load_file"="input"
)

writer_funs = c(
  "fwrite"="file",
  "writeLines"="con",
  "saveWorkbook"="file",
  "ggsave"="filename",
  "save"="file",
  "saveRDS"="file",
  "write.xlsx"="file"
  
)

code_architecture = get_all_sources(verbose=T)

codes_path = unique(c(code_architecture$parent,code_architecture$child))

my_var="safe_fread"
get_var_def(scripts = codes_path,my_var = my_var)

dt = code_architecture

full_unnested_code = unnest_source()

filepath = sample(codes_path,1)

####### tmp ########
missing_vars = list(
  "path" = "",
  "path_sortie" = "/data1/sidep_bases_clean/",
  "path_T" = "/data1/sidep_bases_clean/",
  "path_data" = "/data1/sidep_brut/",
  "path_sorties_agregees" = "data/sorties/",
  "date_MMJJ"=format(Sys.Date(),format="%m%d"),
  "fichier_source"=sprintf("data/verifs/%s_sidep_par_patient(_bis).xlsx",format(Sys.Date(),format="%Y%m%d"))
  # "type_test"=c("pcr","sero","antigenique","antigen-complet","pcr_complet","pcr_respi","pcr_sal_mil","pcr_sal","pcr_mil"),
  # "type_date"=c("prelev","valid")
)

tmp = get_io(filepath,io_funs=reader_funs,missing_vars=missing_vars,mode = "read")

inputs = lapply(codes_path,get_io,io_funs=reader_funs,missing_vars=missing_vars,mode = "read")
names(inputs) <- codes_path

all_missing_vars = readLines("all_missing_vars.txt")
all_missing_vars = unique(all_missing_vars)


guessed_readers = lapply(inputs,function(x)x$guessed_io_fun)
guessed_readers = unlist(guessed_readers)
guessed_readers = unique(guessed_readers)
guessed_readers

all_inputs = rbindlist(lapply(inputs,function(x)x$res))


tmp = get_io(filepath,io_funs=writer_funs,missing_vars=missing_vars,mode = "write")



create_network(all_inputs)

create_network(rbindlist(list(all_inputs,dt),fill = T,use.names = T))

create_network(rbindlist(list(all_inputs, dt), fill = T, use.names = T)) %>% visOptions(
  highlightNearest =  list(
    enabled = TRUE
    # ,algorithm = "hierarchical"
  )
  # collapse = TRUE
)


tmp = get_io(filepath,io_funs=reader_funs,missing_vars=missing_vars,mode = "read")

# UNNEST THE WHOLE CODE =>
# LOOK FOR DEFINITION OF THE OBJECT WE MISS =>
# GET THE LAST BEFORE SOURCE/I/O BECAUSE IT MAY CHANGE

