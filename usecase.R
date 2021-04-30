# options(error=browser)

library(data.table)
library(visNetwork)
library(readxl)
library(yaml)
library(htmlwidgets)
# FIND DEPENDENCIES AND LOAD THE PKG TO BE ABLE TO "match.call" & "match.fun"
pkgs = readLines("../sidep/src/00_Actualisation_pcr_sero.R",encoding = "UTF-8")
pkgs = pkgs[grepl("^library\\(",pkgs)]
sapply(pkgs, function(x) {
  eval(parse(text = x))
})

source("utils.R",local = T,encoding = "UTF-8")
source("sources.R",local = T,encoding = "UTF-8")
source("inputs.R",local = T,encoding = "UTF-8")
source("functions.R",local = T,encoding = "UTF-8")
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
  # "loadWorkbook"="file",
  "readRDS"="file",
  "read_fst"="path",
  "yaml.load_file"="input"
)

writer_funs = c(
  "fwrite"="file",
  "writeLines"="con",
  "saveWorkbook"="file",#attention writeData ajoute des onglets au wb mais n'écrit pas le fichier sur disque !
  # "ggsave"="filename",
  "save"="file",
  "saveRDS"="file",
  "write.xlsx"="file",
  "saveWidget"="file",
  "write.csv"="file",
  "write.table"="file"
  
)

missing_vars = list(
  "path" = "",
  "path_sortie" = "/data1/sidep_bases_clean/",
  "path_T" = "/data1/sidep_bases_clean/",
  "path_data" = "/data1/sidep_brut/",
  "path_sorties_agregees" = "data/sorties/",
  "date_MMJJ"="Date_MMJJ",
  "date_AAAAMMJJ"=format(Sys.Date(),format="%Y%m%d"),
  "date_AAMMJJ"=substr(format(Sys.Date(),format="%Y%m%d"),3,8),
  "jour_date" = Sys.Date(),
  "date_dataset" = Sys.Date(),
  "jour" = Sys.Date()
  
  # "fichier_source"=sprintf("data/verifs/%s_sidep_par_patient(_bis).xlsx",format(Sys.Date(),format="%Y%m%d"))
  # "type_test"=c("pcr","sero","antigenique","antigen-complet","pcr_complet","pcr_respi","pcr_sal_mil","pcr_sal","pcr_mil"),
  # "type_date"=c("prelev","valid")
)
list2env(missing_vars,envir = environment())


io_funs  = c(reader_funs,writer_funs)

code_architecture = get_all_sources(verbose=T)

codes_path = unique(c(code_architecture$parent,code_architecture$child))

my_var="safe_fread"
get_var_def(scripts = codes_path,my_var = my_var)

unnest_source(destfile = "full_code.R")

funs_defs = find_funs_defs()

filepath = sample(codes_path,1)


custom_funs_def = rbindlist(lapply(codes_path,locate_custom_funs_defs),fill = T)[!is.na(child)]
custom_funs_call = rbindlist(lapply(codes_path,locate_funs_calls_in_script,names_funs_defs=names(funs_defs)),fill = T)[!is.na(child)]



# io_custom_funs = data.table(fun = names(io_custom_funs),io = unname(io_custom_funs))


function2output = function2io(writer_funs)
function2input = function2io(reader_funs)

function2output = rm_uneval_expr_unlist(function2output)
function2output = function2output[!child %in% c("fichier_source","fichier_destination")]
function2input = rm_uneval_expr_unlist(function2input)

function2input[,group_parent:="function"]
function2output[,group_parent:="function"]
function2input[,group_child:="input"]
function2output[,group_child:="output"]
function2input[,arrows:="from"]
function2output[,arrows:="to"]

code_architecture[,group_parent:="script"]
code_architecture[,group_child:="script"]
code_architecture[,arrows:="middle"]


# dt = rbindlist(list(code_architecture,custom_funs_def,custom_funs_call,function2input,function2output),fill=T)



####### tmp ########

filepath = "src/utils/exporter_tableau_punaise.R"
# debugonce(get_io)
tmp = get_io(filepath,io_funs=reader_funs,missing_vars=missing_vars,mode = "read",funs_defs=funs_defs)

inputs = lapply(codes_path,get_io,io_funs=reader_funs,missing_vars=missing_vars,mode = "read",funs_defs=funs_defs)
names(inputs) <- codes_path

class(inputs[[1]]$res$child)
purrr::keep(inputs,function(x)class(x$res$child)=="list")
inputs = purrr::discard(inputs,function(x)class(x$res$child)=="list")


guessed_readers = lapply(inputs,function(x)x$guessed_io_fun)
guessed_readers = unlist(guessed_readers)
guessed_readers = unique(guessed_readers)
guessed_readers


all_inputs = rbindlist(lapply(inputs,function(x)x$res))
all_inputs$child
create_network(all_inputs)

# filepath = "src/heatmaps/Heatmaps Sidep (TI, donnees Drees).R"
filepath = "src/00_Actualisation_pcr_sero.R"
# debugonce(get_io)
tmp = get_io(filepath,io_funs=writer_funs,missing_vars=missing_vars,mode = "write",funs_defs=funs_defs)
outputs = lapply(codes_path,get_io,io_funs=writer_funs,missing_vars=missing_vars,mode = "write",funs_defs=funs_defs)

guessed_writers = lapply(outputs,function(x)x$guessed_io_fun)
guessed_writers = unlist(guessed_writers)
guessed_writers = unique(guessed_writers)
guessed_writers
all_outputs = rbindlist(lapply(outputs,function(x)x$res))
all_outputs$child
create_network(all_outputs)

dedup_expr = rbindlist(list(
  data.table("raw"=grep("_sidep_pcr_\\.csv",all_outputs$child,value=T),"std"="sidep_sero.csv"),
  data.table("raw"="paste0(path_sortie, date_AAAAMMJJ, \"_sidep_pcr_\", month[i], \".csv\")","std"="sidep_pcr.csv"),
  data.table("raw"="/data1/sidep_bases_clean/20210429_sidep_pcr_0.csv","std"="sidep_pcr.csv"),
  data.table("raw"="paste0(temp, i, .csv)","std"="sidep_pcr.csv"),
  data.table("raw"=grep("_sidep_sero",all_outputs$child,value=T),"std"="sidep_sero.csv"),
  data.table("raw"="path_zip_sero","std"="sidep_sero.csv"),
  data.table("raw"=grep("_sidep_pcr_salivaire",all_outputs$child,value=T),"std"="sidep_pcr_sal_mil.csv"),
  data.table("raw"="path_zip_pcr_sal_mil","std"="sidep_pcr_sal_mil.csv"),
  data.table("raw"=grep("_sidep_antigen",all_outputs$child,value=T),"std"="sidep_antigen.csv"),
  data.table("raw"="path_zip_antigen","std"="sidep_antigen.csv"),
  data.table("raw"="paste0(path_data, extract[\"MED4\"])","std"="sidep_daily.csv"),
  data.table("raw"="paste0(path_data, extract[\"MED2\"])","std"="sidep_daily.csv"),
  data.table("raw"="today_files_path","std"="sidep_daily.csv"),
  data.table("raw"="mini_corres_files","std"="rattrapageP1P2"),
  data.table("raw"=grep("_corresp_pseudos\\.csv",all_outputs$child,value=T),"std"="rattrapageP1P2"),
  data.table("raw"=grep("_sidep_pcr_0\\.csv",all_inputs$child,value=T),"std"="sidep_pcr.csv")
))
dedup_expr = dedup_expr[!is.na(raw)]

all_inputs[,group_parent:="script"]
all_inputs[,group_child:="input"]
all_inputs[,arrows:="from"]

all_outputs[,group_parent:="script"]
all_outputs[,group_child:="output"]
all_outputs[,arrows:="to"]


dt = rbindlist(list(code_architecture,custom_funs_def,custom_funs_call,function2input,function2output,all_inputs,all_outputs),fill=T)

dt[is.na(arrows)]
dt[is.na(group_child)]
dt[is.na(group_parent)]

dt[,child:=gsub("\"","",child)]
dt[,parent:=gsub("\"","",parent)]

dt[dedup_expr,child:=i.std,on=c("child"="raw")]
dt[dedup_expr,parent:=i.std,on=c("parent"="raw")]
# all_outputs[dedup_expr,child:=i.std,on=c("child"="raw")]


all_missing_vars = readLines("all_missing_vars.txt")
all_missing_vars = unique(all_missing_vars)
all_missing_vars

# il y a un pb avec les saveWorkbook dans nb_tests & export_punaise, peut-être aussi quand il y a référence au pkg::fun openxlsx::saveWorkbook


# create_network(rbindlist(list(all_inputs,code_architecture),fill = T,use.names = T))
table(dt$group_parent)
v <- create_network(dt = dt) %>% 
  visOptions(
  selectedBy = "group",
  highlightNearest = list(enabled =TRUE, degree = 1, hover = T),
  nodesIdSelection = TRUE
)  %>%
  visInteraction(hover = T,tooltipDelay = 50,tooltipStay = 1000) %>%
  visNodes(font=list("size"=0))
# %>%
#   visEvents(hoverNode  = "function(e){
#             var label_info = this.body.data.nodes.get({
#               fields: ['label', 'label_long'],
#               filter: function (item) {
#                 return item.id === e.node
#               },
#               returnType :'Array'
#             });
#             this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
#             }") %>% 
#   visEvents(blurNode  = "function(e){
#             var label_info = this.body.data.nodes.get({
#               fields: ['label', 'label_long'],
#               filter: function (item) {
#                 return item.id === e.node
#               },
#               returnType :'Array'
#             });
#             this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
#             }")

v
visNetwork::visSave(v,"network_prod_sidep_etoile.html")
# visConfigure(v,enabled = T)
v <- visHierarchicalLayout(v)

visNetwork::visSave(v,"network_prod_sidep_hierarchie.html")

