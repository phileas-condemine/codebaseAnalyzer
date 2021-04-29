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
  "loadWorkbook"="file",
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

code_architecture = get_all_sources(verbose=T)

codes_path = unique(c(code_architecture$parent,code_architecture$child))

my_var="safe_fread"
get_var_def(scripts = codes_path,my_var = my_var)


unnest_source(destfile = "full_code.R")

funs_defs = find_funs_defs()

filepath = sample(codes_path,1)

####### tmp ########
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
  "date_dataset" = Sys.Date()
  # "fichier_source"=sprintf("data/verifs/%s_sidep_par_patient(_bis).xlsx",format(Sys.Date(),format="%Y%m%d"))
  # "type_test"=c("pcr","sero","antigenique","antigen-complet","pcr_complet","pcr_respi","pcr_sal_mil","pcr_sal","pcr_mil"),
  # "type_date"=c("prelev","valid")
)

filepath = "src/utils/exporter_tableau_punaise.R"
# debugonce(get_io)
tmp = get_io(filepath,io_funs=reader_funs,missing_vars=missing_vars,mode = "read",funs_defs=funs_defs)

inputs = lapply(codes_path,get_io,io_funs=reader_funs,missing_vars=missing_vars,mode = "read",funs_defs=funs_defs)
names(inputs) <- codes_path



guessed_readers = lapply(inputs,function(x)x$guessed_io_fun)
guessed_readers = unlist(guessed_readers)
guessed_readers = unique(guessed_readers)
guessed_readers

all_inputs = rbindlist(lapply(inputs,function(x)x$res))
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
  data.table("raw"="paste0(path_sortie, date_AAAAMMJJ, \"_sidep_pcr_\", month[i], \".csv\")","std"="sidep_pcr.csv"),
  data.table("raw"="/data1/sidep_bases_clean/20210429_sidep_pcr_0.csv","std"="sidep_pcr.csv"),
  data.table("raw"="paste0(path_data, extract[\"MED4\"])","std"="sidep_daily.csv"),
  data.table("raw"="paste0(path_data, extract[\"MED2\"])","std"="sidep_daily.csv"),
  data.table("raw"="today_files_path","std"="sidep_daily.csv"),
  data.table("raw"="mini_corres_files","std"="rattrapageP1P2"),
  data.table("raw"=grep("_corresp_pseudos\\.csv",all_outputs$child,value=T),"std"="rattrapageP1P2"),
  data.table("raw"=grep("_sidep_pcr_0\\.csv",all_inputs$child,value=T),"std"="sidep_pcr.csv")
))


dedup_expr$raw %in% all_outputs$child
uniqueN(all_outputs$child)
dedup_expr$raw %in% all_inputs$child
uniqueN(all_inputs$child)

all_inputs[dedup_expr,child:=i.std,on=c("child"="raw")]
all_outputs[dedup_expr,child:=i.std,on=c("child"="raw")]


all_missing_vars = readLines("all_missing_vars.txt")
all_missing_vars = unique(all_missing_vars)
all_missing_vars

# il y a un pb avec les saveWorkbook dans nb_tests & export_punaise, peut-être aussi quand il y a référence au pkg::fun openxlsx::saveWorkbook


# create_network(rbindlist(list(all_inputs,code_architecture),fill = T,use.names = T))
v <- create_network(dt = rbindlist(
  list(all_inputs, code_architecture, all_outputs),
  fill = T,
  use.names = T
)) %>% visOptions(
  selectedBy = "group",
  highlightNearest = TRUE,
  nodesIdSelection = TRUE
) %>%
  visHierarchicalLayout()

v <- v %>%
  visInteraction(hover = T) %>%
  visEvents(hoverNode  = "function(e){
            var label_info = this.body.data.nodes.get({
              fields: ['label', 'label_long'],
              filter: function (item) {
                return item.id === e.node
              },
              returnType :'Array'
            });
            this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
            }") %>% 
  visEvents(blurNode  = "function(e){
            var label_info = this.body.data.nodes.get({
              fields: ['label', 'label_long'],
              filter: function (item) {
                return item.id === e.node
              },
              returnType :'Array'
            });
            this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
            }")

v

visNetwork::visSave(v,"network_prod_sidep.html")

