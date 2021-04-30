path_config <- "src/"
jour_date = Sys.Date()
date_AAAAMMJJ = format(jour_date,format="%Y%m%d")
date_AAMMJJ = substr(date_AAAAMMJJ,3,8)
annee <- lubridate::year(jour_date)
date_dataset = as.Date(date_AAAAMMJJ,format="%Y%m%d")
jour_prev_update = Sys.Date()-1
MoisAAAA = c("Mai2020","Juin2020","Juillet2020","Aout2020","Septembre2020",
          "Octobre2020", "Novembre2020", "Decembre2020", "Janvier2021", 
          "Fevrier2021", "Mars2021","Avril2021")
YYYY.mm = c("2020.5",  "2020.6",  "2020.7",  "2020.8",  
            "2020.9",  "2020.10", "2020.11", "2020.12", "2021.1", 
            "2021.2", "2021.3","2021.4")
date_vars = c("day_prelev","day_valid","day_extract_sidep")
posixct_vars = c("date_prelev","date_valid","date_valid_init")
char_vars = c("Pseudonyme", "FINESS_ET", 
              "FINESS_EJ",
              "TypologiePatient", "PremierSymptomes",
              "IRIS", "cat_etb", "Resultat",
              "departement", "CodePostal",
              "fi_init", "RPPSTraitant", 
              "RPPSPrescripteur",
              "ProfessionelSante",
              "reg","AnalyseConclusion","Sexe","TypeAnalyse",
              "Statut","valeur","cat_pp","nom_reg",
              "CodePays", "CPAdresseTemporaire", 
              "FINESSGeographique", 
              "NumDepartementAdresseTemporaire",
              "CodePaysAdresseTemporaire",
              "CodePaysVoyageEtranger",
              "NumCampagneDepistage")
daily_files_char_vars = c("Pseudonyme","Pseudo1","Pseudo2","Sexe",
                          "CodePostal","IRIS","NumDossier",
                          "TypologiePatient","ProfessionelSante","PremierSymptomes",
                          "FINESS","RPPSTraitant","RPPSPrescripteur",
                          "DatePrelevement","DateValidationCR","AnalyseConclusion",
                          "TypeAnalyse","Resultat","Statut",
                          "CodePays","CPAdresseTemporaire","NumDepartementAdresseTemporaire",
                          "CodePaysAdresseTemporaire","CodePaysVoyageEtranger","FINESSGeographique",
                          "NumCampagneDepistage","Joker1","Joker2","Joker3")
new_tag_char_vars = c("RPPSPrelevTA", "AdeliPrelevTA", "idNATPrelevTA", "CPPrelevTA")
run_expertise = T
JP2_fromJm1P1 = F
Jm1 = format(Sys.Date()-1,format="%Y%m%d")
check_collision_p1p2 = F
rewrite_fichier_variants = F
addP2 = T
tempo="j" #j ou we
metadata <- yaml::yaml.load_file(paste0(path_config,"config.yaml"))
for(i in names(metadata)){
  assign(i, metadata[[i]])
}
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table) # Version 1.12.0
library(openxlsx)
library(knitr)
library(rmarkdown)
library(kableExtra)
library(stringi)
library(bit64) # mieux vaut lister les package ici que dans les scripts sourcés
library(ISOweek)
library(lest) # ajout library pour case_when de data.table et non dplyr
library(parallel) # ajout pour parallelisation
library(fasttime)
library(fst)
library(ggplot2)
library(vroom)
get_new_rattrapage = function(){
  
  t1= Sys.time()
  
  new_rattrapage = fst::read_fst("/data1/sidep_bases_clean/numID/p1p2dedupFromRattrapageV2.fst")
  new_rattrapage = data.table(new_rattrapage)
  print(nrow(new_rattrapage))
  
  
  mini_corres_files = list.files("/data1/sidep_bases_clean/correspondance pseudos",full.names = T,pattern = "^[0-9]{4}_corresp_pseudos.csv$")
  mini_rattrapage = rbindlist(pbapply::pblapply(mini_corres_files,fread,select=c("Pseudo1","Pseudo2")))
  mini_rattrapage = unique(mini_rattrapage)
  
  fichier_du_jour = paste0(date_AAAAMMJJ,"_corresp_pseudos.csv")
  if(!fichier_du_jour %in% basename(mini_corres_files)){
    today_files_path = list.files(path_data,full.names = T,pattern=sprintf("CSV_DREES_MED[24]_%s",date_AAMMJJ))
    today_files = rbindlist(lapply(today_files_path, fread,  quote = "",select=c("Pseudo1","Pseudo2"),
                 verbose = F, sep = "|", header = TRUE, colClasses="character", encoding = "Latin-1"))
    mini_rattrapage = rbindlist(list(mini_rattrapage,today_files))
  }
  
  new_rattrapage = rbindlist(list(new_rattrapage,mini_rattrapage),use.names=T)
  new_rattrapage = unique(new_rattrapage)
  
  t2 = Sys.time()
  print(difftime(t2,t1,units="mins"))#2mins
  print(nrow(new_rattrapage))#
  doublonp2_to_p1 = new_rattrapage[,.(nb=.N),by="Pseudo1"][nb>1]
  print(sum(doublonp2_to_p1$nb) / nrow(new_rattrapage))#%
  print(nrow(doublonp2_to_p1) / nrow(new_rattrapage))#%
  new_rattrapage = new_rattrapage[,.SD[1],by="Pseudo1"]
  print(nrow(new_rattrapage))#
  setnames(new_rattrapage,"Pseudo1","Pseudonyme")
  print(nrow(new_rattrapage))#
  new_rattrapage
}
t1 = Sys.time()
new_rattrapage = get_new_rattrapage()
t2 = Sys.time()
print(sprintf("Le chargement du fichier de rattrapage a pris %s minutes.",difftime(t2,t1,units="mins")))#10mins
if(JP2_fromJm1P1){
  print("copy des fichiers P1 de la veille vers DEV")
  files_to_copy = list.files("/data1/sidep_bases_clean/",pattern=paste0("^",Jm1))
  one_file= sample(files_to_copy,1)
  for (one_file in files_to_copy){
    print(one_file)
    file.copy(from = paste0("/data1/sidep_bases_clean/",one_file),
              to = paste0("/data1/sidep_bases_clean/numID/",one_file),overwrite = T)
  }
}
if(run_expertise){
  render(input = paste0(path,"src/viz/expertise_donnees_du_jour_pcr_sero_ajout_pseudo2.Rmd"),
         output_format = "word_document",
         output_file = paste0(path, "data/sorties/expertise/", date_AAAAMMJJ, "_expertise_donnees_du_",
                              ifelse(tempo=='j','jour','week-end'),".docx"))
}
print("01 nettoyage SIDEP")
safe_fread = purrr::quietly(data.table::fread)
prep_data <- function(jour=jour_date, 
                      last_update=jour_prev_update,
                      MoisAAAA,
                      YYYY.mm,
                      day_init="2020-05-20",corres,addP2){
  
  
  
  
  TIME_INIT=Sys.time()
  print(paste0("lancement: ",TIME_INIT))
  
  date_AAAAMMJJ_last = format(last_update,format="%Y%m%d")
  date_AAMMJJ_last = substr(date_AAAAMMJJ_last,3,8)
  extractions = list.files(path_data)
  extractions = grep("CSV_DREES", extractions,value = T)
  extractions = grep(".csv", extractions,value = T)
  
  extractions_to_drop <- extractions%in%c("CSV_DREES_200518200002.csv",
                                          "CSV_DREES_200519090000.csv",
                                          "CSV_DREES_200520090000.csv",
                                          "CSV_DREES_200625130000.csv")
  
  if(any(extractions_to_drop)){extractions <- extractions[-which(extractions_to_drop)]}
  
  extractions <- extractions[!grepl("ini", extractions)] # avant le 1/9 : extractions[-which(grepl("ini", extractions))]
  extractions = sort(extractions, decreasing = T)
  names(extractions) <- substr(extractions,13,16)
  
  print("Chargement des bases journalières à agréger")
  extract = extractions[substr(extractions,16,21)==date_AAMMJJ]
  names(extract) <- substr(extract,11,14)
  
  sidep_add_1 = safe_fread(paste0(path_data, extract["MED4"]),  quote = "",
                                  verbose = F, sep = "|", header = TRUE, colClasses=list(
                                    character = daily_files_char_vars,
                                    numeric = "Age"), encoding = "Latin-1")
  print(extract["MED4"])
  
  w <- sidep_add_1$warnings
  w <- w[grepl("^Stopped early",w)]
  if(length(w)>0){stop(w)}
  sidep_add_1 <- sidep_add_1$result
  
  sidep_add_2 = safe_fread(paste0(path_data, extract["MED2"]), quote = "",
                                  verbose = F, sep = "|", header = TRUE, colClasses=list(
                                    character = daily_files_char_vars,
                                    numeric = "Age"), encoding = "Latin-1")
  print(extract["MED2"])
  
  w <- sidep_add_2$warnings
  w <- w[grepl("^Stopped early",w)]
  if(length(w)>0){stop(w)}
  sidep_add_2 <- sidep_add_2$result
  rm(w)
  
  sidep_add = rbind(sidep_add_1, sidep_add_2)
  rm(sidep_add_1, sidep_add_2)
  
  
  sidep_add$day_extract_sidep = as.Date(date_AAAAMMJJ,format="%Y%m%d")
  
  sidep_add[Pseudo2!="",Pseudo_diff:=as.numeric(Pseudo1!=Pseudo2)]
  
  stockage_pseudo=copy(sidep_add)
  stockage_pseudo[,Annee_prelev:=as.numeric(substr(DatePrelevement,1,4))]
  stockage_pseudo=stockage_pseudo[,c("Pseudo1","Pseudo2","Pseudo_diff","Sexe","Age","Annee_prelev","day_extract_sidep")]
  stockage_pseudo=unique(stockage_pseudo)
  
  
  data.table::fwrite(stockage_pseudo,
                     paste0(path_sortie, "correspondance pseudos/",date_AAAAMMJJ,'_corresp_pseudos.csv'),
                     quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
                     qmethod = c("escape"),dateTimeAs = "ISO", verbose = TRUE)
  
  rm(stockage_pseudo)
  
  
  
  setnames(sidep_add, "Pseudo2", "Pseudonyme")
  sidep_add[,Pseudo1:=NULL]
  print("Suppression des antislash et .br")
  
  sidep_add[, Joker1 := NULL]
  sidep_add[, Joker2 := NULL]
  sidep_add[, Joker3 := NULL]
  
  
  sidep_add$RPPSPrescripteur = stringr::str_extract(sidep_add$RPPSPrescripteur,'([0-9]{1,14})') # enlever les caracteres autres que les chiffres
  sidep_add$RPPSPrescripteur = as.integer64(sidep_add$RPPSPrescripteur)
  sidep_add[RPPSPrescripteur == 0, RPPSPrescripteur := NA]
  
  
  sidep_add$CodePostal = gsub('(\\\\)','', sidep_add$CodePostal) # enlever les antislash
  sidep_add$CodePostal = gsub('(.br)','', sidep_add$CodePostal) # enlever les .br
  
  nettoyage <- function(temp, element){
    for (i in colnames(temp)){
      print(i)
      print(temp[grep(element, get(i)), .N])
      if (temp[grep(element, get(i)), .N] > 0){
        idx <- grep(element, temp[[i]])
        temp[grep(element, get(i)), c(i) := list(stringr::str_replace_all(get(i), element,""))]
      }
      
    }
    return(temp)
  }
  sidep_add <- nettoyage(sidep_add, ";")
  sidep_add <- nettoyage(sidep_add, "\\\\")
  sidep_add <- nettoyage(sidep_add, '"')
  
  sidep_add$FINESS = stringr::str_extract(sidep_add$FINESS,'([0-9AB]{1,15})') 
  sidep_add[, FINESS := ifelse(FINESS == "" | nchar(FINESS) < 8, NA, FINESS)]
  
  
  sidep_add$FINESSGeographique = stringr::str_extract(sidep_add$FINESSGeographique,'([0-9AB]{1,15})') 
  sidep_add[, FINESSGeographique := 
              ifelse(FINESSGeographique == "" | nchar(FINESSGeographique) < 8, NA,
                     FINESSGeographique)]
  
  sidep_add[, regCampagneDepistage := stringr::str_extract(NumCampagneDepistage, "^[A-Za-z]{3}")]
  if (sidep_add[nchar(regCampagneDepistage)<3, .N]>0){
    print("Probleme: il y a des trigrammes dans NumCampagneDepistage qui ont moins de 3 caractères")
  }
  sidep_add[, regCampagneDepistage := NULL]
  
  print("Traitement des dates")
  
  sidep_add[, date_prelev := lubridate::ymd_hms(paste0(substr(DatePrelevement, 1,4), "-", 
                                                       substr(DatePrelevement, 5,6), "-",
                                                       substr(DatePrelevement, 7,8), " ",
                                                       substr(DatePrelevement, 9,10), ":", 
                                                       substr(DatePrelevement, 11,12), ":",
                                                       substr(DatePrelevement, 13,14)))]
  sidep_add[, day_prelev := as.Date(date_prelev)]
  
  sidep_add[, date_valid := lubridate::ymd_hms(paste0(substr(DateValidationCR, 1,4), "-", 
                                                      substr(DateValidationCR, 5,6), "-",
                                                      substr(DateValidationCR, 7,8), " ",
                                                      substr(DateValidationCR, 9,10), ":", 
                                                      substr(DateValidationCR, 11,12), ":",
                                                      substr(DateValidationCR, 13,14)))]
  sidep_add[, day_valid := as.Date(date_valid)]
  
  if(!is.na(day_init)){
    sidep_add = sidep_add[day_prelev >= as.Date(day_init)& day_prelev<=date_dataset]
    sidep_add = sidep_add[is.na(day_valid) | (!is.na(day_valid) & day_valid >= as.Date(day_init) 
                                              & day_valid <=date_dataset)]
  }
  
  sidep_add = sidep_add[day_prelev <= day_extract_sidep]
  
  
  
  
  print("Pseudonymes")
  if(sum(nchar(sidep_add$Pseudonyme,)>64, na.rm = T)!=0){
    sidep_add[nchar(Pseudonyme) %in% c(128, 192), Pseudonyme:= substr(Pseudonyme, 1, 64)]
  }
  
  pseudos_tests = c("78b14ef7c9ed19082b6d2d151678b8a74606f7dd33b811b3f97f41fe20793ea5",
                    "cf77f9f6fc3d4a40260af813aed7c3f80c274660aff9f978a99f98d29544be0c",
                    "e76950f02d9763a2533a4c6b0a3a0621e0dcfd3133c5fc3d1d0064b3324a3d50",
                    "ef9ecddf446243b60b9f15e8c7a521f79f21bfb2f95b0d6d54b4f3533f85a1d1",
                    "9fdcb52ac99ef7497ce75a543cf99afbb8c9af6f8dfd417f2a62b3dbca0ae23d",
                    "cd88cea5ff5a72eca228f63c9513492d45b5b78509e237ab55e0d787f896d747",
                    "c9d915880d6266a23ca95992a009a4491bbed5e3882fbd60b556a9df09dde4a0",
                    "aeaab01f88b19d53b7680a7fbc2a46fba31de13b2f60f75679ea2f971d8ce346",
                    "5161630ccba995deaa8d6ba5012ca521b0b245a9bd5e78658d6311ddffc0f8ec",
                    "4d07e44674b72d5c6ef58605058b671f0884394adf6535b194771325e709034f",
                    "a5c373e024776111dbb999506c28d41e4dca3011a41394493526bca3476136f0",
                    "9a4708216b2624f4461e6aa3b1181f03ecb3d4605d9d658ce6cc8fbf59a17557",
                    "8728ef871def06e09529733e60bebdde00e559a05da5bcf0b91cb13a67374564",
                    "2153039929b6bd83dd502e01d41cd4319e9674c8dedbe28d3d625e0102c5a450",
                    "03b8e61d5548edc7a9490ed86d2fbfcd8edc4746489e8543fdff64d5527e8ed8",
                    "efdaf8f12656c9b015c3410c4a8879b3c07c7b121493f5c2617df8d0db6b17ef",
                    "3563fc0e12e0e2187c0e1117849bcbc849c60afef9f807cc54b90a7e3e6bd818",
                    "4b1081eadac91e9bc974ac8cb2dd437e31dca5d2e88e086d5f68fdffa2efb0a2",
                    "2fed8eadd4765b47d5d034b1624ffc19f9f1a8dd6853887597039952d87489be",
                    "2b90416ea89cd21fe3ae4eb90ac3d8158afe87f4dc6ef43c257a37e857787107", 
                    "68d80449eb5960e1912a1043ee6f388fc75417d87e246e4402538d1ad577ef6a",# sidep_add III
                    "23c3e79e45865410393b5520d9841854427f435f272ddd32d365bb73af5570c2",
                    "5fb63a89cb4a0108ba7e4b5cea5b8a7cad48c64aa24c000f0d7cb49dddf26f43",
                    "7b7fbf4ff01a5920ea08159889b6d1a9ff03f1d26a50d30cd3b142a68e633079",
                    "3bc66709adac0bac1158bbdf7500eda1c370ef0132c65b861856ea677c405470")
  sidep_add = sidep_add[ !(Pseudonyme %in% pseudos_tests)]
  
  
  
  print("AnalyseConclusion")
  
  sidep_add = sidep_add[AnalyseConclusion %in% c("94500-6","94762-2","94563-4","94564-2",
                                                 "94309-2", "94845-5")]
  
  
  print("Doublons parfaits extractions")
  etapes = cbind(var="aucune", n_row = nrow(sidep_add))
  sidep_add <-  unique(sidep_add)
  etapes = rbind(etapes, cbind(var="Duplicates_all_lines", n_row = nrow(sidep_add)))
  
  
  print("Premiers symptômes")
  sidep_add[PremierSymptomes%in%c("","UU", "u",
                                  "non précisé", "Non communiqué",
                                  "NON RENSEIGNE", "non renseignÃ©", "nrens",
                                  "PAS RENSEIGNE", "NR", "Non renseigné",
                                  "NON PRECISE", "Non précisé",
                                  "non renseignée",
                                  "NRENS", "np", "NCNR",
                                  "NRE","NSP","NP","NM","V",
                                  "ne sais pas", "non fait",
                                  "VOIR PRESCRIPTION JOINTE",
                                  "NPC","INC","BILAN PREOP",
                                  "?","nc","nr","0.000000",'0R',
                                  "GAPS2","Demande annulée",
                                  "Non communiqué", "NCNR2"), 
            PremierSymptomes:= "U"]
  sidep_add[PremierSymptomes%in%c("PAS DE SYMPTOME", "a", "Asymptomatique.",
                                  "AUCUN SYMPTOME", "asy", "APSYU", "ASy", 
                                  "ASYM","A","ASYASY","ASYM ", "ASYU", "PASYM", "UASY",
                                  "avant entrée en EHPAD", "avant retour en EHPAD",
                                  "entrée de geriatrie", "entree en geriatrie",
                                  "avant rentrée en ehpad","AVANT TRANSFERT EHPAD",
                                  "BILAN PRE OP", "TEST PRE OP", "TRANSFERT EN  EHPAD",
                                  "Transfert SSR-P"),
            PremierSymptomes:= "ASY"]
  sidep_add[grepl("(Pas de symptomes)|(ASYV)|(VASY)|(Pré opératoire)|(PAS DE SYMPTOMES)|(PRE-OPERATOIRE)", PremierSymptomes), PremierSymptomes:="ASY"]
  sidep_add[grepl("(apparus le jour ou la veille)|(D1)|(S01S01)", PremierSymptomes), PremierSymptomes:="S01"]
  sidep_add[grepl("(apparus 2,3)|(apparus 2, 3)|(D2)|(D3)|(quelques jours)|(Quelques jours)", PremierSymptomes), PremierSymptomes:="S24"]
  sidep_add[PremierSymptomes %in% c("05/05/20"),PremierSymptomes:="S57"] #date plvt 11/05
  sidep_add[grepl("(apparus 5,6)|(apparus 5, 6)|(D5)", PremierSymptomes), PremierSymptomes:="S57"]
  sidep_add[grepl("(SS2SS2)|(D9)|(D11)|(S814)", PremierSymptomes), PremierSymptomes:="SS2"]
  sidep_add[grepl("(apparus entre 8 et 15 jours)|(apparus entre 8 et 14 jours)|(S814)|(ss2)", PremierSymptomes), PremierSymptomes:="SS2"]
  sidep_add[grepl("(apparus plus de 2 semaines)|(apparus plus de deux semaines)|(+ 15 jours)|(>j15)|(SS34)|(SS3SS3)|(S2-S4)", PremierSymptomes), PremierSymptomes:="SS3"]
  sidep_add[grepl("(apparus plus de deux semaines avant le prelevement)|(apparus entre 15 et 28 jours)|(SS3U)", PremierSymptomes), PremierSymptomes:="SS3"]
  sidep_add[grepl("(il y a 1 mois)|(plus de 3 semaines)|(DECEMBRE 19)|(plus de quatre semaines)|(D29)|(SP4S)",PremierSymptomes), PremierSymptomes:="SS3"]
  sidep_add[grepl("(Eruption cutanée)|(colique)|(30/05/20)|(07/06/20)|(202005)|(202006)",PremierSymptomes), PremierSymptomes:="SNA"]
  
  
  
  print("TypologiePatient")
  sidep_add[TypologiePatient == "", TypologiePatient := "U"]
  
  sidep_add[TypologiePatient%in%c("Autre structure d'hébergement collectif",
                                  "Autre structure d'hébergement",
                                  "autre structure d'hébergement collectif"), 
            TypologiePatient:= "A"]
  
  sidep_add[TypologiePatient%in%c("résident en EHPAD","Résident en EHPAD"), 
            TypologiePatient:= "E"]
  sidep_add[TypologiePatient%in%c("HH"), TypologiePatient:= "H"]
  sidep_add[TypologiePatient%in%c("II","i"), TypologiePatient:= "H"]
  
  sidep_add[!TypologiePatient%in%c("I","H","E","C","A","U"), 
            TypologiePatient:= "U"]
  
  
  
  print("Resultat")
  
  sidep_add[Resultat %in% c("negatif","NEGATIF","Négative", "N?gatif", "NN", "NNN", "NEG", "n"),
            Resultat := "N"]
  sidep_add[grepl( "(ninterp)|(ndéterminé)", Resultat), Resultat := "I"]
  sidep_add[Resultat %in% c("PP"), Resultat := "P"]
  
  sidep_add[Resultat=='N',valeur:="N"]
  sidep_add[Resultat=='P',valeur:="P"]
  sidep_add[Resultat=='I',valeur:="I"]
  sidep_add[Resultat=='X',valeur:="X"]
  
  print("Doublons dans les dernières extractions")
  
  
  sidep_add[, na_res := ifelse(Resultat %in% c("","X","I") | is.na(Resultat) | nchar(Resultat)>1, 1, 0)]
  sidep_add[, na_typo := ifelse(TypologiePatient == "U" | is.na(TypologiePatient), 1, 0)]
  sidep_add[, na_cp := ifelse(CodePostal == "" | is.na(CodePostal) | nchar(CodePostal)!=5, 1, 0)]
  sidep_add[, na_sympto := ifelse(PremierSymptomes == "" | is.na(PremierSymptomes) | nchar(PremierSymptomes)=="U", 1, 0)]
  
  sidep_add = sidep_add[, `:=`(date_valid_init = min(date_valid),
                               latest_day_extract = max(day_extract_sidep),
                               res_ok = min(na_res),
                               typo_ok = min(na_typo),
                               cp_ok = min(na_cp),
                               sympto_ok = min(na_sympto)), 
                        by=.(Pseudonyme, date_prelev, AnalyseConclusion)]
  
  sidep_add <-  sidep_add[day_extract_sidep %in% latest_day_extract]
  etapes = rbind(etapes, cbind(var="latest_extract", n_row = nrow(sidep_add)))
  
  sidep_add <-  sidep_add[na_res == res_ok]
  etapes = rbind(etapes, cbind(var="Resultat", n_row = nrow(sidep_add)))
  sidep_add <-  sidep_add[na_typo == typo_ok]
  etapes = rbind(etapes, cbind(var="TypologiePatient", n_row = nrow(sidep_add)))
  sidep_add <-  sidep_add[na_cp == cp_ok]
  etapes = rbind(etapes, cbind(var="CodePostal", n_row = nrow(sidep_add)))
  sidep_add <-  sidep_add[na_sympto == sympto_ok]
  etapes = rbind(etapes, cbind(var="PremierSymptomes", n_row = nrow(sidep_add)))
  
  sidep_add[, date_valid := date_valid_init]
  
  sidep_add = sidep_add[,-c("latest_day_extract", "na_res","res_ok","na_typo",
                            "typo_ok", "na_cp","cp_ok", "na_sympto","sympto_ok")]
  
  sidep_add = sidep_add[,.SD[1], by=c("Pseudonyme", "date_prelev", "AnalyseConclusion")]
  etapes = rbind(etapes, cbind(var="dup_tryptique", n_row = nrow(sidep_add)))
  
  etapes = data.table(etapes)
  etapes[, n_supp := 0]
  etapes[, n_row := as.numeric(n_row)]
  for (i in 2:nrow(etapes)){
    etapes$n_supp[i] = etapes$n_row[i-1]-etapes$n_row[i]
  }
  etapes <<- etapes
  
  print("Finess")
matching_fi = function(data_sidep){
  setnames(data_sidep,"FINESS","fi_init")
  
  load(paste0(path, "data/utils/finess_1116.Rdata")) 
  
  liste_et <- liste_et%>%
                select(DEP_ET=`ET-Département\nCode`,
                    FINESS_EJ=`EJ-N°FINESS`,DEP_EJ=`EJ-Département\nCode`,
                    FINESS_ET=`ET-N°FINESS`)
  sidep_geo <- inner_join(data_sidep,
                           liste_et%>%select(fi_init=`FINESS_ET`,DEP_ET,
                                             FINESS_EJ,DEP_EJ),
                           by='fi_init')
  sidep_geo$FINESS_ET <- sidep_geo$fi_init
  sidep_ju <- inner_join(data_sidep,
                          as_tibble(liste_et)%>%select(fi_init=`FINESS_EJ`,DEP_EJ)%>%
                            group_by(fi_init)%>%slice(1)%>%ungroup(),
                          by='fi_init') 
  sidep_ju$FINESS_EJ <- sidep_ju$fi_init
  sidep_ok <- rbindlist(list(sidep_geo,sidep_ju),fill=T)
  data_sidep <- merge(data_sidep,
                      unique(sidep_ok[,.(fi_init,FINESS_ET,DEP_ET,FINESS_EJ,DEP_EJ)]),
                      by.x='fi_init',by.y='fi_init',all.x = T)
  
  
  bio3_et <- readxl::read_excel(paste0(path,"data/utils/bio3_et_1116.xlsx"))
  
  data_sidep <- left_join(data_sidep,
                     bio3_et%>%group_by(FINESS_EJ)%>%slice(1)%>%ungroup()%>%select(FINESS_EJ,cat_etb),
                     by='FINESS_EJ')
  data_sidep$cat_pp <- ifelse(data_sidep$cat_etb  %in% c("2000","3000"),"privé",NA)
  data_sidep$cat_pp <- ifelse(data_sidep$cat_etb == "1000","public",data_sidep$cat_pp)
  table(data_sidep$cat_pp,exclude=NULL)
  data_sidep = data.table(data_sidep)
  remove(sidep_ok,sidep_ju,sidep_geo)
  
  data_sidep[is.na(DEP_EJ), DEP_EJ := "dep_inconnu"]
  data_sidep[is.na(DEP_ET), DEP_ET := "dep_inconnu"]
  
  reg_dep = data.table::fread(paste0(path,"data/utils/departement2019.csv"))
  reg_dep$reg = as.character(reg_dep$reg)
  reg_dep[nchar(reg)==1, reg := paste0("0", reg)]
   
  ej_et_dep = data.table(liste_et[, c("FINESS_ET","DEP_ET","FINESS_EJ","DEP_EJ")])
  ej_et_dep = merge(ej_et_dep, reg_dep[,.(dep, reg)], by.x="DEP_ET", by.y="dep", all.x=T)
  setnames(ej_et_dep,"reg","REG_ET")
  ej_et_dep = merge(ej_et_dep, reg_dep[,.(dep, reg)], by.x="DEP_EJ", by.y="dep", all.x=T)
  setnames(ej_et_dep,"reg","REG_EJ")
  
  nb_dep_par_ej = ej_et_dep[, .(n_distinct(DEP_ET)), by=.(FINESS_EJ)]
  sum(nb_dep_par_ej$V1==1)/length(unique(ej_et_dep$FINESS_EJ))
  ej_et_same_dep = ej_et_dep[FINESS_EJ %in% nb_dep_par_ej$FINESS_EJ[nb_dep_par_ej$V1==1]]
  ej_et_same_dep = ej_et_same_dep[DEP_ET == DEP_EJ]
  ej_et_same_dep = unique(ej_et_same_dep[, .(FINESS_EJ, DEP_EJ)])
  
  nb_reg_par_ej = ej_et_dep[, .(n_distinct(REG_ET)), by=.(FINESS_EJ)]
  sum(nb_reg_par_ej$V1==1)/length(unique(ej_et_dep$FINESS_EJ))
  ej_et_same_reg = ej_et_dep[FINESS_EJ %in% nb_reg_par_ej$FINESS_EJ[nb_reg_par_ej$V1==1]]
  ej_et_same_reg = ej_et_same_reg[REG_ET == REG_EJ]
  ej_et_same_reg = unique(ej_et_same_reg[, .(FINESS_EJ, REG_EJ)])
  
  ej_et_same_dep <<- ej_et_same_dep
  ej_et_same_reg <<- ej_et_same_reg
  
  data_sidep[, cat_pp:=iconv(cat_pp, from="UTF-8", to="latin1")]
  
  return(data_sidep)
}
  sidep_add=matching_fi(sidep_add)
  
  
  print("Code postal, département et région")
  
  sidep_add[substr(CodePostal, 1, 2) %in% c("00", "99", "XX"), CodePostal := NA]
  sidep_add[CodePostal %in% c("", "0", ".", ",", "aucun","AUCUN"), CodePostal := NA]
  sidep_add[CodePostal == "=59111", CodePostal := "59111"]
  sidep_add[CodePostal == ":78360", CodePostal := "78360"]
  sidep_add[CodePostal == "=35420", CodePostal := "35420"]
  sidep_add[, code_dep := case_when(substr(CodePostal, 1, 3) %in% c("201", "200","207") ~ "2A",
                                    substr(CodePostal, 1, 3) %in% c("202","206") ~ "2B",
                                    substr(CodePostal, 1,2) == "97" ~ substr(CodePostal, 1,3),
                                    !substr(CodePostal, 1,2) %in% c("97", "20") ~ substr(CodePostal, 1,2))]
  corr_CP = data.table(read.xlsx(paste0(path, "data/utils/0618_corresp_CP.xlsx")))
  corr_CP = corr_CP[!is.na(code_dep_corr)&code_dep_corr!=""]
  sidep_add = merge(sidep_add, corr_CP, by="CodePostal", all.x=T)
  sidep_add[, code_dep := ifelse(!is.na(code_dep_corr), code_dep_corr, code_dep)]
  
  
  reg_dep = data.table::fread(paste0(path, "data/utils/departement2019.csv"), encoding = "Latin-1")
  reg_dep$reg = as.character(reg_dep$reg)
  reg_dep[nchar(reg)==1, reg := paste0("0", reg)]
  reg2019 = data.table::fread(paste0(path, "data/utils/region2019.csv"), encoding = "Latin-1")
  reg2019$reg = as.character(reg2019$reg)
  reg2019[nchar(reg)==1, reg := paste0("0", reg)]
  
  sidep_add[ !code_dep %in% reg_dep$dep, code_dep := "dep_inconnu"]
  
  sidep_add[code_dep == "dep_inconnu" & !is.na(IRIS) & nchar(IRIS)==9,
            code_dep := substr(IRIS, 1, 2)]
  
  sidep_add[, departement := ifelse(code_dep != "dep_inconnu", code_dep, ifelse(RPPSPrescripteur != "291991222",DEP_ET,"dep_inconnu"))]
  sidep_add[FINESS_EJ%in%ej_et_same_dep$FINESS_EJ & RPPSPrescripteur != "291991222", 
            departement := ifelse(departement != "dep_inconnu", departement, DEP_EJ)]
  
  sidep_add = merge(sidep_add, reg_dep[,.(dep, reg, ncc)], by.x="departement", by.y="dep", all.x=T)
  sidep_add[!reg %in% reg_dep$reg, reg := "reg_inconnue"]
  setnames(sidep_add, "ncc", "nom_dep")
  
  sidep_add = merge(sidep_add, ej_et_same_reg[,.(FINESS_EJ, REG_EJ)], by="FINESS_EJ", all.x=T)
  sidep_add[FINESS_EJ %in% ej_et_same_reg$FINESS_EJ, 
            reg := ifelse(reg != "reg_inconnue", reg, REG_EJ)]
  sidep_add = sidep_add[,-c("REG_EJ")]
  
  sidep_add = merge(sidep_add, reg2019[,.(reg, libelle3)], by="reg", all.x=T)
  setnames(sidep_add, "libelle3", "nom_reg")
  
  sidep_add = sidep_add[, -c("code_dep","code_dep_corr")]
  
  
  
  print("Professionnel de santé et sexe")
  
  sidep_add[ProfessionelSante%in%c("","UU", ".U", "u","Non communiqué","non fait", "nr",
                                   "Non précisé","np", "NP", "NCDD","NM","NO", "UN",
                                   "NR","NCNR","NCNR2","Demande annulée", "Automatique"),
            ProfessionelSante:= "U"]
  sidep_add[ProfessionelSante%in%c("n","NN"), ProfessionelSante:= "N"]
  sidep_add[ProfessionelSante%in%c("OO","o"), ProfessionelSante:= "O"]
  sidep_add[!(ProfessionelSante %in%c("U","O","N")), ProfessionelSante:= "U"]
  
  sidep_add[Sexe %in% c("O","","N","A"), Sexe:= "U"]
  
  
  print("Traitement des bases journalières terminé")
  
  
  sidep_add = sidep_add[, -c("DatePrelevement", "DateValidationCR","DEP_EJ","DEP_ET", "NumDossier")]
  print("Chargement des bases agrégées") 
  
  
  t1 <- Sys.time()
  temp <- paste0(path_sortie, date_AAAAMMJJ_last, "_sidep_pcr_")
  temp_mois <- MoisAAAA
  if (!file.exists(paste0(temp, "Mai021.csv"))){
    temp_mois <- MoisAAAA[1:(length(MoisAAAA)-1)]
  }
  
  sidep_prev_pcr_lst <- pbapply::pblapply(temp_mois,
                                            function(i) {
                                              dt <- data.table::fread(paste0(temp, i,".csv"), 
                                                                na.strings="",
                                                                colClasses=list(Date=date_vars, 
                                                                                POSIXct = posixct_vars,
                                                                                character = char_vars, 
                                                                                numeric = "Age"),
                                                                verbose = F, sep = ";", header = TRUE, encoding ="Latin-1")
                                              if(addP2){
                                                n = nrow(merge(dt,corres,by="Pseudonyme"))
                                                print(sprintf("%s lignes parmi %s ont eu un p1 remplacé par p2 dans le fichier %s.",n,nrow(dt),i))
                                                dt[corres,Pseudonyme:=i.Pseudo2,on="Pseudonyme"]
                                              }
                                              dt
                                            }
                                            )
  
  t2 <- Sys.time()
  print(t2 - t1) #6.2min
  
  print(t2-TIME_INIT)
  
  t1 <- Sys.time()
  path_zip_sero = paste0(path_sortie, date_AAAAMMJJ_last, "_sidep_sero.csv") 
  print(path_zip_sero)
  sidep_prev_sero = data.table::fread(path_zip_sero, 
                                      na.strings="",
                                      colClasses=list(Date=date_vars, 
                                                      POSIXct = posixct_vars,
                                                      character = char_vars, 
                                                      numeric = "Age"),
                                      verbose = F, sep= ";", header = TRUE, encoding ="Latin-1")
  if(addP2){
    sidep_prev_sero[corres,Pseudonyme:=i.Pseudo2,on="Pseudonyme"]
  }
  t2 <- Sys.time()
  print(t2-t1) # 49s avec pb chemin résolu
  
  
  t1 <- Sys.time()
  path_zip_pcr_sal_mil = paste0(path_sortie, date_AAAAMMJJ_last, "_sidep_pcr_salivaire_milieux_divers.csv") 
  print(path_zip_pcr_sal_mil)
  sidep_prev_pcr_sal_mil = data.table::fread(path_zip_pcr_sal_mil, 
                                             na.strings="",
                                             colClasses=list(Date=date_vars, 
                                                             POSIXct = posixct_vars,
                                                             character = char_vars, 
                                                             numeric = "Age"),
                                             verbose = F, sep= ";", header = TRUE, encoding="Latin-1")
  if(addP2){
    sidep_prev_pcr_sal_mil[corres,Pseudonyme:=i.Pseudo2,on="Pseudonyme"]
  }
  t2 <- Sys.time()
  print(t2-t1)
  
  print("Concaténation des bases en liste par mois")
  sidep_add[, `:=`(RPPSPrescripteur = as.character(RPPSPrescripteur))]
  t1 <- Sys.time()
  
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(date_prelev = fasttime::fastPOSIXct(date_prelev),
            date_valid = fasttime::fastPOSIXct(date_valid),
            date_valid_init = fasttime::fastPOSIXct(date_valid_init),
            Age = as.numeric(Age))])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(date_valid = NULL)])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(date_valid = date_valid_init)])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(day_prelev2 = as.Date(day_prelev)),
      by = day_prelev])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(day_prelev = NULL)])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    data.table::setnames(i, "day_prelev2", "day_prelev"))
  
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(day_valid2 = as.Date(day_valid)),
      by = day_valid])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(day_valid = NULL)])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    data.table::setnames(i, "day_valid2", "day_valid"))
  
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(day_extract_sidep2 = as.Date(day_extract_sidep)),
      by = day_extract_sidep])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    i[,`:=`(day_extract_sidep = NULL)])
  sidep_prev_pcr_lst <- lapply(sidep_prev_pcr_lst, function(i)
    data.table::setnames(i, "day_extract_sidep2", "day_extract_sidep"))
  t2 <- Sys.time()
  print(t2-t1)
  
  last_month = MoisAAAA[length(MoisAAAA)]
  if (!file.exists(paste0(temp, last_month))){
    sidep_prev_pcr_lst[[length(sidep_prev_pcr_lst)+1]] <- data.table::data.table()
  }
  
  t1 <- Sys.time()
  sidep_prev_sero[, mois_prelev := lubridate::month(date_prelev)]
  sidep_prev_sero[, annee_prelev := lubridate::year(date_prelev)]
  sidep_sero_lst <- split(sidep_prev_sero, by = c("annee_prelev", "mois_prelev"))
  t2 <- Sys.time()  
  print(t2-t1) # 48 s
  sidep_sero_lst <- lapply(sidep_sero_lst, function(i) i[, mois_prelev := NULL])
  sidep_sero_lst <- lapply(sidep_sero_lst, function(i) i[, annee_prelev := NULL])
  
  
  sidep_sero_lst <- lapply(sidep_sero_lst, function(i) 
    i[,`:=`(day_prelev = as.Date(day_prelev), 
            day_valid = as.Date(day_valid),
            date_prelev = fasttime::fastPOSIXct(date_prelev),
            date_valid = fasttime::fastPOSIXct(date_valid),
            date_valid_init = fasttime::fastPOSIXct(date_valid_init),
            day_extract_sidep = as.Date(day_extract_sidep),
            Age = as.numeric(Age))]) 
  
  t1 <- Sys.time()
  sidep_prev_pcr_sal_mil[, mois_prelev := lubridate::month(date_prelev)]
  sidep_prev_pcr_sal_mil[, annee_prelev := lubridate::year(date_prelev)]
  sidep_prev_pcr_sal_mil_lst <- split(sidep_prev_pcr_sal_mil, by = c("annee_prelev", "mois_prelev"))
  t2 <- Sys.time()  
  print(t2-t1) # 48 s
  sidep_prev_pcr_sal_mil_lst <- lapply(sidep_prev_pcr_sal_mil_lst, 
                                       function(i) i[, mois_prelev := NULL])
  sidep_prev_pcr_sal_mil_lst <- lapply(sidep_prev_pcr_sal_mil_lst, 
                                       function(i) i[, annee_prelev := NULL])
  sidep_prev_pcr_sal_mil_lst <- lapply(sidep_prev_pcr_sal_mil_lst, function(i) 
    i[,`:=`(day_prelev = as.Date(day_prelev), 
            day_valid = as.Date(day_valid),
            date_prelev = fasttime::fastPOSIXct(date_prelev),
            date_valid = fasttime::fastPOSIXct(date_valid),
            date_valid_init = fasttime::fastPOSIXct(date_valid_init),
            day_extract_sidep = as.Date(day_extract_sidep),
            Age = as.numeric(Age))]) 
  
  
  sidep_add[, mois_prelev := lubridate::month(date_prelev)]
  sidep_add[, annee_prelev := lubridate::year(date_prelev)]
  sidep_add <- sidep_add[order(annee_prelev, mois_prelev)]
  sidep_add_lst <- split(sidep_add, by = c("annee_prelev", "mois_prelev"))
  sidep_add_lst <- lapply(sidep_add_lst, 
                          function(i) i[, mois_prelev := NULL])
  sidep_add_lst <- lapply(sidep_add_lst, 
                          function(i) i[, annee_prelev := NULL])
  
  names(sidep_prev_pcr_lst) <- YYYY.mm
  t1 <- Sys.time()
  sidep_lst <- lapply(YYYY.mm, 
                      function(i) rbindlist(list(sidep_prev_pcr_lst[[i]],
                                                 sidep_sero_lst[[i]],
                                                 sidep_prev_pcr_sal_mil_lst[[i]],
                                                 sidep_add_lst[[i]]),fill = TRUE))
  t2 <- Sys.time()
  print(t2-t1) 
  
  rm(sidep_add, sidep_add_lst, sidep_sero_lst, sidep_prev_pcr_lst, sidep_prev_pcr_sal_mil_lst)
  
  
  print("Doublons dans la base globale")
  etapes_fin = cbind(var="aucune", 
                     n_row = do.call("sum", lapply(sidep_lst, nrow)))
  
  treat_duplicates <- function(temp){
    print(paste0(lubridate::month(temp$date_prelev[1]), " - ", lubridate::year(temp$date_prelev[1])))
    rows_dup <- (duplicated(temp, 
                            by = 
                              c("Pseudonyme", "date_prelev", "AnalyseConclusion"))
                 | duplicated(temp, by = 
                                c("Pseudonyme", "date_prelev", "AnalyseConclusion"),
                              fromLast=TRUE))
    
    
    temp[, `:=`(date_valid_init = date_valid,
                latest_day_extract = day_extract_sidep)]
    temp_to_treat <- temp[rows_dup]
    temp_not_to_treat <- temp[!rows_dup]
    print(nrow(temp_to_treat))
    if (nrow(temp_to_treat)>0){
      temp_to_treat[, `:=`(date_valid_init = min(date_valid),
                           latest_day_extract = max(day_extract_sidep)), 
                    by = c("Pseudonyme", "date_prelev", "AnalyseConclusion")]
      temp_to_treat <-  temp_to_treat[day_extract_sidep %in% latest_day_extract]
      temp_to_treat <- temp_to_treat[, .SD[1], 
                                     by=c("Pseudonyme", "date_prelev", "AnalyseConclusion")]
      temp <- rbindlist(list(temp_to_treat, temp_not_to_treat), fill=T)
    }
    
    temp[, date_valid := date_valid_init]
    
    
    return(temp)
    
  }
  
  t1 <- Sys.time()
  sidep_lst <- lapply(sidep_lst, function(i) treat_duplicates(i)) 
  t2 <- Sys.time()
  print(t2-t1)
  
  etapes_fin = rbind(etapes_fin, cbind(var="dup_tryptique_and_latest_date", 
                                       n_row = do.call("sum", lapply(sidep_lst, nrow))))
  
  etapes_fin = data.table::data.table(etapes_fin)
  etapes_fin[, n_supp := 0]
  etapes_fin[, n_row := as.numeric(n_row)]
  for (i in 2:nrow(etapes_fin)){
    etapes_fin$n_supp[i] = etapes_fin$n_row[i-1]-etapes_fin$n_row[i]
  }
  etapes_fin <<- etapes_fin
  
  
  sidep_lst <- lapply(sidep_lst, function(i) 
    i[,`:=`(latest_day_extract = NULL)]) 
  
  
  extract_dt <- function(temp){
    sidep_sero <- lapply(temp, function(i) i[AnalyseConclusion %in% 
                                               c("94762-2","94563-4","94564-2")])
    sidep_pcr <- lapply(temp, function(i) i[AnalyseConclusion == "94500-6"])
    sidep_pcr_sal_mil <- lapply(temp, function(i) i[AnalyseConclusion %in% c("94309-2", "94845-5")])
    return(list("sidep_sero" = sidep_sero, 
                "sidep_pcr" = sidep_pcr, 
                "sidep_pcr_sal_mil" = sidep_pcr_sal_mil))
    
  }
  t1 <- Sys.time()
  sidep <- extract_dt(sidep_lst)
  t2 <- Sys.time()
  print(t2-t1)
  rm(sidep_lst)
  sidep_sero <<- sidep[["sidep_sero"]]
  sidep_pcr <<- sidep[["sidep_pcr"]]
  sidep_pcr_sal_mil <<- sidep[["sidep_pcr_sal_mil"]]
  rm(sidep)
  
  print(Sys.time()-TIME_INIT)
  
  gc()
  
}
t1 <- Sys.time()
prep_data(jour_date, jour_prev_update, MoisAAAA=MoisAAAA,YYYY.mm = YYYY.mm, day_init="2020-05-20",corres=new_rattrapage,addP2=addP2)
t2 <- Sys.time()
print(t2 - t1) # 41 minutes en journée (vers 16h le 14/04/21)
print("ecriture des datas")
t1 <- Sys.time()
lapply(1:length(sidep_pcr),
       function(i)
         data.table::fwrite(sidep_pcr[[i]],
                            paste0(path_sortie, date_AAAAMMJJ,'_sidep_pcr_', MoisAAAA[i], '.csv'),
                            quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
                            qmethod = c("escape"),dateTimeAs = "ISO", verbose = F))

