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
                            paste0(path_sortie, date_AAAAMMJJ,'_sidep_pcr_', month[i], '.csv'),
                            quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
                            qmethod = c("escape"),dateTimeAs = "ISO", verbose = F))
t2 <- Sys.time()
print(t2-t1)
lapply(sidep_pcr,nrow)
sidep_pcr <- rbindlist(sidep_pcr, fill = TRUE)
sidep_sero <- rbindlist(sidep_sero, fill=T)
sidep_pcr_sal_mil <- rbindlist(sidep_pcr_sal_mil, fill=T)
t1 <- Sys.time()
data.table::fwrite(sidep_sero,
                   paste0(path_sortie, date_AAAAMMJJ,'_sidep_sero.csv'),
                   quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
                   qmethod = c("escape"),dateTimeAs = "ISO", verbose = F)
t2 <- Sys.time()
print(t2-t1)
t1 <- Sys.time()
data.table::fwrite(sidep_pcr_sal_mil,
                   paste0(path_sortie, date_AAAAMMJJ,'_sidep_pcr_salivaire_milieux_divers.csv'),
                   quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
                   qmethod = c("escape"),dateTimeAs = "ISO", verbose = F)
t2 <- Sys.time()
print(t2-t1)
print("04 TAG")
safe_fread = purrr::quietly(data.table::fread)
prep_data_antigenique <- function(jour=jour_date, 
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
  extractions <- extractions[!extractions %in% c("CSV_DREES_200518200002.csv",
                                                 "CSV_DREES_200519090000.csv",
                                                 "CSV_DREES_200520090000.csv",
                                                 "CSV_DREES_200625130000.csv")]
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
  
  print(extract["MED4"])
  print(extract["MED2"])
  
  sidep_add <- sidep_add[AnalyseConclusion %in% c("94558-4")]
  sidep_add$day_extract_sidep = as.Date(date_AAAAMMJJ,format="%Y%m%d")
  
 
  
  print(nrow(sidep_add))
  setnames(sidep_add, "Pseudo2", "Pseudonyme")
  sidep_add[,Pseudo1:=NULL]
  print("Suppression des antislash et .br")
  
  sidep_add[, Joker1 := NULL]
  sidep_add[, Joker2 := NULL]
  sidep_add[, Joker3 := NULL]
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
  
  
  
  sidep_add$RPPSPrescripteur = stringr::str_extract(sidep_add$RPPSPrescripteur,'([0-9]{1,14})') # enlever les caracteres autres que les chiffres
  sidep_add$RPPSPrescripteur = as.integer64(sidep_add$RPPSPrescripteur)
  sidep_add[RPPSPrescripteur == 0, RPPSPrescripteur := NA]
  
  
  sidep_add$CodePostal = gsub('(\\\\)','', sidep_add$CodePostal) # enlever les antislash
  sidep_add$CodePostal = gsub('(.br)','', sidep_add$CodePostal) # enlever les .br
  
  
  sidep_add$FINESS_c <- sidep_add$FINESS
  sidep_add$FINESS = stringr::str_extract(sidep_add$FINESS,'(^[0-9AB]{1,15})') 
  sidep_add[, FINESS := lest::case_when(FINESS == "A" ~NA_character_, TRUE ~FINESS)]
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
  
  
  sidep_add[, FINESS_c_maj := toupper(FINESS_c)]
  sidep_add[, FINESS_c_maj := ifelse(stringr::str_starts(FINESS_c_maj, "MLETEST"), NA, FINESS_c_maj)]
  sidep_add[, RPPSPrelevTA := stringr::str_extract(FINESS_c_maj,'^(RPPS_)([0-9a-zA-Z]+)')]
  sidep_add[, AdeliPrelevTA := stringr::str_extract(FINESS_c_maj,'^(ADELI_)([0-9a-zA-Z]+)')]
  sidep_add[, idNATPrelevTA := stringr::str_extract(FINESS_c_maj,'^(IDNAT_)([0-9a-zA-Z]+)')]
  
  sidep_add[, ProfPrelevTA := stringr::str_extract(FINESS_c_maj,'(PROF_)([a-zA-Z]+)')]
  sidep_add[, CPPrelevTA := stringr::str_extract(FINESS_c_maj,'(CP_)([0-9a-zA-Z]+)')]
  sidep_add[, TopPrelevTA := ifelse(stringr::str_starts(FINESS_c_maj, "(RPPS|ADELI|IDNAT)"), 1, 0)]
  print(sidep_add[, table(TopPrelevTA, ProfPrelevTA)])
  if (nrow(sidep_add[TopPrelevTA == 0 & !(is.na(ProfPrelevTA)), .(FINESS_c)])>0){
    print("Probleme il y a des top prelev TA à 0 alors qu'il y a une profession")
  }
  
  sidep_add[, RPPSPrelevTA := stringr::str_replace(RPPSPrelevTA,'RPPS_', "")]
  sidep_add[, AdeliPrelevTA := stringr::str_replace(AdeliPrelevTA,'ADELI_', "")]
  sidep_add[, idNATPrelevTA := stringr::str_replace(idNATPrelevTA,'IDNAT_', "")]
  
  sidep_add[, ProfPrelevTA := stringr::str_replace(ProfPrelevTA,'PROF_', "")]
  sidep_add[, CPPrelevTA := stringr::str_replace(CPPrelevTA,'CP_', "")]
  
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
  if(sum(nchar(sidep_add$Pseudonyme)>64, na.rm=T)!=0){
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
  sidep_add = sidep_add[AnalyseConclusion %in% c("94558-4")]
  
  
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
  path_zip_antigen = paste0(path_sortie, date_AAAAMMJJ_last, "_sidep_antigen.csv") 
  print(path_zip_antigen)
  sidep_prev_antigen = data.table::fread(path_zip_antigen,
                                         na.strings="",
                                         colClasses=list(Date=date_vars,
                                                         POSIXct = posixct_vars,
                                                         character = c(char_vars,new_tag_char_vars), 
                                                         numeric = "Age"),
                                         verbose = F, sep= ";", header = TRUE, encoding = "Latin-1")
  if(addP2){
    sidep_prev_antigen[corres,Pseudonyme:=i.Pseudo2,on="Pseudonyme"]
  }
  t2 <- Sys.time()
  print(t2-t1)
  
  
  print("Concaténation des bases en liste par mois")
  sidep_add[, `:=`(RPPSPrescripteur = as.character(RPPSPrescripteur))]
  data.table::setnames(sidep_add, "FINESS_c", "FINESS_PS_init")
  sidep_add[, FINESS_c_maj := NULL]
  
  
  t1 <- Sys.time()
  sidep_prev_antigen[, mois_prelev := lubridate::month(date_prelev)]
  sidep_prev_antigen[, annee_prelev := lubridate::year(date_prelev)]
  sidep_antigen_lst <- split(sidep_prev_antigen, by = c("annee_prelev", "mois_prelev"))
  t2 <- Sys.time()
  print(t2-t1) # 48 s
  sidep_antigen_lst <- lapply(sidep_antigen_lst, function(i) i[, mois_prelev := NULL])
  sidep_antigen_lst <- lapply(sidep_antigen_lst, function(i) i[, annee_prelev := NULL])
  
  sidep_antigen_lst <- lapply(sidep_antigen_lst, function(i)
    i[,`:=`(day_prelev = as.Date(day_prelev), # a tester avec fasttime::fastPOSTIxct()
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
  
  
  t1 <- Sys.time()
  sidep_lst <- lapply(YYYY.mm,
                      function(i) rbindlist(list(sidep_antigen_lst[[i]],
                                                 sidep_add_lst[[i]]),fill = TRUE))
  t2 <- Sys.time()
  print(t2-t1) 
  rm(sidep_add, sidep_add_lst)
  
  
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
    sidep_antigenique <- lapply(temp, function(i) i[AnalyseConclusion %in% 
                                                      c("94558-4")])
    return(sidep_antigenique)
  }
  t1 <- Sys.time()
  sidep_antigenique <<- extract_dt(sidep_lst)
  t2 <- Sys.time()
  print(t2-t1)
  
  
  print(Sys.time()-TIME_INIT)
  
  gc()
  
}
t1 <- Sys.time()
prep_data_antigenique(jour_date, jour_prev_update, MoisAAAA=MoisAAAA,YYYY.mm = YYYY.mm, day_init="2020-05-20",corres=new_rattrapage,addP2=addP2)
t2 <- Sys.time()
print(t2 - t1) # 7 minute
sidep_antigenique <- rbindlist(sidep_antigenique, fill=TRUE)
t1 <- Sys.time()
data.table::fwrite(sidep_antigenique,
                   paste0(path_sortie, date_AAAAMMJJ,'_sidep_antigen.csv'),
                   quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
                   qmethod = c("escape"),dateTimeAs = "ISO", verbose = F)
t2 <- Sys.time()
print(t2-t1)
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
run_expertise = F
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
library(ggplot2)
pathtodata = path_sortie
replaceP1P2 = F
if(!"sidep_pcr"%in%ls()){
  print("load sidep_pcr")
  sidep_pcr = pbapply::pblapply(MoisAAAA,function(i) {
    dt <- data.table::fread(paste0(pathtodata, date_AAAAMMJJ,'_sidep_pcr_', i, '.csv'),
                            na.strings="",
                            colClasses=list(
                              Date=date_vars, 
                              POSIXct = posixct_vars,
                              character = char_vars, 
                              numeric = "Age"), 
                            verbose = F, sep = ";", header = TRUE, encoding ="Latin-1")
    if(replaceP1P2){
      n = nrow(merge(dt,new_rattrapage,by="Pseudonyme"))
      print(sprintf("%s lignes parmi %s ont eu un p1 remplacé par p2 dans le fichier %s.",n,nrow(dt),i))
      dt[new_rattrapage,Pseudonyme:=i.Pseudo2,on="Pseudonyme"]
    }
    dt
  })
  
  sidep_pcr <- rbindlist(sidep_pcr, fill = TRUE)
  
  
} else {
  print("sidep_pcr déjà chargé")
}
if(!"sidep_pcr_sal_mil"%in%ls()){
  print("load sidep_pcr_sal_mil")
  
  path_pcr_sal_mil = paste0(pathtodata, date_AAAAMMJJ, "_sidep_pcr_salivaire_milieux_divers.csv") 
  print(path_pcr_sal_mil)
  sidep_pcr_sal_mil = data.table::fread(path_pcr_sal_mil, 
                                        na.strings="",
                                        colClasses=list(
                                          Date=date_vars, 
                                          POSIXct = posixct_vars,
                                          character = char_vars, 
                                          numeric = "Age"), 
                                        verbose = F, sep= ";", header = TRUE, encoding="Latin-1")
  nrow(sidep_pcr_sal_mil)
  nrow(sidep_pcr_sal_mil)
  if(replaceP1P2){
    nrow(merge(sidep_pcr_sal_mil,new_rattrapage,by="Pseudonyme"))
    sidep_pcr_sal_mil[new_rattrapage,Pseudonyme:=i.Pseudo2,on="Pseudonyme"]
  }
}else {
  print("sidep_pcr_sal_mil déjà chargé")
}
if(!"sidep_sero"%in%ls()){
  print("load sidep_sero")
  
  path_sero = paste0(pathtodata, date_AAAAMMJJ, "_sidep_sero.csv") 
  print(path_sero)
  sidep_sero = data.table::fread(path_sero, 
                                 na.strings="",
                                 colClasses=list(
                                   Date=date_vars, 
                                   POSIXct = posixct_vars,
                                   character = char_vars, 
                                   numeric = "Age"), 
                                 verbose = F, sep= ";", header = TRUE, encoding="Latin-1")
  nrow(sidep_sero)
  nrow(sidep_sero)
  if(replaceP1P2){
    nrow(merge(sidep_sero,new_rattrapage,by="Pseudonyme"))
    sidep_sero[new_rattrapage,Pseudonyme:=i.Pseudo2,on="Pseudonyme"]
  }
}else {
  print("sidep_sero déjà chargé")
}
if(!"sidep_antigenique"%in%ls()){
  print("load sidep_antigenique")
  path_zip_antigen = paste0(pathtodata, date_AAAAMMJJ, "_sidep_antigen.csv") 
  print(path_zip_antigen)
  sidep_antigenique = data.table::fread(path_zip_antigen,
                                        na.strings="",
                                        colClasses=list(Date=date_vars,
                                                        POSIXct = posixct_vars,
                                                        character = c(char_vars,new_tag_char_vars), 
                                                        numeric = "Age"),
                                        verbose = F, sep= ";", header = TRUE, encoding = "Latin-1")
  
  
  nrow(sidep_antigenique)
  
  if(replaceP1P2){
    
    sidep_antigenique[new_rattrapage,match_ok:=T,on="Pseudonyme"]
    sidep_antigenique[is.na(match_ok),match_ok:=F]
  }
}else {
  print("sidep_antigenique déjà chargé")
}
print("02a Nb tests")
print(gc(verbose = T))
library(ISOweek)
library(lubridate)
library(data.table)
library(plotly)
library(scales)
library(bit64)
library(openxlsx)
options(scipen = 5000) # to prevent scientific notation
nb_tests <- function(type_test,type_date,day_init,jour){
  
  date_AAAAMMJJ = format(jour,format="%Y%m%d")
  if(type_test=='pcr'){
    sidep <- sidep_pcr
  }else if(type_test=='sero'){
    sidep <- sidep_sero
  }else if(type_test=='antigenique'){
    sidep <- sidep_antigenique
  }else if(type_test=='antigen-complet'){
    sidep <- sidep_antigen_complet
  }else if(type_test=='pcr_complet'){
    sidep <- sidep_pcr_complet
  }else if(type_test=='pcr_respi'){
    sidep <- sidep_pcr_respi
  }else if(type_test=="pcr_sal_mil"){
    sidep <- sidep_pcr_sal_mil
  }else if(type_test=="pcr_sal"){
    sidep <- sidep_pcr_sal_mil[AnalyseConclusion == "94845-5"]
  }else if(type_test=="pcr_mil"){
    sidep <- sidep_pcr_sal_mil[AnalyseConclusion == "94309-2"]
  }
  
  if(type_date == "prelev"){
    sidep <- sidep[, date:=as.Date(get(paste0('day_', type_date)))]
  }else if(type_date == "valid"){
    sidep <- sidep[, date:=as.Date(get(paste0('date_', type_date, '_init')))]
  }
  sidep <- sidep[!is.na(date),]
  sidep <- sidep[date>=as.Date(day_init) & date<jour,]
  
  table(sidep[date=="2020-10-20"]$ProfPrelevTA)
  sum(is.na(sidep[date=="2020-10-20"]$ProfPrelevTA))
  table(sidep[date=="2020-10-20"]$cat_pp)
  sum(is.na(sidep[date=="2020-10-20"]$cat_pp))
  
  
  nb_tests = dcast(sidep[,.(nb_res = (.N)), 
                         by=.(date, cat_pp,valeur)], 
                   date ~ cat_pp+paste0('nb_',valeur), 
                   value.var = 'nb_res')
  nb_tests = nb_tests[order(date)]
  
  nb_tests[is.na(nb_tests)]=0
  
  nb_tests[,nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'nb']
  nb_tests[,nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests)%like%'nb_P|nb_N|nb_I|nb_X'&
             !names(nb_tests)%like%'nb_NA']
  nb_tests[,prop_pos_PNIX:=rowSums(.SD)/nb_tot_PNIX,.SDcols=names(nb_tests)%like%'nb_P']
  
  nb_tests[,public_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
             !names(nb_tests)%like%'public_nb_NA']
  nb_tests[,public_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'public_nb']
  
  nb_tests[,privé_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
             !names(nb_tests)%like%'privé_nb_NA']
  nb_tests[,privé_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'privé_nb']
  
  nb_tests[,NA_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests)%like%'NA_nb_P|NA_nb_N|NA_nb_I|NA_nb_X'&
             !names(nb_tests)%like%'NA_nb_NA']
  nb_tests[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'NA_nb']
  
  nb_tests[,nb_pos:=rowSums(.SD),.SDcols=names(nb_tests)%like%'nb_P']
  nb_tests[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'NA_nb']
  
  
  nb_tests <- nb_tests[,.(date,public_nb_tot_PNIX,privé_nb_tot_PNIX,NA_nb_tot_PNIX,
                          nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  nb_tests_svg <- nb_tests
  nb_tests <- nb_tests_svg
  
  
  
  nb_tests_reg = dcast(sidep[,.(nb_res = (.N)), 
                             by=.(date,reg,cat_pp,valeur)], 
                       date+reg ~cat_pp+paste0('nb_',valeur), 
                       value.var = 'nb_res')
  reg2019 = data.table::fread(paste0("data/utils/region2019_v2.csv"), encoding = "Latin-1")
  reg2019$reg = as.character(reg2019$reg)
  reg2019[nchar(reg)==1, reg := paste0("0", reg)]
  nb_tests_reg = merge(nb_tests_reg, reg2019[,.(reg, libelle)], by="reg", all.x=T)
  setnames(nb_tests_reg, "libelle", "nom_reg")
  nb_tests_reg = nb_tests_reg[order(nom_reg)]
  nb_tests_reg[is.na(nom_reg),nom_reg:='Région inconnue']
  
  
  nb_tests_reg[is.na(nb_tests_reg)]=0
  
  nb_tests_reg[,nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'nb']
  nb_tests_reg[,nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'nb_P|nb_N|nb_I|nb_X'&
                 !names(nb_tests_reg)%like%'nb_NA']
  nb_tests_reg[,prop_pos_PNIX:=rowSums(.SD)/nb_tot_PNIX,.SDcols=names(nb_tests_reg)%like%'nb_P']
  
  nb_tests_reg[,public_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
                 !names(nb_tests_reg)%like%'public_nb_NA']
  nb_tests_reg[,public_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'public_nb']
  
  nb_tests_reg[,privé_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
                 !names(nb_tests_reg)%like%'privé_nb_NA']
  nb_tests_reg[,privé_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'privé_nb']
  
  nb_tests_reg[,NA_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'NA_nb_P|NA_nb_N|NA_nb_I|NA_nb_X'&
                 !names(nb_tests_reg)%like%'NA_nb_NA']
  nb_tests_reg[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'NA_nb']
  
  nb_tests_reg[,nb_pos:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'nb_P']
  
  nb_tests_reg <- nb_tests_reg[,.(date,nom_reg,public_nb_tot_PNIX,privé_nb_tot_PNIX,NA_nb_tot_PNIX,
                                  nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  
  
  nb_tests_dep = dcast(sidep[,.(nb_res = (.N)), 
                             by=.(date,departement,cat_pp,valeur)], 
                       date+departement ~cat_pp+paste0('nb_',valeur), 
                       value.var = 'nb_res')
  
  nb_tests_dep[is.na(nb_tests_dep)]=0
  
  nb_tests_dep[,nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb']
  nb_tests_dep[,nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb_P|nb_N|nb_I|nb_X'&
                 !names(nb_tests_dep)%like%'nb_NA']
  nb_tests_dep[,prop_pos_PNIX:=rowSums(.SD)/nb_tot_PNIX,.SDcols=names(nb_tests_dep)%like%'nb_P']
  
  nb_tests_dep[,public_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
                 !names(nb_tests_dep)%like%'public_nb_NA']
  nb_tests_dep[,public_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'public_nb']
  
  nb_tests_dep[,privé_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
                 !names(nb_tests_dep)%like%'privé_nb_NA']
  nb_tests_dep[,privé_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'privé_nb']
  
  nb_tests_dep[,NA_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_nb_P|NA_nb_N|NA_nb_I|NA_nb_X'&
                 !names(nb_tests_dep)%like%'NA_nb_NA']
  nb_tests_dep[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_nb']
  
  nb_tests_dep[,nb_pos:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb_P']
  
  nb_tests_dep <- nb_tests_dep[,.(date,departement,public_nb_tot_PNIX,privé_nb_tot_PNIX,NA_nb_tot_PNIX,
                                  nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  
  wb <- loadWorkbook("data/sorties/nb_tests/lisez_moi.xlsx")
  
  addWorksheet(wb,'national')
  writeData(wb,'national',nb_tests)
  setColWidths(wb,'national',cols=1:ncol(nb_tests),widths = 12)
  addStyle(wb,sheet='national',style=createStyle(numFmt = "# ##0"),cols=c(2,3,4,5,6,8),rows=1:nrow(nb_tests)+1,gridExpand=T,stack=T)
  addStyle(wb,sheet='national',style=createStyle(numFmt = "0.0 %"),cols=c(7),rows=1:nrow(nb_tests)+1,gridExpand=T,stack=T)
  
  addWorksheet(wb,'regions')
  writeData(wb,'regions',nb_tests_reg)
  setColWidths(wb,'regions',cols=1:ncol(nb_tests_reg),widths = 12)
  addStyle(wb,sheet='regions',style=createStyle(numFmt = "# ##0"),cols=c(3,4,5,6,7,9),rows=1:nrow(nb_tests_reg)+1,gridExpand=T,stack=T)
  addStyle(wb,sheet='regions',style=createStyle(numFmt = "0.0 %"),cols=c(8),rows=1:nrow(nb_tests_reg)+1,gridExpand=T,stack=T)
  
  addWorksheet(wb,'departements')
  writeData(wb,'departements',nb_tests_dep)
  setColWidths(wb,'departements',cols=1:ncol(nb_tests_dep),widths = 12)
  addStyle(wb,sheet='departements',style=createStyle(numFmt = "# ##0"),cols=c(3,4,5,6,7,9),rows=1:nrow(nb_tests_dep)+1,gridExpand=T,stack=T)
  addStyle(wb,sheet='departements',style=createStyle(numFmt = "0.0 %"),cols=c(8),rows=1:nrow(nb_tests_dep)+1,gridExpand=T,stack=T)
  
  saveWorkbook(wb,file=paste0( "data/sorties/nb_tests/",date_AAAAMMJJ,"_sidep_",type_test,"_",type_date,".xlsx"),overwrite = T)
}
nb_tests_AG <- function(type_test,type_date,day_init,jour){
  
  date_AAAAMMJJ = format(jour,format="%Y%m%d")
  
  
  sidep <- data.table::copy(sidep_antigenique)
  
  if(type_date == "prelev"){
    sidep <- sidep[, date:=as.Date(get(paste0('day_', type_date)))]
  }else if(type_date == "valid"){
    sidep <- sidep[, date:=as.Date(get(paste0('date_', type_date, '_init')))]
  }
  sidep <- sidep[!is.na(date),]
  sidep <- sidep[date>=as.Date(day_init) & date<jour,]
  
  
  print(sidep[, unique(ProfPrelevTA)])
  nb_tests = dcast(sidep[,.(nb_res = (.N)), 
                         by=.(date, cat_pp, ProfPrelevTA, valeur)], 
                   date ~ ProfPrelevTA + cat_pp + paste0('nb_',valeur), 
                   value.var = 'nb_res')
  nb_tests = nb_tests[order(date)]
  
  nb_tests[is.na(nb_tests)]=0
  
  nb_tests[,nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'nb']
  nb_tests[,nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'nb_P|nb_N|nb_I|nb_X'&
             !names(nb_tests)%like%'nb_NA']
  nb_tests[,prop_pos_PNIX:=rowSums(.SD,na.rm=TRUE)/nb_tot_PNIX,.SDcols=names(nb_tests)%like%'nb_P']
  
  nb_tests[,public_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
             !names(nb_tests)%like%'public_nb_NA']
  nb_tests[,public_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'NA_public_nb']
  
  nb_tests[,privé_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
             !names(nb_tests)%like%'privé_nb_NA']
  nb_tests[,privé_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'NA_privé_nb']
  
  nb_tests[,nb_pos:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'nb_P']
  
  nb_tests[,MEDECIN_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'MEDECIN_NA_nb_P|MEDECIN_NA_nb_N|MEDECIN_NA_nb_I|MEDECIN_NA_nb_X'&
             !names(nb_tests)%like%'MEDECIN_NA_nb_NA']
  nb_tests[,MEDECIN_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'MEDECIN_NA_nb']
  
  nb_tests[,INFIRMIER_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'INFIRMIER_NA_nb_P|INFIRMIER_NA_nb_N|INFIRMIER_NA_nb_I|INFIRMIER_NA_nb_X'&
             !names(nb_tests)%like%'INFIRMIER_NA_nb_NA']
  nb_tests[,INFIRMIER_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'INFIRMIER_NA_nb']
  
  nb_tests[,PHARMACIEN_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'PHARMACIEN_NA_nb_P|PHARMACIEN_NA_nb_N|PHARMACIEN_NA_nb_I|PHARMACIEN_NA_nb_X'&
             !names(nb_tests)%like%'PHARMACIEN_NA_nb_NA']
  nb_tests[,PHARMACIEN_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'PHARMACIEN_NA_nb']
  
  nb_tests[,DOCTEUR_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'DOCTEUR_NA_nb_P|DOCTEUR_NA_nb_N|DOCTEUR_NA_nb_I|DOCTEUR_NA_nb_X'&
             !names(nb_tests)%like%'DOCTEUR_NA_nb_NA']
  nb_tests[,DOCTEUR_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'DOCTEUR_NA_nb']
  
  nb_tests[,BIOLOGISTE_nb_tot_PNIX:=rowSums(.SD),
           .SDcols=names(nb_tests)%like%'BIOLOGISTE_NA_nb_P|BIOLOGISTE_NA_nb_N|BIOLOGISTE_NA_nb_I|BIOLOGISTE_NA_nb_X'&
             !names(nb_tests)%like%'BIOLOGISTE_NA_nb_NA']
  nb_tests[,BIOLOGISTE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'BIOLOGISTE_NA_nb']
  
  nb_tests[,MASSEURKINESITHERAPE_nb_tot_PNIX:=rowSums(.SD),
           .SDcols=names(nb_tests)%like%'MASSEURKINESITHERAPE_NA_nb_P|MASSEURKINESITHERAPE_NA_nb_N|MASSEURKINESITHERAPE_NA_nb_I|MASSEURKINESITHERAPE_NA_nb_X'&
             !names(nb_tests)%like%'MASSEURKINESITHERAPE_NA_nb_NA']
  nb_tests[,MASSEURKINESITHERAPE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'MASSEURKINESITHERAPE_NA_nb']
  
  nb_tests[,SAGEFEMME_nb_tot_PNIX:=rowSums(.SD),
           .SDcols=names(nb_tests)%like%'SAGEFEMME_NA_nb_P|SAGEFEMME_NA_nb_N|SAGEFEMME_NA_nb_I|SAGEFEMME_NA_nb_X'&
             !names(nb_tests)%like%'SAGEFEMME_NA_nb_NA']
  nb_tests[,SAGEFEMME_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'SAGEFEMME_NA_nb']
  
  nb_tests[,CHIRURGIENDENTISTE_nb_tot_PNIX:=rowSums(.SD),
           .SDcols=names(nb_tests)%like%'CHIRURGIENDENTISTE_NA_nb_P|CHIRURGIENDENTISTE_NA_nb_N|CHIRURGIENDENTISTE_NA_nb_I|CHIRURGIENDENTISTE_NA_nb_X'&
             !names(nb_tests)%like%'CHIRURGIENDENTISTE_NA_nb_NA']
  nb_tests[,CHIRURGIENDENTISTE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'CHIRURGIENDENTISTE_NA_nb']
  
  nb_tests[,NA_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'NA_NA_nb_P|NA_NA_nb_N|NA_NA_nb_I|NA_NA_nb_X']
  
  nb_tests[,NA_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'NA_NA_nb']
  
  nb_tests[, verif := CHIRURGIENDENTISTE_nb_tot_PNIX + SAGEFEMME_nb_tot_PNIX + MASSEURKINESITHERAPE_nb_tot_PNIX+
             BIOLOGISTE_nb_tot_PNIX + DOCTEUR_nb_tot_PNIX + PHARMACIEN_nb_tot_PNIX + INFIRMIER_nb_tot_PNIX + 
             MEDECIN_nb_tot_PNIX + NA_nb_tot_PNIX + privé_nb_tot_PNIX + public_nb_tot_PNIX - nb_tot_PNIX]
  
  if (sum(nb_tests$verif,na.rm=T)>0) print("Il y a un probleme de sommation entre les catégories PNIX.")
  
  nb_tests[, verif := CHIRURGIENDENTISTE_nb_tot + SAGEFEMME_nb_tot + MASSEURKINESITHERAPE_nb_tot+
             BIOLOGISTE_nb_tot + DOCTEUR_nb_tot + PHARMACIEN_nb_tot + INFIRMIER_nb_tot + 
             MEDECIN_nb_tot + NA_nb_tot + privé_nb_tot + public_nb_tot - nb_tot]
  
  if (sum(nb_tests$verif,na.rm=T)>0) print("Il y a un probleme de sommation entre les catégories toute norme confondue.")
  
  nb_tests <- nb_tests[,.(date, MEDECIN_nb_tot_PNIX,PHARMACIEN_nb_tot_PNIX,INFIRMIER_nb_tot_PNIX, DOCTEUR_nb_tot_PNIX,
                          BIOLOGISTE_nb_tot_PNIX,CHIRURGIENDENTISTE_nb_tot_PNIX,SAGEFEMME_nb_tot_PNIX,
                          MASSEURKINESITHERAPE_nb_tot_PNIX,
                          public_nb_tot_PNIX,privé_nb_tot_PNIX, NA_nb_tot,NA_nb_tot_PNIX,
                          nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  nb_tests_top_prelev_ta = dcast(sidep[,.(nb_res = (.N)), 
                                       by=.(date, TopPrelevTA, valeur)], 
                                 date ~ TopPrelevTA + paste0('nb_',valeur), 
                                 value.var = 'nb_res')
  nb_tests_top_prelev_ta = nb_tests_top_prelev_ta[order(date)]
  
  nb_tests_top_prelev_ta[, top_prelev_ta_PNIX := rowSums(.SD, na.rm=TRUE),
                         .SDcols=names(nb_tests_top_prelev_ta)%like%'1_nb_I|1_nb_N|1_nb_P|1_nb_X']
  nb_tests_top_prelev_ta[, top_prelev_ta := rowSums(.SD, na.rm = TRUE),
                         .SDcols=names(nb_tests_top_prelev_ta)%like%'1_nb']
  nb_tests_top_prelev_ta <- nb_tests_top_prelev_ta[, .(date, top_prelev_ta_PNIX, top_prelev_ta)]
  
  nb_tests <- merge(nb_tests, nb_tests_top_prelev_ta, by = "date", all = TRUE)
  
  nb_tests[, verif := top_prelev_ta_PNIX - MEDECIN_nb_tot_PNIX -PHARMACIEN_nb_tot_PNIX- INFIRMIER_nb_tot_PNIX-  DOCTEUR_nb_tot_PNIX-
             BIOLOGISTE_nb_tot_PNIX - CHIRURGIENDENTISTE_nb_tot_PNIX -SAGEFEMME_nb_tot_PNIX -
             MASSEURKINESITHERAPE_nb_tot_PNIX]
  
  if (sum(nb_tests$verif,na.rm=T)>0) print("Il y a un probleme de sommation entre le total PS topprelevTA et le total par profession.")
  
  nb_tests_svg <- nb_tests
  nb_tests <- nb_tests_svg
  
  
  
  nb_tests_reg = dcast(sidep[,.(nb_res = (.N)), 
                             by=.(date,reg, ProfPrelevTA,cat_pp,valeur)], 
                       date+reg ~ProfPrelevTA + cat_pp+paste0('nb_',valeur), 
                       value.var = 'nb_res')
  reg2019 = data.table::fread(paste0("data/utils/region2019_v2.csv"), encoding = "Latin-1")
  reg2019$reg = as.character(reg2019$reg)
  reg2019[nchar(reg)==1, reg := paste0("0", reg)]
  nb_tests_reg = merge(nb_tests_reg, reg2019[,.(reg, libelle)], by="reg", all.x=T)
  data.table::setnames(nb_tests_reg, "libelle", "nom_reg")
  nb_tests_reg = nb_tests_reg[order(nom_reg)]
  nb_tests_reg[is.na(nom_reg),nom_reg:='Région inconnue']
  
  
  nb_tests_reg[is.na(nb_tests_reg)]=0
  
  nb_tests_reg[,nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'nb']
  nb_tests_reg[,nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'nb_P|nb_N|nb_I|nb_X'&
                 !names(nb_tests_reg)%like%'nb_NA']
  nb_tests_reg[,prop_pos_PNIX:=rowSums(.SD,na.rm=TRUE)/nb_tot_PNIX,.SDcols=names(nb_tests_reg)%like%'nb_P']
  
  nb_tests_reg[,public_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
                 !names(nb_tests_reg)%like%'public_nb_NA']
  nb_tests_reg[,public_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'NA_public_nb']
  
  nb_tests_reg[,privé_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
                 !names(nb_tests_reg)%like%'privé_nb_NA']
  nb_tests_reg[,privé_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'NA_privé_nb']
  
  nb_tests_reg[,nb_pos:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'nb_P']
  
  nb_tests_reg[,MEDECIN_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'MEDECIN_NA_nb_P|MEDECIN_NA_nb_N|MEDECIN_NA_nb_I|MEDECIN_NA_nb_X'&
                 !names(nb_tests_reg)%like%'MEDECIN_NA_nb_NA']
  nb_tests_reg[,MEDECIN_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'MEDECIN_NA_nb']
  
  nb_tests_reg[,INFIRMIER_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'INFIRMIER_NA_nb_P|INFIRMIER_NA_nb_N|INFIRMIER_NA_nb_I|INFIRMIER_NA_nb_X'&
                 !names(nb_tests_reg)%like%'INFIRMIER_nb_NA']
  nb_tests_reg[,INFIRMIER_nb_tot:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'INFIRMIER_NA_nb']
  
  nb_tests_reg[,PHARMACIEN_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'PHARMACIEN_NA_nb_P|PHARMACIEN_NA_nb_N|PHARMACIEN_NA_nb_I|PHARMACIEN_NA_nb_X'&
                 !names(nb_tests_reg)%like%'PHARMACIEN_NA_nb_NA']
  nb_tests_reg[,PHARMACIEN_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'PHARMACIEN_NA_nb']
  
  nb_tests_reg[,DOCTEUR_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'DOCTEUR_NA_nb_P|DOCTEUR_NA_nb_N|DOCTEUR_NA_nb_I|DOCTEUR_NA_nb_X'&
                 !names(nb_tests_reg)%like%'DOCTEUR_NA_nb_NA']
  nb_tests_reg[,DOCTEUR_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'DOCTEUR_NA_nb']
  
  nb_tests_reg[,BIOLOGISTE_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'BIOLOGISTE_NA_nb_P|BIOLOGISTE_NA_nb_N|BIOLOGISTE_NA_nb_I|BIOLOGISTE_NA_nb_X'&
                 !names(nb_tests_reg)%like%'BIOLOGISTE_NA_nb_NA']
  nb_tests_reg[,BIOLOGISTE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'BIOLOGISTE_NA_nb']
  
  nb_tests_reg[,MASSEURKINESITHERAPE_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'MASSEURKINESITHERAPE_NA_nb_P|MASSEURKINESITHERAPE_NA_nb_N|MASSEURKINESITHERAPE_NA_nb_I|MASSEURKINESITHERAPE_NA_nb_X'&
                 !names(nb_tests_reg)%like%'MASSEURKINESITHERAPE_NA_nb_NA']
  nb_tests_reg[,MASSEURKINESITHERAPE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'MASSEURKINESITHERAPE_NA_nb']
  
  nb_tests_reg[,SAGEFEMME_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'SAGEFEMME_NA_nb_P|SAGEFEMME_NA_nb_N|SAGEFEMME_NA_nb_I|SAGEFEMME_NA_nb_X'&
                 !names(nb_tests_reg)%like%'SAGEFEMME_NA_nb_NA']
  nb_tests_reg[,SAGEFEMME_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'SAGEFEMME_NA_nb']
  
  nb_tests_reg[,CHIRURGIENDENTISTE_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'CHIRURGIENDENTISTE_NA_nb_P|CHIRURGIENDENTISTE_NA_nb_N|CHIRURGIENDENTISTE_NA_nb_I|CHIRURGIENDENTISTE_NA_nb_X'&
                 !names(nb_tests_reg)%like%'CHIRURGIENDENTISTE_NA_nb_NA']
  nb_tests_reg[,CHIRURGIENDENTISTE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'CHIRURGIENDENTISTE_NA_nb']
  
  
  nb_tests_reg[,NA_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%
                 'NA_NA_nb_P|NA_NA_nb_N|NA_NA_nb_I|NA_NA_nb_X']
  
  nb_tests_reg[,NA_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'NA_NA_nb']
  
  
  nb_tests_reg <- nb_tests_reg[,.(date,nom_reg, MEDECIN_nb_tot_PNIX,PHARMACIEN_nb_tot_PNIX,
                                  INFIRMIER_nb_tot_PNIX, DOCTEUR_nb_tot_PNIX,BIOLOGISTE_nb_tot_PNIX,
                                  CHIRURGIENDENTISTE_nb_tot_PNIX,SAGEFEMME_nb_tot_PNIX,
                                  MASSEURKINESITHERAPE_nb_tot_PNIX,
                                  public_nb_tot_PNIX,privé_nb_tot_PNIX, NA_nb_tot,NA_nb_tot_PNIX,
                                  nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  
  
  nb_tests_dep = dcast(sidep[,.(nb_res = (.N)), 
                             by=.(date,ProfPrelevTA, departement,cat_pp,valeur)], 
                       date+departement ~ ProfPrelevTA + cat_pp+paste0('nb_',valeur), 
                       value.var = 'nb_res')
  
  
  nb_tests_dep[is.na(nb_tests_dep)]=0
  
  nb_tests_dep[,nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb']
  nb_tests_dep[,nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb_P|nb_N|nb_I|nb_X'&
                 !names(nb_tests_dep)%like%'nb_NA']
  nb_tests_dep[,prop_pos_PNIX:=rowSums(.SD)/nb_tot_PNIX,.SDcols=names(nb_tests_dep)%like%'nb_P']
  
  nb_tests_dep[,public_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
                 !names(nb_tests_dep)%like%'public_nb_NA']
  nb_tests_dep[,public_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_public_nb']
  
  nb_tests_dep[,privé_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
                 !names(nb_tests_dep)%like%'privé_nb_NA']
  nb_tests_dep[,privé_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_privé_nb']
  
  nb_tests_dep[,nb_pos:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb_P']
  
  nb_tests_dep[,MEDECIN_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'MEDECIN_NA_nb_P|MEDECIN_NA_nb_N|MEDECIN_NA_nb_I|MEDECIN_NA_nb_X'&
                 !names(nb_tests_dep)%like%'MEDECIN_NA_nb_NA']
  nb_tests_dep[,MEDECIN_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'MEDECIN_NA_nb']
  
  nb_tests_dep[,INFIRMIER_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'INFIRMIER_NA_nb_P|INFIRMIER_NA_nb_N|INFIRMIER_NA_nb_I|INFIRMIER_NA_nb_X'&
                 !names(nb_tests_dep)%like%'INFIRMIER_NA_nb_NA']
  nb_tests_dep[,INFIRMIER_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'INFIRMIER_NA_nb']
  
  nb_tests_dep[,PHARMACIEN_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'PHARMACIEN_NA_nb_P|PHARMACIEN_NA_nb_N|PHARMACIEN_NA_nb_I|PHARMACIEN_NA_nb_X'&
                 !names(nb_tests_dep)%like%'PHARMACIEN_NA_nb_NA']
  nb_tests_dep[,PHARMACIEN_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'PHARMACIEN_NA_nb']
  
  nb_tests_dep[,DOCTEUR_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'DOCTEUR_NA_nb_P|DOCTEUR_NA_nb_N|DOCTEUR_NA_nb_I|DOCTEUR_NA_nb_X'&
                 !names(nb_tests_dep)%like%'DOCTEUR_NA_nb_NA']
  nb_tests_dep[,DOCTEUR_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'DOCTEUR_NA_nb']
  
  nb_tests_dep[,BIOLOGISTE_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'BIOLOGISTE_NA_nb_P|BIOLOGISTE_NA_nb_N|BIOLOGISTE_NA_nb_I|BIOLOGISTE_NA_nb_X'&
                 !names(nb_tests_dep)%like%'BIOLOGISTE_NA_nb_NA']
  nb_tests_dep[,BIOLOGISTE_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'BIOLOGISTE_NA_nb']
  
  nb_tests_dep[,MASSEURKINESITHERAPE_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'MASSEURKINESITHERAPE_NA_nb_P|MASSEURKINESITHERAPE_NA_nb_N|MASSEURKINESITHERAPE_NA_nb_I|MASSEURKINESITHERAPE_NA_nb_X'&
                 !names(nb_tests_dep)%like%'MASSEURKINESITHERAPE_NA_nb_NA']
  nb_tests_dep[,MASSEURKINESITHERAPE_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'MASSEURKINESITHERAPE_NA_nb']
  
  nb_tests_dep[,SAGEFEMME_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'SAGEFEMME_NA_nb_P|SAGEFEMME_NA_nb_N|SAGEFEMME_NA_nb_I|SAGEFEMME_NA_nb_X'&
                 !names(nb_tests_dep)%like%'SAGEFEMME_NA_nb_NA']
  nb_tests_dep[,SAGEFEMME_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'SAGEFEMME_NA_nb']
  
  nb_tests_dep[,CHIRURGIENDENTISTE_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'CHIRURGIENDENTISTE_NA_nb_P|CHIRURGIENDENTISTE_NA_nb_N|CHIRURGIENDENTISTE_NA_nb_I|CHIRURGIENDENTISTE_NA_nb_X'&
                 !names(nb_tests_dep)%like%'CHIRURGIENDENTISTE_NA_nb_NA']
  nb_tests_dep[,CHIRURGIENDENTISTE_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'CHIRURGIENDENTISTE_NA_nb']
  
  
  nb_tests_dep[,NA_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%
                 'NA_NA_nb_P|NA_NA_nb_N|NA_NA_nb_I|NA_NA_nb_X']
  
  nb_tests_dep[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_NA_nb']
  
  nb_tests_dep <- nb_tests_dep[,.(date,departement, MEDECIN_nb_tot_PNIX,PHARMACIEN_nb_tot_PNIX,
                                  INFIRMIER_nb_tot_PNIX, DOCTEUR_nb_tot_PNIX,BIOLOGISTE_nb_tot_PNIX,
                                  CHIRURGIENDENTISTE_nb_tot_PNIX,SAGEFEMME_nb_tot_PNIX,
                                  MASSEURKINESITHERAPE_nb_tot_PNIX,
                                  public_nb_tot_PNIX,privé_nb_tot_PNIX, NA_nb_tot,
                                  nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  
  wb <- loadWorkbook("data/sorties/nb_tests/lisez_moi_TAG.xlsx")
  
  addWorksheet(wb,'national')
  writeData(wb,'national',nb_tests)
  setColWidths(wb,'national',cols=1:ncol(nb_tests),widths = 12)
  
  addWorksheet(wb,'regions')
  writeData(wb,'regions',nb_tests_reg)
  setColWidths(wb,'regions',cols=1:ncol(nb_tests_reg),widths = 12)
  
  addWorksheet(wb,'departements')
  writeData(wb,'departements',nb_tests_dep)
  setColWidths(wb,'departements',cols=1:ncol(nb_tests_dep),widths = 12)
  
  saveWorkbook(wb,file=paste0( "data/sorties/nb_tests/",date_AAAAMMJJ,"_sidep_",type_test,"_",type_date,".xlsx"),overwrite = T)
}
nb_tests(type_test='pcr',type_date='prelev',day_init='2020-05-20',jour=jour_date)
nb_tests(type_test='pcr',type_date='valid',day_init='2020-05-24',jour=jour_date)
nb_tests(type_test='sero',type_date='prelev',day_init='2020-06-10',jour=jour_date)
nb_tests(type_test='sero',type_date='valid',day_init='2020-06-12',jour=jour_date)
nb_tests_AG(type_test='antigenique',type_date='prelev',day_init='2020-10-16',jour=jour_date)
nb_tests_AG(type_test='antigenique',type_date='valid',day_init='2020-10-16',jour=jour_date)
nb_tests(type_test='pcr_sal_mil',type_date='prelev',day_init='2020-10-01',jour=jour_date)
nb_tests(type_test='pcr_sal_mil',type_date='valid',day_init='2020-10-01',jour=jour_date)
print("02a 65+")
print(gc(verbose = T))
library(ISOweek)
library(lubridate)
library(data.table)
library(plotly)
library(scales)
library(bit64)
library(openxlsx)
options(scipen = 5000) # to prevent scientific notation
nb_tests_65 <- function(type_test,type_date,day_init,jour){
  
  date_AAAAMMJJ = format(jour,format="%Y%m%d")
  if(type_test=='pcr'){
    sidep <- sidep_pcr
  }else if(type_test=='sero'){
    sidep <- sidep_sero
  }else if(type_test=='antigenique'){
    sidep <- sidep_antigenique
  }else if(type_test=="pcr_sal_mil"){
    sidep <- sidep_pcr_sal_mil
  }
  
  if(type_date == "prelev"){
    sidep <- sidep[, date:=as.Date(get(paste0('day_', type_date)))]
  }else if(type_date == "valid"){
    sidep <- sidep[, date:=as.Date(get(paste0('date_', type_date, '_init')))]
  }
  sidep <- sidep[!is.na(date),]
  sidep <- sidep[date>=as.Date(day_init) & date<jour,]
  
  sidep = sidep[as.numeric(Age) >= 65]
  
  table(sidep[date=="2020-10-20"]$ProfPrelevTA)
  sum(is.na(sidep[date=="2020-10-20"]$ProfPrelevTA))
  table(sidep[date=="2020-10-20"]$cat_pp)
  sum(is.na(sidep[date=="2020-10-20"]$cat_pp))
  
  
  nb_tests = dcast(sidep[,.(nb_res = (.N)), 
                         by=.(date, cat_pp,valeur)], 
                   date ~ cat_pp+paste0('nb_',valeur), 
                   value.var = 'nb_res')
  nb_tests = nb_tests[order(date)]
  
  nb_tests[is.na(nb_tests)]=0
  
  nb_tests[,nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'nb']
  nb_tests[,nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests)%like%'nb_P|nb_N|nb_I|nb_X'&
             !names(nb_tests)%like%'nb_NA']
  nb_tests[,prop_pos_PNIX:=rowSums(.SD)/nb_tot_PNIX,.SDcols=names(nb_tests)%like%'nb_P']
  
  nb_tests[,public_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
             !names(nb_tests)%like%'public_nb_NA']
  nb_tests[,public_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'public_nb']
  
  nb_tests[,privé_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
             !names(nb_tests)%like%'privé_nb_NA']
  nb_tests[,privé_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'privé_nb']
  
  nb_tests[,NA_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests)%like%'NA_nb_P|NA_nb_N|NA_nb_I|NA_nb_X'&
             !names(nb_tests)%like%'NA_nb_NA']
  nb_tests[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'NA_nb']
  
  nb_tests[,nb_pos:=rowSums(.SD),.SDcols=names(nb_tests)%like%'nb_P']
  nb_tests[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests)%like%'NA_nb']
  
  
  nb_tests <- nb_tests[,.(date,public_nb_tot_PNIX,privé_nb_tot_PNIX,NA_nb_tot_PNIX,
                          nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  nb_tests_svg <- nb_tests
  nb_tests <- nb_tests_svg
  
  
  
  nb_tests_reg = dcast(sidep[,.(nb_res = (.N)), 
                             by=.(date,reg,cat_pp,valeur)], 
                       date+reg ~cat_pp+paste0('nb_',valeur), 
                       value.var = 'nb_res')
  reg2019 = data.table::fread(paste0("data/utils/region2019_v2.csv"), encoding = "Latin-1")
  reg2019$reg = as.character(reg2019$reg)
  reg2019[nchar(reg)==1, reg := paste0("0", reg)]
  nb_tests_reg = merge(nb_tests_reg, reg2019[,.(reg, libelle)], by="reg", all.x=T)
  setnames(nb_tests_reg, "libelle", "nom_reg")
  nb_tests_reg = nb_tests_reg[order(nom_reg)]
  nb_tests_reg[is.na(nom_reg),nom_reg:='Région inconnue']
  
  
  nb_tests_reg[is.na(nb_tests_reg)]=0
  
  nb_tests_reg[,nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'nb']
  nb_tests_reg[,nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'nb_P|nb_N|nb_I|nb_X'&
                 !names(nb_tests_reg)%like%'nb_NA']
  nb_tests_reg[,prop_pos_PNIX:=rowSums(.SD)/nb_tot_PNIX,.SDcols=names(nb_tests_reg)%like%'nb_P']
  
  nb_tests_reg[,public_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
                 !names(nb_tests_reg)%like%'public_nb_NA']
  nb_tests_reg[,public_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'public_nb']
  
  nb_tests_reg[,privé_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
                 !names(nb_tests_reg)%like%'privé_nb_NA']
  nb_tests_reg[,privé_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'privé_nb']
  
  nb_tests_reg[,NA_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'NA_nb_P|NA_nb_N|NA_nb_I|NA_nb_X'&
                 !names(nb_tests_reg)%like%'NA_nb_NA']
  nb_tests_reg[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'NA_nb']
  
  nb_tests_reg[,nb_pos:=rowSums(.SD),.SDcols=names(nb_tests_reg)%like%'nb_P']
  
  nb_tests_reg <- nb_tests_reg[,.(date,nom_reg,public_nb_tot_PNIX,privé_nb_tot_PNIX,NA_nb_tot_PNIX,
                                  nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  
  
  nb_tests_dep = dcast(sidep[,.(nb_res = (.N)), 
                             by=.(date,departement,cat_pp,valeur)], 
                       date+departement ~cat_pp+paste0('nb_',valeur), 
                       value.var = 'nb_res')
  
  nb_tests_dep[is.na(nb_tests_dep)]=0
  
  nb_tests_dep[,nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb']
  nb_tests_dep[,nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb_P|nb_N|nb_I|nb_X'&
                 !names(nb_tests_dep)%like%'nb_NA']
  nb_tests_dep[,prop_pos_PNIX:=rowSums(.SD)/nb_tot_PNIX,.SDcols=names(nb_tests_dep)%like%'nb_P']
  
  nb_tests_dep[,public_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
                 !names(nb_tests_dep)%like%'public_nb_NA']
  nb_tests_dep[,public_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'public_nb']
  
  nb_tests_dep[,privé_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
                 !names(nb_tests_dep)%like%'privé_nb_NA']
  nb_tests_dep[,privé_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'privé_nb']
  
  nb_tests_dep[,NA_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_nb_P|NA_nb_N|NA_nb_I|NA_nb_X'&
                 !names(nb_tests_dep)%like%'NA_nb_NA']
  nb_tests_dep[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_nb']
  
  nb_tests_dep[,nb_pos:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb_P']
  
  nb_tests_dep <- nb_tests_dep[,.(date,departement,public_nb_tot_PNIX,privé_nb_tot_PNIX,NA_nb_tot_PNIX,
                                  nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  
  wb <- loadWorkbook("data/sorties/nb_tests/lisez_moi.xlsx")
  
  addWorksheet(wb,'national')
  writeData(wb,'national',nb_tests)
  setColWidths(wb,'national',cols=1:ncol(nb_tests),widths = 12)
  addStyle(wb,sheet='national',style=createStyle(numFmt = "# ##0"),cols=c(2,3,4,5,6,8),rows=1:nrow(nb_tests)+1,gridExpand=T,stack=T)
  addStyle(wb,sheet='national',style=createStyle(numFmt = "0.0 %"),cols=c(7),rows=1:nrow(nb_tests)+1,gridExpand=T,stack=T)
  
  addWorksheet(wb,'regions')
  writeData(wb,'regions',nb_tests_reg)
  setColWidths(wb,'regions',cols=1:ncol(nb_tests_reg),widths = 12)
  addStyle(wb,sheet='regions',style=createStyle(numFmt = "# ##0"),cols=c(3,4,5,6,7,9),rows=1:nrow(nb_tests_reg)+1,gridExpand=T,stack=T)
  addStyle(wb,sheet='regions',style=createStyle(numFmt = "0.0 %"),cols=c(8),rows=1:nrow(nb_tests_reg)+1,gridExpand=T,stack=T)
  
  addWorksheet(wb,'departements')
  writeData(wb,'departements',nb_tests_dep)
  setColWidths(wb,'departements',cols=1:ncol(nb_tests_dep),widths = 12)
  addStyle(wb,sheet='departements',style=createStyle(numFmt = "# ##0"),cols=c(3,4,5,6,7,9),rows=1:nrow(nb_tests_dep)+1,gridExpand=T,stack=T)
  addStyle(wb,sheet='departements',style=createStyle(numFmt = "0.0 %"),cols=c(8),rows=1:nrow(nb_tests_dep)+1,gridExpand=T,stack=T)
  
  saveWorkbook(wb,file=paste0( "data/sorties/nb_tests/",date_AAAAMMJJ,"_sidep_",type_test,"_",type_date,"_65+.xlsx"),overwrite = T)
}
nb_tests_AG_65 <- function(type_test,type_date,day_init,jour){
  date_AAAAMMJJ = format(jour,format="%Y%m%d")
  sidep <- data.table::copy(sidep_antigenique)
  sidep = sidep[as.numeric(Age) >= 65]
  
  if(type_date == "prelev"){
    sidep <- sidep[, date:=as.Date(get(paste0('day_', type_date)))]
  }else if(type_date == "valid"){
    sidep <- sidep[, date:=as.Date(get(paste0('date_', type_date, '_init')))]
  }
  sidep <- sidep[!is.na(date),]
  sidep <- sidep[date>=as.Date(day_init) & date<jour,]
  
  
  print(sidep[, unique(ProfPrelevTA)])
  nb_tests = dcast(sidep[,.(nb_res = (.N)), 
                         by=.(date, cat_pp, ProfPrelevTA, valeur)], 
                   date ~ ProfPrelevTA + cat_pp + paste0('nb_',valeur), 
                   value.var = 'nb_res')
  nb_tests = nb_tests[order(date)]
  
  nb_tests[is.na(nb_tests)]=0
  
  nb_tests[,nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'nb']
  nb_tests[,nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'nb_P|nb_N|nb_I|nb_X'&
             !names(nb_tests)%like%'nb_NA']
  nb_tests[,prop_pos_PNIX:=rowSums(.SD,na.rm=TRUE)/nb_tot_PNIX,.SDcols=names(nb_tests)%like%'nb_P']
  
  nb_tests[,public_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
             !names(nb_tests)%like%'public_nb_NA']
  nb_tests[,public_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'NA_public_nb']
  
  nb_tests[,privé_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
             !names(nb_tests)%like%'privé_nb_NA']
  nb_tests[,privé_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'NA_privé_nb']
  
  nb_tests[,nb_pos:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'nb_P']
  
  nb_tests[,MEDECIN_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'MEDECIN_NA_nb_P|MEDECIN_NA_nb_N|MEDECIN_NA_nb_I|MEDECIN_NA_nb_X'&
             !names(nb_tests)%like%'MEDECIN_NA_nb_NA']
  nb_tests[,MEDECIN_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'MEDECIN_NA_nb']
  
  nb_tests[,INFIRMIER_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'INFIRMIER_NA_nb_P|INFIRMIER_NA_nb_N|INFIRMIER_NA_nb_I|INFIRMIER_NA_nb_X'&
             !names(nb_tests)%like%'INFIRMIER_NA_nb_NA']
  nb_tests[,INFIRMIER_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'INFIRMIER_NA_nb']
  
  nb_tests[,PHARMACIEN_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'PHARMACIEN_NA_nb_P|PHARMACIEN_NA_nb_N|PHARMACIEN_NA_nb_I|PHARMACIEN_NA_nb_X'&
             !names(nb_tests)%like%'PHARMACIEN_NA_nb_NA']
  nb_tests[,PHARMACIEN_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'PHARMACIEN_NA_nb']
  
  nb_tests[,DOCTEUR_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'DOCTEUR_NA_nb_P|DOCTEUR_NA_nb_N|DOCTEUR_NA_nb_I|DOCTEUR_NA_nb_X'&
             !names(nb_tests)%like%'DOCTEUR_NA_nb_NA']
  nb_tests[,DOCTEUR_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'DOCTEUR_NA_nb']
  
  nb_tests[,BIOLOGISTE_nb_tot_PNIX:=rowSums(.SD),
           .SDcols=names(nb_tests)%like%'BIOLOGISTE_NA_nb_P|BIOLOGISTE_NA_nb_N|BIOLOGISTE_NA_nb_I|BIOLOGISTE_NA_nb_X'&
             !names(nb_tests)%like%'BIOLOGISTE_NA_nb_NA']
  nb_tests[,BIOLOGISTE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'BIOLOGISTE_NA_nb']
  
  nb_tests[,MASSEURKINESITHERAPE_nb_tot_PNIX:=rowSums(.SD),
           .SDcols=names(nb_tests)%like%'MASSEURKINESITHERAPE_NA_nb_P|MASSEURKINESITHERAPE_NA_nb_N|MASSEURKINESITHERAPE_NA_nb_I|MASSEURKINESITHERAPE_NA_nb_X'&
             !names(nb_tests)%like%'MASSEURKINESITHERAPE_NA_nb_NA']
  nb_tests[,MASSEURKINESITHERAPE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'MASSEURKINESITHERAPE_NA_nb']
  
  nb_tests[,SAGEFEMME_nb_tot_PNIX:=rowSums(.SD),
           .SDcols=names(nb_tests)%like%'SAGEFEMME_NA_nb_P|SAGEFEMME_NA_nb_N|SAGEFEMME_NA_nb_I|SAGEFEMME_NA_nb_X'&
             !names(nb_tests)%like%'SAGEFEMME_NA_nb_NA']
  nb_tests[,SAGEFEMME_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'SAGEFEMME_NA_nb']
  
  nb_tests[,CHIRURGIENDENTISTE_nb_tot_PNIX:=rowSums(.SD),
           .SDcols=names(nb_tests)%like%'CHIRURGIENDENTISTE_NA_nb_P|CHIRURGIENDENTISTE_NA_nb_N|CHIRURGIENDENTISTE_NA_nb_I|CHIRURGIENDENTISTE_NA_nb_X'&
             !names(nb_tests)%like%'CHIRURGIENDENTISTE_NA_nb_NA']
  nb_tests[,CHIRURGIENDENTISTE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'CHIRURGIENDENTISTE_NA_nb']
  
  nb_tests[,NA_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
           .SDcols=names(nb_tests)%like%'NA_NA_nb_P|NA_NA_nb_N|NA_NA_nb_I|NA_NA_nb_X']
  
  nb_tests[,NA_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests)%like%'NA_NA_nb']
  
  nb_tests[, verif := CHIRURGIENDENTISTE_nb_tot_PNIX + SAGEFEMME_nb_tot_PNIX + MASSEURKINESITHERAPE_nb_tot_PNIX+
             BIOLOGISTE_nb_tot_PNIX + DOCTEUR_nb_tot_PNIX + PHARMACIEN_nb_tot_PNIX + INFIRMIER_nb_tot_PNIX + 
             MEDECIN_nb_tot_PNIX + NA_nb_tot_PNIX + privé_nb_tot_PNIX + public_nb_tot_PNIX - nb_tot_PNIX]
  
  if (sum(nb_tests$verif,na.rm=T)>0) print("Il y a un probleme de sommation entre les catégories PNIX.")
  
  nb_tests[, verif := CHIRURGIENDENTISTE_nb_tot + SAGEFEMME_nb_tot + MASSEURKINESITHERAPE_nb_tot+
             BIOLOGISTE_nb_tot + DOCTEUR_nb_tot + PHARMACIEN_nb_tot + INFIRMIER_nb_tot + 
             MEDECIN_nb_tot + NA_nb_tot + privé_nb_tot + public_nb_tot - nb_tot]
  
  if (sum(nb_tests$verif,na.rm=T)>0) print("Il y a un probleme de sommation entre les catégories toute norme confondue.")
  
  nb_tests <- nb_tests[,.(date, MEDECIN_nb_tot_PNIX,PHARMACIEN_nb_tot_PNIX,INFIRMIER_nb_tot_PNIX, DOCTEUR_nb_tot_PNIX,
                          BIOLOGISTE_nb_tot_PNIX,CHIRURGIENDENTISTE_nb_tot_PNIX,SAGEFEMME_nb_tot_PNIX,
                          MASSEURKINESITHERAPE_nb_tot_PNIX,
                          public_nb_tot_PNIX,privé_nb_tot_PNIX, NA_nb_tot,NA_nb_tot_PNIX,
                          nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  nb_tests_top_prelev_ta = dcast(sidep[,.(nb_res = (.N)), 
                                       by=.(date, TopPrelevTA, valeur)], 
                                 date ~ TopPrelevTA + paste0('nb_',valeur), 
                                 value.var = 'nb_res')
  nb_tests_top_prelev_ta = nb_tests_top_prelev_ta[order(date)]
  
  nb_tests_top_prelev_ta[, top_prelev_ta_PNIX := rowSums(.SD, na.rm=TRUE),
                         .SDcols=names(nb_tests_top_prelev_ta)%like%'1_nb_I|1_nb_N|1_nb_P|1_nb_X']
  nb_tests_top_prelev_ta[, top_prelev_ta := rowSums(.SD, na.rm = TRUE),
                         .SDcols=names(nb_tests_top_prelev_ta)%like%'1_nb']
  nb_tests_top_prelev_ta <- nb_tests_top_prelev_ta[, .(date, top_prelev_ta_PNIX, top_prelev_ta)]
  
  nb_tests <- merge(nb_tests, nb_tests_top_prelev_ta, by = "date", all = TRUE)
  
  nb_tests[, verif := top_prelev_ta_PNIX - MEDECIN_nb_tot_PNIX -PHARMACIEN_nb_tot_PNIX- INFIRMIER_nb_tot_PNIX-  DOCTEUR_nb_tot_PNIX-
             BIOLOGISTE_nb_tot_PNIX - CHIRURGIENDENTISTE_nb_tot_PNIX -SAGEFEMME_nb_tot_PNIX -
             MASSEURKINESITHERAPE_nb_tot_PNIX]
  
  if (sum(nb_tests$verif,na.rm=T)>0) print("Il y a un probleme de sommation entre le total PS topprelevTA et le total par profession.")
  
  nb_tests_svg <- nb_tests
  nb_tests <- nb_tests_svg
  
  
  
  nb_tests_reg = dcast(sidep[,.(nb_res = (.N)), 
                             by=.(date,reg, ProfPrelevTA,cat_pp,valeur)], 
                       date+reg ~ProfPrelevTA + cat_pp+paste0('nb_',valeur), 
                       value.var = 'nb_res')
  reg2019 = data.table::fread(paste0("data/utils/region2019_v2.csv"), encoding = "Latin-1")
  reg2019$reg = as.character(reg2019$reg)
  reg2019[nchar(reg)==1, reg := paste0("0", reg)]
  nb_tests_reg = merge(nb_tests_reg, reg2019[,.(reg, libelle)], by="reg", all.x=T)
  data.table::setnames(nb_tests_reg, "libelle", "nom_reg")
  nb_tests_reg = nb_tests_reg[order(nom_reg)]
  nb_tests_reg[is.na(nom_reg),nom_reg:='Région inconnue']
  
  
  nb_tests_reg[is.na(nb_tests_reg)]=0
  
  nb_tests_reg[,nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'nb']
  nb_tests_reg[,nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'nb_P|nb_N|nb_I|nb_X'&
                 !names(nb_tests_reg)%like%'nb_NA']
  nb_tests_reg[,prop_pos_PNIX:=rowSums(.SD,na.rm=TRUE)/nb_tot_PNIX,.SDcols=names(nb_tests_reg)%like%'nb_P']
  
  nb_tests_reg[,public_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
                 !names(nb_tests_reg)%like%'public_nb_NA']
  nb_tests_reg[,public_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'NA_public_nb']
  
  nb_tests_reg[,privé_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
                 !names(nb_tests_reg)%like%'privé_nb_NA']
  nb_tests_reg[,privé_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'NA_privé_nb']
  
  nb_tests_reg[,nb_pos:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'nb_P']
  
  nb_tests_reg[,MEDECIN_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'MEDECIN_NA_nb_P|MEDECIN_NA_nb_N|MEDECIN_NA_nb_I|MEDECIN_NA_nb_X'&
                 !names(nb_tests_reg)%like%'MEDECIN_NA_nb_NA']
  nb_tests_reg[,MEDECIN_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'MEDECIN_NA_nb']
  
  nb_tests_reg[,INFIRMIER_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'INFIRMIER_NA_nb_P|INFIRMIER_NA_nb_N|INFIRMIER_NA_nb_I|INFIRMIER_NA_nb_X'&
                 !names(nb_tests_reg)%like%'INFIRMIER_nb_NA']
  nb_tests_reg[,INFIRMIER_nb_tot:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'INFIRMIER_NA_nb']
  
  nb_tests_reg[,PHARMACIEN_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'PHARMACIEN_NA_nb_P|PHARMACIEN_NA_nb_N|PHARMACIEN_NA_nb_I|PHARMACIEN_NA_nb_X'&
                 !names(nb_tests_reg)%like%'PHARMACIEN_NA_nb_NA']
  nb_tests_reg[,PHARMACIEN_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'PHARMACIEN_NA_nb']
  
  nb_tests_reg[,DOCTEUR_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'DOCTEUR_NA_nb_P|DOCTEUR_NA_nb_N|DOCTEUR_NA_nb_I|DOCTEUR_NA_nb_X'&
                 !names(nb_tests_reg)%like%'DOCTEUR_NA_nb_NA']
  nb_tests_reg[,DOCTEUR_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'DOCTEUR_NA_nb']
  
  nb_tests_reg[,BIOLOGISTE_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'BIOLOGISTE_NA_nb_P|BIOLOGISTE_NA_nb_N|BIOLOGISTE_NA_nb_I|BIOLOGISTE_NA_nb_X'&
                 !names(nb_tests_reg)%like%'BIOLOGISTE_NA_nb_NA']
  nb_tests_reg[,BIOLOGISTE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'BIOLOGISTE_NA_nb']
  
  nb_tests_reg[,MASSEURKINESITHERAPE_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'MASSEURKINESITHERAPE_NA_nb_P|MASSEURKINESITHERAPE_NA_nb_N|MASSEURKINESITHERAPE_NA_nb_I|MASSEURKINESITHERAPE_NA_nb_X'&
                 !names(nb_tests_reg)%like%'MASSEURKINESITHERAPE_NA_nb_NA']
  nb_tests_reg[,MASSEURKINESITHERAPE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'MASSEURKINESITHERAPE_NA_nb']
  
  nb_tests_reg[,SAGEFEMME_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'SAGEFEMME_NA_nb_P|SAGEFEMME_NA_nb_N|SAGEFEMME_NA_nb_I|SAGEFEMME_NA_nb_X'&
                 !names(nb_tests_reg)%like%'SAGEFEMME_NA_nb_NA']
  nb_tests_reg[,SAGEFEMME_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'SAGEFEMME_NA_nb']
  
  nb_tests_reg[,CHIRURGIENDENTISTE_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),
               .SDcols=names(nb_tests_reg)%like%'CHIRURGIENDENTISTE_NA_nb_P|CHIRURGIENDENTISTE_NA_nb_N|CHIRURGIENDENTISTE_NA_nb_I|CHIRURGIENDENTISTE_NA_nb_X'&
                 !names(nb_tests_reg)%like%'CHIRURGIENDENTISTE_NA_nb_NA']
  nb_tests_reg[,CHIRURGIENDENTISTE_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'CHIRURGIENDENTISTE_NA_nb']
  
  
  nb_tests_reg[,NA_nb_tot_PNIX:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%
                 'NA_NA_nb_P|NA_NA_nb_N|NA_NA_nb_I|NA_NA_nb_X']
  
  nb_tests_reg[,NA_nb_tot:=rowSums(.SD,na.rm=TRUE),.SDcols=names(nb_tests_reg)%like%'NA_NA_nb']
  
  
  nb_tests_reg <- nb_tests_reg[,.(date,nom_reg, MEDECIN_nb_tot_PNIX,PHARMACIEN_nb_tot_PNIX,
                                  INFIRMIER_nb_tot_PNIX, DOCTEUR_nb_tot_PNIX,BIOLOGISTE_nb_tot_PNIX,
                                  CHIRURGIENDENTISTE_nb_tot_PNIX,SAGEFEMME_nb_tot_PNIX,
                                  MASSEURKINESITHERAPE_nb_tot_PNIX,
                                  public_nb_tot_PNIX,privé_nb_tot_PNIX, NA_nb_tot,NA_nb_tot_PNIX,
                                  nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  
  
  nb_tests_dep = dcast(sidep[,.(nb_res = (.N)), 
                             by=.(date,ProfPrelevTA, departement,cat_pp,valeur)], 
                       date+departement ~ ProfPrelevTA + cat_pp+paste0('nb_',valeur), 
                       value.var = 'nb_res')
  
  
  nb_tests_dep[is.na(nb_tests_dep)]=0
  
  nb_tests_dep[,nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb']
  nb_tests_dep[,nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb_P|nb_N|nb_I|nb_X'&
                 !names(nb_tests_dep)%like%'nb_NA']
  nb_tests_dep[,prop_pos_PNIX:=rowSums(.SD)/nb_tot_PNIX,.SDcols=names(nb_tests_dep)%like%'nb_P']
  
  nb_tests_dep[,public_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'public_nb_P|public_nb_N|public_nb_I|public_nb_X'&
                 !names(nb_tests_dep)%like%'public_nb_NA']
  nb_tests_dep[,public_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_public_nb']
  
  nb_tests_dep[,privé_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'privé_nb_P|privé_nb_N|privé_nb_I|privé_nb_X'&
                 !names(nb_tests_dep)%like%'privé_nb_NA']
  nb_tests_dep[,privé_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_privé_nb']
  
  nb_tests_dep[,nb_pos:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'nb_P']
  
  nb_tests_dep[,MEDECIN_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'MEDECIN_NA_nb_P|MEDECIN_NA_nb_N|MEDECIN_NA_nb_I|MEDECIN_NA_nb_X'&
                 !names(nb_tests_dep)%like%'MEDECIN_NA_nb_NA']
  nb_tests_dep[,MEDECIN_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'MEDECIN_NA_nb']
  
  nb_tests_dep[,INFIRMIER_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'INFIRMIER_NA_nb_P|INFIRMIER_NA_nb_N|INFIRMIER_NA_nb_I|INFIRMIER_NA_nb_X'&
                 !names(nb_tests_dep)%like%'INFIRMIER_NA_nb_NA']
  nb_tests_dep[,INFIRMIER_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'INFIRMIER_NA_nb']
  
  nb_tests_dep[,PHARMACIEN_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'PHARMACIEN_NA_nb_P|PHARMACIEN_NA_nb_N|PHARMACIEN_NA_nb_I|PHARMACIEN_NA_nb_X'&
                 !names(nb_tests_dep)%like%'PHARMACIEN_NA_nb_NA']
  nb_tests_dep[,PHARMACIEN_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'PHARMACIEN_NA_nb']
  
  nb_tests_dep[,DOCTEUR_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'DOCTEUR_NA_nb_P|DOCTEUR_NA_nb_N|DOCTEUR_NA_nb_I|DOCTEUR_NA_nb_X'&
                 !names(nb_tests_dep)%like%'DOCTEUR_NA_nb_NA']
  nb_tests_dep[,DOCTEUR_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'DOCTEUR_NA_nb']
  
  nb_tests_dep[,BIOLOGISTE_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'BIOLOGISTE_NA_nb_P|BIOLOGISTE_NA_nb_N|BIOLOGISTE_NA_nb_I|BIOLOGISTE_NA_nb_X'&
                 !names(nb_tests_dep)%like%'BIOLOGISTE_NA_nb_NA']
  nb_tests_dep[,BIOLOGISTE_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'BIOLOGISTE_NA_nb']
  
  nb_tests_dep[,MASSEURKINESITHERAPE_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'MASSEURKINESITHERAPE_NA_nb_P|MASSEURKINESITHERAPE_NA_nb_N|MASSEURKINESITHERAPE_NA_nb_I|MASSEURKINESITHERAPE_NA_nb_X'&
                 !names(nb_tests_dep)%like%'MASSEURKINESITHERAPE_NA_nb_NA']
  nb_tests_dep[,MASSEURKINESITHERAPE_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'MASSEURKINESITHERAPE_NA_nb']
  
  nb_tests_dep[,SAGEFEMME_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'SAGEFEMME_NA_nb_P|SAGEFEMME_NA_nb_N|SAGEFEMME_NA_nb_I|SAGEFEMME_NA_nb_X'&
                 !names(nb_tests_dep)%like%'SAGEFEMME_NA_nb_NA']
  nb_tests_dep[,SAGEFEMME_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'SAGEFEMME_NA_nb']
  
  nb_tests_dep[,CHIRURGIENDENTISTE_nb_tot_PNIX:=rowSums(.SD),
               .SDcols=names(nb_tests_dep)%like%'CHIRURGIENDENTISTE_NA_nb_P|CHIRURGIENDENTISTE_NA_nb_N|CHIRURGIENDENTISTE_NA_nb_I|CHIRURGIENDENTISTE_NA_nb_X'&
                 !names(nb_tests_dep)%like%'CHIRURGIENDENTISTE_NA_nb_NA']
  nb_tests_dep[,CHIRURGIENDENTISTE_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'CHIRURGIENDENTISTE_NA_nb']
  
  
  nb_tests_dep[,NA_nb_tot_PNIX:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%
                 'NA_NA_nb_P|NA_NA_nb_N|NA_NA_nb_I|NA_NA_nb_X']
  
  nb_tests_dep[,NA_nb_tot:=rowSums(.SD),.SDcols=names(nb_tests_dep)%like%'NA_NA_nb']
  
  nb_tests_dep <- nb_tests_dep[,.(date,departement, MEDECIN_nb_tot_PNIX,PHARMACIEN_nb_tot_PNIX,
                                  INFIRMIER_nb_tot_PNIX, DOCTEUR_nb_tot_PNIX,BIOLOGISTE_nb_tot_PNIX,
                                  CHIRURGIENDENTISTE_nb_tot_PNIX,SAGEFEMME_nb_tot_PNIX,
                                  MASSEURKINESITHERAPE_nb_tot_PNIX,
                                  public_nb_tot_PNIX,privé_nb_tot_PNIX, NA_nb_tot,
                                  nb_tot_PNIX,nb_pos,prop_pos_PNIX,nb_tot)]
  
  
  wb <- loadWorkbook("data/sorties/nb_tests/lisez_moi_TAG.xlsx")
  
  addWorksheet(wb,'national')
  writeData(wb,'national',nb_tests)
  setColWidths(wb,'national',cols=1:ncol(nb_tests),widths = 12)
  
  addWorksheet(wb,'regions')
  writeData(wb,'regions',nb_tests_reg)
  setColWidths(wb,'regions',cols=1:ncol(nb_tests_reg),widths = 12)
  
  addWorksheet(wb,'departements')
  writeData(wb,'departements',nb_tests_dep)
  setColWidths(wb,'departements',cols=1:ncol(nb_tests_dep),widths = 12)
  
  saveWorkbook(wb,file=paste0( "data/sorties/nb_tests/",date_AAAAMMJJ,"_sidep_",type_test,"_",type_date,"_65+.xlsx"),overwrite = T)
}
print(gc(verbose = T))
nb_tests_65(type_test='pcr',type_date='prelev',day_init='2020-05-20',jour=jour_date)
nb_tests_65(type_test='pcr',type_date='valid',day_init='2020-05-24',jour=jour_date)
nb_tests_65(type_test='sero',type_date='prelev',day_init='2020-06-10',jour=jour_date)
nb_tests_65(type_test='sero',type_date='valid',day_init='2020-06-12',jour=jour_date)
nb_tests_AG_65(type_test='antigenique',type_date='prelev',day_init='2020-10-16',jour=jour_date)
nb_tests_AG_65(type_test='antigenique',type_date='valid',day_init='2020-10-16',jour=jour_date)
nb_tests_65(type_test='pcr_sal_mil',type_date='prelev',day_init='2020-10-01',jour=jour_date)
nb_tests_65(type_test='pcr_sal_mil',type_date='valid',day_init='2020-10-01',jour=jour_date)
print("02b")
print(gc(verbose = T))
stats = sidep_pcr[!is.na(day_valid)&day_valid<=as.Date(date_dataset-2),
                  .(N=(.N)), by=.(cat_pp, day_valid)]
stats[is.na(cat_pp),cat_pp:="indéterminé"]
colnames(stats)
colnames(stats)=c("catégorie_juridique","date_de_validation","nombre_tests_pcr")
stats = stats[order(date_de_validation)]
write.table(stats, file=paste0( path, "data/verifs/",date_AAAAMMJJ,
                               "_Nombre_tests_pcr_par_date_de_validation_et_catégorie_juridique.csv"), 
            quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
            qmethod = c("escape"))
stats_sero = sidep_sero[!is.na(day_valid)&day_valid>=as.Date("2020-06-10")&day_valid<=as.Date(date_dataset-2),
                        .(N=(.N)), by=.(cat_pp, day_valid)]
stats_sero[is.na(cat_pp),cat_pp:="indéterminé"]
colnames(stats_sero)
colnames(stats_sero)=c("catégorie_juridique","date_de_validation","nombre_tests_sero")
stats_sero = stats_sero[order(date_de_validation)]
write.table(stats_sero, file=paste0( path, "data/verifs/",date_AAAAMMJJ,
                                "_Nombre_tests_sero_par_date_de_validation_et_catégorie_juridique.csv"), 
            quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
            qmethod = c("escape"))
stats$nombre_tests_pcr = as.character(stats$nombre_tests_pcr)
stats_sero$date_de_validation = as.Date(stats_sero$date_de_validation)
all_stats = merge(stats, stats_sero, by=c("date_de_validation","catégorie_juridique"),all.x=T)
write.table(all_stats, file=paste0( path, "data/verifs/",date_AAAAMMJJ,
                                     "_Nombre_tests_pcr_et_sero_par_date_de_validation_et_catégorie_juridique.csv"), 
            quote=TRUE,dec=".", row.names=FALSE, col.names=TRUE, sep =";",
            qmethod = c("escape"))
list_files= list.files(path_sortie, full.names =  TRUE)
list_files = grep(date_AAAAMMJJ, list_files,value = T)
list_files = grep(".csv", list_files,value = T)
print("zip it")
print(gc(verbose = T))
t1 = Sys.time()
zip::zipr(zipfile=paste0(path_sortie, date_AAAAMMJJ,'_historique_sidep.zip'),
          files=list_files)
t2 = Sys.time()
difftime(t2,t1,units="mins")#27 mins !
print("WOAW THAT IS SLOW !")
print("07")
print(gc(verbose = T))
library(openxlsx)
library(ggplot2)
library(data.table)
message("Il faut que la base Sidep PCR soit chargées dans l'environnement de travail")
exporter_tableau_punaise <- function(donnees, onglet = "tableau1", format_donnees = "NUMBER",
                                     noms_lignes = FALSE, fichier_source = NULL, fichier_destination = NULL,
                                     colonnes_donnees = 2:(ncol(donnees)+1),
                                     lignes_donnees = 4:(nrow(donnees)+3), titre = NULL, aides_lecture = NULL,
                                     note = NULL, lecture = NULL, champ = NULL, sources = NULL,
                                     lignes_style_specifique = NULL, style_specifique_voulu = NULL,
                                     colonnes_format_specifique = NULL, format_specifique_voulu = NULL,
                                     nombre_decimales_voulues = NULL, visualiser = FALSE,
                                     lignes_a_ajuster = c("header", "noms_colonnes", "footer"),
                                     ajustements_hauteurs_lignes = c(25, 35, 150),
                                     garder_premiere_case = TRUE,
                                     sauvegarde = T){
  
  options("openxlsx.borderColour" = "black")
  options("openxlsx.borderStyle" = "hair")
  options("openxlsx.numFmt" = format_donnees)
  
  if (length(lignes_donnees)>nrow(donnees)){
    print("Erreur, le nombre de lignes des donnees depasse le nombre de lignes du tableau en entree")
  }
  if (length(colonnes_donnees)> ncol(donnees)){
    print("Erreur, le nombre de colonnes des donnees depasse le nombre de colonnes du tableau en entree")
  }
  
  if (class(donnees)[1] == "tbl_df"){
    donnees <- as.data.frame(donnees)
    if (noms_lignes == TRUE){
      colonnes_donnees <- colonnes_donnees[1:length(colonnes_donnees)-1]
    }
  }
  else{
    donnees <- as.data.frame(donnees)
  }
  
  c_titres_lignes <- 1
  if (noms_lignes == TRUE){
    c_titres_lignes <- 2
    colonnes_donnees <- colonnes_donnees + 1
    style_titres_lignes <- openxlsx::createStyle(halign = "LEFT", valign = "CENTER",
                                                 border = "TopBottomLeftRight", wrapText = T)
    if (garder_premiere_case == FALSE){
      style_premiere_case <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER",
                                                   border = "BottomRight", wrapText = T)
    }
    else{
      style_premiere_case <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER",
                                                   textDecoration = "bold", border = "TopBottomLeftRight",
                                                   wrapText = T)
    }
  }
  
  
  if (is.null(fichier_source)){
    wb <- openxlsx::createWorkbook()
    if (is.null(fichier_destination)){
      print("Veuillez indiquer un fichier sur lequelle ecrire les donnees")
      return(wb)
    }
  }
  else{
    wb <- openxlsx::loadWorkbook(file = fichier_source)
    if (onglet %in% openxlsx::getSheetNames(fichier_source)){
      print("Erreur : ce nom d'onglet existe deja")
      return(wb)
    }
  }
  
  sheet <- openxlsx::addWorksheet(wb, sheetName = onglet)
  
  hs1 <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER", textDecoration = "bold",
                               border = "TopBottomLeftRight", wrapText = T)
  openxlsx::writeData(wb,sheet = onglet, donnees, startCol = 2, startRow = 3,
                      borders = "all", headerStyle = hs1)
  
  
  body_style <- openxlsx::createStyle(numFmt = format_donnees,border = "TopBottomLeftRight",
                                      halign = "CENTER", valign = "top")
  openxlsx::addStyle(wb, sheet = onglet, body_style,rows = lignes_donnees,cols = c(2,colonnes_donnees),
                     gridExpand = T)
  
  if (noms_lignes == TRUE){
    if (garder_premiere_case == FALSE){
      openxlsx::writeData(wb, onglet, x = " ", startCol = 2, startRow = 3)
    }
    openxlsx::addStyle(wb, sheet = onglet, style_premiere_case, rows = 3, cols = 2)
    openxlsx::addStyle(wb, sheet = onglet, style_titres_lignes, rows = lignes_donnees, cols = 2)
  }
  
  openxlsx::setColWidths(wb, sheet = onglet, cols = c(1), widths = 3)
  openxlsx::showGridLines(wb, onglet,showGridLines = F)
  openxlsx::modifyBaseFont(wb, fontSize = 8, fontColour = "black",fontName = "Arial")
  
  largeur_ajuster <- 1.5
  largeur_vec_header <- NULL
  for (i in 1 : length(colnames(donnees))){
    largeur <- 0
    largeur2 <- 0
    j <- 1
    while (j <= nchar(colnames(donnees)[i])){
      if ((substr(colnames(donnees)[i], j,j) == "\r") | (substr(colnames(donnees)[i], j,j) == "\n")){
        if (largeur > largeur2){
          largeur2 <- largeur
        }
        largeur <- 0
        j <- j + 1
      }
      else{
        largeur <- largeur + 1
        j <- j + 1
      }
    }
    largeur_vec_header <- c(largeur_vec_header, (max(largeur, largeur2) + largeur_ajuster))
  }
  largeur_vec <- apply(donnees, 2, function(x) max(nchar(as.character(x)) + largeur_ajuster, na.rm = TRUE))
  largeur_vec_max <- pmax(largeur_vec, largeur_vec_header)
  openxlsx::setColWidths(wb, sheet = onglet,
                         cols = 2 : (length(colonnes_donnees) + 2), widths = largeur_vec_max)
  
  if (!is.null(lignes_style_specifique)){
    if (is.null(style_specifique_voulu)){
      style_specifique_voulu <- rep(("bold"), length(lignes_style_specifique))
    }
    if (length(lignes_style_specifique) != length(style_specifique_voulu)){
      print("Erreur : le nombre de lignes sur lesquelles appliquer un style est different du nombre de styles
            correspondant")
      if (length(lignes_style_specifique) > length(style_specifique_voulu)){
        bold_ajoute <- rep("bold", length(lignes_style_specifique) - length(style_specifique_voulu))
        style_specifique_voulu <- c(style_specifique_voulu, bold_ajoute)
      }
      else{
        style_specifique_voulu <- style_specifique_voulu[1 : length(lignes_style_specifique)]
      }
    }
    for (i in 1 : length(lignes_style_specifique)){
      style_specifique <- openxlsx::createStyle(textDecoration = style_specifique_voulu[i],
                                                numFmt = format_donnees, halign = "CENTER", valign = "CENTER")
      openxlsx::addStyle(wb, sheet = onglet, style_specifique, rows = 3 + lignes_style_specifique[i],
                         cols = c(2,colonnes_donnees), gridExpand = T, stack = TRUE)
    }
}
  
  if (!is.null(colonnes_format_specifique)){
    if (is.null(nombre_decimales_voulues)){
      nombre_decimales_voulues <- rep(2, length(colonnes_format_specifique))
    }
    for (i in 1 : length(colonnes_format_specifique)){
      if (format_specifique_voulu[i] %in% c("PERCENTAGE", "CURRENCY", "NUMBER", "SCIENTIFIC")){
        nb_dec = "0."
      }
      if (nombre_decimales_voulues[i] > 0){
        for (j in 1 : nombre_decimales_voulues[i]){
          nb_dec = paste0(nb_dec, "0")
        }
      }
      if (nombre_decimales_voulues[i] == 0){
        nb_dec <- "0."
      }
      if(format_specifique_voulu[i] == "PERCENTAGE"){
        bon_format <- paste0(nb_dec, " %")
      }
      else if(format_specifique_voulu[i] == "CURRENCY"){
        bon_format <- paste0(nb_dec, " \u20AC")
      }
      else if(format_specifique_voulu[i] %in% c("NUMBER", "SCIENTIFIC")){
        bon_format <- nb_dec
      }
      else{
        bon_format <- format_specifique_voulu[i]
      }
      style_format_specifique <- openxlsx::createStyle(numFmt = bon_format)
      openxlsx::addStyle(wb, sheet = onglet, style_format_specifique, rows = 3 : (length(lignes_donnees) + 3),
                         cols = 1 + colonnes_format_specifique[i], gridExpand = T, stack = TRUE)
    }
  }
  
  titre_style <- openxlsx::createStyle(halign = "LEFT", textDecoration = "bold", valign = "top",
                                       wrapText = TRUE)
  
  openxlsx::writeData(wb, onglet, titre, startCol = 2, startRow = 2)
  openxlsx::setRowHeights(wb, onglet, 1, 10)
  openxlsx::setRowHeights(wb, onglet, 2, 15)
  
  openxlsx::addStyle(wb, onglet, titre_style, rows = 2, cols = 2)
  openxlsx::mergeCells(wb, onglet, cols = c(2,colonnes_donnees), rows = 2)
  
  
  footer_style <- openxlsx::createStyle(halign = "LEFT", valign = "TOP", wrapText = T)
  
  if (is.null(aides_lecture)){
    aides_lecture = ""
  }
  else{
    if (length(aides_lecture)>1){
      new_aides_lecture <- paste0("* ", aides_lecture[1])
      for (i in 2:(length(aides_lecture))){
        new_aides_lecture <- paste(new_aides_lecture, paste0("* ", aides_lecture[i]), sep = "\n")
      }
      aides_lecture <- new_aides_lecture
    }
    aides_lecture = paste(aides_lecture, "", sep = "\n")
  }
  if (is.null(note)){
    note <- ""
  }
  else{
    note <- paste0("Note > ", paste(note, "", sep = "\n"))
  }
  if (is.null(lecture)){
    lecture <- ""
  }
  else{
    lecture <- paste0("Lecture > ", paste(lecture, "", sep = "\n"))
  }
  if (is.null(champ)){
    champ <- ""
  }
  else{
    champ <- paste0("Champ > ", paste(champ, "", sep = "\n"))
  }
  if (is.null(sources)){
    sources <- ""
  }
  else{
    sources <- paste0("Sources > ", sources)
  }
  
  legende <- paste0(aides_lecture, note, lecture, champ, sources)
  openxlsx::writeData(wb, onglet, legende, startCol = 2, startRow = lignes_donnees[length(lignes_donnees)] + 2)
  
  openxlsx::mergeCells(wb, onglet, cols = c(2,colonnes_donnees),
                       rows = lignes_donnees[length(lignes_donnees)] + 2)
  openxlsx::addStyle(wb, onglet, footer_style, rows = lignes_donnees[length(lignes_donnees)] + 2, cols = 2)
  openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 1, 10)
  
  if (!is.null(lignes_a_ajuster)){
    for (i in 1 : length(ajustements_hauteurs_lignes)){
      if (lignes_a_ajuster[i] == "header"){
        openxlsx::setRowHeights(wb, onglet, 2, heights = ajustements_hauteurs_lignes[i])
      }
      else if (lignes_a_ajuster[i] == "noms_colonnes"){
        openxlsx::setRowHeights(wb, onglet, 3, heights = ajustements_hauteurs_lignes[i])
      }
      else if (lignes_a_ajuster[i] == "footer"){
        openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 2,
                                heights = ajustements_hauteurs_lignes[i])
      }
    }
    if (!("header" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, 2, heights = 25)
    }
    if (!("noms_colonnes" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, 3, heights = 35)
    }
    if (!("footer" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 2,
                              heights = 150)
    }
  }
  if (visualiser == TRUE){
    openxlsx::openXL(wb)
  }
  if(sauvegarde == T){
    if (is.null(fichier_destination)){
      openxlsx::saveWorkbook(wb, fichier_source, overwrite = TRUE)
    }
    else{
      openxlsx::saveWorkbook(wb, fichier_destination, overwrite = TRUE)
    }
  }
  return(wb)
}
jour <- Sys.Date()
data.table::setkeyv(sidep_pcr, "Pseudonyme")
b = sidep_pcr[(!is.na(valeur)) & 
                valeur %in% c("P", "N"), c("nb_pos", "nb_tests") := 
                list(sum(valeur == "P"), .N), by = Pseudonyme]
drop_c = c("AnalyseConclusion","TypeAnalyse","RPPSTraitant","IRIS","Statut",
           "res","sem_valid","sem_jour_deb","sem_jour_fin","cat_delai_12h", "cat_delai",
           "day", "delai",
           "FINESS_EJ","departement","CodePostal","fi_init","TypologiePatient", "PremierSymptomes",
           "RPPSPrescripteur", "FINESS_ET", "cat_etb", "cat_pp", "nom_dep", "nom_reg",
           "reg","Sexe","ProfessionelSante", "tranche_age2",
           "sem_prelev")
b <- b[, !..drop_c] # on essaye de diminuer taille en ram 
print("Si vous avez un warning c'est parce que certaines variables sont creees dans la note donc 
      si vous ne l'avez pas fait tourner il n'y a pas ces variables. Cela ne pose pas probleme.")
b[,day_valid_init := as.Date(date_valid_init)]
partie2 = b[nb_tests > 1 & nb_pos == 0]
partie2 = partie2[, min_date_prelev_par_pseudo := min(date_prelev), by = Pseudonyme]
partie2 = partie2[date_prelev == min_date_prelev_par_pseudo] 
col_drop = "min_date_prelev_par_pseudo"
partie2 = partie2[, !..col_drop] 
dim(partie2)
partie3 = b[nb_tests > 1 & nb_pos > 0 & valeur == "P"] 
partie3 = partie3[, min_date_prelev_par_pseudo := min(date_prelev), by = Pseudonyme]
partie3 = partie3[date_prelev == min_date_prelev_par_pseudo] 
col_drop = "min_date_prelev_par_pseudo"
partie3 = partie3[, !..col_drop]
dim(partie3) 
sidep_dedoub = rbind(partie3, partie2)
rm(partie2, partie3)
sidep_dedoub = rbind(sidep_dedoub, b[nb_tests == 1]) 
rm(b)
nrow(sidep_dedoub) 
tab_patients_par_met = sidep_dedoub[, 
                                    keyby = .(day_prelev), 
                                    .(nb_pos_patients = sum(valeur == "P"), 
                                      nb_patients = .N)]
exporter_tableau_punaise(tab_patients_par_met, fichier_destination = 
                           paste0("data/verifs/",jour,"_sidep_par_patient.xlsx"),
                         onglet = "date_prelev")
tab_patients_valid = sidep_dedoub[, 
                                  keyby = .(day_valid_init), 
                                  .(nb_pos_patients = sum(valeur == "P"), 
                                    nb_patients = .N)]
exporter_tableau_punaise(tab_patients_valid, fichier_source = paste0("data/verifs/",jour,"_sidep_par_patient.xlsx"),
                         onglet = "date_valid")
tab_patients_extract = sidep_dedoub[, 
                                    keyby = .(day_extract_sidep), 
                                    .(nb_pos_patients = sum(valeur == "P"), 
                                      nb_patients = .N)]
exporter_tableau_punaise(tab_patients_extract, fichier_source = paste0("data/verifs/",jour,"_sidep_par_patient.xlsx"),
                         onglet = "day_extract_sidep")
tab_patients_valid_65 = sidep_dedoub[as.numeric(Age) >= 65, 
                                  keyby = .(day_valid_init), 
                                  .(nb_pos_patients = sum(valeur == "P"), 
                                    nb_patients = .N)]
exporter_tableau_punaise(tab_patients_valid_65, fichier_source = paste0("data/verifs/",jour,"_sidep_par_patient.xlsx"),
                         onglet = "date_valid_65")
print("07bis")
print(gc(verbose = T))
library(openxlsx)
library(ggplot2)
library(data.table)
message("Il faut que la base Sidep PCR soit chargée dans l'environnement de travail")
exporter_tableau_punaise <- function(donnees, onglet = "tableau1", format_donnees = "NUMBER",
                                     noms_lignes = FALSE, fichier_source = NULL, fichier_destination = NULL,
                                     colonnes_donnees = 2:(ncol(donnees)+1),
                                     lignes_donnees = 4:(nrow(donnees)+3), titre = NULL, aides_lecture = NULL,
                                     note = NULL, lecture = NULL, champ = NULL, sources = NULL,
                                     lignes_style_specifique = NULL, style_specifique_voulu = NULL,
                                     colonnes_format_specifique = NULL, format_specifique_voulu = NULL,
                                     nombre_decimales_voulues = NULL, visualiser = FALSE,
                                     lignes_a_ajuster = c("header", "noms_colonnes", "footer"),
                                     ajustements_hauteurs_lignes = c(25, 35, 150),
                                     garder_premiere_case = TRUE,
                                     sauvegarde = T){
  
  options("openxlsx.borderColour" = "black")
  options("openxlsx.borderStyle" = "hair")
  options("openxlsx.numFmt" = format_donnees)
  
  if (length(lignes_donnees)>nrow(donnees)){
    print("Erreur, le nombre de lignes des donnees depasse le nombre de lignes du tableau en entree")
  }
  if (length(colonnes_donnees)> ncol(donnees)){
    print("Erreur, le nombre de colonnes des donnees depasse le nombre de colonnes du tableau en entree")
  }
  
  if (class(donnees)[1] == "tbl_df"){
    donnees <- as.data.frame(donnees)
    if (noms_lignes == TRUE){
      colonnes_donnees <- colonnes_donnees[1:length(colonnes_donnees)-1]
    }
  }
  else{
    donnees <- as.data.frame(donnees)
  }
  
  c_titres_lignes <- 1
  if (noms_lignes == TRUE){
    c_titres_lignes <- 2
    colonnes_donnees <- colonnes_donnees + 1
    style_titres_lignes <- openxlsx::createStyle(halign = "LEFT", valign = "CENTER",
                                                 border = "TopBottomLeftRight", wrapText = T)
    if (garder_premiere_case == FALSE){
      style_premiere_case <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER",
                                                   border = "BottomRight", wrapText = T)
    }
    else{
      style_premiere_case <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER",
                                                   textDecoration = "bold", border = "TopBottomLeftRight",
                                                   wrapText = T)
    }
  }
  
  
  if (is.null(fichier_source)){
    wb <- openxlsx::createWorkbook()
    if (is.null(fichier_destination)){
      print("Veuillez indiquer un fichier sur lequelle ecrire les donnees")
      return(wb)
    }
  }
  else{
    wb <- openxlsx::loadWorkbook(file = fichier_source)
    if (onglet %in% openxlsx::getSheetNames(fichier_source)){
      print("Erreur : ce nom d'onglet existe deja")
      return(wb)
    }
  }
  
  sheet <- openxlsx::addWorksheet(wb, sheetName = onglet)
  
  hs1 <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER", textDecoration = "bold",
                               border = "TopBottomLeftRight", wrapText = T)
  openxlsx::writeData(wb,sheet = onglet, donnees, startCol = 2, startRow = 3,
                      borders = "all", headerStyle = hs1)
  
  
  body_style <- openxlsx::createStyle(numFmt = format_donnees,border = "TopBottomLeftRight",
                                      halign = "CENTER", valign = "top")
  openxlsx::addStyle(wb, sheet = onglet, body_style,rows = lignes_donnees,cols = c(2,colonnes_donnees),
                     gridExpand = T)
  
  if (noms_lignes == TRUE){
    if (garder_premiere_case == FALSE){
      openxlsx::writeData(wb, onglet, x = " ", startCol = 2, startRow = 3)
    }
    openxlsx::addStyle(wb, sheet = onglet, style_premiere_case, rows = 3, cols = 2)
    openxlsx::addStyle(wb, sheet = onglet, style_titres_lignes, rows = lignes_donnees, cols = 2)
  }
  
  openxlsx::setColWidths(wb, sheet = onglet, cols = c(1), widths = 3)
  openxlsx::showGridLines(wb, onglet,showGridLines = F)
  openxlsx::modifyBaseFont(wb, fontSize = 8, fontColour = "black",fontName = "Arial")
  
  largeur_ajuster <- 1.5
  largeur_vec_header <- NULL
  for (i in 1 : length(colnames(donnees))){
    largeur <- 0
    largeur2 <- 0
    j <- 1
    while (j <= nchar(colnames(donnees)[i])){
      if ((substr(colnames(donnees)[i], j,j) == "\r") | (substr(colnames(donnees)[i], j,j) == "\n")){
        if (largeur > largeur2){
          largeur2 <- largeur
        }
        largeur <- 0
        j <- j + 1
      }
      else{
        largeur <- largeur + 1
        j <- j + 1
      }
    }
    largeur_vec_header <- c(largeur_vec_header, (max(largeur, largeur2) + largeur_ajuster))
  }
  largeur_vec <- apply(donnees, 2, function(x) max(nchar(as.character(x)) + largeur_ajuster, na.rm = TRUE))
  largeur_vec_max <- pmax(largeur_vec, largeur_vec_header)
  openxlsx::setColWidths(wb, sheet = onglet,
                         cols = 2 : (length(colonnes_donnees) + 2), widths = largeur_vec_max)
  
  if (!is.null(lignes_style_specifique)){
    if (is.null(style_specifique_voulu)){
      style_specifique_voulu <- rep(("bold"), length(lignes_style_specifique))
    }
    if (length(lignes_style_specifique) != length(style_specifique_voulu)){
      print("Erreur : le nombre de lignes sur lesquelles appliquer un style est different du nombre de styles
            correspondant")
      if (length(lignes_style_specifique) > length(style_specifique_voulu)){
        bold_ajoute <- rep("bold", length(lignes_style_specifique) - length(style_specifique_voulu))
        style_specifique_voulu <- c(style_specifique_voulu, bold_ajoute)
      }
      else{
        style_specifique_voulu <- style_specifique_voulu[1 : length(lignes_style_specifique)]
      }
    }
    for (i in 1 : length(lignes_style_specifique)){
      style_specifique <- openxlsx::createStyle(textDecoration = style_specifique_voulu[i],
                                                numFmt = format_donnees, halign = "CENTER", valign = "CENTER")
      openxlsx::addStyle(wb, sheet = onglet, style_specifique, rows = 3 + lignes_style_specifique[i],
                         cols = c(2,colonnes_donnees), gridExpand = T, stack = TRUE)
    }
}
  
  if (!is.null(colonnes_format_specifique)){
    if (is.null(nombre_decimales_voulues)){
      nombre_decimales_voulues <- rep(2, length(colonnes_format_specifique))
    }
    for (i in 1 : length(colonnes_format_specifique)){
      if (format_specifique_voulu[i] %in% c("PERCENTAGE", "CURRENCY", "NUMBER", "SCIENTIFIC")){
        nb_dec = "0."
      }
      if (nombre_decimales_voulues[i] > 0){
        for (j in 1 : nombre_decimales_voulues[i]){
          nb_dec = paste0(nb_dec, "0")
        }
      }
      if (nombre_decimales_voulues[i] == 0){
        nb_dec <- "0."
      }
      if(format_specifique_voulu[i] == "PERCENTAGE"){
        bon_format <- paste0(nb_dec, " %")
      }
      else if(format_specifique_voulu[i] == "CURRENCY"){
        bon_format <- paste0(nb_dec, " \u20AC")
      }
      else if(format_specifique_voulu[i] %in% c("NUMBER", "SCIENTIFIC")){
        bon_format <- nb_dec
      }
      else{
        bon_format <- format_specifique_voulu[i]
      }
      style_format_specifique <- openxlsx::createStyle(numFmt = bon_format)
      openxlsx::addStyle(wb, sheet = onglet, style_format_specifique, rows = 3 : (length(lignes_donnees) + 3),
                         cols = 1 + colonnes_format_specifique[i], gridExpand = T, stack = TRUE)
    }
  }
  
  titre_style <- openxlsx::createStyle(halign = "LEFT", textDecoration = "bold", valign = "top",
                                       wrapText = TRUE)
  
  openxlsx::writeData(wb, onglet, titre, startCol = 2, startRow = 2)
  openxlsx::setRowHeights(wb, onglet, 1, 10)
  openxlsx::setRowHeights(wb, onglet, 2, 15)
  
  openxlsx::addStyle(wb, onglet, titre_style, rows = 2, cols = 2)
  openxlsx::mergeCells(wb, onglet, cols = c(2,colonnes_donnees), rows = 2)
  
  
  footer_style <- openxlsx::createStyle(halign = "LEFT", valign = "TOP", wrapText = T)
  
  if (is.null(aides_lecture)){
    aides_lecture = ""
  }
  else{
    if (length(aides_lecture)>1){
      new_aides_lecture <- paste0("* ", aides_lecture[1])
      for (i in 2:(length(aides_lecture))){
        new_aides_lecture <- paste(new_aides_lecture, paste0("* ", aides_lecture[i]), sep = "\n")
      }
      aides_lecture <- new_aides_lecture
    }
    aides_lecture = paste(aides_lecture, "", sep = "\n")
  }
  if (is.null(note)){
    note <- ""
  }
  else{
    note <- paste0("Note > ", paste(note, "", sep = "\n"))
  }
  if (is.null(lecture)){
    lecture <- ""
  }
  else{
    lecture <- paste0("Lecture > ", paste(lecture, "", sep = "\n"))
  }
  if (is.null(champ)){
    champ <- ""
  }
  else{
    champ <- paste0("Champ > ", paste(champ, "", sep = "\n"))
  }
  if (is.null(sources)){
    sources <- ""
  }
  else{
    sources <- paste0("Sources > ", sources)
  }
  
  legende <- paste0(aides_lecture, note, lecture, champ, sources)
  openxlsx::writeData(wb, onglet, legende, startCol = 2, startRow = lignes_donnees[length(lignes_donnees)] + 2)
  
  openxlsx::mergeCells(wb, onglet, cols = c(2,colonnes_donnees),
                       rows = lignes_donnees[length(lignes_donnees)] + 2)
  openxlsx::addStyle(wb, onglet, footer_style, rows = lignes_donnees[length(lignes_donnees)] + 2, cols = 2)
  openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 1, 10)
  
  if (!is.null(lignes_a_ajuster)){
    for (i in 1 : length(ajustements_hauteurs_lignes)){
      if (lignes_a_ajuster[i] == "header"){
        openxlsx::setRowHeights(wb, onglet, 2, heights = ajustements_hauteurs_lignes[i])
      }
      else if (lignes_a_ajuster[i] == "noms_colonnes"){
        openxlsx::setRowHeights(wb, onglet, 3, heights = ajustements_hauteurs_lignes[i])
      }
      else if (lignes_a_ajuster[i] == "footer"){
        openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 2,
                                heights = ajustements_hauteurs_lignes[i])
      }
    }
    if (!("header" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, 2, heights = 25)
    }
    if (!("noms_colonnes" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, 3, heights = 35)
    }
    if (!("footer" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 2,
                              heights = 150)
    }
  }
  if (visualiser == TRUE){
    openxlsx::openXL(wb)
  }
  if(sauvegarde == T){
    if (is.null(fichier_destination)){
      openxlsx::saveWorkbook(wb, fichier_source, overwrite = TRUE)
    }
    else{
      openxlsx::saveWorkbook(wb, fichier_destination, overwrite = TRUE)
    }
  }
  return(wb)
}
jour <- Sys.Date()
keep_c = c("Pseudonyme", "date_valid_init", "date_prelev", "Age", "AnalyseConclusion", "departement", "valeur", "day_extract_sidep")
b = rbind(sidep_pcr[, ..keep_c],
          sidep_antigenique[, ..keep_c])
b = rbind(b,
          sidep_pcr_sal_mil[, ..keep_c])
b = b[(!is.na(valeur)) & 
        valeur %in% c("P", "N"), c("nb_pos", "nb_tests") := 
        list(sum(valeur == "P"), .N), by = Pseudonyme]
b[, day_valid_init := as.Date(date_valid_init)]
partie2 = b[nb_tests > 1 & nb_pos == 0]
partie2 = partie2[, min_date_prelev_par_pseudo := min(date_prelev), by = Pseudonyme]
partie2 = partie2[date_prelev == min_date_prelev_par_pseudo] 
col_drop = "min_date_prelev_par_pseudo"
partie2 = partie2[, !..col_drop] 
dim(partie2)
partie3 = b[nb_tests > 1 & nb_pos > 0 & valeur == "P"] 
partie3 = partie3[, min_date_prelev_par_pseudo := min(date_prelev), by = Pseudonyme]
partie3 = partie3[date_prelev == min_date_prelev_par_pseudo] 
col_drop = "min_date_prelev_par_pseudo"
partie3 = partie3[, !..col_drop]
dim(partie3) 
sidep_dedoub = rbind(partie3, partie2)
rm(partie2, partie3)
sidep_dedoub = rbind(sidep_dedoub, b[nb_tests == 1])  # On remet aussi ceux qui n'ont qu'un test
rm(b)
nrow(sidep_dedoub) 
sidep_dedoub[, day_prelev := as.Date(date_prelev)]
tab_patients_par_met = sidep_dedoub[, 
                                    keyby = .(day_prelev), 
                                    .(nb_pos_patients = sum(valeur == "P"), 
                                      nb_patients = .N)]
exporter_tableau_punaise(tab_patients_par_met, fichier_destination = 
                           paste0("data/verifs/",jour,"_sidep_par_patient_bis.xlsx"),
                         onglet = "date_prelev")
tab_patients_valid = sidep_dedoub[, 
                                  keyby = .(day_valid_init), 
                                  .(nb_pos_patients = sum(valeur == "P"), 
                                    nb_patients = .N)]
exporter_tableau_punaise(tab_patients_valid, fichier_source = paste0("data/verifs/",jour,"_sidep_par_patient_bis.xlsx"),
                         onglet = "date_valid")
tab_patients_extract = sidep_dedoub[, 
                                    keyby = .(day_extract_sidep), 
                                    .(nb_pos_patients = sum(valeur == "P"), 
                                      nb_patients = .N)]
exporter_tableau_punaise(tab_patients_extract, fichier_source = paste0("data/verifs/",jour,"_sidep_par_patient_bis.xlsx"),
                         onglet = "day_extract_sidep")
tab_patients_valid_65 = sidep_dedoub[as.numeric(Age) >= 65, 
                                     keyby = .(day_valid_init), 
                                     .(nb_pos_patients = sum(valeur == "P"), 
                                       nb_patients = .N)]
exporter_tableau_punaise(tab_patients_valid_65, fichier_source = paste0("data/verifs/",jour,"_sidep_par_patient_bis.xlsx"),
                         onglet = "date_valid_65")
print("TAP")
print(gc(verbose = T))
library(dplyr)
library(openxlsx)
library(ggplot2)
TABLE_RESULTATS = data.frame()
NB_PCR = dim(sidep_pcr[as.Date(date_valid_init) >= Sys.Date() - 8 & 
                         as.Date(date_valid_init) <= Sys.Date() - 2 & 
                         valeur %in% c("P", "N", "I", "X")])[1]
NB_TAG = dim(sidep_antigenique[as.Date(date_prelev) >= Sys.Date() - 8 &
                                 as.Date(date_prelev) <= Sys.Date() - 2 & 
                                 valeur %in% c("P", "N", "I", "X")])[1]
dim(sidep_antigenique[as.Date(date_valid_init)== "2021-03-09" & valeur %in% c("P", "N", "I", "X")])
NB_SALMIL = dim(sidep_pcr_sal_mil[as.Date(date_valid_init) >= Sys.Date() - 8 &
                                    as.Date(date_valid_init) <= Sys.Date() - 2 & 
                                    valeur %in% c("P", "N", "I", "X")])[1]
TABLE_RESULTATS = rbind(TABLE_RESULTATS,
                        data.frame(indicateur = "Nombre de tests",
                                   valeur = NB_PCR + NB_TAG + NB_SALMIL))
PART_TAG = NB_TAG / (NB_PCR + NB_TAG + NB_SALMIL)
TABLE_RESULTATS = rbind(TABLE_RESULTATS,
                        data.frame(indicateur = "Part des TAG",
                                   valeur = PART_TAG))
TABLE_RESULTATS = rbind(TABLE_RESULTATS,
                        data.frame(indicateur = "Part des PCR (y compris salivaires)",
                                   valeur = 1 - PART_TAG))
FILTRE_DELAI = 10
t1 = setDT(sidep_pcr)
t3 = setDT(sidep_pcr_sal_mil)
t1[, delai := as.numeric(difftime(date_valid_init, date_prelev, units="days"))]
t3[, delai := as.numeric(difftime(date_valid_init, date_prelev, units="days"))]
t_empilee = rbind(t1[as.Date(date_valid_init) >= Sys.Date() - 8 &
                       as.Date(date_valid_init) <= Sys.Date() - 2 & 
                       delai >= 0 & 
                       delai < FILTRE_DELAI, c("delai")],
                  t3[as.Date(date_valid_init) >= Sys.Date() - 8 &
                       as.Date(date_valid_init) <= Sys.Date() - 2 & 
                       delai >= 0 & 
                       delai < FILTRE_DELAI, c("delai")]
)
TABLE_RESULTATS = rbind(TABLE_RESULTATS,
                        data.frame(indicateur = "Proportion des tests rendus en moins de 24h",
                                   valeur = sum(t_empilee$delai < 1) / dim(t_empilee)[1]))
rm(t1, t3, t_empilee)
t1 = sidep_pcr[!is.na(valeur) & 
                 valeur %in% c("P", "N") & 
                 as.Date(date_valid_init) >= Sys.Date() - 8 &
               as.Date(date_valid_init) <= Sys.Date() - 2]
t1[, delai := as.numeric(difftime(date_valid_init, date_prelev, units="days"))]
t1 = t1[!is.na(delai) & 
        delai >= 0 & 
        delai < FILTRE_DELAI]
t1[, symptomes := dplyr::case_when(PremierSymptomes == "ASY" ~ "Asy",
                            PremierSymptomes == "S01" ~ "0-1",
                            PremierSymptomes == "S24" ~ "2-4",
                            PremierSymptomes == "S57" ~ "5-7",
                            PremierSymptomes == "SS2" ~ "8-15",
                            PremierSymptomes == "SS3" ~ "> 15",
                            PremierSymptomes == "U" ~ "NR")]
t1[, symptomes := ifelse(is.na(symptomes), "NR", symptomes)]
t1[, symptomes_ordre := dplyr::case_when(symptomes == "Asy" ~ 1,
                                         symptomes == "0-1" ~ 2,
                                         symptomes == "2-4" ~ 3,
                                         symptomes == "5-7" ~ 4,
                                         symptomes == "8-15" ~ 5,
                                         symptomes == "> 15" ~ 6,
                                         symptomes == "NR" ~ 7)]
 
delai_sympt_nat = t1[, .(n = .N), by = c("symptomes", "symptomes_ordre")]
delai_sympt_nat = delai_sympt_nat[, N := sum(n)]
delai_sympt_nat = delai_sympt_nat[, prop := n / N]
setorder(delai_sympt_nat, symptomes_ordre)
delai_sympt_nat = delai_sympt_nat[, c("symptomes", "prop")]
setnames(delai_sympt_nat, c("symptomes", "prop"), c("indicateur", "valeur"))
TABLE_RESULTATS = rbind(TABLE_RESULTATS,
                        delai_sympt_nat)
write.xlsx(TABLE_RESULTATS, paste0("/production/echange/sidep/codes_ccs/TAP/", Sys.Date(), " - reporting TAP.xlsx"))
if(rewrite_fichier_variants){
  
  
  tab_rattrapee=data.table::fread("/data1/sidep_bases_clean/bases_clean_1604/Sauvegarde variants du 15012021 au 17032021.csv", quote = "",
                                  sep = "|", header = TRUE, colClasses=list(
                                    character = c("Pseudonyme","Sexe",
                                                  "CodePostal","IRIS","NumDossier",
                                                  "TypologiePatient","ProfessionelSante","PremierSymptomes",
                                                  "FINESS","RPPSTraitant","RPPSPrescripteur",
                                                  "DatePrelevement","DateValidationCR","AnalyseConclusion",
                                                  "TypeAnalyse","Resultat","Statut",
                                                  "CodePays","CPAdresseTemporaire","NumDepartementAdresseTemporaire",
                                                  "CodePaysAdresseTemporaire","CodePaysVoyageEtranger","FINESSGeographique",
                                                  "NumCampagneDepistage","Joker1","Joker2","Joker3"),
                                    numeric = "Age"), encoding = "Latin-1")
  
  assertthat::assert_that(sum(nchar(tab_rattrapee$Pseudonyme)!=64)==0,msg="Le Pseudonyme devrait toujours être sur 64 caractères")
  
  print(nrow(tab_rattrapee))
  print(nrow(merge(tab_rattrapee,new_rattrapage,by="Pseudonyme")))
  
  tab_rattrapee[new_rattrapage,Pseudonyme:=i.Pseudo2,on="Pseudonyme"]
  data.table::fwrite(tab_rattrapee,
                     paste0(path_sortie, "Sauvegarde variants du 15012021 au 17032021.csv"),
                     dec=".", row.names=FALSE, sep ="|",quote = FALSE,
                     qmethod = c("escape"),dateTimeAs = "ISO", verbose = TRUE)
  
}
print("10")
addP2 = F #deja fait dans les scripts 01/04 
print("variants A")
print(gc(verbose = T))
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
if(!exists("sidep_pcr")){
  
  TIME_INIT=Sys.time()
  print(paste0("lancement: ",TIME_INIT))
  
  
  
  
  t1 <- Sys.time()
  temp <- paste0(path_sortie, date_AAAAMMJJ, "_sidep_pcr_")
  sidep_pcr <- pbapply::pblapply(MoisAAAA,
                                 function(i) {
                                   dt <- data.table::fread(paste0(temp, i,".csv"), 
                                                           na.strings="",
                                                           colClasses=list(
                                                             Date=date_vars, 
                                                             POSIXct = posixct_vars,
                                                             character = char_vars, 
                                                             numeric = "Age"), 
                                                           verbose = F, sep = ";", header = TRUE, encoding ="Latin-1")
                                   dt
                                 }
  )
  t2 <- Sys.time()
  print(t2 - t1) 
  
  
  t1 <- Sys.time()
  
  sidep_pcr <- lapply(sidep_pcr, function(i)
    i[,`:=`(date_prelev = fasttime::fastPOSIXct(date_prelev),
            date_valid = fasttime::fastPOSIXct(date_valid),
            date_valid_init = fasttime::fastPOSIXct(date_valid_init)
    )])
  
  sidep_pcr <- lapply(sidep_pcr, function(i)
    i[grepl("priv", cat_etb ), cat_etb := "Laboratoire privé"])
  
  sidep_pcr <- rbindlist(sidep_pcr, fill=T)
  t2 <- Sys.time()
  print(t2  - TIME_INIT)
} else {print("sidep_pcr existe deja")}
if(!exists("sidep_sero")){
  t1 <- Sys.time()
  
  path_zip_sero = paste0(path_sortie, date_AAAAMMJJ, "_sidep_sero.csv") 
  print(path_zip_sero)
  sidep_sero = data.table::fread(path_zip_sero, 
                                 na.strings="",
                                 colClasses=list(Date=date_vars, 
                                                 POSIXct = posixct_vars,
                                                 character = char_vars, 
                                                 numeric = "Age"),
                                 verbose = F, sep= ";", header = TRUE, encoding ="Latin-1")
  t2 <- Sys.time()
  print(t2-t1)
} else {print("sidep_sero existe deja")}
if(!exists("sidep_pcr_sal_mil")){
  t1 <- Sys.time()
  path_zip_pcr_sal_mil = paste0(path_sortie, date_AAAAMMJJ, "_sidep_pcr_salivaire_milieux_divers.csv") 
  print(path_zip_pcr_sal_mil)
  sidep_pcr_sal_mil = data.table::fread(path_zip_pcr_sal_mil, 
                                        na.strings="",
                                        colClasses=list(Date=date_vars, 
                                                        POSIXct = posixct_vars,
                                                        character = char_vars, 
                                                        numeric = "Age"),
                                        verbose = F, sep= ";", header = TRUE, encoding="Latin-1")
  t2 <- Sys.time()
  print(t2-t1)
} else {print("sidep_pcr_sal_mil existe deja")}
if(!exists("sidep_antigen_complet")){
  
  if(!exists("sidep_antigenique")){
  t1 <- Sys.time()
  path_zip_antigen = paste0(path_sortie, date_AAAAMMJJ, "_sidep_antigen.csv") 
  print(path_zip_antigen)
  sidep_antigen_complet = data.table::fread(path_zip_antigen,
                                            na.strings="",
                                            colClasses=list(Date=date_vars,
                                                            POSIXct = posixct_vars,
                                                            character = c(char_vars,new_tag_char_vars), 
                                                            numeric = "Age"),
                                            verbose = F, sep= ";", header = TRUE, encoding = "Latin-1")
  t2 <- Sys.time()
  print(t2-t1)
  } else {
    sidep_antigen_complet = sidep_antigenique
    rm(sidep_antigenique);gc()
  }
} else {print("sidep_antigen_complet existe deja")}
print("variants B")
print(gc(verbose = T))
message("Il faut avoir chargé les bases PCR et antigéniques => le faire à partir du script 08 (sans exécuter la note)")
variants_pre1803=data.table::fread(paste0(path_sortie, "Sauvegarde variants du 15012021 au 17032021.csv"), quote = "",
                                   verbose = TRUE, sep = "|", header = TRUE, colClasses=list(
                                     character = daily_files_char_vars,
                                     numeric = "Age"), encoding = "Latin-1")
liste_fichiers = dir("/data1/sidep_brut/bases_dechiffrees/")
liste_fichiers = liste_fichiers[grep("CSV_DREES_MED", liste_fichiers)]
tab_rattrapee = NULL
liste_des_dates = gsub("-", "", substr(as.character(seq.Date(from = as.Date("2021-03-18"),
                                                             to = Sys.Date(),
                                                             by = 1)), 3, 10))
liste_des_dates=liste_des_dates[which(!(liste_des_dates %in% c("210323")))]
safe_fread = purrr::quietly(data.table::fread)
for (item in liste_des_dates){
  print(item)
  fichiers = liste_fichiers[grep(item, liste_fichiers)]
  
  med4 = safe_fread(paste0("/data1/sidep_brut/bases_dechiffrees/", fichiers[grep("MED4", fichiers)]), fill=T, quote = "",
                           verbose = F, sep = "|", header = TRUE, colClasses=list(
                             character = daily_files_char_vars,
                             Integer = "Age"), encoding = "Latin-1")
  
  w <- med4$warnings #idée Philéas, implémenté le 25 mars
  w <- w[grepl("^Stopped early",w)]
  if(length(w)>0){stop(w)}
  med4 <- med4$result
  
  med2 = safe_fread(paste0("/data1/sidep_brut/bases_dechiffrees/", fichiers[grep("MED2", fichiers)]), fill=T, quote = "",
                           verbose = F, sep = "|", header = TRUE, colClasses=list(
                             character = daily_files_char_vars,
                             Integer = "Age"), encoding = "Latin-1")
  w <- med2$warnings
  w <- w[grepl("^Stopped early",w)]
  if(length(w)>0){stop(w)}
  med2 <- med2$result
  
  tab_add = rbind(med4,
                  med2)  
  tab_rattrapee = rbind(tab_rattrapee,
                        tab_add)  
}
rm(med2,med4,tab_add)
tab_rattrapee = tab_rattrapee[!(Joker2 == "" & (Joker3 == "" | is.na(Joker3)))]
tab_rattrapee[,Pseudo1:=NULL]
setnames(tab_rattrapee,"Pseudo2","Pseudonyme")
tab_rattrapee=rbind(variants_pre1803,tab_rattrapee) 
tab_rattrapee = unique(tab_rattrapee)
noms_sans_date_valid = names(tab_rattrapee)[names(tab_rattrapee) != "DateValidationCR"]
tab_rattrapee[, PremiereDateValidationCRVariant := as.character(min(as.numeric(DateValidationCR))), by = noms_sans_date_valid]
noms_keep = c("PremiereDateValidationCRVariant", noms_sans_date_valid)
aux = tab_rattrapee[, ..noms_keep]
tab_rattrapee = unique(aux)
tab_rattrapee[, .(nb = .N), by = "AnalyseConclusion"] #juste un affichage? donc à commenter
tab_rattrapee[, .(nb = .N), by = "Resultat"]
noms_communs = intersect(names(sidep_pcr), names(sidep_antigen_complet))
sidep_antigen_complet[, day_valid := as.Date(day_valid)]
sidep_pcr[, day_extract_sidep := as.Date(day_extract_sidep)]
sidep_base = rbind(sidep_pcr[, ..noms_communs],
                   sidep_antigen_complet[, ..noms_communs])
tab_rattrapee[, date_prelev := lubridate::ymd_hms(paste0(substr(DatePrelevement, 1,4), "-", 
                                                         substr(DatePrelevement, 5,6), "-",
                                                         substr(DatePrelevement, 7,8), " ",
                                                         substr(DatePrelevement, 9,10), ":", 
                                                         substr(DatePrelevement, 11,12), ":",
                                                         substr(DatePrelevement, 13,14)))]
tab_rattrapee[, num_identification := seq(.N)]
sidep_base[, num_identification_sidep_base := seq(.N)]
tab_rattrapee[, indic_tab_rattrapee := 1]
sidep_base[, indic_sidep_base := 1]
table = merge(tab_rattrapee,
              sidep_base, 
              by = c("Pseudonyme", "date_prelev"),
              all.y = T)
out = table[, .(nb = .N), by = "num_identification"]
setorder(out, -nb)
out = table[, .(nb = .N), by = "num_identification_sidep_base"]
setorder(out, -nb)
table[, compte := .N, by = "num_identification_sidep_base"]
p1 = table[compte == 1] # On garde forcément si on a un appariemment 1:1
aux = table[compte > 1]
setorder(aux, num_identification_sidep_base, PremiereDateValidationCRVariant)
aux[, num_ordre := seq(.N), by = "num_identification_sidep_base"]
aux = aux[num_ordre == 1]
table = rbind(p1,
              aux[, !c("num_ordre")])
if (dim(table)[1] != dim(sidep_base)[1]) message("Problème d'appariemment : pas le même nombre de lignes avant/après ajout des variants")
table[, info_sur_variant := 0]
table[num_identification != "", info_sur_variant := 1]
table[, check := .N, by = "num_identification"]
table(table$check) #On ne doit avoir que des 1 sinon ça veut dire qu'on a fait un appariemment 1:N. On a en pratique des 2 : ce sont les cas où on a une info variant qui colle avec une ligne TAG et une ligne PCR. Ce sont des cas qu'on va double compter sauf si on dédoublonne rigoureusement
sum(table[info_sur_variant == 1, (Joker2 == "" | is.na(Joker2)) & (Joker3 == "" | is.na(Joker3))])
sum(table[info_sur_variant == 10, (Joker2 != "" & !is.na(Joker2)) | (Joker3 != "" & !is.na(Joker3))])
out = table[, .(nb = .N), by = "info_sur_variant"]
setorder(out, -nb)
out
out = table[, .(nb = .N), by = "Joker2"]
setorder(out, -nb)
table[, date_valid_info_variant := paste0(substr(PremiereDateValidationCRVariant, 1,4), "-", 
                                          substr(PremiereDateValidationCRVariant, 5,6), "-",
                                          substr(PremiereDateValidationCRVariant, 7,8), " ",
                                          substr(PremiereDateValidationCRVariant, 9,10), ":", 
                                          substr(PremiereDateValidationCRVariant, 11,12), ":",
                                          substr(PremiereDateValidationCRVariant, 13,14))]
table[, date_valid_info_variant := lubridate::ymd_hms(date_valid_info_variant)]
table[info_sur_variant == 1, delai_remontee_variant := as.numeric(difftime(date_valid_info_variant, date_valid_init, units = "days"))]
any(table$info_sur_variant == 1 & table$delai_remontee_variant == 0)
sum(table$info_sur_variant == 1 & table$delai_remontee_variant == 0) # On compte le nombre de tests qui remontent d'un coup 1ere intention + variant
sum(table$info_sur_variant == 1) 
sum(table$info_sur_variant == 1 & table$delai_remontee_variant == 0) / sum(table$info_sur_variant == 1)
table[, day_valid_init := as.Date(date_valid_init)]
table[, delai_classique := difftime(date_valid_init, date_prelev, unit = "days")]
table[, day_valid_init := as.Date(date_valid_init)]
table[, Joker2_recode := ""]
table[Joker2 == "ABS_20I/501Y.V1-20H/501Y.V2-20J/501Y.V3", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABS_201/501Y.V1-20H/501Y.V2-20J/501Y.V3", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABS_20I/501Y.V1-20H/501Y.V2/20J/501Y.V3", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABS_20I/501Y.V1-20H/501Y.V2-20J/501Y.V3ABS_20I/501Y.V1-20H/501Y.V2-20J/501Y.V3", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABS_20I/501Y.V1-20J/501Y.V3-20H/501Y.V2", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABSVA", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABSV", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "Sars-CoV-2 pos (absence de lignage 20I/501Y.V1 et 20H/501Y.V2 et 20J/501Y.V3)", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABS_20I/501Y.V1 20H/501Y.V2 20J/501Y.V3", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABS_20I/501Y.V1-20H/501YV2-20J/501Y.V3", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABS_20I/501V1-20H/501Y.V2-20J/501Y.V3", Joker2_recode := "NON UK - NON SA - NON BR"]
table[Joker2 == "ABS_20I/501Y.V1-20H/501Y.V2", Joker2_recode := "NON UK - NON SA"]
table[Joker2 == "ABS_201/501Y.V1-20H/501Y.V2", Joker2_recode := "NON UK - NON SA"]
table[Joker2 == "ABS_20I/501Y.V1", Joker2_recode := "NON UK"]
table[Joker2 == "ABS_20l/501Y.V1", Joker2_recode := "NON UK"]
table[Joker2 == "ABS_20H/501Y.V2", Joker2_recode := "NON SA"]
table[Joker2 == "ABS_20J/501Y.V3", Joker2_recode := "NON BR"]
table[Joker2 == "PCR_20I/501Y.V1", Joker2_recode := "OUI UK"]
table[Joker2 == "PCR_20H/501Y.V1", Joker2_recode := "OUI UK"]
table[Joker2 == "PCR_20I/501Y.V1PCR_20I/501Y.V1", Joker2_recode := "OUI UK"]
table[Joker2 == "PCR_20l/501Y.V1", Joker2_recode := "OUI UK"]
table[Joker2 == "PCR_201/501Y.V1", Joker2_recode := "OUI UK"]
table[Joker2 == "PCR_ 20I/501Y.V1", Joker2_recode := "OUI UK"]
table[Joker2 == "ANGLP", Joker2_recode := "OUI UK"]
table[Joker2 == "PCR_20H/501Y.V2", Joker2_recode := "OUI SA"]
table[Joker2 == "PCR_20H/501Y.V2-20J/501Y.V3", Joker2_recode := "OUI SA ou BR"]
table[Joker2 == "PCR_20I/501Y.V1-20H/501Y.V2-20J/501Y.V3", Joker2_recode := "OUI UK ou SA ou BR"]
table[Joker2 == "PREL_NC", Joker2_recode := "Non conforme"]
table[Joker2 == "le prélèvement n'a pas permis de réaliser la recherche de variant.", Joker2_recode := "Non conforme"]
table[Joker2 == "VAR_IND", Joker2_recode := "Indéterminé"]
table[Joker2 == "IND", Joker2_recode := "Indéterminé"]
table[Joker2 == "INDET", Joker2_recode := "Indéterminé"]
table[Joker2 == "VAR_SUSP", Joker2_recode := "Suspicion"]
table[Joker2 == "SUSP_VAR", Joker2_recode := "Suspicion"]
table[Joker2 == "SUSP", Joker2_recode := "Suspicion"]
table[Joker2 == "SUSPICION", Joker2_recode := "Suspicion"]
table[Joker2 == "var_susp", Joker2_recode := "Suspicion"]
table[Joker2 == "VAR_SUS", Joker2_recode := "Suspicion"]
table[Joker2 == "Présence d'un variant suspectée", Joker2_recode := "Suspicion"]
sum(table$Joker2 != "" & table$Joker2_recode == "" & table$info_sur_variant == 1)
sum(table$Joker2 != "" & table$Joker2_recode == "" & table$info_sur_variant == 1) / sum(table$Joker2 != "" & table$info_sur_variant == 1)
aux = table[Joker2 != "" & Joker2_recode == ""]
out = aux[, .(nb = .N), by = "Joker2"]
setorder(out, -nb)
table[Joker2_recode == "" & info_sur_variant == 1, Joker2_recode := "Joker2 manquant ou non recodable"]
out = table[, .(nb = .N), by = "Joker2_recode"]
setorder(out, -nb)
table[, Joker2_simplifie := ""]
table[Joker2_recode %in% c("OUI UK",
                           "OUI SA ou BR",
                           "OUI UK ou SA ou BR",
                           "OUI SA"), Joker2_simplifie := "Oui"]
table[Joker2_recode %in% c("NON UK",
                           "NON UK - NON SA",
                           "NON UK - NON SA - NON BR"), Joker2_simplifie := "Non"]
table[Joker2_recode %in% c("Suspicion"), Joker2_simplifie := "Suspicion"]
table[Joker2_recode %in% c("Non conforme"), Joker2_simplifie := "Non conforme"]
table[Joker2_recode %in% c("Indéterminé"), Joker2_simplifie := "Indéterminé"]
table[Joker2_recode %in% c("Joker2 manquant ou non recodable"), Joker2_simplifie := "Non recodable"]
out2 = table[, .(nb = .N), by = "Joker2_simplifie"]
setorder(out2, -nb)
out = table[valeur == "N", .(nb = .N), by = "Joker2_simplifie"]
setorder(out, -nb)
out = table[valeur == "P", .(nb = .N), by = "Joker2_simplifie"]
setorder(out, -nb)
print("variants C")
print(gc(verbose = T))
tg_pcr = table[!is.na(valeur) & AnalyseConclusion.y == "94500-6"]
tg_pcr = tg_pcr[, .(nb_tot = sum(valeur %in% c("P", "N", "I", "X")),
                    nb_positifs = sum(valeur == "P"),
                    nb_info_variant = sum(info_sur_variant == 1 & valeur == "P"),
                    nb_variant_oui = sum(Joker2_simplifie == "Oui" & valeur == "P"),
                    nb_variant_non = sum(Joker2_simplifie == "Non" & valeur == "P"),
                    nb_variant_suspicion = sum(Joker2_simplifie == "Suspicion" & valeur == "P"),
                    nb_variant_nc = sum(Joker2_simplifie == "Non conforme" & valeur == "P"),
                    nb_variant_indet = sum(Joker2_simplifie == "Indéterminé" & valeur == "P"),
                    nb_variant_non_recodable = sum(Joker2_simplifie == "Non recodable" & valeur == "P"),
                    nb_variant_oui_uk = sum(Joker2_recode == "OUI UK" & valeur == "P"),
                    nb_variant_oui_sa_ou_br = sum(Joker2_recode == "OUI SA ou BR" & valeur == "P"),
                    nb_variant_oui_autre = sum(Joker2_recode %in% c("OUI SA", "OUI UK ou SA ou BR") & valeur == "P")), 
                by = c("day_valid_init")]
wb = createWorkbook()
addWorksheet(wb, "PCR only")
writeData(wb, "PCR only", tg_pcr)
openxlsx::saveWorkbook(wb, paste0(path,"data/sorties/variants/", Sys.Date(), " - table_pcr_variants.xlsx"), overwrite = T)
somme_7j <- function(x) {
  res <- ifelse(is.na(data.table::shift(x, 6)), NA, x +
                  data.table::shift(x, 1) +
                  data.table::shift(x, 2) + 
                  data.table::shift(x, 3) +  
                  data.table::shift(x, 4) +  
                  data.table::shift(x, 5) +  
                  data.table::shift(x, 6))
  return(res)
  
}
setorder(tg_pcr, day_valid_init)
tg_pcr[, `:=`(`Taux de positivité - PCR` = somme_7j(nb_positifs) / somme_7j(nb_tot),
              `Proportion de criblés parmi les positifs - PCR` = (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)) / somme_7j(nb_positifs),
              `Proportion de variant parmi les positifs criblés - PCR` = (somme_7j(nb_variant_oui) + somme_7j(nb_variant_suspicion) + somme_7j(nb_variant_indet)) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
              `Proportion de variant UK parmi les positifs criblés - PCR` = somme_7j(nb_variant_oui_uk) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
              `Proportion de variant SA ou BR parmi les positifs criblés - PCR` = somme_7j(nb_variant_oui_sa_ou_br) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
              `Proportion de variant autre parmi les positifs criblés - PCR` = (somme_7j(nb_variant_oui_autre) + somme_7j(nb_variant_suspicion) + somme_7j(nb_variant_indet)) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)))]
tg = table[!is.na(valeur)]
tg = tg[, .(nb_tot = sum(valeur %in% c("P", "N", "I", "X")),
            nb_positifs = sum(valeur == "P"),
            nb_info_variant = sum(info_sur_variant == 1 & valeur == "P"),
            nb_variant_oui = sum(Joker2_simplifie == "Oui" & valeur == "P"),
            nb_variant_non = sum(Joker2_simplifie == "Non" & valeur == "P"),
            nb_variant_suspicion = sum(Joker2_simplifie == "Suspicion" & valeur == "P"),
            nb_variant_nc = sum(Joker2_simplifie == "Non conforme" & valeur == "P"),
            nb_variant_indet = sum(Joker2_simplifie == "Indéterminé" & valeur == "P"),
            nb_variant_non_recodable = sum(Joker2_simplifie == "Non recodable" & valeur == "P"),
            nb_variant_oui_uk = sum(Joker2_recode == "OUI UK" & valeur == "P"),
            nb_variant_oui_sa_ou_br = sum(Joker2_recode == "OUI SA ou BR" & valeur == "P"),
            nb_variant_oui_autre = sum(Joker2_recode %in% c("OUI SA", "OUI UK ou SA ou BR") & valeur == "P")), 
        by = c("day_valid_init")]
wb = createWorkbook()
addWorksheet(wb, "PCR et TAG")
writeData(wb, "PCR et TAG", tg)
openxlsx::saveWorkbook(wb, paste0(path,"data/sorties/variants/", Sys.Date(), " - table_tot_variants.xlsx"), overwrite = T)
setorder(tg, day_valid_init)
tg[, `:=`(`Taux de positivité` = somme_7j(nb_positifs) / somme_7j(nb_tot))]
tg_all = merge(tg[, c("day_valid_init", "Taux de positivité")],
               tg_pcr[, c("day_valid_init",
                          "Proportion de variant UK parmi les positifs criblés - PCR",
                          "Proportion de variant SA ou BR parmi les positifs criblés - PCR",
                          "Proportion de variant autre parmi les positifs criblés - PCR",
                          "Proportion de criblés parmi les positifs - PCR")])
tg_all[, `dont estimation du TP attribuable au variant UK` := `Proportion de variant UK parmi les positifs criblés - PCR` * `Taux de positivité`]
tg_all[, `dont estimation du TP attribuable au variant SA ou BR` := `Proportion de variant SA ou BR parmi les positifs criblés - PCR` * `Taux de positivité`]
tg_all[, `dont estimation du TP attribuable au variant autre` := `Proportion de variant autre parmi les positifs criblés - PCR` * `Taux de positivité`]
setnames(tg_all, "Proportion de criblés parmi les positifs - PCR", "Proportion des tests RT-PCR positifs faisant l'objet d'un criblage")
tg_all[, `Proportion des tests RT-PCR positifs faisant l'objet d'un criblage\n(échelle de droite)` := `Proportion des tests RT-PCR positifs faisant l'objet d'un criblage` * 8 / 60]
graphique_decomp_tp_fra_hebdo = ggplot() +
  geom_bar(data = melt(tg_all[day_valid_init >= "2021-01-31" & 
                                day_valid_init <= Sys.Date() - 2, 
                              c("day_valid_init", 
                                "dont estimation du TP attribuable au variant UK", 
                                "dont estimation du TP attribuable au variant SA ou BR",
                                "dont estimation du TP attribuable au variant autre")], 
                       id.vars = "day_valid_init"), 
           aes(day_valid_init, value, group = variable, fill = variable), 
           stat = "identity",
           width = .5) +
  geom_line(data = melt(tg_all[day_valid_init >= "2021-01-01" & 
                                 day_valid_init <= Sys.Date() - 1, 
                               c("day_valid_init",
                                 "Taux de positivité",
                                 "Proportion des tests RT-PCR positifs faisant l'objet d'un criblage\n(échelle de droite)")], 
                        id.vars = "day_valid_init",
                        size = 1.1), 
            aes(day_valid_init, value, group = variable, color = variable)) + 
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = .5, size = 10),
        plot.subtitle = element_text(hjust = .5, size = 8),
        plot.caption = element_text(hjust = 0, size = 8),
        axis.title.x = element_text(size = 8)) +
  xlab("Date de validation du résultat du test de première intention") + 
  guides(fill = guide_legend(nrow = 3),
         color = guide_legend(nrow = 3)) + 
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")) + 
  scale_color_manual(values = c(rgb(0, 0, 128/255), rgb(1, 204/255, 0), rgb(1, 102/255, 0))) + 
  labs(title = "Evolution du taux de positivité des tests (RT-PCR et TAg, non dédoublonnés)\net des informations issues du criblage", 
       subtitle = "Moyennes hebdomadaires glissantes",
       caption = "A partir des résultats obtenus sur les tests RT-PCR positifs criblés, on réalise une estimation de la décomposition du taux de positivité entre\nles différents variants. Cela repose sur l'hypothèse que les tests actuellement criblés sont effectivement représentatifs de l'ensemble des tests.") +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~ . * 60 / 8, labels = scales::percent)) + 
  geom_vline(xintercept = Sys.Date() - 4, 
             linetype = "dotted")
graphique_decomp_tp_fra_hebdo
ggsave(graphique_decomp_tp_fra_hebdo, 
       filename = paste0(path,"data/sorties/variants/", Sys.Date(), " - graphique_decomp_tp_fra_hebdo.jpg"),
       width = 8, height = 5, dpi = 800)
setorder(table, nom_dep)
LISTE_DEP = na.omit(unique(table$nom_dep))
pdf(paste0(path,"data/sorties/variants/", Sys.Date(), " - decomposition_tp_par_departement.pdf"))
for (DEP in LISTE_DEP){
  
  tg_pcr = table[!is.na(valeur) & AnalyseConclusion.y == "94500-6" & nom_dep == DEP]
  tg_pcr = tg_pcr[, .(nb_tot = sum(valeur %in% c("P", "N", "I", "X")),
                      nb_positifs = sum(valeur == "P"),
                      nb_info_variant = sum(info_sur_variant == 1 & valeur == "P"),
                      nb_variant_oui = sum(Joker2_simplifie == "Oui" & valeur == "P"),
                      nb_variant_non = sum(Joker2_simplifie == "Non" & valeur == "P"),
                      nb_variant_suspicion = sum(Joker2_simplifie == "Suspicion" & valeur == "P"),
                      nb_variant_nc = sum(Joker2_simplifie == "Non conforme" & valeur == "P"),
                      nb_variant_indet = sum(Joker2_simplifie == "Indéterminé" & valeur == "P"),
                      nb_variant_non_recodable = sum(Joker2_simplifie == "Non recodable" & valeur == "P"),
                      nb_variant_oui_uk = sum(Joker2_recode == "OUI UK" & valeur == "P"),
                      nb_variant_oui_sa_ou_br = sum(Joker2_recode == "OUI SA ou BR" & valeur == "P"),
                      nb_variant_oui_autre = sum(Joker2_recode %in% c("OUI SA", "OUI UK ou SA ou BR") & valeur == "P")), 
                  by = c("day_valid_init")]
  
  setorder(tg_pcr, day_valid_init)
  tg_pcr[, `:=`(`Taux de positivité - PCR` = somme_7j(nb_positifs) / somme_7j(nb_tot),
                `Proportion de criblés parmi les positifs - PCR` = (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)) / somme_7j(nb_positifs),
                `Proportion de variant parmi les positifs criblés - PCR` = (somme_7j(nb_variant_oui) + somme_7j(nb_variant_suspicion) + somme_7j(nb_variant_indet)) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
                `Proportion de variant UK parmi les positifs criblés - PCR` = somme_7j(nb_variant_oui_uk) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
                `Proportion de variant SA ou BR parmi les positifs criblés - PCR` = somme_7j(nb_variant_oui_sa_ou_br) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
                `Proportion de variant autre parmi les positifs criblés - PCR` = (somme_7j(nb_variant_oui_autre) + somme_7j(nb_variant_suspicion) + somme_7j(nb_variant_indet)) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)))]
  
  tg = table[!is.na(valeur) & nom_dep == DEP]
  tg = tg[, .(nb_tot = sum(valeur %in% c("P", "N", "I", "X")),
              nb_positifs = sum(valeur == "P"),
              nb_info_variant = sum(info_sur_variant == 1 & valeur == "P"),
              nb_variant_oui = sum(Joker2_simplifie == "Oui" & valeur == "P"),
              nb_variant_non = sum(Joker2_simplifie == "Non" & valeur == "P"),
              nb_variant_suspicion = sum(Joker2_simplifie == "Suspicion" & valeur == "P"),
              nb_variant_nc = sum(Joker2_simplifie == "Non conforme" & valeur == "P"),
              nb_variant_indet = sum(Joker2_simplifie == "Indéterminé" & valeur == "P"),
              nb_variant_non_recodable = sum(Joker2_simplifie == "Non recodable" & valeur == "P"),
              nb_variant_oui_uk = sum(Joker2_recode == "OUI UK" & valeur == "P"),
              nb_variant_oui_sa_ou_br = sum(Joker2_recode == "OUI SA ou BR" & valeur == "P"),
              nb_variant_oui_autre = sum(Joker2_recode %in% c("OUI SA", "OUI UK ou SA ou BR") & valeur == "P")), 
          by = c("day_valid_init")]
  
  setorder(tg, day_valid_init)
  tg[, `:=`(`Taux de positivité` = somme_7j(nb_positifs) / somme_7j(nb_tot))]
  
  tg_all = merge(tg[, c("day_valid_init", "Taux de positivité")],
                 tg_pcr[, c("day_valid_init",
                            "Proportion de variant UK parmi les positifs criblés - PCR",
                            "Proportion de variant SA ou BR parmi les positifs criblés - PCR",
                            "Proportion de variant autre parmi les positifs criblés - PCR",
                            "Proportion de criblés parmi les positifs - PCR")],
                 all = T)
  
  
  tg_all[, `dont estimation du TP attribuable au variant UK` := `Proportion de variant UK parmi les positifs criblés - PCR` * `Taux de positivité`]
  tg_all[, `dont estimation du TP attribuable au variant SA ou BR` := `Proportion de variant SA ou BR parmi les positifs criblés - PCR` * `Taux de positivité`]
  tg_all[, `dont estimation du TP attribuable au variant autre` := `Proportion de variant autre parmi les positifs criblés - PCR` * `Taux de positivité`]
  
  setnames(tg_all, "Proportion de criblés parmi les positifs - PCR", "Proportion des tests RT-PCR positifs faisant l'objet d'un criblage")
  tg_all[, `Proportion des tests RT-PCR positifs faisant l'objet d'un criblage\n(échelle de droite)` := `Proportion des tests RT-PCR positifs faisant l'objet d'un criblage` * 8 / 60]
  
  print(ggplot() +
          geom_bar(data = melt(tg_all[day_valid_init >= "2021-01-31" & 
                                        day_valid_init <= Sys.Date() - 2, 
                                      c("day_valid_init", 
                                        "dont estimation du TP attribuable au variant UK", 
                                        "dont estimation du TP attribuable au variant SA ou BR",
                                        "dont estimation du TP attribuable au variant autre")], 
                               id.vars = "day_valid_init"), 
                   aes(day_valid_init, value, group = variable, fill = variable), 
                   stat = "identity",
                   width = .5) +
          geom_line(data = melt(tg_all[day_valid_init >= "2021-01-01" & 
                                         day_valid_init <= Sys.Date() - 1, 
                                       c("day_valid_init",
                                         "Taux de positivité",
                                         "Proportion des tests RT-PCR positifs faisant l'objet d'un criblage\n(échelle de droite)")], 
                                id.vars = "day_valid_init",
                                size = 1.1), 
                    aes(day_valid_init, value, group = variable, color = variable)) + 
          theme_minimal() +
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(hjust = .5, size = 10),
                plot.subtitle = element_text(hjust = .5, size = 8),
                plot.caption = element_text(hjust = 0, size = 8),
                axis.title.x = element_text(size = 8),
                legend.text = element_text(size = 6)) +
          xlab("Date de validation du résultat du test de première intention") + 
          guides(fill = guide_legend(nrow = 3),
                 color = guide_legend(nrow = 3)) + 
          scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")) + 
          scale_color_manual(values = c(rgb(0, 0, 128/255), rgb(1, 204/255, 0), rgb(1, 102/255, 0))) + 
          labs(title = "Evolution du taux de positivité des tests (RT-PCR et TAg, non dédoublonnés)\net des informations issues du criblage", 
               subtitle = paste0("Département ", DEP, " - Moyennes hebdomadaires glissantes"),
               caption = "A partir des résultats obtenus sur les tests RT-PCR positifs criblés, on réalise une estimation de la décomposition\ndu taux de positivité entre les différents variants. Cela repose sur l'hypothèse que les tests actuellement\ncriblés sont effectivement représentatifs de l'ensemble des tests.") +
          scale_y_continuous(labels = scales::percent, 
                             sec.axis = sec_axis(~ . * 60 / 8, labels = scales::percent)) + 
          geom_vline(xintercept = Sys.Date() - 4, 
                     linetype = "dotted")
  )
  
  
}
dev.off()
LISTE_REG = na.omit(unique(table$nom_reg))
LISTE_REG = LISTE_REG[order(LISTE_REG)]
pdf(paste0(path,"data/sorties/variants/", Sys.Date(), " - decomposition_tp_par_region.pdf"))
for (REG in LISTE_REG){
  tg_pcr = table[!is.na(valeur) & AnalyseConclusion.y == "94500-6" & nom_reg == REG]
  tg_pcr = tg_pcr[, .(nb_tot = sum(valeur %in% c("P", "N", "I", "X")),
                      nb_positifs = sum(valeur == "P"),
                      nb_info_variant = sum(info_sur_variant == 1 & valeur == "P"),
                      nb_variant_oui = sum(Joker2_simplifie == "Oui" & valeur == "P"),
                      nb_variant_non = sum(Joker2_simplifie == "Non" & valeur == "P"),
                      nb_variant_suspicion = sum(Joker2_simplifie == "Suspicion" & valeur == "P"),
                      nb_variant_nc = sum(Joker2_simplifie == "Non conforme" & valeur == "P"),
                      nb_variant_indet = sum(Joker2_simplifie == "Indéterminé" & valeur == "P"),
                      nb_variant_non_recodable = sum(Joker2_simplifie == "Non recodable" & valeur == "P"),
                      nb_variant_oui_uk = sum(Joker2_recode == "OUI UK" & valeur == "P"),
                      nb_variant_oui_sa_ou_br = sum(Joker2_recode == "OUI SA ou BR" & valeur == "P"),
                      nb_variant_oui_autre = sum(Joker2_recode %in% c("OUI SA", "OUI UK ou SA ou BR") & valeur == "P")), 
                  by = c("day_valid_init")]
  
  setorder(tg_pcr, day_valid_init)
  tg_pcr[, `:=`(`Taux de positivité - PCR` = somme_7j(nb_positifs) / somme_7j(nb_tot),
                `Proportion de criblés parmi les positifs - PCR` = (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)) / somme_7j(nb_positifs),
                `Proportion de variant parmi les positifs criblés - PCR` = (somme_7j(nb_variant_oui) + somme_7j(nb_variant_suspicion) + somme_7j(nb_variant_indet)) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
                `Proportion de variant UK parmi les positifs criblés - PCR` = somme_7j(nb_variant_oui_uk) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
                `Proportion de variant SA ou BR parmi les positifs criblés - PCR` = somme_7j(nb_variant_oui_sa_ou_br) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)),
                `Proportion de variant autre parmi les positifs criblés - PCR` = (somme_7j(nb_variant_oui_autre) + somme_7j(nb_variant_suspicion) + somme_7j(nb_variant_indet)) / (somme_7j(nb_info_variant) - somme_7j(nb_variant_non_recodable) - somme_7j(nb_variant_nc)))]
  
  tg = table[!is.na(valeur) & nom_reg == REG]
  tg = tg[, .(nb_tot = sum(valeur %in% c("P", "N", "I", "X")),
              nb_positifs = sum(valeur == "P"),
              nb_info_variant = sum(info_sur_variant == 1 & valeur == "P"),
              nb_variant_oui = sum(Joker2_simplifie == "Oui" & valeur == "P"),
              nb_variant_non = sum(Joker2_simplifie == "Non" & valeur == "P"),
              nb_variant_suspicion = sum(Joker2_simplifie == "Suspicion" & valeur == "P"),
              nb_variant_nc = sum(Joker2_simplifie == "Non conforme" & valeur == "P"),
              nb_variant_indet = sum(Joker2_simplifie == "Indéterminé" & valeur == "P"),
              nb_variant_non_recodable = sum(Joker2_simplifie == "Non recodable" & valeur == "P"),
              nb_variant_oui_uk = sum(Joker2_recode == "OUI UK" & valeur == "P"),
              nb_variant_oui_sa_ou_br = sum(Joker2_recode == "OUI SA ou BR" & valeur == "P"),
              nb_variant_oui_autre = sum(Joker2_recode %in% c("OUI SA", "OUI UK ou SA ou BR") & valeur == "P")), 
          by = c("day_valid_init")]
  
  setorder(tg, day_valid_init)
  tg[, `:=`(`Taux de positivité` = somme_7j(nb_positifs) / somme_7j(nb_tot))]
  
  tg_all = merge(tg[, c("day_valid_init", "Taux de positivité")],
                 tg_pcr[, c("day_valid_init",
                            "Proportion de variant UK parmi les positifs criblés - PCR",
                            "Proportion de variant SA ou BR parmi les positifs criblés - PCR",
                            "Proportion de variant autre parmi les positifs criblés - PCR",
                            "Proportion de criblés parmi les positifs - PCR")],
                 all = T)
  
  
  tg_all[, `dont estimation du TP attribuable au variant UK` := `Proportion de variant UK parmi les positifs criblés - PCR` * `Taux de positivité`]
  tg_all[, `dont estimation du TP attribuable au variant SA ou BR` := `Proportion de variant SA ou BR parmi les positifs criblés - PCR` * `Taux de positivité`]
  tg_all[, `dont estimation du TP attribuable au variant autre` := `Proportion de variant autre parmi les positifs criblés - PCR` * `Taux de positivité`]
  
  setnames(tg_all, "Proportion de criblés parmi les positifs - PCR", "Proportion des tests RT-PCR positifs faisant l'objet d'un criblage")
  tg_all[, `Proportion des tests RT-PCR positifs faisant l'objet d'un criblage\n(échelle de droite)` := `Proportion des tests RT-PCR positifs faisant l'objet d'un criblage` * 8 / 60]
  
  print(ggplot() +
          geom_bar(data = melt(tg_all[day_valid_init >= "2021-01-31" & 
                                        day_valid_init <= Sys.Date() - 2, 
                                      c("day_valid_init", 
                                        "dont estimation du TP attribuable au variant UK", 
                                        "dont estimation du TP attribuable au variant SA ou BR",
                                        "dont estimation du TP attribuable au variant autre")], 
                               id.vars = "day_valid_init"), 
                   aes(day_valid_init, value, group = variable, fill = variable), 
                   stat = "identity",
                   width = .5) +
          geom_line(data = melt(tg_all[day_valid_init >= "2021-01-01" & 
                                         day_valid_init <= Sys.Date() - 1, 
                                       c("day_valid_init",
                                         "Taux de positivité",
                                         "Proportion des tests RT-PCR positifs faisant l'objet d'un criblage\n(échelle de droite)")], 
                                id.vars = "day_valid_init",
                                size = 1.1), 
                    aes(day_valid_init, value, group = variable, color = variable)) + 
          theme_minimal() +
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(hjust = .5, size = 10),
                plot.subtitle = element_text(hjust = .5, size = 8),
                plot.caption = element_text(hjust = 0, size = 8),
                axis.title.x = element_text(size = 8),
                legend.text = element_text(size = 6)) +
          xlab("Date de validation du résultat du test de première intention") + 
          guides(fill = guide_legend(nrow = 3),
                 color = guide_legend(nrow = 3)) + 
          scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")) + 
          scale_color_manual(values = c(rgb(0, 0, 128/255), rgb(1, 204/255, 0), rgb(1, 102/255, 0))) + 
          labs(title = "Evolution du taux de positivité des tests (RT-PCR et TAg, non dédoublonnés)\net des informations issues du criblage", 
               subtitle = paste0("Région ", REG, " - Moyennes hebdomadaires glissantes"),
               caption = "A partir des résultats obtenus sur les tests RT-PCR positifs criblés, on réalise une estimation de la décomposition\ndu taux de positivité entre les différents variants. Cela repose sur l'hypothèse que les tests actuellement\ncriblés sont effectivement représentatifs de l'ensemble des tests.") +
          scale_y_continuous(labels = scales::percent, 
                             sec.axis = sec_axis(~ . * 60 / 8, labels = scales::percent)) + 
          geom_vline(xintercept = Sys.Date() - 4, 
                     linetype = "dotted")
  )
  
  
}
dev.off()
print("variants D")
print(gc(verbose = T))
tg_pcr = table[!is.na(valeur) & AnalyseConclusion.y == "94500-6" & day_valid_init >= "2021-01-01"]
tg_pcr = tg_pcr[, .(nb_pos_pcr = sum(valeur == "P"),
                    nb_info_variant_pcr = sum(info_sur_variant == 1 & valeur == "P"),
                    nb_variant_oui_pcr = sum(Joker2_simplifie == "Oui" & valeur == "P"),
                    nb_variant_non_pcr = sum(Joker2_simplifie == "Non" & valeur == "P"),
                    nb_variant_suspicion_pcr = sum(Joker2_simplifie == "Suspicion" & valeur == "P"),
                    nb_variant_nc_pcr = sum(Joker2_simplifie == "Non conforme" & valeur == "P"),
                    nb_variant_indet_pcr = sum(Joker2_simplifie == "Indéterminé" & valeur == "P"),
                    nb_variant_non_recodable_pcr = sum(Joker2_simplifie == "Non recodable" & valeur == "P"),
                    nb_variant_oui_uk_pcr = sum(Joker2_recode == "OUI UK" & valeur == "P"),
                    nb_variant_oui_sa_ou_br_pcr = sum(Joker2_recode == "OUI SA ou BR" & valeur == "P"),
                    nb_variant_oui_autre_pcr = sum(Joker2_recode %in% c("OUI SA", "OUI UK ou SA ou BR") & valeur == "P")), 
                by = c("nom_dep", "departement", "day_valid_init")]
tg = table[!is.na(valeur) & day_valid_init >= "2021-01-01"]
tg = tg[, .(nb_tot_pcr_tag = sum(valeur %in% c("P", "N", "I", "X")),
            nb_positifs_pcr_tag = sum(valeur == "P")), 
        by = c("nom_dep", "departement", "day_valid_init")]
t_out = merge(tg_pcr, tg)
somme_7j <- function(x) {
  res <- ifelse(is.na(data.table::shift(x, 6)), NA, x +
                  data.table::shift(x, 1) +
                  data.table::shift(x, 2) + 
                  data.table::shift(x, 3) +  
                  data.table::shift(x, 4) +  
                  data.table::shift(x, 5) +  
                  data.table::shift(x, 6))
  return(res)
  
}
pop_spf = fread("data/utils/pop_dep_spf.csv")
pop_spf[nchar(dep) == 1, dep := paste0("0", dep)]
t_out = merge(t_out, 
              pop_spf,
              by.x = "departement",
              by.y = "dep",
              all.x = T)
setorder(t_out, departement, day_valid_init)
t_out[, tp := somme_7j(nb_positifs_pcr_tag) / somme_7j(nb_tot_pcr_tag), by = "departement"]
t_out[, tp_variant_uk := tp * somme_7j(nb_variant_oui_uk_pcr) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, tp_variant_sabr := tp * somme_7j(nb_variant_oui_sa_ou_br_pcr) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, tp_variant_autre := tp * (somme_7j(nb_variant_oui_autre_pcr) + somme_7j(nb_variant_indet_pcr) + somme_7j(nb_variant_suspicion_pcr)) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, tp_variant_non := tp - tp_variant_uk - tp_variant_sabr - tp_variant_autre, by = "departement"]
t_out[, delta_tp := tp - shift(tp, 7), by = "departement"]
t_out[, delta_tp_variant_uk := tp_variant_uk - shift(tp_variant_uk, 7), by = "departement"]
t_out[, delta_tp_variant_sabr := tp_variant_sabr - shift(tp_variant_sabr, 7), by = "departement"]
t_out[, delta_tp_variant_non := tp_variant_non - shift(tp_variant_non, 7), by = "departement"]
t_out[, ti := somme_7j(nb_positifs_pcr_tag) / pop, by = "departement"]
t_out[, ti_variant_uk := ti * somme_7j(nb_variant_oui_uk_pcr) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, ti_variant_sabr := ti * somme_7j(nb_variant_oui_sa_ou_br_pcr) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, ti_variant_autre := ti * (somme_7j(nb_variant_oui_autre_pcr) + somme_7j(nb_variant_indet_pcr) + somme_7j(nb_variant_suspicion_pcr)) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, ti_variant_non := ti - ti_variant_uk - ti_variant_sabr - ti_variant_autre, by = "departement"]
t_out[, delta_ti := ti - shift(ti, 7), by = "departement"]
t_out[, delta_ti_variant_uk := ti_variant_uk - shift(ti_variant_uk, 7), by = "departement"]
t_out[, delta_ti_variant_sabr := ti_variant_sabr - shift(ti_variant_sabr, 7), by = "departement"]
t_out[, delta_ti_variant_non := ti_variant_non - shift(ti_variant_non, 7), by = "departement"]
t_out[, part_variant := (somme_7j(nb_variant_oui_pcr) + somme_7j(nb_variant_indet_pcr) + somme_7j(nb_variant_suspicion_pcr)) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, part_variant_uk := somme_7j(nb_variant_oui_uk_pcr) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, part_variant_sabr := somme_7j(nb_variant_oui_sa_ou_br_pcr) / (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)), by = "departement"]
t_out[, part_variant_quot := (nb_variant_oui_pcr + nb_variant_indet_pcr + nb_variant_suspicion_pcr) / (nb_info_variant_pcr - nb_variant_non_recodable_pcr - nb_variant_nc_pcr), by = "departement"]
t_out[, part_variant_uk_quot := nb_variant_oui_uk_pcr / (nb_info_variant_pcr - nb_variant_non_recodable_pcr - nb_variant_nc_pcr), by = "departement"]
t_out[, part_variant_sabr_quot := nb_variant_oui_sa_ou_br_pcr / (nb_info_variant_pcr - nb_variant_non_recodable_pcr - nb_variant_nc_pcr), by = "departement"]
t_out[, delta_part_variant := part_variant - shift(part_variant, 7), by = "departement"]
t_out[, delta_part_variant_uk := part_variant_uk - shift(part_variant_uk, 7), by = "departement"]
t_out[, delta_part_variant_sabr := part_variant_sabr - shift(part_variant_sabr, 7), by = "departement"]
t_out[, delta_part_variant_quot := part_variant_quot - shift(part_variant_quot, 7), by = "departement"]
t_out[, delta_part_variant_uk_quot := part_variant_uk_quot - shift(part_variant_uk_quot, 7), by = "departement"]
t_out[, delta_part_variant_sabr_quot := part_variant_sabr_quot - shift(part_variant_sabr_quot, 7), by = "departement"]
t_out[, part_cribles := (somme_7j(nb_info_variant_pcr) - somme_7j(nb_variant_non_recodable_pcr) - somme_7j(nb_variant_nc_pcr)) / somme_7j(nb_pos_pcr), by = "departement"]
t_out[, delta_part_cribles := part_cribles - shift(part_cribles, 7), by = "departement"]
write.csv(t_out, paste0(path,"data/sorties/variants/", Sys.Date(), " - table_pour_carto.csv"))
print("variants E")
print(gc(verbose = T))
wb = loadWorkbook("src/utils/Modèle fichier variants.xlsx")
t = setDT(read.xlsx(paste0("data/sorties/nb_tests/", gsub("-", "", Sys.Date()), "_sidep_pcr_valid.xlsx"),
          sheet = "national", 
          detectDates = T))
writeData(wb,
          "pcr valid pr check", 
          t[date <= Sys.Date(), c("date", "nb_pos", "nb_tot_PNIX")])
rm(t)
t = setDT(read.xlsx(paste0("data/sorties/nb_tests/", gsub("-", "", Sys.Date()), "_sidep_antigenique_valid.xlsx"),
                    sheet = "national", 
                    detectDates = T))
t = merge(data.frame(date = seq.Date(from = min(t[date <= Sys.Date()]$date),
                                 to = max(t[date <= Sys.Date()]$date),
                                 by = 1)),
      t[date <= Sys.Date(), c("date", "nb_pos", "nb_tot_PNIX")],
      all.x = T)
setorder(t, date)
writeData(wb,
          "tag valid pr check", 
          t)
rm(t)
t = setDT(read.xlsx(paste0("data/sorties/variants/", Sys.Date(), " - table_pcr_variants.xlsx"),
                    detectDates = T))
setorder(t, day_valid_init)
t[, variant_oui_quot := nb_variant_oui / (nb_info_variant - nb_variant_nc - nb_variant_non_recodable)]
writeData(wb,
          "PCR only", 
          t[!is.na(day_valid_init) & day_valid_init < Sys.Date()])
rm(t)
t = setDT(read.xlsx(paste0("data/sorties/variants/", Sys.Date(), " - table_tot_variants.xlsx"),
                    detectDates = T))
setorder(t, day_valid_init)
writeData(wb,
          "PCR et TAG", 
          t[!is.na(day_valid_init) & day_valid_init < Sys.Date()])
rm(t)
saveWorkbook(wb, paste0("data/sorties/variants/", gsub("-", "", Sys.Date()), " table_pcr_variants - avec graphiques.xlsx"))
print("variants F")
print(gc(verbose = T))
library(officer)
library(data.table)
library(leaflet)
library(sf)
library(htmltools)
library(openxlsx)
if (!paste0(Sys.Date(), " - table_pour_carto.csv") %in% dir("data/sorties/variants/")){
  message("Attention, les données ne sont pas à jour : le programme ne peut pas tourner.")
} else{
  
  data_sidep = fread(paste0("data/sorties/variants/", Sys.Date(), " - table_pour_carto.csv"))
  data_sidep = data_sidep[!is.na(departement) & departement != "dep_inconnu" & departement != "97"]
  setorder(data_sidep, departement, day_valid_init)
  data_sidep = data_sidep[, delta_part_cribles := part_cribles - shift(part_cribles, 7), by = "departement"]
  
  setorder(data_sidep, departement, day_valid_init)
  data_sidep[, evo_pourcentage_tp := tp / shift(tp, 7) - 1, by = "departement"]
  data_sidep[, evo_pourcentage_tp_uk := tp_variant_uk / shift(tp_variant_uk, 7) - 1, by = "departement"]
  data_sidep[, evo_pourcentage_tp_sabr := tp_variant_sabr / shift(tp_variant_sabr, 7) - 1, by = "departement"]
  data_sidep[is.nan(evo_pourcentage_tp_sabr), evo_pourcentage_tp_sabr := NA]
  data_sidep[evo_pourcentage_tp_sabr == Inf, evo_pourcentage_tp_sabr := 10]
  data_sidep[evo_pourcentage_tp_sabr == -Inf, evo_pourcentage_tp_sabr := -10]
  data_sidep[, evo_pourcentage_tp_non := tp_variant_non / shift(tp_variant_non, 7) - 1, by = "departement"]
  
  setorder(data_sidep, departement, day_valid_init)
  data_sidep[, evo_pourcentage_ti := ti / shift(ti, 7) - 1, by = "departement"]
  data_sidep[, evo_pourcentage_ti_uk := ti_variant_uk / shift(ti_variant_uk, 7) - 1, by = "departement"]
  data_sidep[, evo_pourcentage_ti_sabr := ti_variant_sabr / shift(ti_variant_sabr, 7) - 1, by = "departement"]
  data_sidep[is.nan(evo_pourcentage_ti_sabr), evo_pourcentage_ti_sabr := NA]
  data_sidep[evo_pourcentage_ti_sabr == Inf, evo_pourcentage_ti_sabr := 10]
  data_sidep[evo_pourcentage_ti_sabr == -Inf, evo_pourcentage_ti_sabr := -10]
  data_sidep[, evo_pourcentage_ti_non := ti_variant_non / shift(ti_variant_non, 7) - 1, by = "departement"]
  
  
  
  load("data/utils/carte_dep.Rdata")
  bornes <- unname(st_bbox(carte_dep))
  
  
  
  
  cartographie = function(data,
                          titre,
                          coefficient = 1,
                          suffixe = "",
                          delta = F,
                          reverse_delta = T){
    
    multiplie = function(x) {return(x * coefficient)}
    
    names(data) = c("departement", "valeur")
    carte_dep <- merge(carte_dep,
                       data,
                       by.x = "CC_2",
                       by.y = "departement")
    
    
    if (delta == F) {
      palette_de_couleur_aplat <- colorNumeric(
        "viridis", reverse=TRUE,
        domain = c(min(na.omit(data[nchar(departement) == 2]$valeur)), max(na.omit(data[nchar(departement) == 2]$valeur)))
      )
    } else {
      palette_de_couleur_aplat <- colorNumeric(
        "RdBu", reverse=reverse_delta,
        domain = c(-max(abs(na.omit(data[nchar(departement) == 2]$valeur))), max(abs(na.omit(data[nchar(departement) == 2]$valeur))))
      )
    }
    
    leaflet(carte_dep) %>%
      fitBounds(lng1 = bornes[1],
                lng2 = bornes[3],
                lat1 = bornes[2],
                lat2 = bornes[4]) %>%
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
      )%>%
      addPolygons(data = carte_dep,
                  layerId = ~GID_2,
                  stroke = T,
                  color = "white",
                  weight = 1,
                  fillOpacity = 0.8,
                  fillColor = ~palette_de_couleur_aplat(valeur),
                  label = paste0(carte_dep@data$NAME_2, " : ", round(coefficient * carte_dep@data$valeur, 1), suffixe),
                  labelOptions = labelOptions(
                    opacity = 0.7,
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto"
                  )
      ) %>%
      addLegend(position = "topright",
                pal = palette_de_couleur_aplat,
                values = carte_dep$valeur,
                labels = paste0(carte_dep$valeur, "%"),
                opacity = .8,
                labFormat = labelFormat(suffix = suffixe,
                                        transform = multiplie)) %>%
      addControl(HTML(titre),
                 position="bottomleft", layerId = "title")
    
  }
  
  
  
  G_1.1 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "tp")]
  carte_tp = cartographie(data = G_1.1, 
                          titre = "1.1. Taux de positivité des tests (non dédoublonnés) à J-2",
                          coefficient = 100,
                          suffixe = " %")
  carte_tp
  
  G_1.2 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "tp_variant_uk")]
  carte_tpv_uk = cartographie(data = G_1.2, 
                              titre = "1.2. Estimation du taux de positivité des tests (non dédoublonnés) attribuable au variant UK à J-2",
                              coefficient = 100,
                              suffixe = " %")
  carte_tpv_uk
  
  G_1.3 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "tp_variant_sabr")]
  carte_tpv_sabr = cartographie(data = G_1.3, 
                                titre = "1.3. Estimation du taux de positivité des tests (non dédoublonnés) attribuable au variant SA/BR à J-2",
                                coefficient = 100,
                                suffixe = " %")
  carte_tpv_sabr
  
  G_1.4 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "tp_variant_non")]
  carte_tpv_non = cartographie(data = G_1.4, 
                               titre = "1.4. Estimation du taux de positivité des tests (non dédoublonnés) non attribuable à un variant à J-2",
                               coefficient = 100,
                               suffixe = " %")
  carte_tpv_non
  
  
  
  
  G_2.1 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "delta_tp")]
  carte_evo_tp = cartographie(data = G_2.1, 
                              titre = "2.1. Delta hebdomadaire du taux de positivité des tests (non dédoublonnés) à J-2",
                              coefficient = 100,
                              suffixe = " point(s)",
                              delta = T)
  carte_evo_tp
  
  G_2.2 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "delta_tp_variant_uk")]
  carte_evo_tpv_uk = cartographie(data = G_2.2, 
                                  titre = "2.2. Delta hebdomadaire de l'estimation du taux de positivité des tests (non dédoublonnés) attribuable au variant UK à J-2",
                                  coefficient = 100,
                                  suffixe = " point(s)",
                                  delta = T)
  carte_evo_tpv_uk
  
  G_2.3 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "delta_tp_variant_sabr")]
  carte_evo_tpv_sabr = cartographie(data = G_2.3, 
                                    titre = "2.3. Delta hebdomadaire de l'estimation du taux de positivité des tests (non dédoublonnés) attribuable au variant SA/BR à J-2",
                                    coefficient = 100,
                                    suffixe = " point(s)",
                                    delta = T)
  carte_evo_tpv_sabr
  
  G_2.4 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "delta_tp_variant_non")]
  carte_evo_tpv_non = cartographie(data = G_2.4, 
                                   titre = "2.4. Delta hebdomadaire de l'estimation du taux de positivité des tests (non dédoublonnés) non attribuable à un variant à J-2",
                                   coefficient = 100,
                                   suffixe = " point(s)",
                                   delta = T)
  carte_evo_tpv_non
  
  
  
  
  G_2.1_bis = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "evo_pourcentage_tp")]
  carte_evo_pourcentage_tp = cartographie(data = G_2.1_bis, 
                                          titre = "2.1.bis. Variation hebdomadaire du taux de positivité des tests (non dédoublonnés) à J-2",
                                          coefficient = 100,
                                          suffixe = "%",
                                          delta = T)
  carte_evo_pourcentage_tp
  
  G_2.2_bis = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "evo_pourcentage_tp_uk")]
  carte_evo_pourcentage_tpv_uk = cartographie(data = G_2.2_bis, 
                                              titre = "2.2.bis. Variation hebdomadaire de l'estimation du taux de positivité des tests (non dédoublonnés) attribuable au variant UK à J-2",
                                              coefficient = 100,
                                              suffixe = "%",
                                              delta = T)
  carte_evo_pourcentage_tpv_uk
  
  G_2.3_bis = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "evo_pourcentage_tp_sabr")]
  carte_evo_pourcentage_tpv_sabr = cartographie(data = G_2.3_bis, 
                                                titre = "2.3.bis. Variation hebdomadaire de l'estimation du taux de positivité des tests (non dédoublonnés) attribuable au variant SA/BR à J-2",
                                                coefficient = 100,
                                                suffixe = "%",
                                                delta = T)
  carte_evo_pourcentage_tpv_sabr
  
  G_2.4_bis = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "evo_pourcentage_tp_non")]
  carte_evo_pourcentage_tpv_non = cartographie(data = G_2.4_bis, 
                                               titre = "2.4.bis. Variation hebdomadaire de l'estimation du taux de positivité des tests (non dédoublonnés) non attribuable à un variant à J-2",
                                               coefficient = 100,
                                               suffixe = "%",
                                               delta = T)
  carte_evo_pourcentage_tpv_non
  
  
  
  
  
  G_3.1 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "ti")]
  carte_ti = cartographie(data = G_3.1, 
                          titre = "3.1. Taux \"d'incidence\" des tests (non dédoublonnés) à J-2",
                          coefficient = 1e5,
                          suffixe = " pour 100 000 habitants")
  carte_ti
  
  G_3.2 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "ti_variant_uk")]
  carte_tiv_uk = cartographie(data = G_3.2, 
                              titre = "3.2. Estimation du taux \"d'incidence\" des tests (non dédoublonnés) attribuable au variant UK à J-2",
                              coefficient = 1e5,
                              suffixe = " pour 100 000 habitants")
  carte_tiv_uk
  
  G_3.3 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "ti_variant_sabr")]
  carte_tiv_sabr = cartographie(data = G_3.3, 
                                titre = "3.3. Estimation du taux \"d'incidence\" des tests (non dédoublonnés) attribuable au variant SA/BR à J-2",
                                coefficient = 1e5,
                                suffixe = " pour 100 000 habitants")
  carte_tiv_sabr
  
  G_3.4 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "ti_variant_non")]
  carte_tiv_non = cartographie(data = G_3.4, 
                               titre = "3.4. Estimation du taux \"d'incidence\" des tests (non dédoublonnés) non attribuable à un variant à J-2",
                               coefficient = 1e5,
                               suffixe = " pour 100 000 habitants")
  carte_tiv_non
  
  
  
  
  G_4.1 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "delta_ti")]
  carte_evo_ti = cartographie(data = G_4.1, 
                              titre = "4.1. Delta hebdomadaire du taux \"d'incidence\" des tests (non dédoublonnés) à J-2",
                              coefficient = 1e5,
                              suffixe = " pour 100 000 habitants",
                              delta = T)
  carte_evo_ti
  
  G_4.2 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "delta_ti_variant_uk")]
  carte_evo_tiv_uk = cartographie(data = G_4.2, 
                                  titre = "4.2. Delta hebdomadaire de l'estimation du taux \"d'incidence\" des tests (non dédoublonnés) attribuable au variant UK à J-2",
                                  coefficient = 1e5,
                                  suffixe = " pour 100 000 habitants",
                                  delta = T)
  carte_evo_tiv_uk
  
  G_4.3 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "delta_ti_variant_sabr")]
  carte_evo_tiv_sabr = cartographie(data = G_4.3, 
                                    titre = "4.3. Delta hebdomadaire de l'estimation du taux \"d'incidence\" des tests (non dédoublonnés) attribuable au variant SA/BR à J-2",
                                    coefficient = 1e5,
                                    suffixe = " pour 100 000 habitants",
                                    delta = T)
  carte_evo_tiv_sabr
  
  G_4.4 = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "delta_ti_variant_non")]
  carte_evo_tiv_non = cartographie(data = G_4.4, 
                                   titre = "4.4. Delta hebdomadaire de l'estimation du taux \"d'incidence\" des tests (non dédoublonnés) non attribuable à un variant à J-2",
                                   coefficient = 1e5,
                                   suffixe = " pour 100 000 habitants",
                                   delta = T)
  carte_evo_tiv_non
  
  
  
  
  G_4.1_bis = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "evo_pourcentage_ti")]
  carte_evo_pourcentage_ti = cartographie(data = G_4.1_bis, 
                                          titre = "4.1.bis. Variation hebdomadaire du taux d'\"incidence\" des tests (non dédoublonnés) à J-2",
                                          coefficient = 100,
                                          suffixe = "%",
                                          delta = T)
  carte_evo_pourcentage_ti
  
  G_4.2_bis = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "evo_pourcentage_ti_uk")]
  carte_evo_pourcentage_tiv_uk = cartographie(data = G_4.2_bis, 
                                              titre = "4.2.bis. Variation hebdomadaire de l'estimation du taux d'\"incidence\" des tests (non dédoublonnés) attribuable au variant UK à J-2",
                                              coefficient = 100,
                                              suffixe = "%",
                                              delta = T)
  carte_evo_pourcentage_tiv_uk
  
  G_4.3_bis = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "evo_pourcentage_ti_sabr")]
  carte_evo_pourcentage_tiv_sabr = cartographie(data = G_4.3_bis, 
                                                titre = "4.3.bis. Variation hebdomadaire de l'estimation du taux d'\"incidence\" des tests (non dédoublonnés) attribuable au variant SA/BR à J-2",
                                                coefficient = 100,
                                                suffixe = "%",
                                                delta = T)
  carte_evo_pourcentage_tiv_sabr
  
  G_4.4_bis = data_sidep[day_valid_init == Sys.Date() - 2, c("departement", "evo_pourcentage_ti_non")]
  carte_evo_pourcentage_tiv_non = cartographie(data = G_4.4_bis, 
                                               titre = "4.4.bis. Variation hebdomadaire de l'estimation du taux d'\"incidence\" des tests (non dédoublonnés) non attribuable à un variant à J-2",
                                               coefficient = 100,
                                               suffixe = "%",
                                               delta = T)
  carte_evo_pourcentage_tiv_non
  
  
  
  
  G_5.1 = data_sidep[day_valid_init == Sys.Date() - 3, c("departement", "part_variant")]
  carte_tpcriblage = cartographie(data = G_5.1, 
                                  titre = "5.1. Taux de tests positifs criblés anormaux (non dédoublonnés) à J-3",
                                  coefficient = 100,
                                  suffixe = " %")
  carte_tpcriblage
  
  G_5.2 = data_sidep[day_valid_init == Sys.Date() - 3, c("departement", "part_variant_uk")]
  carte_tpcriblage_uk = cartographie(data = G_5.2, 
                                     titre = "5.2. Taux de tests positifs criblés anormaux UK (non dédoublonnés) à J-3",
                                     coefficient = 100,
                                     suffixe = " %")
  carte_tpcriblage_uk
  
  G_5.3 = data_sidep[day_valid_init == Sys.Date() - 3, c("departement", "part_variant_sabr")]
  carte_tpcriblage_sabr = cartographie(data = G_5.3, 
                                       titre = "5.3. Taux de tests positifs criblés anormaux SA-BR (non dédoublonnés) à J-3",
                                       coefficient = 100,
                                       suffixe = " %")
  carte_tpcriblage_sabr
  
  
  
  
  
  G_6.1 = data_sidep[day_valid_init == Sys.Date() - 3, c("departement", "delta_part_variant")]
  carte_evo_tpcriblage = cartographie(data = G_6.1, 
                                      titre = "6.1. Delta hebdomadaire du taux de tests positifs criblés anormaux (non dédoublonnés) à J-3",
                                      coefficient = 100,
                                      suffixe = " point(s)",
                                      delta = T)
  carte_evo_tpcriblage
  
  G_6.2 = data_sidep[day_valid_init == Sys.Date() - 3, c("departement", "delta_part_variant_uk")]
  carte_evo_tpcriblage_uk = cartographie(data = G_6.2, 
                                         titre = "6.2. Delta hebdomadaire du taux de tests positifs criblés anormaux UK (non dédoublonnés) à J-3",
                                         coefficient = 100,
                                         suffixe = " point(s)",
                                         delta = T)
  carte_evo_tpcriblage_uk
  
  G_6.3 = data_sidep[day_valid_init == Sys.Date() - 3, c("departement", "delta_part_variant_sabr")]
  carte_evo_tpcriblage_sabr = cartographie(data = G_6.3, 
                                           titre = "6.3. Delta hebdomadaire du taux de tests positifs criblés anormaux SA-BR (non dédoublonnés) à J-3",
                                           coefficient = 100,
                                           suffixe = " point(s)",
                                           delta = T)
  carte_evo_tpcriblage_sabr
  
  
  
  
  G_7.1 = data_sidep[day_valid_init == Sys.Date() - 3, c("departement", "part_cribles")]
  carte_partcriblage = cartographie(data = G_7.1, 
                                    titre = "7.1. Part de tests criblés parmi les tests positifs (non dédoublonnés) à J-3",
                                    coefficient = 100,
                                    suffixe = " %")
  carte_partcriblage
  
  G_7.2 = data_sidep[day_valid_init == Sys.Date() - 3, c("departement", "delta_part_cribles")]
  carte_evo_partcriblage = cartographie(data = G_7.2, 
                                        titre = "7.2. Delta hebdomadaire de la part de tests criblés parmi les tests positifs (non dédoublonnés) à J-3",
                                        coefficient = 100,
                                        suffixe = " point(s)",
                                        delta = T)
  carte_evo_partcriblage
  
  
  rmarkdown::render(input = "src/variants/Cartes variants.Rmd",
                    output_file = paste0("data/sorties/variants/", Sys.Date(), "_cartes_variants.html"))
  
  
  
  TABLE_FINALE = G_1.1[, c("departement")]
  table_des_dep = fread("data/utils/departement2019.csv", encoding = "UTF-8")
  TABLE_FINALE = merge(TABLE_FINALE, 
                       table_des_dep[, c("dep", "libelle")],
                       by.x = "departement",
                       by.y = "dep")
  
  for (nom_table in c("G_1.1", "G_1.2", "G_1.3", "G_1.4", 
                      "G_2.1", "G_2.2", "G_2.3", "G_2.4", 
                      "G_2.1_bis", "G_2.2_bis", "G_2.3_bis", "G_2.4_bis", 
                      "G_3.1", "G_3.2", "G_3.3", "G_3.4", 
                      "G_4.1", "G_4.2", "G_4.3", "G_4.4", 
                      "G_4.1_bis", "G_4.2_bis", "G_4.3_bis", "G_4.4_bis", 
                      "G_5.1", "G_5.2", "G_5.3", 
                      "G_6.1", "G_6.2", "G_6.3", 
                      "G_7.1", "G_7.2")){
    
    t = eval(parse(text = nom_table))
    names(t)[2] = nom_table
    TABLE_FINALE =  merge(TABLE_FINALE, 
                          t,
                          by = "departement",
                          all = T)
    
  }
  
  TABLE_FINALE[, `:=`(G_1.1 = 100 * G_1.1,
                      G_1.2 = 100 * G_1.2,
                      G_1.3 = 100 * G_1.3,
                      G_1.4 = 100 * G_1.4,
                      G_2.1 = 100 * G_2.1,
                      G_2.2 = 100 * G_2.2,
                      G_2.3 = 100 * G_2.3,
                      G_2.4 = 100 * G_2.4,
                      G_2.1_bis = 100 * G_2.1_bis,
                      G_2.2_bis = 100 * G_2.2_bis,
                      G_2.3_bis = 100 * G_2.3_bis,
                      G_2.4_bis = 100 * G_2.4_bis,
                      G_3.1 = 1e5 * G_3.1,
                      G_3.2 = 1e5 * G_3.2,
                      G_3.3 = 1e5 * G_3.3,
                      G_3.4 = 1e5 * G_3.4,
                      G_4.1 = 1e5 * G_4.1,
                      G_4.2 = 1e5 * G_4.2,
                      G_4.3 = 1e5 * G_4.3,
                      G_4.4 = 1e5 * G_4.4,
                      G_4.1_bis = 100 * G_4.1_bis,
                      G_4.2_bis = 100 * G_4.2_bis,
                      G_4.3_bis = 100 * G_4.3_bis,
                      G_4.4_bis = 100 * G_4.4_bis,
                      G_5.1 = 100 * G_5.1,
                      G_5.2 = 100 * G_5.2,
                      G_5.3 = 100 * G_5.3,
                      G_6.1 = 100 * G_6.1,
                      G_6.2 = 100 * G_6.2,
                      G_6.3 = 100 * G_6.3,
                      G_7.1 = 100 * G_7.1,
                      G_7.2 = 100 * G_7.2)]
  
  wb <- createWorkbook()
  addWorksheet(wb, "Données cartes")
  writeData(wb, "Données cartes", TABLE_FINALE)
  addStyle(wb, 
           "Données cartes",
           style = createStyle(fgFill = "black",
                               fontName = "Times New Roman", 
                               halign = "center",
                               fontColour = "white"), 
           rows = 1, 
           cols = 1:dim(TABLE_FINALE)[2],
           gridExpand = T)
  addStyle(wb, 
           "Données cartes",
           style = createStyle(fgFill = rgb(150/255, 150/255, 150/255),
                               fontName = "Times New Roman"), 
           rows = (1:dim(t)[1]) + 1, 
           cols = which(names(TABLE_FINALE) %in% c("departement", "libelle")),
           gridExpand = T)
  addStyle(wb, 
           "Données cartes",
           style = createStyle(fgFill = "white",
                               fontName = "Times New Roman",
                               halign = "center"), 
           rows = (1:dim(t)[1]) + 1, 
           cols = which(!(names(TABLE_FINALE) %in% c("departement", "libelle"))),
           gridExpand = T)
  
  saveWorkbook(wb, paste0("data/sorties/variants/", Sys.Date(), " - données carte.xlsx"), overwrite = T)
  
  
  
  
  
}
print("variants ZIP")
print(gc(verbose = T))
path_fabrice = paste0(path, "data/sorties/dossier_quotidien_fabrice/")
if (!dir.exists(paste0(path_fabrice, Sys.Date()))) dir.create(paste0(path_fabrice, Sys.Date()))
path_fabrice_du_jour = paste0(path_fabrice, Sys.Date(), "/")
file.copy(from = paste0(path, "data/sorties/variants/", gsub("-", "", Sys.Date()), " table_pcr_variants - avec graphiques.xlsx"),
          to = paste0(path_fabrice, gsub("-", "", Sys.Date()), " table_pcr_variants - avec graphiques.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/variants/", Sys.Date(), " - decomposition_tp_par_departement.pdf"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - decomposition_tp_par_departement.pdf"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/variants/", Sys.Date(), " - decomposition_tp_par_region.pdf"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - decomposition_tp_par_region.pdf"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/variants/", Sys.Date(), " - graphique_decomp_tp_fra_hebdo.jpg"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - graphique_decomp_tp_fra_hebdo.jpg"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/variants/", Sys.Date(), " - table_pour_carto.csv"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - table_pour_carto.csv"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/variants/", Sys.Date(), "_cartes_variants.html"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_cartes_variants.html"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/variants/", Sys.Date(), " - données carte.xlsx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - données carte.xlsx"),
          overwrite = T)
zip::zipr(zipfile = paste0(path_fabrice, Sys.Date(), '_export_quotidien_variants.zip'),
          files = paste0(path_fabrice_du_jour, dir(path_fabrice_du_jour)))
file.remove(paste0(path_fabrice_du_jour, dir(path_fabrice_du_jour)))
file.remove(paste0(path_fabrice, Sys.Date()))
print(gc(verbose = T))
rm(list = ls())
gc()
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
print("Heatmaps")
library(data.table)
library(ggplot2)
library(openxlsx)
library(officer)
library(flextable)
library(dplyr)
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
somme_7j <- function(x) {
  res <- ifelse(is.na(shift(x, 6)), NA, x + shift(x, 1) + shift(x, 2) + shift(x, 3) + 
                  shift(x, 4) + shift(x, 5) + shift(x, 6))
  return(res)
}
Creer_une_boite <- function(texte){
  aux = data.frame(matrix(NA, nrow = 0, ncol = 1)) 
  names(aux) = texte
  return(aux)
}
DATE_DEPART = "2021-01-16" # La date de départ pour les heatmaps
SEUIL_TP = 0.05
SEUIL_TI = 200
DATE_FIXE = "2021-01-16"
DATE_DREES = gsub("-", "", Sys.Date())
chemin_sorties_quot = paste0(path, "data/sorties/nb_tests/") 
chemin_utils_sidep = paste0(path, "data/utils/") 
chemin_sortie_heatmaps = paste0(path, "data/sorties/heatmaps/") 
data_sidep_reg_drees_pcr = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_valid.xlsx"),
                                           sheet = "regions",
                                           detectDates = T))
data_sidep_dep_drees_pcr = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_valid.xlsx"),
                                           sheet = "departements",
                                           detectDates = T))
data_sidep_reg_drees_tag = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_antigenique_valid.xlsx"),
                                           sheet = "regions",
                                           detectDates = T))
data_sidep_dep_drees_tag = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_antigenique_valid.xlsx"),
                                           sheet = "departements",
                                           detectDates = T))
data_sidep_reg_drees_pcr_sal_mil = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_sal_mil_valid.xlsx"),
                                           sheet = "regions",
                                           detectDates = T))
data_sidep_dep_drees_pcr_sal_mil = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_sal_mil_valid.xlsx"),
                                           sheet = "departements",
                                           detectDates = T))
data_sidep_reg_drees = rbind(data_sidep_reg_drees_pcr[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")],
                             data_sidep_reg_drees_tag[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")],
                             data_sidep_reg_drees_pcr_sal_mil[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")])
data_sidep_reg_drees = data_sidep_reg_drees[, .(nb_pos = sum(nb_pos),
                                                nb_tot_PNIX = sum(nb_tot_PNIX)),
                                            by = c("date", "nom_reg")]
data_sidep_dep_drees = rbind(data_sidep_dep_drees_pcr[, c("date", "departement", "nb_pos", "nb_tot_PNIX")],
                             data_sidep_dep_drees_tag[, c("date", "departement", "nb_pos", "nb_tot_PNIX")],
                             data_sidep_dep_drees_pcr_sal_mil[, c("date", "departement", "nb_pos", "nb_tot_PNIX")])
data_sidep_dep_drees = data_sidep_dep_drees[, .(nb_pos = sum(nb_pos),
                                                nb_tot_PNIX = sum(nb_tot_PNIX)),
                                            by = c("date", "departement")]
libelle_reg = fread(paste0(chemin_utils_sidep, "region2019.csv"), encoding = "UTF-8")
libelle_reg[, reg := as.character(reg)]
libelle_dep = fread(paste0(chemin_utils_sidep, "departement2019.csv"), encoding = "UTF-8")
population_dep = fread(paste0(path, "/data/utils/pop_dep_spf.csv"))
population_dep[, dep := ifelse(nchar(dep) == 1, paste0("0", dep), dep)]
population_reg = fread(paste0(path, "/data/utils/pop_reg_spf.csv"))
setorder(data_sidep_reg_drees, nom_reg, date)
data_sidep_reg_drees = data_sidep_reg_drees[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")]
data_sidep_reg_drees[, nb_pos_7j := somme_7j(nb_pos), by = "nom_reg"]
data_sidep_reg_drees[, nb_tot_PNIX_7j := somme_7j(nb_tot_PNIX), by = "nom_reg"]
population_reg[nom_region == "Ile-de-France", nom_region := "Île-de-France"]
population_reg[nom_region == "Centre-Val de Loire", nom_region := "Centre Val-de-Loire"]
population_reg[nom_region == "Grand Est", nom_region := "Grand-Est"]
population_reg[nom_region == "Nouvelle-Aquitaine", nom_region := "Nouvelle Aquitaine"]
data_sidep_reg_drees = merge(data_sidep_reg_drees,
      population_reg,
      by.x = "nom_reg",
      by.y = "nom_region")
data_sidep_reg_drees[, ti := nb_pos_7j / pop * 1e5, by = "nom_reg"]
setorder(data_sidep_reg_drees, nom_reg, date)
data_sidep_reg_drees[, evo_ti_1 := ti / shift(ti, 1) - 1, by = "nom_reg"]
data_sidep_reg_drees[, evo_ti_7 := ti / shift(ti, 7) - 1, by = "nom_reg"]
data_sidep_reg_drees[, var_ti_7 := ti - shift(ti, 7), by = "nom_reg"]
for (REGION in unique(data_sidep_reg_drees$nom_reg)){
  val_fixe = as.numeric(data_sidep_reg_drees[date == DATE_FIXE & nom_reg == REGION, "ti"])
  data_sidep_reg_drees[nom_reg == REGION, evo_ti_fixe := ti / val_fixe - 1, by = "nom_reg"]
  data_sidep_reg_drees[nom_reg == REGION, var_ti_fixe := ti - val_fixe, by = "nom_reg"]
}
setorder(data_sidep_dep_drees, departement, date)
data_sidep_dep_drees = data_sidep_dep_drees[, c("date", "departement", "nb_pos", "nb_tot_PNIX")]
data_sidep_dep_drees[, nb_pos_7j := somme_7j(nb_pos), by = "departement"]
data_sidep_dep_drees[, nb_tot_PNIX_7j := somme_7j(nb_tot_PNIX), by = "departement"]
data_sidep_dep_drees = merge(data_sidep_dep_drees,
                             population_dep,
                             by.x = "departement",
                             by.y = "dep")
data_sidep_dep_drees[, ti := nb_pos_7j / pop * 1e5, by = "departement"]
setorder(data_sidep_dep_drees, departement, date)
data_sidep_dep_drees[, evo_ti_1 := ti / shift(ti, 1) - 1, by = "departement"]
data_sidep_dep_drees[, evo_ti_7 := ti / shift(ti, 7) - 1, by = "departement"]
data_sidep_dep_drees[, var_ti_7 := ti - shift(ti, 7), by = "departement"]
for (DEPARTEMENT in unique(data_sidep_dep_drees$departement)){
  val_fixe = as.numeric(data_sidep_dep_drees[date == DATE_FIXE & departement == DEPARTEMENT, "ti"])
  data_sidep_dep_drees[departement == DEPARTEMENT, evo_ti_fixe := ti / val_fixe - 1, by = "departement"]
  data_sidep_dep_drees[departement == DEPARTEMENT, var_ti_fixe := ti - val_fixe, by = "departement"]
}
data_sidep_dep_drees = merge(data_sidep_dep_drees, 
                             libelle_dep, 
                             by.x = "departement",
                             by.y = "dep")
aux = libelle_reg[, c("reg", "libelle")]
aux[, libelle_reg := libelle]
aux[, libelle := NULL]
aux[, reg := as.numeric(reg)]
data_sidep_dep_drees = merge(data_sidep_dep_drees, 
                             aux, by = "reg")
setorder(data_sidep_dep_drees, libelle)
tg = data_sidep_reg_drees[date >= DATE_DEPART & 
                            nom_reg != "Région inconnue", c("date", "nom_reg", "ti", "evo_ti_7",
                                                            "var_ti_7", "evo_ti_fixe", "var_ti_fixe")]
class = tg[date == max(date)]
setorder(class, ti)
tg[, nom_reg := factor(nom_reg, class$nom_reg)]
G_france_ti_niv = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = ti)) + 
  scale_fill_distiller(palette = RColorBrewer::brewer.pal(9, "YlGn"),
                       direction = 1) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Date de validation") +
  coord_fixed() +
  geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$evo_ti_7), 0.1, floor)
max = plyr::round_any(max(tg$evo_ti_7), 0.1, ceiling)
borne = max(abs(min), max)
G_france_ti_var7 = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = evo_ti_7)) + 
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white", 
                       midpoint = 0,
                       limit = c(-borne, borne)) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Date de validation") +
  coord_fixed() +
  geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$var_ti_7), 0.1, floor)
max = plyr::round_any(max(tg$var_ti_7), 0.1, ceiling)
borne = max(abs(min), max)
G_france_ti_var7_delta = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = var_ti_7)) + 
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white", 
                       midpoint = 0,
                       limit = c(-borne, borne)) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$evo_ti_fixe), 0.1, floor)
max = plyr::round_any(max(tg$evo_ti_fixe), 0.1, ceiling)
borne = max(abs(min), max)
G_france_ti_varfixe = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = evo_ti_fixe)) + 
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white", 
                       midpoint = 0,
                       limit = c(-borne, borne)) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$var_ti_fixe), 0.1, floor)
max = plyr::round_any(max(tg$var_ti_fixe), 0.1, ceiling)
borne = max(abs(min), max)
G_france_ti_varfixe_delta = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = var_ti_fixe)) + 
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white", 
                       midpoint = 0,
                       limit = c(-borne, borne)) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
             size = .2)
fichier_niv = file.path(tempdir(), "niv.png")
ggsave(plot = G_france_ti_niv, 
       filename = fichier_niv, width = 5.8, height = 6)
fichier_evo_7 = file.path(tempdir(), "evo_7.png")
ggsave(plot = G_france_ti_var7, 
       filename = fichier_evo_7, width = 5.8, height = 6)
fichier_var_7 = file.path(tempdir(), "var_7.png")
ggsave(plot = G_france_ti_var7_delta, 
       filename = fichier_var_7, width = 5.8, height = 6)
fichier_evo_fixe = file.path(tempdir(), "evo_fixe.png")
ggsave(plot = G_france_ti_varfixe, 
       filename = fichier_evo_fixe, width = 5.8, height = 6)
fichier_var_fixe = file.path(tempdir(), "var_fixe.png")
ggsave(plot = G_france_ti_varfixe_delta, 
       filename = fichier_var_fixe, width = 5.8, height = 6)
PPT_EVO = read_pptx() %>% 
  add_slide(layout = "Title Slide", master = "Office Theme") %>% 
  ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>% 
  ph_with(value = external_img(src = fichier_evo_7, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>% 
  ph_with(value = external_img(src = fichier_evo_fixe, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>% 
  
  ph_with(value = Creer_une_boite(toupper("France")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 10) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 0.5, left = 0)) %>%
  ph_with(value = Creer_une_boite("Niveau du \"taux d'incidence\" des tests") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 1/6)) %>% 
  ph_with(value = Creer_une_boite("Glissement hebdomadaire du \"taux d'incidence\" des tests") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 3.5 )) %>% 
  ph_with(value = Creer_une_boite(paste0("Variation du \"taux d'incidence\" des tests par rapport au ", DATE_FIXE)) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 6 + 5/6)) %>% 
  ph_with(value = Creer_une_boite(paste0("Les régions au dessus de la barre horizontale ont un \"taux d'incidence\" supérieur ou égal à ", SEUIL_TI, " pour 100 000 habitants. Les régions sont classées par \"taux d'incidence\" décroissant.")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>% 
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6, left = 1)) %>% 
  ph_with(value = Creer_une_boite(paste0("Attention, le \"taux d'incidence\" présenté ici correspond au cumul de tests positifs sur 7 jours rapporté au nombre d'habitants, en date de validation. Il n'est pas totalement comparable aux taux d'incidence officiel, calculé par Santé Publique France, qui comptabilise les patients en date de prélèvement et est de meilleure qualité en termes épidémiologiques.")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            bold(part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>% 
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6.6, left = 1)) 
PPT_DELTA = read_pptx() %>% 
  add_slide(layout = "Title Slide", master = "Office Theme") %>% 
  ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>% 
  ph_with(value = external_img(src = fichier_var_7, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>% 
  ph_with(value = external_img(src = fichier_var_fixe, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>% 
  
  ph_with(value = Creer_une_boite(toupper("France")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 10) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 0.5, left = 0)) %>%
  ph_with(value = Creer_une_boite("Niveau du \"taux d'incidence\" des tests") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 1/6)) %>% 
  ph_with(value = Creer_une_boite("Delta hebdomadaire du \"taux d'incidence\" des tests") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 3.5 )) %>% 
  ph_with(value = Creer_une_boite(paste0("Delta du \"taux d'incidence\" des tests par rapport au ", DATE_FIXE)) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 6 + 5/6)) %>% 
  ph_with(value = Creer_une_boite(paste0("Les régions au dessus de la barre horizontale ont un \"taux d'incidence\" supérieur ou égal à ", SEUIL_TI, " pour 100 000 habitants. Les régions sont classées par \"taux d'incidence\" décroissant.")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>% 
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6, left = 1)) %>% 
  ph_with(value = Creer_une_boite(paste0("Attention, le \"taux d'incidence\" présenté ici correspond au cumul de tests positifs sur 7 jours rapporté au nombre d'habitants, en date de validation. Il n'est pas totalement comparable aux taux d'incidence officiel, calculé par Santé Publique France, qui comptabilise les patients en date de prélèvement et est de meilleure qualité en termes épidémiologiques.")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            bold(part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>% 
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6.6, left = 1)) 
for (LIB_REG in unique(data_sidep_dep_drees$libelle_reg)){
  print(LIB_REG)
  tg = data_sidep_dep_drees[date >= DATE_DEPART & 
                              libelle_reg == LIB_REG, 
                            c("date", "libelle", "ti", "evo_ti_7",
                              "var_ti_7",
                              "evo_ti_fixe",
                              "var_ti_fixe")]
  
  class = tg[date == max(date)]
  setorder(class, ti)
  
  tg[, libelle := factor(libelle, class$libelle)]
  
  G_reg_ti_niv = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = ti)) + 
    scale_fill_distiller(palette = RColorBrewer::brewer.pal(9, "YlGn"),
                         direction = 1) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() 
  
  if(any(class$ti >= SEUIL_TI)){
    G_reg_ti_niv = G_reg_ti_niv + 
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
                 size = .2)
  }
  
  min = plyr::round_any(min(tg$evo_ti_7), 0.1, floor)
  max = plyr::round_any(max(tg$evo_ti_7), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_ti_var7 = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = evo_ti_7)) + 
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white", 
                         midpoint = 0,
                         limit = c(-borne, borne)) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() + 
    xlab("Date de validation") 
  
  if(any(class$ti >= SEUIL_TI)){
    G_reg_ti_var7 = G_reg_ti_var7 + 
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
                 size = .2)
  }
  
  
  min = plyr::round_any(min(tg$var_ti_7), 0.1, floor)
  max = plyr::round_any(max(tg$var_ti_7), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_ti_var7_delta = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = var_ti_7)) + 
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white", 
                         midpoint = 0,
                         limit = c(-borne, borne)) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() + 
    xlab("Date de validation") 
  
  if(any(class$ti >= SEUIL_TI)){
    G_reg_ti_var7_delta = G_reg_ti_var7_delta + 
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
                 size = .2)
  }
  
  min = plyr::round_any(min(tg$evo_ti_fixe), 0.1, floor)
  max = plyr::round_any(max(tg$evo_ti_fixe), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_ti_varfixe = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = evo_ti_fixe)) + 
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white", 
                         midpoint = 0,
                         limit = c(-borne, borne)) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() + 
    xlab("Date de validation") 
  
  if(any(class$ti >= SEUIL_TI)){
    G_reg_ti_varfixe = G_reg_ti_varfixe + 
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
                 size = .2)
  }
  
  
  min = plyr::round_any(min(tg$var_ti_fixe), 0.1, floor)
  max = plyr::round_any(max(tg$var_ti_fixe), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_ti_varfixe_delta = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = var_ti_fixe)) + 
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white", 
                         midpoint = 0,
                         limit = c(-borne, borne)) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() + 
    xlab("Date de validation") 
  
  if(any(class$ti >= SEUIL_TI)){
    G_reg_ti_varfixe_delta = G_reg_ti_varfixe_delta + 
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$ti >= SEUIL_TI)) - 0.5,
                 size = .2)
  }
  
  
  
  fichier_niv = file.path(tempdir(), "niv.png")
  ggsave(plot = G_reg_ti_niv, 
         filename = fichier_niv, width = 5.8, height = 6)
  fichier_evo_7 = file.path(tempdir(), "evo_7.png")
  ggsave(plot = G_reg_ti_var7, 
         filename = fichier_evo_7, width = 5.8, height = 6)
  fichier_var_7 = file.path(tempdir(), "var_7.png")
  ggsave(plot = G_reg_ti_var7_delta, 
         filename = fichier_var_7, width = 5.8, height = 6)
  fichier_evo_fixe = file.path(tempdir(), "evo_fixe.png")
  ggsave(plot = G_reg_ti_varfixe, 
         filename = fichier_evo_fixe, width = 5.8, height = 6)
  fichier_var_fixe = file.path(tempdir(), "var_fixe.png")
  ggsave(plot = G_reg_ti_varfixe_delta, 
         filename = fichier_var_fixe, width = 5.8, height = 6)
  
  
  PPT_EVO = PPT_EVO %>% 
    add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    
    ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>% 
    ph_with(value = external_img(src = fichier_evo_7, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>% 
    ph_with(value = external_img(src = fichier_evo_fixe, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>% 
    
    ph_with(value = Creer_une_boite(toupper(LIB_REG)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 10) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 0.5, left = 0)) %>%
    ph_with(value = Creer_une_boite("Niveau du \"taux d'incidence\" des tests") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 1/6)) %>% 
    ph_with(value = Creer_une_boite("Glissement hebdomadaire du \"taux d'incidence\" des tests") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 3.5 )) %>% 
    ph_with(value = Creer_une_boite(paste0("Variation du \"taux d'incidence\" des tests par rapport au ", DATE_FIXE)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 6 + 5/6)) %>%
    
    ph_with(value = Creer_une_boite(paste0("Les départements au dessus de la barre horizontale (si elle existe) ont un \"taux d'incidence\" supérieur ou égal à ", SEUIL_TI, " pour 100 000 habitants. Les départements sont classées par \"taux d'incidence\" décroissant.")) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>% 
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6, left = 1)) 
  
  PPT_DELTA = PPT_DELTA %>% 
    add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    
    ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>% 
    ph_with(value = external_img(src = fichier_var_7, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>% 
    ph_with(value = external_img(src = fichier_var_fixe, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>% 
    
    ph_with(value = Creer_une_boite(toupper(LIB_REG)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 10) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 0.5, left = 0)) %>%
    ph_with(value = Creer_une_boite("Niveau du \"taux d'incidence\" des tests") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 1/6)) %>% 
    ph_with(value = Creer_une_boite("Delta hebdomadaire du \"taux d'incidence\" des tests") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 3.5 )) %>% 
    ph_with(value = Creer_une_boite(paste0("Delta du \"taux d'incidence\" des tests par rapport au ", DATE_FIXE)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 6 + 5/6)) %>%
    
    ph_with(value = Creer_une_boite(paste0("Les départements au dessus de la barre horizontale (si elle existe) ont un \"taux d'incidence\" supérieur ou égal à ", SEUIL_TI, " pour 100 000 habitants. Les départements sont classées par \"taux d'incidence\" décroissant.")) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>% 
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6, left = 1)) 
  
}
path_ppt_evo = paste0(chemin_sortie_heatmaps, Sys.Date()," - heatmap ti (donnees drees) - evolution.pptx")
if(file.exists(path_ppt_evo))file.remove(path_ppt_delta)
print(PPT_EVO,target = path_ppt_evo)
path_ppt_delta = paste0(chemin_sortie_heatmaps, Sys.Date()," - heatmap ti (donnees drees) - delta.pptx")
if(file.exists(path_ppt_delta))file.remove(path_ppt_delta)
print(PPT_DELTA,target = path_ppt_delta)
write.csv(data_sidep_dep_drees, paste0(chemin_sortie_heatmaps, Sys.Date(), " - data_dep_ti.csv"))
write.csv(data_sidep_reg_drees, paste0(chemin_sortie_heatmaps, Sys.Date(), " - data_reg_ti.csv"))
print(gc(verbose = T))
library(data.table)
library(ggplot2)
library(openxlsx)
library(officer)
library(flextable)
library(dplyr)
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
somme_7j <- function(x) {
  res <- ifelse(is.na(shift(x, 6)), NA, x + shift(x, 1) + shift(x, 2) + shift(x, 3) + 
                  shift(x, 4) + shift(x, 5) + shift(x, 6))
  return(res)
}
Creer_une_boite <- function(texte){
  aux = data.frame(matrix(NA, nrow = 0, ncol = 1)) 
  names(aux) = texte
  return(aux)
}
DATE_DEPART = "2021-01-16" # La date de départ pour les heatmaps
SEUIL_TP = 0.05
SEUIL_TI = 200
DATE_FIXE = "2021-01-16"
DATE_DREES = gsub("-", "", Sys.Date())
chemin_sorties_quot = paste0(path, "data/sorties/nb_tests/") 
chemin_utils_sidep = paste0(path, "data/utils/") 
chemin_sortie_heatmaps = paste0(path, "data/sorties/heatmaps/") 
data_sidep_reg_drees_pcr = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_valid.xlsx"),
                                           sheet = "regions",
                                           detectDates = T))
data_sidep_dep_drees_pcr = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_valid.xlsx"),
                                           sheet = "departements",
                                           detectDates = T))
data_sidep_reg_drees_tag = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_antigenique_valid.xlsx"),
                                           sheet = "regions",
                                           detectDates = T))
data_sidep_dep_drees_tag = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_antigenique_valid.xlsx"),
                                           sheet = "departements",
                                           detectDates = T))
data_sidep_reg_drees_pcr_sal_mil = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_sal_mil_valid.xlsx"),
                                           sheet = "regions",
                                           detectDates = T))
data_sidep_dep_drees_pcr_sal_mil = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_sal_mil_valid.xlsx"),
                                           sheet = "departements",
                                           detectDates = T))
data_sidep_reg_drees = rbind(data_sidep_reg_drees_pcr[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")],
                             data_sidep_reg_drees_tag[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")],
                             data_sidep_reg_drees_pcr_sal_mil[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")])
data_sidep_reg_drees = data_sidep_reg_drees[, .(nb_pos = sum(nb_pos),
                                                nb_tot_PNIX = sum(nb_tot_PNIX)),
                                            by = c("date", "nom_reg")]
data_sidep_dep_drees = rbind(data_sidep_dep_drees_pcr[, c("date", "departement", "nb_pos", "nb_tot_PNIX")],
                             data_sidep_dep_drees_tag[, c("date", "departement", "nb_pos", "nb_tot_PNIX")],
                             data_sidep_dep_drees_pcr_sal_mil[, c("date", "departement", "nb_pos", "nb_tot_PNIX")])
data_sidep_dep_drees = data_sidep_dep_drees[, .(nb_pos = sum(nb_pos),
                                                nb_tot_PNIX = sum(nb_tot_PNIX)),
                                            by = c("date", "departement")]
libelle_reg = fread(paste0(chemin_utils_sidep, "region2019.csv"), encoding = "UTF-8")
libelle_reg[, reg := as.character(reg)]
libelle_dep = fread(paste0(chemin_utils_sidep, "departement2019.csv"), encoding = "UTF-8")
setorder(data_sidep_reg_drees, nom_reg, date)
data_sidep_reg_drees = data_sidep_reg_drees[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")]
data_sidep_reg_drees[, nb_pos_7j := somme_7j(nb_pos), by = "nom_reg"]
data_sidep_reg_drees[, nb_tot_PNIX_7j := somme_7j(nb_tot_PNIX), by = "nom_reg"]
data_sidep_reg_drees[, tp := nb_pos_7j / nb_tot_PNIX_7j, by = "nom_reg"]
setorder(data_sidep_reg_drees, nom_reg, date)
data_sidep_reg_drees[, evo_tp_1 := tp / shift(tp, 1) - 1, by = "nom_reg"]
data_sidep_reg_drees[, evo_tp_7 := tp / shift(tp, 7) - 1, by = "nom_reg"]
data_sidep_reg_drees[, var_tp_7 := tp - shift(tp, 7), by = "nom_reg"]
for (REGION in unique(data_sidep_reg_drees$nom_reg)){
  val_fixe = as.numeric(data_sidep_reg_drees[date == DATE_FIXE & nom_reg == REGION, "tp"])
  data_sidep_reg_drees[nom_reg == REGION, evo_tp_fixe := tp / val_fixe - 1, by = "nom_reg"]
  data_sidep_reg_drees[nom_reg == REGION, var_tp_fixe := tp - val_fixe, by = "nom_reg"]
}
setorder(data_sidep_dep_drees, departement, date)
data_sidep_dep_drees = data_sidep_dep_drees[, c("date", "departement", "nb_pos", "nb_tot_PNIX")]
data_sidep_dep_drees[, nb_pos_7j := somme_7j(nb_pos), by = "departement"]
data_sidep_dep_drees[, nb_tot_PNIX_7j := somme_7j(nb_tot_PNIX), by = "departement"]
data_sidep_dep_drees[, tp := nb_pos_7j / nb_tot_PNIX_7j, by = "departement"]
setorder(data_sidep_dep_drees, departement, date)
data_sidep_dep_drees[, evo_tp_1 := tp / shift(tp, 1) - 1, by = "departement"]
data_sidep_dep_drees[, evo_tp_7 := tp / shift(tp, 7) - 1, by = "departement"]
data_sidep_dep_drees[, var_tp_7 := tp - shift(tp, 7), by = "departement"]
for (DEPARTEMENT in unique(data_sidep_dep_drees$departement)){
  val_fixe = as.numeric(data_sidep_dep_drees[date == DATE_FIXE & departement == DEPARTEMENT, "tp"])
  data_sidep_dep_drees[departement == DEPARTEMENT, evo_tp_fixe := tp / val_fixe - 1, by = "departement"]
  data_sidep_dep_drees[departement == DEPARTEMENT, var_tp_fixe := tp - val_fixe, by = "departement"]
}
data_sidep_dep_drees = merge(data_sidep_dep_drees, 
                             libelle_dep, 
                             by.x = "departement",
                             by.y = "dep")
aux = libelle_reg[, c("reg", "libelle")]
aux[, libelle_reg := libelle]
aux[, libelle := NULL]
aux[, reg := as.numeric(reg)]
data_sidep_dep_drees = merge(data_sidep_dep_drees, 
                             aux, by = "reg")
setorder(data_sidep_dep_drees, libelle)
tg = data_sidep_reg_drees[date >= DATE_DEPART & 
                            nom_reg != "Région inconnue", c("date", "nom_reg", "tp", "evo_tp_7",
                                                            "var_tp_7", "evo_tp_fixe", "var_tp_fixe")]
class = tg[date == max(date)]
setorder(class, tp)
tg[, nom_reg := factor(nom_reg, class$nom_reg)]
G_france_tp_niv = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = tp)) + 
  scale_fill_distiller(palette = RColorBrewer::brewer.pal(9, "YlGn"),
                       direction = 1) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Date de validation") +
  coord_fixed() +
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$evo_tp_7), 0.1, floor)
max = plyr::round_any(max(tg$evo_tp_7), 0.1, ceiling)
borne = max(abs(min), max)
G_france_tp_var7 = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = evo_tp_7)) + 
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white", 
                       midpoint = 0,
                       limit = c(-borne, borne)) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$var_tp_7), 0.1, floor)
max = plyr::round_any(max(tg$var_tp_7), 0.1, ceiling)
borne = max(abs(min), max)
G_france_tp_var7_delta = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = var_tp_7)) + 
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white", 
                       midpoint = 0,
                       limit = c(-borne, borne)) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
 xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$evo_tp_fixe), 0.1, floor)
max = plyr::round_any(max(tg$evo_tp_fixe), 0.1, ceiling)
borne = max(abs(min), max)
G_france_tp_varfixe = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = evo_tp_fixe)) + 
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white", 
                       midpoint = 0,
                       limit = c(-borne, borne)) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
   xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$var_tp_fixe), 0.1, floor)
max = plyr::round_any(max(tg$var_tp_fixe), 0.1, ceiling)
borne = max(abs(min), max)
G_france_tp_varfixe_delta = ggplot() + 
  geom_tile(data = tg, 
            aes(date, nom_reg, fill = var_tp_fixe)) + 
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white", 
                       midpoint = 0,
                       limit = c(-borne, borne)) + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
fichier_niv = file.path(tempdir(), "niv.png")
ggsave(plot = G_france_tp_niv, 
       filename = fichier_niv, width = 5.8, height = 6)
fichier_evo_7 = file.path(tempdir(), "evo_7.png")
ggsave(plot = G_france_tp_var7, 
       filename = fichier_evo_7, width = 5.8, height = 6)
fichier_var_7 = file.path(tempdir(), "var_7.png")
ggsave(plot = G_france_tp_var7_delta, 
       filename = fichier_var_7, width = 5.8, height = 6)
fichier_evo_fixe = file.path(tempdir(), "evo_fixe.png")
ggsave(plot = G_france_tp_varfixe, 
       filename = fichier_evo_fixe, width = 5.8, height = 6)
fichier_var_fixe = file.path(tempdir(), "var_fixe.png")
ggsave(plot = G_france_tp_varfixe_delta, 
       filename = fichier_var_fixe, width = 5.8, height = 6)
PPT_EVO = read_pptx() %>% 
  add_slide(layout = "Title Slide", master = "Office Theme") %>% 
  ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>% 
  ph_with(value = external_img(src = fichier_evo_7, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>% 
  ph_with(value = external_img(src = fichier_evo_fixe, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>% 
  
  ph_with(value = Creer_une_boite(toupper("France")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 10) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 0.5, left = 0)) %>%
  ph_with(value = Creer_une_boite("Niveau du taux de positivité des tests") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 1/6)) %>% 
  ph_with(value = Creer_une_boite("Glissement hebdomadaire du taux de positivité des tests") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 3.5 )) %>% 
  ph_with(value = Creer_une_boite(paste0("Variation du taux de positivité des tests par rapport au ", DATE_FIXE)) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 6 + 5/6)) %>% 
  ph_with(value = Creer_une_boite("Source : Si-Dep ; traitements : Drees ; taux de positivité des tests (sans dédoublonnage).") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>% 
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6.3, left = 1)) %>% 
  ph_with(value = Creer_une_boite(paste0("Les régions au dessus de la barre horizontale ont un taux de positivité supérieur ou égal à ", 100 * SEUIL_TP, " %. Les régions sont classées par taux de positivité décroissant.")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>% 
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6, left = 1)) 
PPT_DELTA = read_pptx() %>% 
  add_slide(layout = "Title Slide", master = "Office Theme") %>% 
  ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>% 
  ph_with(value = external_img(src = fichier_var_7, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>% 
  ph_with(value = external_img(src = fichier_var_fixe, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>% 
  
  ph_with(value = Creer_une_boite(toupper("France")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 10) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 0.5, left = 0)) %>%
  ph_with(value = Creer_une_boite("Niveau du taux de positivité des tests") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 1/6)) %>% 
  ph_with(value = Creer_une_boite("Delta hebdomadaire du taux de positivité des tests") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 3.5 )) %>% 
  ph_with(value = Creer_une_boite(paste0("Delta du taux de positivité des tests par rapport au ", DATE_FIXE)) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 6 + 5/6)) %>% 
  ph_with(value = Creer_une_boite("Source : Si-Dep ; traitements : Drees ; taux de positivité des tests (sans dédoublonnage).") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>% 
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6.3, left = 1)) %>% 
  ph_with(value = Creer_une_boite(paste0("Les régions au dessus de la barre horizontale ont un taux de positivité supérieur ou égal à ", 100 * SEUIL_TP, " %. Les régions sont classées par taux de positivité décroissant.")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>% 
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6, left = 1)) 
for (LIB_REG in unique(data_sidep_dep_drees$libelle_reg)){
  
  tg = data_sidep_dep_drees[date >= DATE_DEPART & 
                              libelle_reg == LIB_REG, 
                            c("date", "libelle", "tp", "evo_tp_7",
                              "var_tp_7",
                              "evo_tp_fixe",
                              "var_tp_fixe")]
  
  class = tg[date == max(date)]
  setorder(class, tp)
  
  tg[, libelle := factor(libelle, class$libelle)]
  
  G_reg_tp_niv = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = tp)) + 
    scale_fill_distiller(palette = RColorBrewer::brewer.pal(9, "YlGn"),
                         direction = 1) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() 
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_niv = G_reg_tp_niv + 
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  min = plyr::round_any(min(tg$evo_tp_7), 0.1, floor)
  max = plyr::round_any(max(tg$evo_tp_7), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_tp_var7 = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = evo_tp_7)) + 
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white", 
                         midpoint = 0,
                         limit = c(-borne, borne)) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() + 
    xlab("Date de validation") 
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_var7 = G_reg_tp_var7 + 
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  min = plyr::round_any(min(tg$var_tp_7), 0.1, floor)
  max = plyr::round_any(max(tg$var_tp_7), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_tp_var7_delta = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = var_tp_7)) + 
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white", 
                         midpoint = 0,
                         limit = c(-borne, borne)) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() + 
     xlab("Date de validation") 
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_var7_delta = G_reg_tp_var7_delta + 
      xlab("Date de validation") +
       geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  min = plyr::round_any(min(tg$evo_tp_fixe), 0.1, floor)
  max = plyr::round_any(max(tg$evo_tp_fixe), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_tp_varfixe = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = evo_tp_fixe)) + 
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white", 
                         midpoint = 0,
                         limit = c(-borne, borne)) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() + 
     xlab("Date de validation") 
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_varfixe = G_reg_tp_varfixe + 
      xlab("Date de validation") +
       geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  
  min = plyr::round_any(min(tg$var_tp_fixe), 0.1, floor)
  max = plyr::round_any(max(tg$var_tp_fixe), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_tp_varfixe_delta = ggplot() + 
    geom_tile(data = tg, 
              aes(date, libelle, fill = var_tp_fixe)) + 
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white", 
                         midpoint = 0,
                         limit = c(-borne, borne)) + 
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) + 
    coord_fixed() + 
    xlab("Date de validation") 
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_varfixe_delta = G_reg_tp_varfixe_delta + 
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  
  fichier_niv = file.path(tempdir(), "niv.png")
  ggsave(plot = G_reg_tp_niv, 
         filename = fichier_niv, width = 5.8, height = 6)
  fichier_evo_7 = file.path(tempdir(), "evo_7.png")
  ggsave(plot = G_reg_tp_var7, 
         filename = fichier_evo_7, width = 5.8, height = 6)
  fichier_var_7 = file.path(tempdir(), "var_7.png")
  ggsave(plot = G_reg_tp_var7_delta, 
         filename = fichier_var_7, width = 5.8, height = 6)
  fichier_evo_fixe = file.path(tempdir(), "evo_fixe.png")
  ggsave(plot = G_reg_tp_varfixe, 
         filename = fichier_evo_fixe, width = 5.8, height = 6)
  fichier_var_fixe = file.path(tempdir(), "var_fixe.png")
  ggsave(plot = G_reg_tp_varfixe_delta, 
         filename = fichier_var_fixe, width = 5.8, height = 6)
  
    
  PPT_EVO = PPT_EVO %>% 
    add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    
    ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>% 
    ph_with(value = external_img(src = fichier_evo_7, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>% 
    ph_with(value = external_img(src = fichier_evo_fixe, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>% 
    
    ph_with(value = Creer_une_boite(toupper(LIB_REG)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 10) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 0.5, left = 0)) %>%
    ph_with(value = Creer_une_boite("Niveau du taux de positivité des tests") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 1/6)) %>% 
    ph_with(value = Creer_une_boite("Glissement hebdomadaire du taux de positivité des tests") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 3.5 )) %>% 
    ph_with(value = Creer_une_boite(paste0("Variation du taux de positivité des tests par rapport au ", DATE_FIXE)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 6 + 5/6)) %>%
    ph_with(value = Creer_une_boite("Source : Si-Dep ; traitements : Drees ; taux de positivité des tests (sans dédoublonnage).") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>% 
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6.3, left = 1)) %>% 
    ph_with(value = Creer_une_boite(paste0("Les départements au dessus de la barre horizontale (si elle existe) ont un taux de positivité supérieur ou égal à ", 100 * SEUIL_TP, " %. Les départements sont classées par taux de positivité décroissant.")) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>% 
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6, left = 1)) 
  
  
  PPT_DELTA = PPT_DELTA %>% 
    add_slide(layout = "Title Slide", master = "Office Theme") %>% 
    
    ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>% 
    ph_with(value = external_img(src = fichier_var_7, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>% 
    ph_with(value = external_img(src = fichier_var_fixe, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>% 
    
    ph_with(value = Creer_une_boite(toupper(LIB_REG)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 10) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 0.5, left = 0)) %>%
    ph_with(value = Creer_une_boite("Niveau du taux de positivité des tests") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 1/6)) %>% 
    ph_with(value = Creer_une_boite("Delta hebdomadaire du taux de positivité des tests") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 3.5 )) %>% 
    ph_with(value = Creer_une_boite(paste0("Delta du taux de positivité des tests par rapport au ", DATE_FIXE)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 6 + 5/6)) %>%
    ph_with(value = Creer_une_boite("Source : Si-Dep ; traitements : Drees ; taux de positivité des tests (sans dédoublonnage).") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>% 
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6.3, left = 1)) %>% 
    ph_with(value = Creer_une_boite(paste0("Les départements au dessus de la barre horizontale (si elle existe) ont un taux de positivité supérieur ou égal à ", 100 * SEUIL_TP, " %. Les départements sont classées par taux de positivité décroissant.")) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>% 
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6, left = 1)) 
  
}
path_ppt_evo = paste0(chemin_sortie_heatmaps, Sys.Date()," - heatmap tp (donnees drees) - evolution.pptx")
if(file.exists(path_ppt_evo))file.remove(path_ppt_delta)
print(PPT_EVO,target = path_ppt_evo)
path_ppt_delta = paste0(chemin_sortie_heatmaps, Sys.Date()," - heatmap tp (donnees drees) - delta.pptx")
if(file.exists(path_ppt_delta))file.remove(path_ppt_delta)
print(PPT_DELTA,target = path_ppt_delta)
write.csv(data_sidep_dep_drees, paste0(chemin_sortie_heatmaps, Sys.Date(), " - data_dep_tp.csv"))
write.csv(data_sidep_reg_drees, paste0(chemin_sortie_heatmaps, Sys.Date(), " - data_reg_tp.csv"))
print(gc(verbose = T))
library(data.table)
library(ggplot2)
library(openxlsx)
library(officer)
library(flextable)
library(dplyr)
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
somme_7j <- function(x) {
  res <- ifelse(is.na(shift(x, 6)), NA, x + shift(x, 1) + shift(x, 2) + shift(x, 3) + 
                  shift(x, 4) + shift(x, 5) + shift(x, 6))
  return(res)
}
Creer_une_boite <- function(texte){
  aux = data.frame(matrix(NA, nrow = 0, ncol = 1)) 
  names(aux) = texte
  return(aux)
}
DATE_DEPART = "2021-01-16" # La date de départ pour les heatmaps
SEUIL_TP = 0.05
SEUIL_TI = 200
DATE_FIXE = "2021-01-16"
DATE_DREES = gsub("-", "", Sys.Date())
chemin_sorties_quot = paste0(path, "data/sorties/nb_tests/") 
chemin_utils_sidep = paste0(path, "data/utils/") 
chemin_sortie_heatmaps = paste0(path, "data/sorties/heatmaps/") 
data_sidep_reg_drees_pcr_65 = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_valid_65+.xlsx"),
                                              sheet = "regions",
                                              detectDates = T))
data_sidep_dep_drees_pcr_65 = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_valid_65+.xlsx"),
                                              sheet = "departements",
                                              detectDates = T))
data_sidep_reg_drees_tag_65 = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_antigenique_valid_65+.xlsx"),
                                              sheet = "regions",
                                              detectDates = T))
data_sidep_dep_drees_tag_65 = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_antigenique_valid_65+.xlsx"),
                                              sheet = "departements",
                                              detectDates = T))
data_sidep_reg_drees_pcr_sal_mil_65 = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_sal_mil_valid_65+.xlsx"),
                                                      sheet = "regions",
                                                      detectDates = T))
data_sidep_dep_drees_pcr_sal_mil_65 = setDT(read.xlsx(paste0(chemin_sorties_quot, DATE_DREES, "_sidep_pcr_sal_mil_valid_65+.xlsx"),
                                                      sheet = "departements",
                                                      detectDates = T))
data_sidep_reg_drees_65 = rbind(data_sidep_reg_drees_pcr_65[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")],
                                data_sidep_reg_drees_tag_65[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")],
                                data_sidep_reg_drees_pcr_sal_mil_65[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")])
data_sidep_reg_drees_65 = data_sidep_reg_drees_65[, .(nb_pos = sum(nb_pos),
                                                      nb_tot_PNIX = sum(nb_tot_PNIX)),
                                                  by = c("date", "nom_reg")]
data_sidep_dep_drees_65 = rbind(data_sidep_dep_drees_pcr_65[, c("date", "departement", "nb_pos", "nb_tot_PNIX")],
                                data_sidep_dep_drees_tag_65[, c("date", "departement", "nb_pos", "nb_tot_PNIX")],
                                data_sidep_dep_drees_pcr_sal_mil_65[, c("date", "departement", "nb_pos", "nb_tot_PNIX")])
data_sidep_dep_drees_65 = data_sidep_dep_drees_65[, .(nb_pos = sum(nb_pos),
                                                      nb_tot_PNIX = sum(nb_tot_PNIX)),
                                                  by = c("date", "departement")]
libelle_reg = fread(paste0(chemin_utils_sidep, "region2019.csv"), encoding = "UTF-8")
libelle_reg[, reg := as.character(reg)]
libelle_dep = fread(paste0(chemin_utils_sidep, "departement2019.csv"), encoding = "UTF-8")
setorder(data_sidep_reg_drees_65, nom_reg, date)
data_sidep_reg_drees_65 = data_sidep_reg_drees_65[, c("date", "nom_reg", "nb_pos", "nb_tot_PNIX")]
data_sidep_reg_drees_65[, nb_pos_7j := somme_7j(nb_pos), by = "nom_reg"]
data_sidep_reg_drees_65[, nb_tot_PNIX_7j := somme_7j(nb_tot_PNIX), by = "nom_reg"]
data_sidep_reg_drees_65[, tp := nb_pos_7j / nb_tot_PNIX_7j, by = "nom_reg"]
setorder(data_sidep_reg_drees_65, nom_reg, date)
data_sidep_reg_drees_65[, evo_tp_1 := tp / shift(tp, 1) - 1, by = "nom_reg"]
data_sidep_reg_drees_65[, evo_tp_7 := tp / shift(tp, 7) - 1, by = "nom_reg"]
data_sidep_reg_drees_65[, var_tp_7 := tp - shift(tp, 7), by = "nom_reg"]
for (REGION in unique(data_sidep_reg_drees_65$nom_reg)){
  val_fixe = as.numeric(data_sidep_reg_drees_65[date == DATE_FIXE & nom_reg == REGION, "tp"])
  data_sidep_reg_drees_65[nom_reg == REGION, evo_tp_fixe := tp / val_fixe - 1, by = "nom_reg"]
  data_sidep_reg_drees_65[nom_reg == REGION, var_tp_fixe := tp - val_fixe, by = "nom_reg"]
}
setorder(data_sidep_dep_drees_65, departement, date)
data_sidep_dep_drees_65 = data_sidep_dep_drees_65[, c("date", "departement", "nb_pos", "nb_tot_PNIX")]
data_sidep_dep_drees_65[, nb_pos_7j := somme_7j(nb_pos), by = "departement"]
data_sidep_dep_drees_65[, nb_tot_PNIX_7j := somme_7j(nb_tot_PNIX), by = "departement"]
data_sidep_dep_drees_65[, tp := nb_pos_7j / nb_tot_PNIX_7j, by = "departement"]
setorder(data_sidep_dep_drees_65, departement, date)
data_sidep_dep_drees_65[, evo_tp_1 := tp / shift(tp, 1) - 1, by = "departement"]
data_sidep_dep_drees_65[, evo_tp_7 := tp / shift(tp, 7) - 1, by = "departement"]
data_sidep_dep_drees_65[, var_tp_7 := tp - shift(tp, 7), by = "departement"]
for (DEPARTEMENT in unique(data_sidep_dep_drees_65$departement)){
  val_fixe = as.numeric(data_sidep_dep_drees_65[date == DATE_FIXE & departement == DEPARTEMENT, "tp"])
  data_sidep_dep_drees_65[departement == DEPARTEMENT, evo_tp_fixe := tp / val_fixe - 1, by = "departement"]
  data_sidep_dep_drees_65[departement == DEPARTEMENT, var_tp_fixe := tp - val_fixe, by = "departement"]
}
data_sidep_dep_drees_65 = merge(data_sidep_dep_drees_65, 
                                libelle_dep, 
                                by.x = "departement",
                                by.y = "dep")
aux = libelle_reg[, c("reg", "libelle")]
aux[, libelle_reg := libelle]
aux[, libelle := NULL]
aux[, reg := as.numeric(reg)]
data_sidep_dep_drees_65 = merge(data_sidep_dep_drees_65, 
                                aux, by = "reg")
setorder(data_sidep_dep_drees_65, libelle)
write.csv(data_sidep_dep_drees_65, paste0(chemin_sortie_heatmaps, Sys.Date(), " - data_dep_tp_65.csv"))
write.csv(data_sidep_reg_drees_65, paste0(chemin_sortie_heatmaps, Sys.Date(), " - data_reg_tp_65.csv"))
tg = data_sidep_reg_drees_65[date >= DATE_DEPART &
                               nom_reg != "Région inconnue", c("date", "nom_reg", "tp", "evo_tp_7",
                                                               "var_tp_7", "evo_tp_fixe", "var_tp_fixe")]
class = tg[date == max(date)]
setorder(class, tp)
tg[, nom_reg := factor(nom_reg, class$nom_reg)]
G_france_tp_niv = ggplot() +
  geom_tile(data = tg,
            aes(date, nom_reg, fill = tp)) +
  scale_fill_distiller(palette = RColorBrewer::brewer.pal(9, "YlGn"),
                       direction = 1) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Date de validation") +
  coord_fixed() +
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$evo_tp_7), 0.1, floor)
max = plyr::round_any(max(tg$evo_tp_7), 0.1, ceiling)
borne = max(abs(min), max)
G_france_tp_var7 = ggplot() +
  geom_tile(data = tg,
            aes(date, nom_reg, fill = evo_tp_7)) +
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white",
                       midpoint = 0,
                       limit = c(-borne, borne)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$var_tp_7), 0.1, floor)
max = plyr::round_any(max(tg$var_tp_7), 0.1, ceiling)
borne = max(abs(min), max)
G_france_tp_var7_delta = ggplot() +
  geom_tile(data = tg,
            aes(date, nom_reg, fill = var_tp_7)) +
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white",
                       midpoint = 0,
                       limit = c(-borne, borne)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$evo_tp_fixe), 0.1, floor)
max = plyr::round_any(max(tg$evo_tp_fixe), 0.1, ceiling)
borne = max(abs(min), max)
G_france_tp_varfixe = ggplot() +
  geom_tile(data = tg,
            aes(date, nom_reg, fill = evo_tp_fixe)) +
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white",
                       midpoint = 0,
                       limit = c(-borne, borne)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
min = plyr::round_any(min(tg$var_tp_fixe), 0.1, floor)
max = plyr::round_any(max(tg$var_tp_fixe), 0.1, ceiling)
borne = max(abs(min), max)
G_france_tp_varfixe_delta = ggplot() +
  geom_tile(data = tg,
            aes(date, nom_reg, fill = var_tp_fixe)) +
  scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                       high = rgb(192 / 255, 0, 0),
                       mid = "white",
                       midpoint = 0,
                       limit = c(-borne, borne)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Date de validation") +
  coord_fixed()+
  geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
             size = .2)
fichier_niv = file.path(tempdir(), "niv.png")
ggsave(plot = G_france_tp_niv,
       filename = fichier_niv, width = 5.8, height = 6)
fichier_evo_7 = file.path(tempdir(), "evo_7.png")
ggsave(plot = G_france_tp_var7,
       filename = fichier_evo_7, width = 5.8, height = 6)
fichier_var_7 = file.path(tempdir(), "var_7.png")
ggsave(plot = G_france_tp_var7_delta,
       filename = fichier_var_7, width = 5.8, height = 6)
fichier_evo_fixe = file.path(tempdir(), "evo_fixe.png")
ggsave(plot = G_france_tp_varfixe,
       filename = fichier_evo_fixe, width = 5.8, height = 6)
fichier_var_fixe = file.path(tempdir(), "var_fixe.png")
ggsave(plot = G_france_tp_varfixe_delta,
       filename = fichier_var_fixe, width = 5.8, height = 6)
PPT_EVO = read_pptx() %>%
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>%
  ph_with(value = external_img(src = fichier_evo_7, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>%
  ph_with(value = external_img(src = fichier_evo_fixe, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>%
  
  ph_with(value = Creer_une_boite(toupper("France")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 10) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 0.5, left = 0)) %>%
  ph_with(value = Creer_une_boite("Niveau du taux de positivité des tests (65 ans et plus)") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 1/6)) %>%
  ph_with(value = Creer_une_boite("Glissement hebdomadaire du taux de positivité des tests (65 ans et plus)") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 3.5 )) %>%
  ph_with(value = Creer_une_boite(paste0("Variation du taux de positivité des tests (65 ans et plus) par rapport au ", DATE_FIXE)) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 6 + 5/6)) %>%
  ph_with(value = Creer_une_boite("Source : Si-Dep ; traitements : Drees ; taux de positivité des tests (65 ans et plus) (sans dédoublonnage).") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>%
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6.3, left = 1)) %>%
  ph_with(value = Creer_une_boite(paste0("Les régions au dessus de la barre horizontale ont un taux de positivité (65 ans et plus) supérieur ou égal à ", 100 * SEUIL_TP, " %. Les régions sont classées par taux de positivité décroissant.")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>%
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6, left = 1))
PPT_DELTA = read_pptx() %>%
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>%
  ph_with(value = external_img(src = fichier_var_7, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>%
  ph_with(value = external_img(src = fichier_var_fixe, width = 10/3, height = 5),
          location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>%
  
  ph_with(value = Creer_une_boite(toupper("France")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 10) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 0.5, left = 0)) %>%
  ph_with(value = Creer_une_boite("Niveau du taux de positivité des tests (65 ans et plus)") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 1/6)) %>%
  ph_with(value = Creer_une_boite("Delta hebdomadaire du taux de positivité des tests (65 ans et plus)") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 3.5 )) %>%
  ph_with(value = Creer_une_boite(paste0("Delta du taux de positivité des tests (65 ans et plus) par rapport au ", DATE_FIXE)) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "center", part = "header") %>%
            bold(part = "header") %>%
            color(color = "white", part = "header") %>%
            bg(bg = rgb(0, 0, 128/255), part = "header") %>%
            width(width = 3) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header"),
          location = ph_location(top = 1.5, left = 6 + 5/6)) %>%
  ph_with(value = Creer_une_boite("Source : Si-Dep ; traitements : Drees ; taux de positivité des tests (65 ans et plus) (sans dédoublonnage).") %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>%
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6.3, left = 1)) %>%
  ph_with(value = Creer_une_boite(paste0("Les régions au dessus de la barre horizontale ont un taux de positivité (65 ans et plus) supérieur ou égal à ", 100 * SEUIL_TP, " %. Les régions sont classées par taux de positivité décroissant.")) %>%
            flextable() %>%
            border_remove() %>%
            flextable::align(align = "left", part = "header") %>%
            width(width = 8) %>%
            height(height = 0.3, part = "header") %>%
            font(fontname = "Calibri", part = "header") %>%
            fontsize(size = 8, part = "header"),
          location = ph_location(top = 6, left = 1))
for (LIB_REG in unique(data_sidep_dep_drees_65$libelle_reg)){
  
  tg = data_sidep_dep_drees_65[date >= DATE_DEPART &
                                 libelle_reg == LIB_REG,
                               c("date", "libelle", "tp", "evo_tp_7",
                                 "var_tp_7",
                                 "evo_tp_fixe",
                                 "var_tp_fixe")]
  
  class = tg[date == max(date)]
  setorder(class, tp)
  
  tg[, libelle := factor(libelle, class$libelle)]
  
  G_reg_tp_niv = ggplot() +
    geom_tile(data = tg,
              aes(date, libelle, fill = tp)) +
    scale_fill_distiller(palette = RColorBrewer::brewer.pal(9, "YlGn"),
                         direction = 1) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_fixed()
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_niv = G_reg_tp_niv +
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  min = plyr::round_any(min(tg$evo_tp_7), 0.1, floor)
  max = plyr::round_any(max(tg$evo_tp_7), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_tp_var7 = ggplot() +
    geom_tile(data = tg,
              aes(date, libelle, fill = evo_tp_7)) +
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white",
                         midpoint = 0,
                         limit = c(-borne, borne)) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_fixed() +
    xlab("Date de validation")
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_var7 = G_reg_tp_var7 +
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  
  min = plyr::round_any(min(tg$var_tp_7), 0.1, floor)
  max = plyr::round_any(max(tg$var_tp_7), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_tp_var7_delta = ggplot() +
    geom_tile(data = tg,
              aes(date, libelle, fill = var_tp_7)) +
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white",
                         midpoint = 0,
                         limit = c(-borne, borne)) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_fixed() +
    xlab("Date de validation")
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_var7_delta = G_reg_tp_var7_delta +
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  min = plyr::round_any(min(tg$evo_tp_fixe), 0.1, floor)
  max = plyr::round_any(max(tg$evo_tp_fixe), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_tp_varfixe = ggplot() +
    geom_tile(data = tg,
              aes(date, libelle, fill = evo_tp_fixe)) +
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white",
                         midpoint = 0,
                         limit = c(-borne, borne)) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_fixed() +
    xlab("Date de validation")
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_varfixe = G_reg_tp_varfixe +
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  
  min = plyr::round_any(min(tg$var_tp_fixe), 0.1, floor)
  max = plyr::round_any(max(tg$var_tp_fixe), 0.1, ceiling)
  borne = max(abs(min), max)
  
  
  G_reg_tp_varfixe_delta = ggplot() +
    geom_tile(data = tg,
              aes(date, libelle, fill = var_tp_fixe)) +
    scale_fill_gradient2(low = rgb(0, 0, 128 / 255),
                         high = rgb(192 / 255, 0, 0),
                         mid = "white",
                         midpoint = 0,
                         limit = c(-borne, borne)) +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    coord_fixed() +
    xlab("Date de validation")
  
  if(any(class$tp >= SEUIL_TP)){
    G_reg_tp_varfixe_delta = G_reg_tp_varfixe_delta +
      xlab("Date de validation") +
      geom_hline(yintercept = min(which(class$tp >= SEUIL_TP)) - 0.5,
                 size = .2)
  }
  
  
  fichier_niv = file.path(tempdir(), "niv.png")
  ggsave(plot = G_reg_tp_niv,
         filename = fichier_niv, width = 5.8, height = 6)
  fichier_evo_7 = file.path(tempdir(), "evo_7.png")
  ggsave(plot = G_reg_tp_var7,
         filename = fichier_evo_7, width = 5.8, height = 6)
  fichier_var_7 = file.path(tempdir(), "var_7.png")
  ggsave(plot = G_reg_tp_var7_delta,
         filename = fichier_var_7, width = 5.8, height = 6)
  fichier_evo_fixe = file.path(tempdir(), "evo_fixe.png")
  ggsave(plot = G_reg_tp_varfixe,
         filename = fichier_evo_fixe, width = 5.8, height = 6)
  fichier_var_fixe = file.path(tempdir(), "var_fixe.png")
  ggsave(plot = G_reg_tp_varfixe_delta,
         filename = fichier_var_fixe, width = 5.8, height = 6)
  
  
  PPT_EVO = PPT_EVO %>%
    add_slide(layout = "Title Slide", master = "Office Theme") %>%
    
    ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>%
    ph_with(value = external_img(src = fichier_evo_7, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>%
    ph_with(value = external_img(src = fichier_evo_fixe, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>%
    
    ph_with(value = Creer_une_boite(toupper(LIB_REG)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 10) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 0.5, left = 0)) %>%
    ph_with(value = Creer_une_boite("Niveau du taux de positivité des tests (65 ans et plus)") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 1/6)) %>%
    ph_with(value = Creer_une_boite("Glissement hebdomadaire du taux de positivité des tests (65 ans et plus)") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 3.5 )) %>%
    ph_with(value = Creer_une_boite(paste0("Variation du taux de positivité des tests (65 ans et plus) par rapport au ", DATE_FIXE)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 6 + 5/6)) %>%
    ph_with(value = Creer_une_boite("Source : Si-Dep ; traitements : Drees ; taux de positivité des tests (65 ans et plus) (sans dédoublonnage).") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>%
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6.3, left = 1)) %>%
    ph_with(value = Creer_une_boite(paste0("Les départements au dessus de la barre horizontale (si elle existe) ont un taux de positivité (65 ans et plus) supérieur ou égal à ", 100 * SEUIL_TP, " %. Les départements sont classées par taux de positivité décroissant.")) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>%
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6, left = 1))
  
  
  PPT_DELTA = PPT_DELTA %>%
    add_slide(layout = "Title Slide", master = "Office Theme") %>%
    
    ph_with(value = external_img(src = fichier_niv, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 0), use_loc_size = FALSE) %>%
    ph_with(value = external_img(src = fichier_var_7, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 10/3), use_loc_size = FALSE) %>%
    ph_with(value = external_img(src = fichier_var_fixe, width = 10/3, height = 5),
            location = ph_location(top = 1.5, left = 20/3), use_loc_size = FALSE) %>%
    
    ph_with(value = Creer_une_boite(toupper(LIB_REG)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 10) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 0.5, left = 0)) %>%
    ph_with(value = Creer_une_boite("Niveau du taux de positivité des tests (65 ans et plus)") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 1/6)) %>%
    ph_with(value = Creer_une_boite("Delta hebdomadaire du taux de positivité des tests (65 ans et plus)") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 3.5 )) %>%
    ph_with(value = Creer_une_boite(paste0("Delta du taux de positivité des tests (65 ans et plus) par rapport au ", DATE_FIXE)) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "center", part = "header") %>%
              bold(part = "header") %>%
              color(color = "white", part = "header") %>%
              bg(bg = rgb(0, 0, 128/255), part = "header") %>%
              width(width = 3) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header"),
            location = ph_location(top = 1.5, left = 6 + 5/6)) %>%
    ph_with(value = Creer_une_boite("Source : Si-Dep ; traitements : Drees ; taux de positivité des tests (65 ans et plus) (sans dédoublonnage).") %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>%
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6.3, left = 1)) %>%
    ph_with(value = Creer_une_boite(paste0("Les départements au dessus de la barre horizontale (si elle existe) ont un taux de positivité (65 ans et plus) supérieur ou égal à ", 100 * SEUIL_TP, " %. Les départements sont classées par taux de positivité décroissant.")) %>%
              flextable() %>%
              border_remove() %>%
              flextable::align(align = "left", part = "header") %>%
              width(width = 8) %>%
              height(height = 0.3, part = "header") %>%
              font(fontname = "Calibri", part = "header") %>%
              fontsize(size = 8, part = "header"),
            location = ph_location(top = 6, left = 1))
  
}
path_ppt_evo = paste0(chemin_sortie_heatmaps, Sys.Date()," - heatmap tp (donnees drees) - evolution_65.pptx")
if(file.exists(path_ppt_evo))file.remove(path_ppt_delta)
print(PPT_EVO,target = path_ppt_evo)
path_ppt_delta = paste0(chemin_sortie_heatmaps, Sys.Date()," - heatmap tp (donnees drees) - delta_65.pptx")
if(file.exists(path_ppt_delta))file.remove(path_ppt_delta)
print(PPT_DELTA,target = path_ppt_delta)
print(gc(verbose = T))
print("Comparaison SPF")
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
library(data.table)
library(ggplot2)
library(plotly)
drees_ti_reg = fread(paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - data_reg_ti.csv"), encoding = "UTF-8")
drees_ti_dep = fread(paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - data_dep_ti.csv"), encoding = "UTF-8")
drees_tp_reg = fread(paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - data_reg_tp.csv"), encoding = "UTF-8")
drees_tp_dep = fread(paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - data_dep_tp.csv"), encoding = "UTF-8")
spf_dep = fread("/production/echange/spf_vm_sidep/raw/sp_ti_tp_7j_dep.csv", encoding = "UTF-8")
spf_dep[, ti_spf := P / pop * 1e5]
spf_dep[, tp_spf := P / T]
spf_dep[, semaine_glissante := substr(semaine_glissante, nchar(semaine_glissante) - 9, nchar(semaine_glissante))]
spf_reg = fread("/production/echange/spf_vm_sidep/raw/sp_ti_tp_7j_reg.csv", encoding = "UTF-8")
spf_reg[, ti_spf := P / pop * 1e5]
spf_reg[, tp_spf := P / T]
spf_reg[, semaine_glissante := substr(semaine_glissante, nchar(semaine_glissante) - 9, nchar(semaine_glissante))]
drees_ti_reg[, date := as.character(date)]
compar_ti_reg = merge(drees_ti_reg[, c("date", "reg", "ti")],
                      spf_reg[, c("semaine_glissante", "reg", "nom_region", "ti_spf")],
                      by.x = c("date", "reg"),
                      by.y = c("semaine_glissante", "reg"),
                      all = T
)
compar_ti_reg[, ti_drees := ti]
compar_ti_reg[, date := as.Date(date)]
pdf(paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check ti reg.pdf"))
for (NOM_REG in na.omit(unique(compar_ti_reg$nom_region))){
  
  tg = compar_ti_reg[reg ==  as.numeric(unique(compar_ti_reg[nom_region == NOM_REG, c("reg")])), c("date", "ti_spf", "ti_drees")]
  print(ggplot() + 
          geom_line(data = melt(tg, id.vars = "date"),
                    aes(date, value, group = variable, color = variable),
                    size = 1) + 
          scale_color_manual(values = c(rgb(0, 0, 128/255),
                                        rgb(1, 102/255, 0))) + 
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title.y = element_blank()) + 
          labs(title = paste0("Taux d'incidence - ", NOM_REG)) + 
          xlab("Date (SPF : prélèvement ; Drees : résultat)"))
}
dev.off()
drees_ti_dep[, date := as.character(date)]
compar_ti_dep = merge(drees_ti_dep[, c("date", "departement", "ti", "libelle")],
                      spf_dep[, c("semaine_glissante", "dep", "ti_spf")],
                      by.x = c("date", "departement"),
                      by.y = c("semaine_glissante", "dep"),
                      all = T
)
compar_ti_dep[, ti_drees := ti]
compar_ti_dep[, date := as.Date(date)]
pdf(paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check ti dep.pdf"))
for (NOM_DEP in na.omit(unique(compar_ti_dep$libelle))){
  tg = compar_ti_dep[libelle == NOM_DEP, c("date", "ti_spf", "ti_drees")]
  print(ggplot() + 
          geom_line(data = melt(tg, id.vars = "date"),
                    aes(date, value, group = variable, color = variable),
                    size = 1) + 
          scale_color_manual(values = c(rgb(0, 0, 128/255),
                                        rgb(1, 102/255, 0))) + 
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title.y = element_blank()) + 
          labs(title = paste0("Taux d'incidence - ", NOM_DEP)) + 
          xlab("Date (SPF : prélèvement ; Drees : résultat)"))
}
dev.off()
spf_reg[nom_region == "Ile-de-France", nom_region := "Île-de-France"]
spf_reg[nom_region == "Centre-Val de Loire", nom_region := "Centre Val-de-Loire"]
spf_reg[nom_region == "Grand Est", nom_region := "Grand-Est"]
spf_reg[nom_region == "Nouvelle-Aquitaine", nom_region := "Nouvelle Aquitaine"]
drees_tp_reg = drees_tp_reg[nom_reg != "Région inconnue"]
drees_tp_reg[, date := as.character(date)]
compar_tp_reg = merge(drees_tp_reg[, c("date", "nom_reg", "tp")],
                      spf_reg[, c("semaine_glissante", "reg", "nom_region", "tp_spf")],
                      by.x = c("date", "nom_reg"),
                      by.y = c("semaine_glissante", "nom_region"),
                      all = T
)
compar_tp_reg[, tp_drees := tp]
compar_tp_reg[, date := as.Date(date)]
pdf(paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check tp reg.pdf"))
for (NOM_REG in na.omit(unique(compar_tp_reg$nom_reg))){
  tg = compar_tp_reg[nom_reg == NOM_REG, c("date", "tp_spf", "tp_drees")]
  print(ggplot() + 
          geom_line(data = melt(tg, id.vars = "date"),
                    aes(date, value, group = variable, color = variable),
                    size = 1) + 
          scale_color_manual(values = c(rgb(0, 0, 128/255),
                                        rgb(1, 102/255, 0))) + 
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title.y = element_blank()) + 
          labs(title = paste0("Taux de positivité - ", NOM_REG)) + 
          xlab("Date (SPF : prélèvement ; Drees : résultat)"))
}
dev.off()
drees_tp_dep[, date := as.character(date)]
compar_tp_dep = merge(drees_tp_dep[, c("date", "departement", "tp", "libelle")],
                      spf_dep[, c("semaine_glissante", "dep", "tp_spf")],
                      by.x = c("date", "departement"),
                      by.y = c("semaine_glissante", "dep"),
                      all = T
)
compar_tp_dep[, tp_drees := tp]
compar_tp_dep[, date := as.Date(date)]
pdf(paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check tp dep.pdf"))
for (NOM_DEP in na.omit(unique(compar_tp_dep$libelle))){
  tg = compar_tp_dep[libelle == NOM_DEP, c("date", "tp_spf", "tp_drees")]
  print(ggplot() + 
          geom_line(data = melt(tg, id.vars = "date"),
                    aes(date, value, group = variable, color = variable),
                    size = 1) + 
          scale_color_manual(values = c(rgb(0, 0, 128/255),
                                        rgb(1, 102/255, 0))) + 
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title.y = element_blank()) + 
          labs(title = paste0("Taux de positivité - ", NOM_DEP)) + 
          xlab("Date (SPF : prélèvement ; Drees : résultat)"))
}
dev.off()
drees_tp_reg[, date := as.Date(date)]
htmlwidgets::saveWidget(ggplotly(ggplot() +
                                   geom_line(data = drees_tp_reg[!nom_reg %in% c("La Réunion",
                                                                                 "Guadeloupe",
                                                                                 "Martinique",
                                                                                 "Mayotte",
                                                                                 "Guyane",
                                                                                 "Région inconnue")],
                                             aes(date, tp, group = nom_reg, color = nom_reg),
                                             size = 1) +
                                   theme(legend.position = "bottom",
                                         legend.title = element_blank(),
                                         axis.title.y = element_blank()) +
                                   labs(title = paste0("Taux de positivité des tests"))),
                        paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - tp par région longue période.html"))
print(gc(verbose = T))
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
library(data.table)
library(ggplot2)
library(plotly)
drees_tp_reg = fread(paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - data_reg_tp_65.csv"), encoding = "UTF-8")
drees_tp_dep = fread(paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - data_dep_tp_65.csv"), encoding = "UTF-8")
spf_dep = fread("/production/echange/spf_vm_sidep/raw/sp_ti_tp_7j_dep_65.csv", encoding = "UTF-8")
spf_dep[, pop := as.numeric(sub(",", ".", pop))]
spf_dep[, ti_spf := P / pop * 1e5]
spf_dep[, tp_spf := P / T]
spf_dep[, semaine_glissante := substr(semaine_glissante, nchar(semaine_glissante) - 9, nchar(semaine_glissante))]
spf_reg = fread("/production/echange/spf_vm_sidep/raw/sp_ti_tp_7j_reg_65.csv", encoding = "UTF-8")
spf_reg[, pop := as.numeric(sub(",", ".", pop))]
spf_reg[, ti_spf := P / pop * 1e5]
spf_reg[, tp_spf := P / T]
spf_reg[, semaine_glissante := substr(semaine_glissante, nchar(semaine_glissante) - 9, nchar(semaine_glissante))]
spf_reg[nom_region == "Ile-de-France", nom_region := "Île-de-France"]
spf_reg[nom_region == "Centre-Val de Loire", nom_region := "Centre Val-de-Loire"]
spf_reg[nom_region == "Grand Est", nom_region := "Grand-Est"]
spf_reg[nom_region == "Nouvelle-Aquitaine", nom_region := "Nouvelle Aquitaine"]
drees_tp_reg = drees_tp_reg[nom_reg != "Région inconnue"]
drees_tp_reg[, date := as.character(date)]
compar_tp_reg = merge(drees_tp_reg[, c("date", "nom_reg", "tp")],
                      spf_reg[, c("semaine_glissante", "reg", "nom_region", "tp_spf")],
                      by.x = c("date", "nom_reg"),
                      by.y = c("semaine_glissante", "nom_region"),
                      all = T
)
compar_tp_reg[, tp_drees := tp]
compar_tp_reg[, date := as.Date(date)]
pdf(paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check tp reg - 65.pdf"))
for (NOM_REG in na.omit(unique(compar_tp_reg$nom_reg))){
  tg = compar_tp_reg[nom_reg == NOM_REG, c("date", "tp_spf", "tp_drees")]
  print(ggplot() + 
          geom_line(data = melt(tg, id.vars = "date"),
                    aes(date, value, group = variable, color = variable),
                    size = 1) + 
          scale_color_manual(values = c(rgb(0, 0, 128/255),
                                        rgb(1, 102/255, 0))) + 
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title.y = element_blank()) + 
          labs(title = paste0("Taux de positivité - ", NOM_REG)) + 
          xlab("Date (SPF : prélèvement ; Drees : résultat)"))
}
dev.off()
drees_tp_dep[, date := as.character(date)]
compar_tp_dep = merge(drees_tp_dep[, c("date", "departement", "tp", "libelle")],
                      spf_dep[, c("semaine_glissante", "dep", "tp_spf")],
                      by.x = c("date", "departement"),
                      by.y = c("semaine_glissante", "dep"),
                      all = T
)
compar_tp_dep[, tp_drees := tp]
compar_tp_dep[, date := as.Date(date)]
pdf(paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check tp dep - 65.pdf"))
for (NOM_DEP in na.omit(unique(compar_tp_dep$libelle))){
  tg = compar_tp_dep[libelle == NOM_DEP, c("date", "tp_spf", "tp_drees")]
  print(ggplot() + 
          geom_line(data = melt(tg, id.vars = "date"),
                    aes(date, value, group = variable, color = variable),
                    size = 1) + 
          scale_color_manual(values = c(rgb(0, 0, 128/255),
                                        rgb(1, 102/255, 0))) + 
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title = element_blank()) + 
          labs(title = paste0("Taux de positivité - ", NOM_DEP)) + 
          xlab("Date (SPF : prélèvement ; Drees : résultat)"))
}
dev.off()
drees_tp_reg[, date := as.Date(date)]
htmlwidgets::saveWidget(ggplotly(ggplot() +
                                   geom_line(data = drees_tp_reg[!nom_reg %in% c("La Réunion",
                                                                                 "Guadeloupe",
                                                                                 "Martinique",
                                                                                 "Mayotte",
                                                                                 "Guyane",
                                                                                 "Région inconnue")],
                                             aes(date, tp, group = nom_reg, color = nom_reg),
                                             size = 1) +
                                   theme(legend.position = "bottom",
                                         legend.title = element_blank(),
                                         axis.title.y = element_blank()) +
                                   labs(title = paste0("Taux de positivité des tests - 65 ans et plus"))),
                        paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - tp par région longue période - 65.html"))
print(gc(verbose = T))
print("09")
print(gc(verbose = T))
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
path_fabrice = paste0(path, "data/sorties/dossier_quotidien_fabrice/")
if (!dir.exists(paste0(path_fabrice, Sys.Date()))) dir.create(paste0(path_fabrice, Sys.Date()))
path_fabrice_du_jour = paste0(path_fabrice, Sys.Date(), "/")
file.copy(from = paste0(path, "data/verifs/", Sys.Date(), "_sidep_par_patient.xlsx"),
            to = paste0(path_fabrice_du_jour, Sys.Date(), "_sidep_par_patient.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/verifs/", Sys.Date(), "_sidep_par_patient_bis.xlsx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_sidep_par_patient_bis.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/expertise/", date_AAAAMMJJ, "_expertise_donnees_du_jour.docx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_expertise_donnees_du_jour.docx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_antigenique_valid_65+.xlsx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_sidep_antigenique_valid_65+.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_valid_65+.xlsx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_sidep_pcr_valid_65+.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_sal_mil_valid_65+.xlsx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_sidep_pcr_sal_mil_valid_65+.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_antigenique_valid.xlsx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_sidep_antigenique_valid.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_valid.xlsx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_sidep_pcr_valid.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_sal_mil_valid.xlsx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), "_sidep_pcr_sal_mil_valid.xlsx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - heatmap ti (donnees drees) - evolution.pptx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - heatmap ti (donnees drees) - evolution.pptx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - heatmap ti (donnees drees) - delta.pptx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - heatmap ti (donnees drees) - delta.pptx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - heatmap tp (donnees drees) - evolution.pptx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - heatmap tp (donnees drees) - evolution.pptx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - heatmap tp (donnees drees) - delta.pptx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - heatmap tp (donnees drees) - delta.pptx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - heatmap tp (donnees drees) - evolution_65.pptx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - heatmap tp (donnees drees) - evolution_65.pptx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/heatmaps/", Sys.Date(), " - heatmap tp (donnees drees) - delta_65.pptx"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - heatmap tp (donnees drees) - delta_65.pptx"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check ti reg.pdf"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - check ti reg.pdf"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check ti dep.pdf"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - check ti dep.pdf"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check tp reg.pdf"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - check tp reg.pdf"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check tp dep.pdf"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - check tp dep.pdf"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check tp reg - 65.pdf"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - check tp reg - 65.pdf"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - check tp dep - 65.pdf"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - check tp dep - 65.pdf"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - tp par région longue période.html"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - tp par région longue période.html"),
          overwrite = T)
file.copy(from = paste0(path, "data/sorties/compar drees spf/", Sys.Date(), " - tp par région longue période - 65.html"),
          to = paste0(path_fabrice_du_jour, Sys.Date(), " - tp par région longue période - 65.html"),
          overwrite = T)
zip::zipr(zipfile = paste0(path_fabrice, Sys.Date(), '_export_quotidien_sidep.zip'),
          files = paste0(path_fabrice_du_jour, dir(path_fabrice_du_jour)))
file.remove(paste0(path_fabrice_du_jour, dir(path_fabrice_du_jour)))
file.remove(paste0(path_fabrice, Sys.Date()))
print("prep fichier 2")
print(gc(verbose = T))
library(openxlsx)
t1 = setDT(read.xlsx(paste0(path,"data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_valid.xlsx"),
                     sheet = "national",
                     detectDates = T))
t2 = setDT(read.xlsx(paste0(path,"data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_antigenique_valid.xlsx"),
                     sheet = "national",
                     detectDates = T))
t3 = setDT(read.xlsx(paste0(path,"data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_sal_mil_valid.xlsx"),
                     sheet = "national",
                     detectDates = T))
t = merge(t1, 
          t2[, c("date", "nb_pos", "prop_pos_PNIX", "nb_tot")],
          by = "date",
          all = T)
t = merge(t, 
          t3[, c("date", "nb_pos", "prop_pos_PNIX", "nb_tot")],
          by = "date",
          all = T)
t1 = setDT(read.xlsx(paste0(path,"data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_valid_65+.xlsx"),
                     sheet = "national",
                     detectDates = T))
t2 = setDT(read.xlsx(paste0(path,"data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_antigenique_valid_65+.xlsx"),
                     sheet = "national",
                     detectDates = T))
t3 = setDT(read.xlsx(paste0(path,"data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_sal_mil_valid_65+.xlsx"),
                     sheet = "national",
                     detectDates = T))
t_65 = merge(t1, 
             t2[, c("date", "nb_pos", "prop_pos_PNIX", "nb_tot")],
             by = "date",
             all = T)
t_65 = merge(t_65,
          t3[, c("date", "nb_pos", "prop_pos_PNIX", "nb_tot")],
          by = "date",
          all = T)
modele_fichier = loadWorkbook(paste0(path,"src/utils/Modèle fichier 2 note quotidienne.xlsx"))
writeData(modele_fichier, "national brut", t)
writeData(modele_fichier, "national 65+ brut", t_65)
saveWorkbook(modele_fichier, paste0(path,"data/sorties/dossier_quotidien_fabrice/", Sys.Date()," - fichier 2 note quotidienne.xlsx"),
             overwrite = T)
print("prep fichier 1")
print(gc(verbose = T))
library(openxlsx)
t1 = setDT(read.xlsx(paste0(path,"data/verifs/", Sys.Date(), "_sidep_par_patient.xlsx"),
                     sheet = "date_valid",
                     detectDates = T))
t1 = t1[!is.na(day_valid_init) & day_valid_init != "", c("day_valid_init", "nb_pos_patients")]
t1[, day_valid_init := seq.Date(from = as.Date("2020-05-21"), 
                                by = 1,
                                length.out = dim(t1)[1])]
t1 = t1[day_valid_init <= Sys.Date()]
t1 = rbind(t1, 
           data.frame(day_valid_init = as.Date("2020-05-20"),
                      nb_pos_patients = NA))
setorder(t1, day_valid_init)
setnames(t1, "day_valid_init", "Date")
setnames(t1, "nb_pos_patients", "Série PCR Drees dédoublonnée")
t2 = setDT(read.xlsx(paste0(path,"data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_antigenique_valid.xlsx"),
                     sheet = "national",
                     detectDates = T))
t2 = t2[, c("date", "nb_pos")]
t2[, date := date + 1]
setnames(t2, "date", "Date")
setnames(t2, "nb_pos", "Série TAG Drees non dédoublonnée")
t_all = merge(t1, t2, all.x = T,
              by = "Date")
t3 = setDT(read.xlsx(paste0(path,"data/sorties/nb_tests/", date_AAAAMMJJ, "_sidep_pcr_sal_mil_valid.xlsx"),
                     sheet = "national",
                     detectDates = T))
t3 = t3[, c("date", "nb_pos")]
t3[, date := date + 1]
setnames(t3, "date", "Date")
setnames(t3, "nb_pos", "Série salmil Drees non dédoublonnée")
t_all = merge(t_all, t3, all.x = T,
              by = "Date")
t4 = setDT(read.xlsx(paste0(path,"data/verifs/", Sys.Date(), "_sidep_par_patient_bis.xlsx"),
                     sheet = "date_valid",
                     detectDates = T))
t4 = t4[!is.na(day_valid_init) & day_valid_init != "", c("day_valid_init", "nb_pos_patients")]
t4[, day_valid_init := seq.Date(from = as.Date("2020-05-21"), 
                                by = 1,
                                length.out = dim(t1)[1])]
t4 = t4[day_valid_init <= Sys.Date()]
t4 = rbind(t4, 
           data.frame(day_valid_init = as.Date("2020-05-20"),
                      nb_pos_patients = NA))
setorder(t4, day_valid_init)
setnames(t4, "day_valid_init", "Date")
setnames(t4, "nb_pos_patients", "Série PCR Drees dédoublonnée PCR + TAG + salivaires")
t_all = merge(t_all, t4, all.x = T,
              by = "Date")
t5 = setDT(read.xlsx(paste0(path,"data/sorties/variants/", date_AAAAMMJJ, " table_pcr_variants - avec graphiques.xlsx"),
                     sheet = "PCR only",
                     detectDates = T))
t5 = t5[, c("day_valid_init", "variant_oui_quot")]
t5[, day_valid_init := day_valid_init + 1]
setnames(t5, "day_valid_init", "Date")
t_all = merge(t_all, t5, all.x = T,
              by = "Date")
modele_fichier = loadWorkbook(paste0(path,"src/utils/Modèle fichier 1 note quotidienne.xlsx"))
writeData(modele_fichier, "input R", t_all)
saveWorkbook(modele_fichier, paste0(path,"data/sorties/dossier_quotidien_fabrice/", Sys.Date()," - fichier 1 note quotidienne.xlsx"),
             overwrite = T)
liste = dir(paste0(path,"data/sorties/dossier_quotidien_fabrice/export_gitlab/"))
for (element in liste) file.remove(paste0(path,"data/sorties/dossier_quotidien_fabrice/export_gitlab/", element))
for (element in c(paste0(Sys.Date(), c(" - fichier 1 note quotidienne.xlsx", 
                                       " - fichier 2 note quotidienne.xlsx", 
                                       "_export_quotidien_sidep.zip",
                                       "_export_quotidien_variants.zip")),
                  paste0(date_AAAAMMJJ, " table_pcr_variants - avec graphiques.xlsx"))){
  file.copy(paste0(path,"data/sorties/dossier_quotidien_fabrice/", element),
            paste0(path,"data/sorties/dossier_quotidien_fabrice/export_gitlab/", element))
}
if (check_collision_p1p2){
  p1_ = unique(new_rattrapage[Pseudonyme!=Pseudo2,c("Pseudonyme")])
  p2_ = unique(new_rattrapage[Pseudonyme!=Pseudo2,c("Pseudo2")])
  
  collisionp1p2 = merge(p1_,p2_,by.x="Pseudonyme",by.y="Pseudo2")#966 vs 16M de pseudo, pas de pb...
  print(nrow(collisionp1p2))
  print(nrow(p1_))
  print(nrow(p2_))
}

