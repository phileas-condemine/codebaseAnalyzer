for (item in liste_des_dates){
  print(item)
  fichiers = liste_fichiers[grep(item, liste_fichiers)]
  
  med4 = safe_fread(paste0("/data1/sidep_brut/bases_dechiffrees/", fichiers[grep("MED4", fichiers)]), fill=T, quote = "",
                    verbose = TRUE, sep = "|", header = TRUE, colClasses=list(
                      character = daily_files_char_vars,
                      Integer = "Age"), encoding = "Latin-1")
  
  w <- med4$warnings #idée Philéas, implémenté le 25 mars
  w <- w[grepl("^Stopped early",w)]
  if(length(w)>0){stop(w)}
  med4 <- med4$result
  
  med2 = safe_fread(paste0("/data1/sidep_brut/bases_dechiffrees/", fichiers[grep("MED2", fichiers)]), fill=T, quote = "",
                    verbose = TRUE, sep = "|", header = TRUE, colClasses=list(
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