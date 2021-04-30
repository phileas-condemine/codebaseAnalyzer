
create_network = function(dt){
  assertthat::assert_that(all(c("parent","child")%in%names(dt)))
  nodes = data.table(label = unique(c(dt$parent,dt$child)))
  nodes$id = 1:nrow(nodes)
  dt_group_child = dt[,.(group_child=paste(sort(unique(group_child)),collapse="/")),by="child"]
  dt_group_parent = dt[,.(group_parent=paste(sort(unique(group_parent)),collapse="/")),by="parent"]
  
  if("group_child"%in%names(dt)){
    nodes[dt_group_child,group:=i.group_child,on=c("label"="child")]
  }
  if("group_parent"%in%names(dt)){
    nodes[dt_group_parent,group:=i.group_parent,on=c("label"="parent")]
  }
  
  nodes[,title:=paste0("<b>",label,"</b><br/>",group)]
  
  edges = copy(dt)
  edges[nodes,from:=i.id,on=c("parent"="label")]
  edges[nodes,to:=i.id,on=c("child"="label")]
  # edges[,arrows:=case_when(
  #   group == "input" ~ "from",
  #   group == "output" ~ "to",
  #   is.na(group) ~ "middle"#source file
  # )]
  visNetwork(nodes, edges, width = "100%",height = "800px")
}
