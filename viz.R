
create_network = function(dt){
  assertthat::assert_that(all(c("parent","child")%in%names(dt)))
  nodes = data.table(title = unique(c(dt$parent,dt$child)))
  nodes[,label_long:=title]
  nodes[,label:=""]
  nodes$id = 1:nrow(nodes)
  dt_groups = dt[,.(group=paste(sort(unique(group)),collapse="/")),by="child"]
  
  if("group"%in%names(dt)){
    nodes[dt_groups,group:=i.group,on=c("title"="child")]
    nodes[dt,group:="script",on=c("title"="parent")]
  }
  edges = copy(dt)
  edges[nodes,from:=i.id,on=c("parent"="title")]
  edges[nodes,to:=i.id,on=c("child"="title")]
  edges[,arrows:=case_when(
    group == "input" ~ "from",
    group == "output" ~ "to",
    is.na(group) ~ "middle"#source file
  )]
  visNetwork(nodes, edges, width = "100%",height = "800px")
}
