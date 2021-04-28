
create_network = function(dt){
  assertthat::assert_that(all(c("parent","child")%in%names(dt)))
  nodes = data.table(title = unique(c(dt$parent,dt$child)))
  nodes$id = 1:nrow(nodes)
  if("group"%in%names(dt)){
    nodes[dt,group:="script",on=c("title"="parent")]
    nodes[dt,group:=i.group,on=c("title"="child")]
  }
  edges = copy(dt)
  edges[nodes,from:=i.id,on=c("parent"="title")]
  edges[nodes,to:=i.id,on=c("child"="title")]
  visNetwork(nodes, edges, width = "100%")
}
