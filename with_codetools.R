# library(codetools)
# findGlobals(fread)

# library(BiocManager)
# BiocManager::install("graph")
# BiocManager::install("Rgraphviz")

library(CodeDepends)
library(Rgraphviz)
library(igraph)
library(visNetwork)

f = "../sidep/src/00_Actualisation_pcr_sero.R"
f = "../sidep/src/01_Nettoyage_sidep_pcr - catch warning.R"
sc = readScript(f)
inputs = getInputs(sc)
g = makeVariableGraph( info = inputs)
plot(g)
ig <- igraph::graph_from_graphnel(g)

visNetwork::visIgraph(ig)


dtm = getDetailedTimelines(sc, inputs)
par(bg = 'white')
plot(dtm)




