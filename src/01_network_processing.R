#' public-facing analysis for networked action situation manuscript
#' @pjbitterman
#' created 2021-01-23
#' last updated 2023-02-23 for publication in PLOS ONE
#' 


library(sna)
library(intergraph)
library(tidygraph)
library(graphlayouts) 
library(ggraph)
library(multinet)
library(ggthemes)
library(ggsci)
library(GGally)
library(vroom)
library(janitor)
library(ergm.ego)
library(tidyverse)


# read the saved network
# this is a tidygraph object (typeof() returns "list")
net.props.reduced <- read_rds("./data/derived/network.rds")


# read the non-network descriptive data
nn.all <- read_rds("./data/derived/non_network_data.rds")


## The network contains actors (organizations) and
## "action situations" (in Ostrom parlance - places where actors engage)
## It's a multiplex network, with 5 types of edges among actors
## (financial exch., info sharing, project coord., reporting, tech assist.)
## the 6th mode is coded as "issue engagement" and connects actors to action sits

## RESEARCH QUESTION(s)
## RQ1: What predicts actor engagement in action situations?
## RO2: How are action situations connected? By what type of actors?


# plot it
net.props.reduced %>% 
  activate(edges) %>%
  ggraph(layout = "fr") +
  geom_edge_link(alpha = 0.3, aes(colour = edgeDesc)) +
  geom_node_point(aes(colour = nodeType), size = 3) +
  #geom_node_text(aes(label = name)) +
  scale_color_brewer(palette = "Set1") +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14),
        strip.text.x = element_text(size = 12))


## calculate edge weights for join collaborators participating in same AS ----
## based on help from Matt Hamilton on 2021-06-30

# make a simple lookup table
# link nodenames to their ids
gnodes <- net.props.reduced %>% activate(nodes) %>% as_tibble() %>%
  mutate(internalNodeId = seq(1:nrow(.))) %>%
  dplyr::select(name, nodeType, internalNodeId)

# get action situations and their ids
asits <- gnodes %>% filter(nodeType == "actionSit")
asitsIds <- asits$internalNodeId


# function to get the subgraph connected to a node
f_calc_ego_subgraph <- function(a.tidygraph, nodeId){
  toReturn = a.tidygraph %>% 
    activate(edges) %>% 
    filter(edge_is_to(nodeId) | edge_is_from(nodeId)) %>%
    activate(nodes) %>%
    filter(!node_is_isolated())
  return(toReturn)
}

# function to get connected nodes as a list
f_get_nodes_as_list <- function(a.tidygraph, nodeName){
  a.df <- a.tidygraph %>% 
    activate(nodes) %>% 
    as_tibble() %>% 
    dplyr::filter(name != nodeName) %>%
    dplyr::filter(!str_detect(name, "eng."))
  return(a.df$name)
}



# get the list of all edges between orgs and action situations
all.edges <- net.props.reduced %>% 
  activate(edges) %>%
  as_tibble()

all.edges.wnames <- all.edges %>% 
  left_join(., gnodes, by = c("from" = "internalNodeId")) %>%
  mutate(fromName = name, fromType = nodeType) %>%
  dplyr::select(-name, -nodeType) %>%
  left_join(., gnodes, by = c("to" = "internalNodeId")) %>%
  mutate(toName = name, toType = nodeType) %>%
  dplyr::select(-name, -nodeType)

org.to.as.edges <- all.edges.wnames %>% 
  dplyr::filter(fromType == "org" & toType == "actionSit")



# calculates the number and proportion of 
# "actors jointly coordinating in an action situation"
# for example, if actor A participates in action situation X, 
# how many actors with an edge to actor A also participate in situation X?
# calculate both the count and proportion. 
# Thanks to Matt Hamilton at OSU for the idea 
# 

# start with all modes in the multiplex network, then do one-by-one
# i don't like repeating so much code, and
# there's probably an easier way to write a generic function that 
# can both be used to map over the tibble AND bring in the network,
# but I'd rather get the analysis done than make this super elegant

f_calculate_joint_collabs <- function(...){
  current <- tibble(...)
  
  asit.subgraph <- net.props.reduced %>% f_calc_ego_subgraph(., current$to)
  actor.subgraph <- net.props.reduced %>% f_calc_ego_subgraph(., current$from)
  asit.nodeNames <- asit.subgraph %>% f_get_nodes_as_list(., current$toName)
  actor.nodeNames <- actor.subgraph %>% f_get_nodes_as_list(., current$fromName)
  
  collab.intersection = actor.nodeNames %in% asit.nodeNames
  
  collab.counts = sum(collab.intersection)
  collab.proportion = sum(collab.intersection) / length(actor.nodeNames)
  
  current$collabCounts = collab.counts
  current$collabProportion = collab.proportion
  
  current <- current %>% mutate(collabProportion = 
                                  ifelse(is.nan(collabProportion), 0,
                                         collabProportion))
  
  return(current)
}

el.joint.collabs <- org.to.as.edges %>% 
  pmap_dfr(f_calculate_joint_collabs)


### Info sharing

net.props.reduced.info <- net.props.reduced %>%
  activate(edges) %>% 
  dplyr::filter(edgeDesc == "info_sharing" |
                  edgeDesc == "issueEng")

# calculates join collaborators using information sharing mode
f_calculate_joint_collabs_info <- function(...){
  current <- tibble(...)
  
  asit.subgraph <- net.props.reduced.info %>% f_calc_ego_subgraph(., current$to)
  actor.subgraph <- net.props.reduced.info %>% f_calc_ego_subgraph(., current$from)
  asit.nodeNames <- asit.subgraph %>% f_get_nodes_as_list(., current$toName)
  actor.nodeNames <- actor.subgraph %>% f_get_nodes_as_list(., current$fromName)
  
  collab.intersection = actor.nodeNames %in% asit.nodeNames
  
  collab.counts = sum(collab.intersection)
  collab.proportion = sum(collab.intersection) / length(actor.nodeNames)
  
  current$collabCountsInfo = collab.counts
  current$collabProportionInfo = collab.proportion
  
  current <- current %>% mutate(collabProportionInfo = 
                                  ifelse(is.nan(collabProportionInfo), 0,
                                         collabProportionInfo))
  
  return(current)
}

el.joint.collabs.info <- org.to.as.edges %>% 
  pmap_dfr(f_calculate_joint_collabs_info)


### financial exchange

net.props.reduced.fin <- net.props.reduced %>%
  activate(edges) %>% 
  dplyr::filter(edgeDesc == "financial_ex" |
                  edgeDesc == "issueEng")

# calculates join collaborators using financial exchange mode
f_calculate_joint_collabs_fin <- function(...){
  current <- tibble(...)
  
  asit.subgraph <- net.props.reduced.fin %>% f_calc_ego_subgraph(., current$to)
  actor.subgraph <- net.props.reduced.fin %>% f_calc_ego_subgraph(., current$from)
  asit.nodeNames <- asit.subgraph %>% f_get_nodes_as_list(., current$toName)
  actor.nodeNames <- actor.subgraph %>% f_get_nodes_as_list(., current$fromName)
  
  collab.intersection = actor.nodeNames %in% asit.nodeNames
  
  collab.counts = sum(collab.intersection)
  collab.proportion = sum(collab.intersection) / length(actor.nodeNames)
  
  current$collabCountsFin = collab.counts
  current$collabProportionFin = collab.proportion
  
  current <- current %>% mutate(collabProportionFin = 
                                  ifelse(is.nan(collabProportionFin), 0,
                                         collabProportionFin))
  
  return(current)
}

el.joint.collabs.fin <- org.to.as.edges %>% 
  pmap_dfr(f_calculate_joint_collabs_fin)


### project coordination

net.props.reduced.proj <- net.props.reduced %>%
  activate(edges) %>% 
  dplyr::filter(edgeDesc == "proj_coord" |
                  edgeDesc == "issueEng")

# calculates join collaborators using project coordination mode
f_calculate_joint_collabs_proj <- function(...){
  current <- tibble(...)
  
  asit.subgraph <- net.props.reduced.proj %>% f_calc_ego_subgraph(., current$to)
  actor.subgraph <- net.props.reduced.proj %>% f_calc_ego_subgraph(., current$from)
  asit.nodeNames <- asit.subgraph %>% f_get_nodes_as_list(., current$toName)
  actor.nodeNames <- actor.subgraph %>% f_get_nodes_as_list(., current$fromName)
  
  collab.intersection = actor.nodeNames %in% asit.nodeNames
  
  collab.counts = sum(collab.intersection)
  collab.proportion = sum(collab.intersection) / length(actor.nodeNames)
  
  current$collabCountsProj = collab.counts
  current$collabProportionProj = collab.proportion
  
  current <- current %>% mutate(collabProportionProj = 
                                  ifelse(is.nan(collabProportionProj), 0,
                                         collabProportionProj))
  
  return(current)
}

el.joint.collabs.proj <- org.to.as.edges %>% 
  pmap_dfr(f_calculate_joint_collabs_proj)


### reporting

net.props.reduced.rep <- net.props.reduced %>%
  activate(edges) %>% 
  dplyr::filter(edgeDesc == "report" |
                  edgeDesc == "issueEng")

# calculates join collaborators using reporting mode
f_calculate_joint_collabs_rep <- function(...){
  current <- tibble(...)
  
  asit.subgraph <- net.props.reduced.rep %>% f_calc_ego_subgraph(., current$to)
  actor.subgraph <- net.props.reduced.rep %>% f_calc_ego_subgraph(., current$from)
  asit.nodeNames <- asit.subgraph %>% f_get_nodes_as_list(., current$toName)
  actor.nodeNames <- actor.subgraph %>% f_get_nodes_as_list(., current$fromName)
  
  collab.intersection = actor.nodeNames %in% asit.nodeNames
  
  collab.counts = sum(collab.intersection)
  collab.proportion = sum(collab.intersection) / length(actor.nodeNames)
  
  current$collabCountsRep = collab.counts
  current$collabProportionRep = collab.proportion
  
  current <- current %>% mutate(collabProportionRep = 
                                  ifelse(is.nan(collabProportionRep), 0,
                                         collabProportionRep))
  
  return(current)
}

el.joint.collabs.rep <- org.to.as.edges %>% 
  pmap_dfr(f_calculate_joint_collabs_rep)


### technical assistance

net.props.reduced.ta <- net.props.reduced %>%
  activate(edges) %>% 
  dplyr::filter(edgeDesc == "tech_assist" |
                  edgeDesc == "issueEng")

# calculates join collaborators using technical assistance mode
f_calculate_joint_collabs_ta <- function(...){
  current <- tibble(...)
  
  asit.subgraph <- net.props.reduced.ta %>% f_calc_ego_subgraph(., current$to)
  actor.subgraph <- net.props.reduced.ta %>% f_calc_ego_subgraph(., current$from)
  asit.nodeNames <- asit.subgraph %>% f_get_nodes_as_list(., current$toName)
  actor.nodeNames <- actor.subgraph %>% f_get_nodes_as_list(., current$fromName)
  
  collab.intersection = actor.nodeNames %in% asit.nodeNames
  
  collab.counts = sum(collab.intersection)
  collab.proportion = sum(collab.intersection) / length(actor.nodeNames)
  
  current$collabCountsTech = collab.counts
  current$collabProportionTech = collab.proportion
  
  current <- current %>% mutate(collabProportionTech = 
                                  ifelse(is.nan(collabProportionTech), 0,
                                         collabProportionTech))
  
  return(current)
}

el.joint.collabs.ta <- org.to.as.edges %>% 
  pmap_dfr(f_calculate_joint_collabs_ta)


### calc'd edge weights ----

# in tibbles
#el.joint.collabs
#el.joint.collabs.info
#el.joint.collabs.fin
#el.joint.collabs.proj
#el.joint.collabs.rep
#el.joint.collabs.ta

# quick helper function for data clean
f_easyselect <- function(a.tibble){
  toReturn <- a.tibble %>% 
    dplyr::select(fromName, toName, starts_with("collab"))
  return(toReturn)
}

# get just the to, from, and collaboration metrics
eljc.all <- el.joint.collabs %>% f_easyselect()
eljc.info <- el.joint.collabs.info %>% f_easyselect()
eljc.fin <- el.joint.collabs.fin %>% f_easyselect()
eljc.proj <- el.joint.collabs.proj %>% f_easyselect()
eljc.rep <- el.joint.collabs.rep %>% f_easyselect()
eljc.ta <- el.joint.collabs.ta %>% f_easyselect()

# join them all in a big table such that we have every actor-AS pair
# and for each mode
eljc.joined <- eljc.all %>% 
  left_join(., eljc.info, by = c("fromName", "toName")) %>%
  left_join(., eljc.fin, by = c("fromName", "toName")) %>%
  left_join(., eljc.proj, by = c("fromName", "toName")) %>%
  left_join(., eljc.rep, by = c("fromName", "toName")) %>%
  left_join(., eljc.ta, by = c("fromName", "toName"))




### bipartite graph ----
# making the bipartite graph was a pain, and I couldn't get it to work
# in tidygraph - it wouldn't treat it as a bipartite. This is a workaround
tdf <- eljc.joined %>% dplyr::select(fromName, toName) %>% table()
g <- graph.incidence(tdf, directed = T)
is.bipartite(g)
is.directed(g)

# NOW use tidygraph
net.bipart <- as_tbl_graph(g, directed = T) %>% 
  activate(nodes) %>%
  mutate(type = ifelse(str_detect(name, "eng."), F, T)) %>%
  mutate(nodeType = as.factor(ifelse(str_detect(name, "eng."), "actionSit", "org")))
is.bipartite(net.bipart)
is.directed(net.bipart)


# ok, we have a bipartite graph
# let's join the edge properties back

### add edge properties ----
lt.nodes <- net.bipart %>%
  activate(nodes) %>%
  as_tibble() %>%
  mutate(quickId = seq(1:nrow(.)))

# cleanup edge properties and make a lookup table
lt.big <- eljc.joined %>%
  left_join(., lt.nodes, by = c("fromName" = "name")) %>%
  mutate(fromId = quickId) %>%
  dplyr::select(-quickId, -type, -nodeType) %>%
  left_join(., lt.nodes, by = c("toName" = "name")) %>%
  mutate(toId = quickId) %>%
  dplyr::select(-quickId, -type, -nodeType)

# add edge properties to a network
net.bipart.edgeprops <- net.bipart %>%
  activate(edges) %>%
  left_join(., lt.big, by = c("from" = "fromId", "to" = "toId")) %>%
  dplyr::select(-fromName, -toName) %>%
  dplyr::filter(!is.na(collabCounts))


### add node properties ----

# Action situation properties coded by hand, 
# org/actor properties from survey

# org/actor properties

df.props.orgs <- nn.all %>%
  dplyr::select(name = alterFullName, starts_with("work."), scale) %>%
  group_by(name, scale) %>%
  summarise(work.wastewater = max(work.wastewater),
            work.forestry = max(work.forestry),
            work.rivercorr = max(work.rivercorr),
            work.ag = max(work.ag),
            work.dev = max(work.dev),
            work.sw = max(work.sw),
            work.floodhaz = max(work.floodhaz)) %>%
# above summarise is b/c we have multiple responses from some actors
  mutate(scale = ifelse(scale %in% c("municipality", "watershed"), "subbasin", "basinorlarger"))

# action situations properties

df.props.asits <- tibble(
  name = c("eng.cwacs", "eng.legnatres",
           "eng.legag", "eng.greeninfracollab", 
           "eng.munisw", "eng.tbp",
           "eng.vtcwn", "eng.watershedalliance"),
  work.wastewater = c(1, 1, 0, 0, 0, 1, 1, 1),
  work.forestry = c(1, 1, 1, 0, 0, 1, 1, 0),
  work.rivercorr = c(1, 1, 0, 0, 0, 1, 0, 1),
  work.ag = c(1, 0, 1, 0, 0, 1, 1, 1),
  work.dev = c(1, 0, 0, 1, 1, 1, 1, 0),
  work.sw = c(1, 1, 0, 1, 1, 1, 1, 1),
  work.floodhaz = c(1, 0, 1, 1, 1, 1, 0, 0),
  scale = c("subbasin", "basinorlarger", 
            "basinorlarger", "basinorlarger", 
            "subbasin", "subbasin", 
            "basinorlarger", "basinorlarger")
)


# put them together
df.props.bound <- bind_rows(df.props.orgs, df.props.asits) %>%
  mutate(work.wastewater = as.logical(work.wastewater),
         work.forestry = as.logical(work.forestry),
         work.rivercorr = as.logical(work.rivercorr),
         work.ag = as.logical(work.ag),
         work.dev = as.logical(work.dev),
         work.sw = as.logical(work.sw),
         work.floodhaz = as.logical(work.floodhaz))

# grab the degree centrality figures from earlier
df.degCent <- net.props.reduced %>%
  activate(nodes) %>%
  as_tibble() %>%
  dplyr::select(name, deg.cent)

# make a bipartite graph with all the properties joined to it
net.bipart.props <- net.bipart.edgeprops %>%
  activate(nodes) %>%
  left_join(., df.props.bound, by = "name") %>%
  left_join(., df.degCent, by = "name") %>%
  mutate(#type = as.character(type), 
         nodeType = as.character(nodeType))

### cut-off parameter ----

# a parameter to cutoff less central nodes. gets rid of small orgs, 
# aids in making the ergm tenable
degreeCutoffParam <- 55

net.bipart.props.trimmed <- net.bipart.props %>% 
  activate(nodes) %>%
  dplyr::filter(nodeType == "actionSit" | deg.cent > degreeCutoffParam) %>%
  mutate(is_actor = ifelse(nodeType == "org", T, F))



### plot the bipartite graph
net.bipart.props.trimmed %>% 
  ggraph(layout = "bipartite") +
  geom_edge_link(alpha = 0.3, 
                 arrow = arrow(angle = 30, length = unit(0.1, "inches"),
                               ends = "last", type = "closed")) +
  geom_node_point(aes(colour = nodeType, shape = as.factor(scale)), size = 3) +
  #geom_node_text(aes(label = name)) +
  scale_color_brewer(palette = "Set1") +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14),
        strip.text.x = element_text(size = 12))

### convert to a Network object ----


# how many actors?
n_actors <- length(V(net.bipart.props.trimmed)) - 8



# make it
net.bipartite.asnet <- net.bipart.props.trimmed %>% 
  intergraph::asNetwork(bipartite_col = "is_actor")

# intergraph doesn't support bipartite graphs, so do it manually
net.bipartite.asnet$gal$bipartite <- n_actors
net.bipartite.asnet$gal$directed <- F


# ERGMs ----



## Bipartite ----


### scale, function, collab ----


edgelist.all <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCounts) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCounts) %>%
  dplyr::select(-from) %>%
  as.matrix()

edgelist.all[!is.na(edgelist.all)] <- 1
edgelist.all[is.na(edgelist.all)] <- 0

# reorder the columns just to be sure
col.order <- colnames(edgelist.all) %>% sort()
edgelist.all[, col.order]


m.collabs.all <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCounts) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCounts) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.all[is.na(m.collabs.all)] <- 0



m.collabs.info <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsInfo) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsInfo) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.info[is.na(m.collabs.info)] <- 0


m.collabs.fin <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsFin) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsFin) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.fin[is.na(m.collabs.fin)] <- 0


m.collabs.proj <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsProj) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsProj) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.proj[is.na(m.collabs.proj)] <- 0


m.collabs.rep <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsRep) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsRep) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.rep[is.na(m.collabs.rep)] <- 0


m.collabs.ta <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsTech) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsTech) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.ta[is.na(m.collabs.ta)] <- 0



m.collabs.ta <- m.collabs.ta * 1.000

m.collabs.ta.list <- t(m.collabs.ta) %>% as.list() %>% unlist()
m.collabs.ta.list.reduced <- m.collabs.ta.list[!is.na(m.collabs.ta.list)] 



network::set.edge.value(net.bipartite.asnet, "m.collabs.ta.ea", m.collabs.ta.list.reduced)
net.bipartite.asnet 
network::get.edge.value(net.bipartite.asnet, "m.collabs.ta.ea")
network::get.edges(net.bipartite.asnet, "1")


# b1 is actor
# b2 is action situation

#### MANUSCRIPT ergm #1 ----


em.scale.function.mx <- ergm(net.bipartite.asnet ~ edges + 
                               gwb1degree(decay = 3.2, fixed = T) +
                               gwb2degree(decay = 4.5, fixed = T) +
                               #gwb2degree() +
                               #gwesp(decay = 0.8, fixed = T) +
                               #nodecov("deg.cent") +
                               b1factor("scale") +
                               b2factor("scale") +
                               nodematch("scale") +
                               ## actor factors
                               #b1factor("work.wastewater") +
                               #b1factor("work.forestry") +
                               #b1factor("work.rivercorr") +
                               #b1factor("work.ag") +
                               #b1factor("work.dev") +
                               #b1factor("work.sw") +
                               ## action situation factors
                               #b2factor("work.wastewater") +
                               #b2factor("work.forestry") +
                               #b2factor("work.rivercorr") +
                             b2factor("work.ag") +
                               b2factor("work.dev") +
                               # b2factor("work.sw") +
                               #nodematch("work.wastewater") +
                               #nodematch("work.forestry") +
                               #nodematch("work.rivercorr") +
                               nodematch("work.ag") +
                               nodematch("work.dev") , 
                             #nodematch("work.sw") +
                             #edgecov(m.collabs.all) +
                             #edgecov(m.collabs.info) +
                             # edgecov(m.collabs.fin) +
                             #edgecov(m.collabs.proj) +
                             #edgecov(m.collabs.rep) +
                             #edgecov(m.collabs.ta),
                             edgeverbose = T,
                             control = control.ergm(MCMC.burnin = 3500,
                                                    MCMC.samplesize = 7000,
                                                    MCMC.interval = 7000,
                                                    MCMLE.maxit = 20,
                                                    drop = T,
                                                    seed = 8675309,
                                                    parallel = 4))

summary(em.scale.function.mx)

#mcmc.diagnostics(em.scale.function.jointcollabs)
em.scale.function.mx$mle.lik
logLik(em.scale.function.mx)

gof(em.scale.function.mx, GOF = ~model) %>% plot()


lapply(em.scale.function.mx[1], exp)
em.scale.function.mx


sum.bip <- summary(em.scale.function.mx)
sum.bip.out <- sum.bip$coefficients %>% as_tibble()
sum.bip.out$var <- rownames(sum.bip$coefficients)
sum.bip.out %>% 
  mutate(est_std = paste0(round(Estimate, 2), " (", round(`Std. Error`, 2), ")")) %>%
  write_csv(., "bip_output.csv")



#### MANUSCRIPT ergm #2 ----
em.scale.function.jointcollabs <- ergm(net.bipartite.asnet ~ edges + 
                                         gwb1degree(decay = 3.2, fixed = T) +
                                         gwb2degree(decay = 4.5, fixed = T) +
                                         #gwb2degree(decay = .4, fixed = T) +
                                         #gwesp(decay = .1, fixed = T) +
                                         #nodecov("deg.cent") +
                                         b1factor("scale") +
                                         b2factor("scale") +
                                         nodematch("scale") +
                                         ## actor factors
                                         #b1factor("work.wastewater") +
                                         #b1factor("work.forestry") +
                                         #b1factor("work.rivercorr") +
                                         #b1factor("work.ag") +
                                         #b1factor("work.dev") +
                                         #b1factor("work.sw") +
                                         ## action situation factors
                                         #b2factor("work.wastewater") +
                                         #b2factor("work.forestry") +
                                         #b2factor("work.rivercorr") +
                                       b2factor("work.ag") +
                                         b2factor("work.dev") +
                                         # b2factor("work.sw") +
                                         #nodematch("work.wastewater") +
                                         #nodematch("work.forestry") +
                                         #nodematch("work.rivercorr") +
                                         nodematch("work.ag") +
                                         nodematch("work.dev") + 
                                         #nodematch("work.sw") +
                                         #edgecov(m.collabs.all) +
                                         edgecov(m.collabs.info) +
                                         edgecov(m.collabs.fin) +
                                         edgecov(m.collabs.proj) +
                                         edgecov(m.collabs.rep) +
                                         edgecov(m.collabs.ta),
                                       edgeverbose = T,
                                       control = control.ergm(MCMC.burnin = 3500,
                                                              MCMC.samplesize = 7000,
                                                              MCMC.interval = 7000,
                                                              MCMLE.maxit = 20,
                                                              drop = T,
                                                              seed = 8675309,
                                                              parallel = 4))

summary(em.scale.function.jointcollabs)

#mcmc.diagnostics(em.scale.function.jointcollabs)
em.scale.function.jointcollabs$loglikelihood
em.scale.function.jointcollabs$mle.lik
gof(em.scale.function.jointcollabs, GOF = ~model) %>% plot()


lapply(em.scale.function.jointcollabs[1], exp)
em.scale.function.jointcollabs


sum.bip <- summary(em.scale.function.jointcollabs)
sum.bip.out <- sum.bip$coefficients %>% as_tibble()
sum.bip.out$var <- rownames(sum.bip$coefficients)
sum.bip.out %>% 
  mutate(est_std = paste0(round(Estimate, 2), " (", round(`Std. Error`, 2), ")")) %>%
  write_csv(., "bip_output_collabs.csv")



