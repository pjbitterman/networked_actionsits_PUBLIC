#' public-facing analysis for networked action situation manuscript
#' @pjbitterman
#' created 2021-01-23
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

# ERGM would not differentiate by edge properties using a tidygraph
# object - what DID seem to work is converting it to a Network
# data structure, and going from there. However, intergraph::
# doesn't support bipartite graphs, so there's some manual work below

# how many actor?
n_actors <- length(V(net.bipart.props.trimmed)) - 8

# make it
net.bipartite.asnet <- net.bipart.props.trimmed %>% 
  intergraph::asNetwork(bipartite_col = "is_actor")

# intergraph doesn't support bipartite graphs, so do it manually
net.bipartite.asnet$gal$bipartite <- n_actors
net.bipartite.asnet$gal$directed <- F

### RUN ERGMS ----

#### scale only ----

summary((net.bipartite.asnet ~ edges + 
           #gwdegree() + 
           gwb1degree() +
           #gwb2degree() +
           b1cov("deg.cent") + 
           #b1factor("scale", levels = T) + # skipped???
           #b2factor("scale", levels = T) +
           #nodefactor("scale") +
           nodematch("scale", diff = F)))


em.scale <- ergm(net.bipartite.asnet ~ edges + 
                  gwb1degree() +
                 # gwb2degree() +
                  b1cov("deg.cent") +
                 # b1factor("scale", levels = T) + 
                 # b2factor("scale", levels = T) +
                  nodefactor("scale") +
                  nodematch("scale", diff = F),
                  edgeverbose = T,
                  control = control.ergm(MCMC.burnin = 3500,
                                         MCMC.samplesize = 7000,
                                         MCMC.interval = 7000,
                                         MCMLE.maxit = 20,
                                         drop = T,
                                         seed = 8675309,
                                         parallel = 4))
                                         #resume = "step_018.RData",
                                         #checkpoint = "step_%03d.RData"))
summary(em.scale)
mcmc.diagnostics(em.scale)
em.scale$loglikelihood
plot(gof(em.scale))
lapply(em.scale[1], exp)
em.scale


#### scale and functions ----

em.scale.function <- ergm(net.bipartite.asnet ~ edges + 
                    gwb1degree() +
                    gwb2degree() +
                    b1cov("deg.cent") +
                    #nodecov("deg.cent") +
                    nodefactor("scale") +
                    nodematch("scale") +
                    b2factor("work.wastewater") +
                    b2factor("work.forestry") +
                    b2factor("work.rivercorr") +
                    b2factor("work.ag") +
                    b2factor("work.dev") +
                    b2factor("work.sw") +
                    nodematch("work.wastewater") +
                    nodematch("work.forestry") +
                    nodematch("work.rivercorr") +
                    nodematch("work.ag") +
                    nodematch("work.dev") +
                    nodematch("work.sw"),
                  edgeverbose = T,
                  control = control.ergm(MCMC.burnin = 3500,
                                         MCMC.samplesize = 7000,
                                         MCMC.interval = 7000,
                                         MCMLE.maxit = 20,
                                         drop = T,
                                         seed = 8675309,
                                         parallel = 4))

summary(em.scale.function)
mcmc.diagnostics(em.scale.function)
em.scale.function$loglikelihood
plot(gof(em.scale.function))
lapply(em.scale.function[1], exp)
em.scale.function


#### scale, function, collab (TIDY WAY)----

# this does NOT work

em.scale.function.jointcollabs.tidy <- ergm(net.bipartite.asnet ~ edges + 
                                         nodecov("deg.cent") +
                                         nodefactor("scale") +
                                         nodematch("scale") +
                                         # b2factor("work.wastewater") +
                                         # b2factor("work.forestry") +
                                         # b2factor("work.rivercorr") +
                                         # b2factor("work.ag") +
                                         # b2factor("work.dev") +
                                         # b2factor("work.sw") +
                                         nodematch("work.wastewater") +
                                         nodematch("work.forestry") +
                                         nodematch("work.rivercorr") +
                                         nodematch("work.ag") +
                                         nodematch("work.dev") +
                                         nodematch("work.sw") +
                                         # collaboration metrics
                                         edgecov(net.bipartite.asnet, "collabProportion") +
                                         edgecov(net.bipartite.asnet, "collabCounts"), # this doesn't work
                                       edgeverbose = T,
                                       control = control.ergm(MCMC.burnin = 3500,
                                                              MCMC.samplesize = 7000,
                                                              MCMC.interval = 7000,
                                                              MCMLE.maxit = 20,
                                                              drop = T,
                                                              seed = 8675309,
                                                              parallel = 4))


summary(em.scale.function.jointcollabs.tidy)

mcmc.diagnostics(em.scale.function.jointcollabs.tidy)
em.scale.function.jointcollabs.tidy$loglikelihood
plot(gof(em.scale.function.jointcollabs.tidy))
lapply(em.scale.function.jointcollabs.tidy[1], exp)
em.scale.function.jointcollabs.tidy



#### scale, function, collab ----

##### bunch of matrices -----

# my method of putting the collaboration into the ergm using  
# tidygraph syntax doesn't work
# so create (and use) a bunch of matrices at the last minute
m.collabs.all <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCounts) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCounts) %>%
  dplyr::select(-from) %>%
  as.matrix()

# be sure to preserve order of columns in matrix
col.order <- colnames(m.collabs.all)

# all modes
m.collabs.all <- m.collabs.all[, col.order]
m.collabs.all[is.na(m.collabs.all)] <- 0


# information sharing
m.collabs.info <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsInfo) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsInfo) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.info <- m.collabs.info[, col.order]
m.collabs.info[is.na(m.collabs.info)] <- 0

# financial exchange
m.collabs.fin <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsFin) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsFin) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.fin <- m.collabs.fin[, col.order]
m.collabs.fin[is.na(m.collabs.fin)] <- 0

# project coordination
m.collabs.proj <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsProj) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsProj) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.proj <- m.collabs.proj[, col.order]
m.collabs.proj[is.na(m.collabs.proj)] <- 0

# reporting
m.collabs.rep <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsRep) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsRep) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.rep <- m.collabs.rep[, col.order]
m.collabs.rep[is.na(m.collabs.rep)] <- 0

# technical assistance
m.collabs.ta <- net.bipart.props.trimmed %>% 
  activate(edges) %>%
  as_tibble() %>%
  dplyr::select(from, to, collabCountsTech) %>%
  pivot_wider(., id_cols = from, names_from = to, values_from = collabCountsTech) %>%
  dplyr::select(-from) %>%
  as.matrix()

m.collabs.ta <- m.collabs.ta[, col.order]
m.collabs.ta[is.na(m.collabs.ta)] <- 0




summary((net.bipartite.asnet ~ edges + 
           #gwdegree() + 
           gwb1degree() +
           #gwb2degree() +
           b1cov("deg.cent") + 
           #b1factor("scale", levels = T) + # skipped???
           #b2factor("scale", levels = T) +
           #nodefactor("scale") +
           edgecov(m.collabs.all) +
           edgecov(m.collabs.info) +
           edgecov(m.collabs.fin) +
           edgecov(m.collabs.proj) +
           edgecov(m.collabs.rep) +
           edgecov(m.collabs.ta) +
           #edgecov(net.bipartite.asnet, "collabCounts") +
           nodematch("scale", diff = F)))

mixingmatrix(net.bipartite.asnet, "deg.cent")


### MATT, THIS IS WHERE I'M STRUGGLING ----


### try to run the ergm ---- 
# add the joint collaboration matrices into the model
em.scale.function.jointcollabs <- ergm(net.bipartite.asnet ~ edges + 
                    gwdegree() + 
                    gwb2degree() +
                    #nodecov("deg.cent") +
                    nodefactor("scale") +
                    nodematch("scale") +
                    b2factor("work.wastewater") +
                    #b2factor("work.forestry") +
                    #b2factor("work.rivercorr") +
                    b2factor("work.ag") +
                    b2factor("work.dev") +
                    #b2factor("work.sw") +
                    # nodematch("work.wastewater") +
                    # nodematch("work.forestry") +
                    # nodematch("work.rivercorr") +
                    # nodematch("work.ag") +
                    # nodematch("work.dev") +
                    # nodematch("work.sw") +
                    edgecov(m.collabs.all) +
                    edgecov(m.collabs.info) +
                    edgecov(m.collabs.fin) +
                    edgecov(m.collabs.proj) +
                    edgecov(m.collabs.rep) +
                    edgecov(m.collabs.ta),
                    ### if the next two lines run, the Pr(>|z|) goes to 1
                    ### for everything
                    #edgecov(net.bipartite.asnet, "collabProportion"),
                    #edgecov(net.bipartite.asnet, "collabCounts"), # doesn't work
                  edgeverbose = T,
                  control = control.ergm(MCMC.burnin = 3500,
                                         MCMC.samplesize = 7000,
                                         MCMC.interval = 7000,
                                         MCMLE.maxit = 25,
                                         drop = T,
                                         seed = 8675309,
                                         parallel = 4))

summary(em.scale.function.jointcollabs)

mcmc.diagnostics(em.scale.function.jointcollabs)
em.scale.function.jointcollabs$loglikelihood
plot(gof(em.scale.function.jointcollabs))
lapply(em.scale.function.jointcollabs[1], exp)
em.scale.function.jointcollabs

