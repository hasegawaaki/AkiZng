install.packages("rlang")

# Load libraries
library(igraph)
library(ggplot2)
library(intergraph)
library(ergm)
library(network)
library(blockmodels)
library(mclust)

# Load data
edges <- read.csv("edges.csv")
attributes <- read.csv("attributes.csv")

# Create directed network graph
g <- graph_from_data_frame(edges, directed = TRUE, vertices = attributes)

# Q1: Plot the network
set.seed(123)
plot(g,
     vertex.label = NA,
     vertex.size = 5,
     edge.arrow.size = 0.3,
     main = "Fish Interaction Network",
     vertex.color = ifelse(V(g)$Habitat == "reef", "lightblue", "coral"))
# The network is fairly dense, indicating that many species interact with one another.

# Summary of node attributes
summary(g)
# Nodes: 145(each node represents a fish species)
# Edges: 2694 (directed interactions between species)

table(attributes$ActivityPeriod)
# diurnal: 75 nodes, nocturnal: 70 nodes
# The near-equal distribution of diurnal/nocturnal species suggests that activity timing  could still shape sub-network interactions.

table(attributes$Habitat)
# reef: 68 nodes, seagrass: 77 nodes
# The network spans across both habitat types fairly evenly, as indicated by the nearly equal split.

table(attributes$FeedingGuild)
# piscivores: 77, zoobenthivores: 34, zooplanktivorous: 34
# Piscivores form the largest group.

# Transitivity and reciprocity
transitivity(g, type = "global")
# 0.2064007
# There's a moderate tendency for species that interact with the same others to also interact with each other.
# This suggests that while small local interaction loops happen, the broader structure is more diffuse.

reciprocity(g)
# 0.3132888
# reciprocal relationships are not dominant but still significant. 
# In ecological terms, this might reflect predator-prey loops or mutualistic behaviors

# Degree distributions
in_deg <- degree(g, mode = "in")
out_deg <- degree(g, mode = "out")

# Plot degree distributions
hist(in_deg, breaks = 20, main = "In-Degree Distribution", col = "steelblue", xlab = "In-Degree")
hist(out_deg, breaks = 20, main = "Out-Degree Distribution", col = "tomato", xlab = "Out-Degree")
# Both in and out degrees cluster around 18, with maxima around 30–34
# The histograms imply that most species both receive and send a similar number of interactions.

# Centrality measures
bet  <- betweenness(g, directed = TRUE)
clo  <- closeness(g, mode = "out")
eigv <- evcent(g)$vector
cent <- data.frame(
  node = V(g)$name,
  InDegree  = in_deg,
  OutDegree = out_deg,
  Betweenness = bet,
  Closeness   = clo,
  Eigenvector = eigv,
  stringsAsFactors = FALSE
)
cat("Top 5 by betweenness:\n")
print(head(cent[order(-cent$Betweenness), ], 5))
cat("Top 5 by closeness:\n")
print(head(cent[order(-cent$Closeness), ], 5))
cat("Top 5 by eigenvector:\n")
 
  # Node 22 consistently ranks high across betweenness and closeness, suggesting it's a key bridge and can quickly reach others—likely facilitates connecting groups and spreading information.
# Node 58 tops the eigenvector ranking and appears in the betweenness top five too, meaning it may not lie on the shortest paths between others but is strongly embedded within a highly connected region of the network.
# Nodes 31 and 17 show up in closeness and eigenvector lists, indicating they’re highly accessible species and embedded in influential parts of the network.
# This can guide future ecological monitoring, suggesting which species might best reflect the health of the broader system or which could cause trophic imbalance if disrupted.

# Assortativity tests
# Habitat assortativity is strongly negative (-0.735), suggesting significant cross-habitat interaction between species from seagrass beds and the main reef.
# Feeding Guild (-0.016): Near zero. Feeding type doesn’t seem to influence who interacts with whom,suggesting interactions aren’t structured around diet similarity.
# Activity Period assortativity is strongly positive (0.757). Species with the same activity period (diurnal or nocturnal) tend to interact much more with each other. This is expected, since species active at the same time are more likely to encounter each other.


# Q2: Step 1: Convert igraph to network object for statnet
net <- asNetwork(g)

# Clean attribute names
network::set.vertex.attribute(net, "Habitat", attributes$Habitat)
network::set.vertex.attribute(net, "Feeding_Guild", attributes$FeedingGuild)
network::set.vertex.attribute(net, "Activity_Period", attributes$ActivityPeriod)

# Fit null model (only edges term)
model_null <- ergm(net ~ edges)
summary(model_null)

# Fit full ERGM model
model_full <- ergm(net ~ edges + mutual +
                     nodematch("Habitat") +
                     nodematch("Feeding_Guild") +
                     nodematch("Activity_Period"))

summary(model_full)
# edges estimate dropped from -1.91 to -2.84. Once we account for actual structure in the network, the baseline tie probability drops, any two fish are even less likely to interact by sheer chance.
# Mutuality is not significant (0.02, p = 0.76).There’s no strong evidence that interactions are typically reciprocated.Its presence in the model doesn’t improve fit and may be considered for removal.
# Habitat homophily is significantly negative (-2.24, p < 0.001).Species in the same habitat are less likely to interact—this aligns with the earlier negative assortativity.
# Feeding Guild is not significant (0.04, p = 0.37).Similar diets don’t predict interaction, so feeding roles cut straight across the network.
# Activity Period is a strong positive predictor (2.37, p < 0.001). This is the most pronounced effect in the model. Species active at the same time (diurnal/nocturnal) are far more likely to interact.
# Residual deviance fell from 16058 to 12082, and AIC dropped substantially from 16,060 to 12,092—indicating a better fit.
# Insignificant terms (mutual, Feeding_Guild) can be dropped in the revised model to reduce complexity.

# Step 2: Fit refined model
model_refined <- ergm(net ~ edges + 
                        nodematch("Habitat") + 
                        nodematch("Activity_Period"))
summary(model_refined)
# The AIC dropped slightly from 12092 to 12089, showing a minor improvement in fit despite using fewer terms。
# The effect sizes and significance remain consistent, confirming that Activity_Period has a strong positive impact on tie formation, while Habitat continues to show a strong negative effect.
# The decision to remove weak terms is supported by the fact that doing so did not impair model fit.
# The model now clearly tells us that species active at the same time are more likely to interact, while those from the same habitat are less likely.
# The baseline probability of a tie (edges ≈ –2.83) remains low, as expected in a sparse biological network.

# Step 3: Run goodness-of-fit diagnostics
gof_results <- gof(model_refined)
plot(gof_results)
# The model’s simulated networks closely match the observed minimum geodesic distances.
# This suggests the model captures the network’s overall connectivity, such as how efficiently interactions propagate.
# In short, the refined model quite well replicates the global reachability pattern.

# Q3: Step 1: Create undirected graph for modularity
net <- graph_from_data_frame(edges, directed = FALSE, vertices = attributes)

# Apply Louvain community detection
louvain_comm <- cluster_louvain(net)
# The Louvain algorithm is a modularity-based approach that iteratively groups nodes to maximise modularity, a measure of how well a network divides into communities with dense internal connections and sparse external links.

# Assign communities to nodes
V(net)$louvain_group <- louvain_comm$membership

# Modularity score
modularity_score <- modularity(net, V(net)$louvain_group)
cat("Modularity Score:", round(modularity_score, 2), "\n")
# Modularity Score: 0.38. Significant clustering is shown by values greater than 0.3.
# This confirms meaningful community structure in the fish interaction network.

# Compare communities with node attributes
cross_tab <- table(V(net)$louvain_group, V(net)$Habitat)
print("Community vs. Habitat:")
print(cross_tab)
#    reef seagrass
# 1   35       40
# 2   33       37
# The communities include a mix of both habitat types, and the split is fairly balanced in each group.
# This suggests that habitat is not the main driver behind the modular structure.

# Plot communities
set.seed(123)
plot(louvain_comm, net, 
     vertex.color = V(net)$louvain_group, 
     vertex.label = NA,
     vertex.size = 5,
     main = "Louvain Communities Colored by Habitat")
# Nodes from different habitats (reef = yellow, seagrass = blue) are clustered based on their community assignment, not habitat.
# Each community has a visible density of black edges, whereas red edges connecting across them.
# The intra-community links dominate, which is what the Louvain method is designed to maximise.
# The cross-habitat mixing shows that fish species tend to form cross-environmental clusters, likely due to behavioural or ecological factors not captured by habitat alone.
# These results demonstrate that although there is modular structure in the fish network, habitat divison is not the primary driver of this structure. 
# Other factors, such as activity period or interaction frequency, are probably in effect instead.

# Step 2: Create adjacency matrix
adj_mat <- as.matrix(as_adjacency_matrix(net, sparse = FALSE))

# Fit SBM for binary, undirected network
sbm_model <- BM_bernoulli("SBM", adj_mat)
sbm_model$estimate()

# Number of blocks selected
best_model <- sbm_model$memberships[[which.max(sbm_model$ICL)]]
block_assignments <- best_model$Z
num_blocks <- ncol(block_assignments)
cat("Number of blocks:", num_blocks, "\n")
# Number of blocks: 4 
# The model identified 4 distinct blocks in the network based on the ICL criterion. 

# Step 3 & 4: Block Connectivity Matrix
block_matrix <- sbm_model$model_parameters[[which.max(sbm_model$ICL)]]$pi
print(block_matrix)
#            [,1]       [,2]       [,3]       [,4]
# [1,] 0.13141113 0.79719197 0.01403933 0.10537861
# [2,] 0.79719197 0.13199736 0.11431709 0.01965986
# [3,] 0.01403933 0.11431709 0.11178664 0.76036268
# [4,] 0.10537861 0.01965986 0.76036268 0.12016440
# There’s a strong reciprocal connection between Blocks 1 and 2 (0.797), suggesting these two sets of species engage heavily across group boundaries.
# Strong mutual interaction (0.76) is also seen in Blocks 3 and 4, which mirrors Blocks 1–2 dynamics.
# Within-block probabilities are moderate to low (~0.11–0.13), meaning nodes in the same block aren’t necessarily forming dense cliques.
# Very low tie probabilities between Blocks 1/2 and 3/4 suggest that the two block pairs operate quite separately from each other.

# Convert soft assignment to hard
assigned_blocks <- apply(block_assignments, 1, which.max)

# Assign to graph object
V(net)$sbm_group <- assigned_blocks

# Plot network coloured by SBM group
plot(net,
     vertex.color = V(net)$sbm_group,
     vertex.label = NA,
     vertex.size = 5,
     main = "Network Coloured by SBM Blocks")
# Nodes within the same region (e.g. orange & blue, or green & yellow) are highly interconnected. This aligns with the π matrix.
# Few connections within blocks (diagonal probabilities ~0.12–0.13), consistent with low intra-group interaction.

# Compare blocks to covariates with tables
table(Block = assigned_blocks, Habitat = V(net)$Habitat)
#      Habitat
# Block reef seagrass
# 1    0       40
# 2   35        0
# 3    0       37
# 4   33        0
# SBM blocks perfectly separate reef and seagrass species.
# Blocks 1 and 3 are entirely seagrass, while Blocks 2 and 4 are entirely reef.
# This suggests that habitat use plays a defining role in shaping how species connect.

table(Block = assigned_blocks, Activity = V(net)$ActivityPeriod)
#      Activity
# Block diurnal nocturnal
# 1      40         0
# 2      35         0
# 3       0        37
# 4       0        33
# SBM blocks perfectly separate diurnal and nocturnal.
# Blocks 1 & 2 are fully diurnal, Blocks 3 & 4 are fully nocturnal.
# This indicates that activity period is a dominant feature in structuring interaction.

table(Block = assigned_blocks, Feeding = V(net)$FeedingGuild)
#      Feeding
# Block piscivores zoobenthivores zooplanktivorous
# 1         19              8               13
# 2         21             10                4
# 3         22              5               10
# 4         15             11                7
# The distribution is more mixed. No single block is dominated by a specific feeding guild.
# Feeding strategy is not a major structural determinant of interactions in this network. 
# Fish with different diets still interact widely across trophic lines.

# Convert traits to numeric groupings
habitat_numeric <- as.numeric(as.factor(V(net)$Habitat))
activity_numeric <- as.numeric(as.factor(V(net)$ActivityPeriod))
feeding_numeric <- as.numeric(as.factor(V(net)$FeedingGuild))

# ARI comparisons
ari_habitat <- adjustedRandIndex(assigned_blocks, habitat_numeric)
ari_activity <- adjustedRandIndex(assigned_blocks, activity_numeric)
ari_feeding <- adjustedRandIndex(assigned_blocks, feeding_numeric)

cat("ARI - Habitat:", ari_habitat, "\n")
# ARI - Habitat: 0.4944237
# This suggests a moderate to strong alignment between SBM block assignments and habitat types (reef vs seagrass).
# While not a perfect match, it supports each block mostly consists of either reef or seagrass species.
# Habitat is an important factor shaping how species interact in the network.

cat("ARI - Activity:", ari_activity, "\n")
# ARI - Activity: 0.4964229
# This confirms that time of activity (diurnal vs nocturnal) is just as influential in determining network structure as habitat.
# Diurnal and nocturnal species were clearly separated into different blocks.
# The timing of species activity plays a major role in shaping interaction opportunities.

cat("ARI - Feeding Guild:", ari_feeding, "\n")
# ARI - Feeding Guild: 0.00384766
# This value is extremely low, meaning almost no alignment between SBM blocks and feeding type.
# Piscivores, zoobenthivores, and zooplanktivores are mixed within blocks, indicating no guild-based clustering.
# Diet does not play a structuring role in this social network.

# Step 5: Compare SBM blocks to Louvain partition
table(SBM = assigned_blocks, Louvain = V(net)$louvain_group)
#    Louvain
# SBM  1  2
# 1  40  0
# 2  35  0
# 3  0  37
# 4  0  33
# SBM Blocks 1 & 2 align entirely with Louvain Community 1 (seagrass and reef species).
# SBM Blocks 3 & 4 align entirely with Louvain Community 2 (seagrass and reef species).
# Both methods split the network into reef and seagrass groups.
# SBM further divides Louvain communities into diurnal (Blocks 1–2) and nocturnal (Blocks 3–4) subgroups.
# Structural Differences via π Matrix
# High cross-block probabilities between diurnal reef/seagrass (Blocks 1–2: 0.797) and nocturnal reef/seagrass (Blocks 3–4: 0.760).
# Low within-block probabilities (~0.12–0.13), indicating sparse intra-group interactions.
# Louvain identifies habitat-driven communities (reef vs. seagrass) but ignores temporal activity.
# SBM captures more meaningful layers in the network, showing diurnal/nocturnal subgroups within habitats.
# SBM provides a better structure for ecologically interpretation, While Louvain gives a quick overview of broad structure.
# SBM distinguishes structural roles within groups.
# SBM produces blocks that align with known biological traits (Habitat and Activity Period).
# SBM reflects the statistical patterns in how species interact.
# SBM aligns directly with π matrix and covariate tables.
