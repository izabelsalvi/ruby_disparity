# Script to plot and analyze the RevBayes output on the simulated/sampled 25% tree
##################################################################################
# Set working directory and load libraries
setwd("~/Documents/Princeton/tiago_rotation/clock_models/RevBayes/RB_simulations_fulltree_output_moreGens")
library(RevGadgets)
library(ggplot2)
library(ggtree)

##################################################################################
# Start with RevGadgets 

## Plot MAP tree
tree <- readTrees("rel_mvBM_fulltree_MAP_job2.tre")
plotTree(tree, color_branch_by="branch_rates", tip_labels_size=2)

## Plot mean rates from the .trees file
### Read in the trace
trace <- readTrace("rel_mvBM_fulltree_job2.log", burnin=0.1) 

### Read in the trees
trees <- readTrees("rel_mvBM_fulltree_job2.trees", tree_name="Tree")

### use processBranchData to combine them
data_tree <- processBranchData(trees, trace, burnin=0.1,summary="mean")

### Plot the tree with the mean rates
plotTree(data_tree, color_branch_by="branch_rates", tip_labels_size=2)

### Save the tree object as RDS
saveRDS(data_tree, "results/data_tree_fulltree.rds")

##################################################################################
# Do NOT need to run any of the below code, but I left it in just in case it helps with any double checking/debugging
##################################################################################
# Use ggtree/treeio - ensure it matches RevGadgets plots

sim.data.path <- "../../../simulating/simulated_data_v2/BM10_conditional_constPshift.rds"

## Add node labels to the data_tree using the node_name_map file
### Read in the name map file
output.node.label.df <- read.table("node_name_map_job2.txt",
                                   sep="\t")
colnames(output.node.label.df) <- c("index", "label")

### Add labels to the data_tree object
data_tree <- readRDS("results/data_tree_fulltree.rds")
data_tree <- data_tree[[1]][[1]]
data_tree@data$index <- as.integer(data_tree@data$index)
data_tree@data$label <- output.node.label.df$label[match(data_tree@data$index, as.integer(output.node.label.df$index))]
# ADD THE (ACTUALLY CORRECT???) MEAN BRANCH RATES
trace <- readTrace("rel_mvBM_fulltree_job2.log", burnin=0.1) 
data_tree@data$mean_branch_rates <- apply(trace[[1]], 2, mean)[paste0('branch_rates[', 1:(length(data_tree@phylo$tip.label) + data_tree@phylo$Nnode - 1),']')][data_tree@data$index]
# Save the @data (input to get_combined_tree)
write.csv(data_tree@data, "results/data_tree_fulltree_processed.csv", row.names=FALSE, quote=FALSE)


## Source helpful functions - need to expand get_combined_tree to handle RevBayes output
source("../../../helpful_functions.R")

combined_tree <- get_combined_tree(sim.data.path, 
                                   RR=NULL,
                                   RB=data_tree@data,
                                   ignore_outliers = FALSE)

## Plot !!
### Inferred
ggtree(combined_tree, aes(color = rates.RBrelNorm), layout="circular", size=1) +
  geom_tiplab(size=2) +
  scale_color_gradient2(
    low = "red",        # Color for values < 1
    mid = "grey",      # Color for value = 1
    high = "blue",      # Color for values > 1
    midpoint = 0,       # The value to treat as the midpoint
    name = "Relative log normalized inferred rate values"
  ) +
  theme(legend.position = "right") +
  ggtitle("RevBayes 25% Sampled Tree")

### True
ggtree(combined_tree, aes(color = sigma2.relNorm), layout="circular", size=1) +
  geom_tiplab(size=2) +
  scale_color_gradient2(
    low = "red",        # Color for values < 1
    mid = "grey",      # Color for value = 1
    high = "blue",      # Color for values > 1
    midpoint = 0,       # The value to treat as the midpoint
    name = "True BM rate"
  ) +
  theme(legend.position = "right")

### True 'observed'
ggtree(combined_tree, aes(color = rates.relNorm), layout="circular", size=1) +
  geom_tiplab(size=2) +
  scale_color_gradient2(
    low = "red",        # Color for values < 1
    mid = "grey",      # Color for value = 1
    high = "blue",      # Color for values > 1
    midpoint = 0,       # The value to treat as the midpoint
    name = "True rate"
  ) +
  theme(legend.position = "right")

