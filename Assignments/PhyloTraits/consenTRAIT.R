################################################################################
#                                                                              #
#   consenTRAIT function                                                       #
#                                                                              #
#   Originally by: Adam Martiny                                                #
#   Minor Modification by: Mario Muscarella                                    #
#                                                                              #
#   Last Update: 2015-02-18                                                    #
#                                                                              #
#   Citation: Martiny et al. 2013. Phylogenetic conservatism of functional     #
#               traits in microorganisms. ISME J. 7(4): 830–838                #
#                                                                              #
################################################################################

require("data.table")||install.packages("data.table");require("data.table")
require("adephylo")||install.packages("adephylo");require("adephylo")
require("ape")||install.packages("ape");require("ape")

consenTRAIT <- as.function(data = "", phy = ""){
  if (is.phylo(phy) = FALSE){
    stop("'", phy, "' is not a phylo object.")
  }
  if (is.rooted(phy)) = FALSE{
    stop("'", phy, "' must be rooted")
  }
  tree <- phy
  table <- data

  # loading Newick tree (multitree) - replace to read.nexus if using nexus tress
  # tree_all = read.tree(args[1],keep.multi = TRUE)
  # Loading trait table w. no headers
  # table = read.table(args[2], sep = "\t", header=FALSE)

  # Starting script
  Mean_all <- matrix(NA, nrow = ncol(table), ncol = 100)

  # testing if table and tree contain the same entries - else drop tips
  z <- subset(tree$tip.label,!(tree$tip.label %in% rownames(table)))
  if (length(z) > 0) {
    drop.tip(tree,z)
    }

  # Replacing Negative Branch Lengths - e.g., from PHYLIP
  tree$edge.length[tree$edge.length <= 0] =  0.00001
  subtree <- subtrees(tree, wait = FALSE)

  cluster_mean <- numeric(length=0)

  # Loop Through All Traits
  for (j in 1:ncol(table)) {

    trait <- colnames(table)[j]

    # Print Status
    print(paste("Analyzing",trait[j],"!!!!!!!!!!!!!!!!!!!!!!!"))

    # Loading Trait Table
    table_tmp <- table[ , j, drop = FALSE]
    colnames(table_tmp) <- c("Trait")
    table_tmp$ID <- rownames(table_tmp)


    # removing all entries not in tree
    table_tmp2 <- data.table(table_tmp)
    setkey(table_tmp2,ID)
    table2<-table_tmp2[intersect(table_tmp2$ID,tree$tip.label)]
    setkey(table2,ID)

    #initializing result vectors and file names
    positives <- vector(mode="list",length = 0)
    cluster_size <- numeric(length = 0)
    cluster_size_file <- paste("R_cluster_size_",j,".txt",sep="")

    cluster_dist <- numeric(length = 0)
    cluster_dist_file <- paste("R_cluster_dist_",j,".txt",sep="")

    #initalizing files (Remove??)
    if (m == 1) {
      cat(c("trait","tree","distance","cluster_size"), file = cluster_size_file, sep = "\t", fill = FALSE, labels = NULL,append = FALSE)
      cat("\n", file = cluster_size_file, fill = FALSE, labels = NULL,append = TRUE)

      cat(c("trait","tree","distance"), file = cluster_dist_file, sep = "\t", fill = FALSE, labels = NULL,append = FALSE)
      cat("\n", file = cluster_dist_file, fill = FALSE, labels = NULL,append = TRUE)
    }

    #loop through all subtrees and determining if any subtrees have >90% positives
    for (i in 1:length(subtree)){
      tip_names<-subtree[[i]]$tip.label
        #change this value if you want a new threshold
        if (mean(table2[tip_names][,Trait]) > 0.9 ) {
          match_test<-match(tip_names,positives)
        if (all(is.na(match_test))) {
          positives<-c(positives,tip_names)
          cluster_dist<-distRoot(subtree[[i]],tip_names, method=c("p"))
          cluster_size<-c(cluster_size,mean(cluster_dist))
          # printing to files###
          cat(c(j,m,mean(cluster_dist),length(cluster_dist)), file = cluster_size_file, sep = "\t", fill = FALSE, labels = NULL,append = TRUE)
          cat("\n", file = cluster_size_file, fill = FALSE, labels = NULL,append = TRUE)

          cat(j,m,cluster_dist, file = cluster_dist_file, sep = "\t", fill = FALSE, labels = NULL,append = TRUE)
          cat("\n", file = cluster_dist_file, fill = FALSE, labels = NULL,append = TRUE)


          #print(cluster_dist)
        }
        else if (any(is.na(match_test))) {
          print("some NAs - something is weird")
        }
        else {
          #print(tip_names)
          #print("found cluster before")
        }
      }
    }


    ##### find singletons ######
    a<-table2[table2$Trait == 1,][,ID]
    g<-as.character(a)

    singletons_names = setdiff(g,positives)
    if (length(singletons_names) > 0) {
      for (h in 1:length(singletons_names)){
        # weigh singletons with half
        singleton_edges = 0.5*tree$edge.length[which.edge(tree,singletons_names[h])] #here we use half the distance for singletons
        cluster_size<-c(cluster_size,singleton_edges)

        cat(c(j,m,singleton_edges,1), file = cluster_size_file, sep = "\t", fill = FALSE, labels = NULL,append = TRUE)
        cat("\n", file = cluster_size_file, sep = "\t", fill = FALSE, labels = NULL,append = TRUE)
      }

    }
    Mean_all[j-1,m] = mean(cluster_size)
  }




  return(Mean_all)
}
