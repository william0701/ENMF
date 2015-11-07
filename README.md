ENMF
=======
### Briefly review of ENMF
Evolutionary Nonnegative Matrix Factorization (ENMF) is an NMF algorithm based method for evolutionary dynamic pathways in time series networks. Which consists of two components: local clusters discovery as well as local clusters mapping. The first component makes use of NMF algorithm to obtain local clusters at each stage using the smooth framework in evolutionary clustering. After obtaining the local clusters at each stage, we construct bi-partite graph for local clusters at two consecutive steps by regarding each local cluster as a node and the number of edge and nodes conserved in both local clusters as edge weight. We explore the mutual information to obtain the optimal mapping. More details refer to the paper. 
###  Running ENMF
* Coding Langurage:R language
* Prerequisite: R>=3.0, NMF package
* Input: There are two types of parameters must be specified by users
  * A: an  n*n*T three-dimensional matrix with, where n is the number of nodes within the network, and T is the number of stages.
  * K: a vector to specify the number of local clusters in each stages.
  * α: the relevant weight for evolutionary clustering algorithm.
* The step by step tutorial is depicted as
```R
#Step 1: data load
# users are required to store the time series networks as an three dimension matrix and specify the number of local clusters at each stage
  lname = load("time-series networks"); 
  networks = A;
  cluster_index = K
  α=0.8
# Step 2: obtain the local clusters
  source("./module_discovery.r");
  local_result = module_discovery(networks,cluster_index,α);
# here we obtain the local clusters for each stage 
# Step 3： mapping the local clusters for two consecutive stages
  source("./evolutionary_cluster.r");
  for(i in 1:(T-1))
  {
    map_result = evolutionary_cluster(local_result[,1:cluster_index[i],i], local_result[,1:cluster_index[i+1],i+1],networks[,,c(i,i+1)]);
    print(map_result);
  }
```
