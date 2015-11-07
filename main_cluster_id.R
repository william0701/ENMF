lname = load("Combine_4.RData");
print(lname)
library(NMF)

cluster_in_all = c(3:9)*20;
for(tempi in 3:9)
{

 

 

cluster_index = rep(cluster_in_all[tempi],4)
#network = networks;
networks[is.na(networks)] = 0
#networks = array(runif(1000000,0,1),dim=c(1000,1000,2))
#cluster_index = rep(4,10); # fixed community 

alpha = 0.80
source("./module_discovery.r");
result = module_discovery(networks,cluster_index,alpha);

output_file = paste(tempi,"RData",sep=".")
save(result,file=output_file);
}



#save(result,file="result.RData")
