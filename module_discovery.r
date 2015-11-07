#module mapping procedure, this code consists of three steps
# step 1: construct the biparititon graph for two adjacent time points
# step 2: using a heuristic method to discover the module map 

#******** There are three parameters ************* 
#**  parition_1/_2: the two paritioning result ***
#**   network2: the corresponding networks ***
#*************************************************   
module_discovery <- function(networks,k,alpha)
{

dim_info = dim(networks);
index = 0;
num_gene = dim_info[1];
if(length(dim_info)==3)
{
 print("the network is correct");
 if(num_gene < max(k))
 {
  print("error: the number of clusters is greater than the number of genes!!!!");
  }else
 {
 index =1;
  }
}else
{
print("please enter an three dimensional matrix ")

}


if(index)
{
#define the number parameters 
partion = array(0,dim=c(num_gene,max(k),length(k)));
#nmf(tempmatrix,k[i],"ns",theta=0.7,seed = "nndsvd");
res = nmf(networks[,,1],k[1],"ns",theta=0.7,maxIter=5000,seed = "nndsvd"); 
source("./cluster_parition.r") #basis(res)
temp = cluster_parition(coef(res),1);
#heatmap(networks[,,1],cexRow=0.2)
print("the first is finished");
#temp = cluster_parition(basis(res),1);
partion[,c(1:k[1]),1] = temp;
for(i in 2:dim(networks)[3])
{
  tempmatrix = networks[,,i-1]+alpha*(networks[,,i]-networks[,,i-1]);
  tempmatrix = 0.618*tempmatrix/2 + 0.618*0.618*tempmatrix%*%tempmatrix/6+0.618*0.618*0.618*tempmatrix%*%tempmatrix%*%tempmatrix/30
  tempmatrix = tempmatrix/max(tempmatrix)
 tempmatrix = tempmatrix+0.0000000001
 diag(tempmatrix) = 0;
 save(tempmatrix,file="tempnetwork.RData")
  print(c(i,length(which(is.na(tempmatrix)))))
  res = nmf(tempmatrix,k[i],"ns",theta=0.7,maxIter=5000,seed = "nndsvd");
  temp = cluster_parition(coef(res),1);
  partion[,c(1:k[i]),i] = temp;

 }


partion;




}

}
