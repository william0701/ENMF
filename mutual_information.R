mutual_information <- function(partion1,partion2)
{

cluster1 = unique(partion1)
cluster2 = unique(partion2)

num_cluster1 = length(cluster1)
num_cluster2 = length(cluster2)
conf_matrix = array(0,dim=c(num_cluster1,num_cluster2));

for(i in 1:num_cluster1)
{
 for(j in 1:num_cluster2)
{
  temp1 = which(partion1==cluster1[i])
  temp2 = which(partion2==cluster2[j])
    
 conf_matrix[i,j] = length(intersect(temp1,temp2))

 } 
 }
#print(conf_matrix)
nmi = 0;
for(i in 1:num_cluster1)
{
 for(j in 1:num_cluster2)
{
if(conf_matrix[i,j]>0)
{
  nmi = nmi-2*conf_matrix[i,j]*log2(conf_matrix[i,j]*sum(conf_matrix)/(sum(conf_matrix[i,])*sum(conf_matrix[,j])))
 }

 } 
 }

den_value = 0;
for(i in 1:num_cluster1)
{
 den_value = den_value+sum(conf_matrix[i,])*log2(sum(conf_matrix[i,])/sum(conf_matrix))
}
for(i in 1:num_cluster2)
{
 den_value = den_value+sum(conf_matrix[,i])*log2(sum(conf_matrix[,i])/sum(conf_matrix))
}

#print(den_value)
norm_nmi = nmi/den_value;


#********here we calculate the error distance **************
num_gene = length(partion1)
partion_matrix1 = array(0,dim=c(num_gene,num_cluster1))
partion_matrix2 = array(0,dim=c(num_gene,num_cluster2))
for(i in 1:num_cluster1)
{
  temp1 = which(partion1==cluster1[i])
  if(length(temp1))
  { partion_matrix1[temp1,i] = 1;}
}

for(i in 1:num_cluster2)
{
  temp1 = which(partion2==cluster2[i])
  if(length(temp1))
  { partion_matrix2[temp1,i] = 1;}
}

distance_matrix = partion_matrix1%*%t(partion_matrix1)-partion_matrix2%*%t(partion_matrix2);
dvalue = sqrt(sum(distance_matrix*distance_matrix))
c(norm_nmi,dvalue);
}
