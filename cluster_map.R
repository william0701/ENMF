
# clusters mapping using mutual information

cluster_map <- function(map_matrix)
{

num_cluster_1 = dim(map_matrix)[1] -1;
num_cluster_2 = dim(map_matrix)[2] -1;

map_result = array(0,dim=c(num_cluster_1,num_cluster_2));
mi_info = array(0,dim=c(num_cluster_1,num_cluster_2));

for(i in 1:num_cluster_1)
{
 for(j in 1:num_cluster_2)
  {
   if(map_matrix[i,j]>0)
    {
       
     mi_info[i,j] = map_matrix[i,j]/map_matrix[num_cluster_1+1,num_cluster_2+1]*log(map_matrix[i,j]*map_matrix[num_cluster_1+1,num_cluster_2+1]/map_matrix[i,num_cluster_2+1]/map_matrix[num_cluster_1+1,j])
    }


  }

 }

mi_info[mi_info<0] = 0;
index = 1;
while(index)
{

# each iteration we obtain the best matched groups between two consecutive timesteps
if(max(mi_info)>0)
{
max_value = which(mi_info==max(mi_info),arr.ind=T);
cluster1 = max_value[1,1];
cluster2 = max_value[1,2];

delta_mi = 4*mi_info[cluster1,cluster2]-sum(mi_info[cluster1,])-sum(mi_info[,cluster2]);
if(delta_mi > 0)
{
  map_result[cluster1,cluster2] = 1;
  mi_info[cluster1,]=0;
  mi_info[,cluster2]=0;
  
 }else
 {
   index = 0;
  }


}else
{
index = 0;

}





}
#print(mi_info)
for(i in 1:dim(map_result)[1])
{
temp1 = which(map_result[i,]==1);
print(c(i,temp1))

}
print(map_result)

}
