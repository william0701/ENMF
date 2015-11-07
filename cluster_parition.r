
cluster_parition <- function(matrix,threshold)
{
#this function is to obtain the parition results from coef matrix of the matrix factorization
matrix = t(matrix);
matrix = data.matrix(matrix);
num_gene = dim(matrix)[1];
num_cluster = dim(matrix)[2]
tempresult = array(0,dim=c(num_gene,num_cluster));

for(i in 1:num_gene)
{

 temp1 = which.max(matrix[i,]);
 if(length(temp1)==1)
 {
   tempresult[i,temp1] = 1;
  }


#print(c(i,temp1,matrix[i,]))

}
save(tempresult,matrix,file="parition_result.RData");
tempresult;




}
