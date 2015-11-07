
#module mapping procedure, this code consists of three steps
# step 1: construct the biparititon graph for two adjacent time points
# step 2: using a heuristic method to discover the module map 

#******** There are three parameters ************* 
#**  parition_1/_2: the two paritioning result ***
#**   network2: the corresponding networks ***
#*************************************************   
evolutionary_cluster <- function(parition1,parition2,networks)
{

#********** checking the paritioning result **************
#*** two criteria: 1. multiple network 2. non-overlap ****

partion_index = 0;
overlap_index = 0;
dim_index1 = dim(parition1);
dim_index2 = dim(parition2);
if((length(dim_index1)==2)&&(length(dim_index2)==2))
{
tempvalue1 = sort(unique(as.vector(parition1)));
tempvalue2 = sort(unique(as.vector(parition2)));
#print(tempvalue)
if((length(tempvalue1)==2)&&(tempvalue1[1] ==0)&&(tempvalue1[2] ==1)&&(length(tempvalue2)==2)&&(tempvalue2[1] ==0)&&(tempvalue2[2] ==1))
{

  for(i in 1:dim(parition1)[1])
{
 
   temp= which(parition1[i,]==1);
   if(length(temp)==1)
    {
     
      }else
      {
       overlap_index = overlap_index +1;
       }
 }   

  for(i in 1:dim(parition2)[1])
{
 
   temp= which(parition2[i,]==1);
   if(length(temp)==1)
    {
     
      }else
      {
       overlap_index = overlap_index +1;
       print(i);
       }
 }
 if(overlap_index == 0)
  {
    print("step 1: paritioning result checking .... [ok]");
   partion_index = 1;
   }else
   {
     print("The node overlap is not allowed");
     
   }
  }else
  {
     print("paritioning matrix is not correct, should be in 0,1 index matrix");
   }

}else
{
 print("error: the paritioning results do not match the number of networks");
  }

#*********************************************************

#**** step 2: to construct the bipartite ********
if(partion_index)
{

print("step 2: construction of biparition .....")
p1_num = dim(parition1)[2];
p2_num = dim(parition2)[2];
bipart = array(0,dim=c(p1_num+1,p2_num+1)); #the adjacency matrix 
size1 = size2 =c();
  for(i in 1:dim(parition1)[2])
   { size1 = c(size1,length(which(parition1[,i]==1))); }

  for(i in 1:dim(parition2)[2])
   { size2 = c(size2,length(which(parition2[,i]==1))); }
  print(size1)
  print(size2)
  for(i in 1:p1_num)
  {
  for(j in 1:p2_num)
  {
    #here compute the module links 
    g1_node = which(parition1[,i]==1);
    g2_node = which(parition2[,j]==1);
    com_node = sort(intersect(g1_node,g2_node));
    # computing 
    
     bipart[i,j] = length(com_node);
     if(length(com_node)>1)
     {
      temp_matrix = networks[com_node,com_node,];
      temp_count = 0;
      for(i1 in 1:dim(temp_matrix)[1])
       {
        for(i2 in 1:dim(temp_matrix)[2])
       {
         if((temp_matrix[i1,i2,1]==1)&&(temp_matrix[i1,i2,2]==1))
         {
           temp_count = temp_count +1;
          } 
         } 
         
       } 
       
       bipart[i,j] =  bipart[i,j]+2*temp_count;
     }
     
   }

#**** step 2.2. obtain the array(t-1) and array(t) and totally values
 for(i in 1:p1_num)
  {
    g1_node = which(parition1[,i]==1);
    g2_node = c(1:dim(parition1)[1]);
    com_node = sort(intersect(g1_node,g2_node));

  bipart[i,p2_num+1] = length(com_node);
     if(length(com_node)>1)
     {
      temp_matrix = networks[com_node,com_node,];
      temp_count = 0;
      for(i1 in 1:dim(temp_matrix)[1])
       {
        for(i2 in 1:dim(temp_matrix)[2])
       {
         if((temp_matrix[i1,i2,1]==1)&&(temp_matrix[i1,i2,2]==1))
         {
           temp_count = temp_count+1;
          }
        }
       }

       }

    bipart[i,p2_num+1] = bipart[i,p2_num+1]+2*temp_count;
   }


for(i in 1:p2_num)
  {
    g1_node = which(parition2[,i]==1);
    g2_node = c(1:dim(parition1)[1]);
    com_node = sort(intersect(g1_node,g2_node));

  bipart[p1_num+1,i] = length(com_node);
     if(length(com_node)>1)
     {
      temp_matrix = networks[com_node,com_node,];
      temp_count = 0;
      for(i1 in 1:dim(temp_matrix)[1])
       {
        for(i2 in 1:dim(temp_matrix)[2])
       {
         if((temp_matrix[i1,i2,1]==1)&&(temp_matrix[i1,i2,2]==1))
         {
           temp_count = temp_count+1;
          }
        }
       }

       }

    bipart[p1_num+1,i] = bipart[p1_num+1,i]+2*temp_count;
   }

 
# total values 
     temp_count = 0;
      for(i1 in 1:dim(networks)[1])
       {
        for(i2 in 1:dim(networks)[2])
       {
         if((networks[i1,i2,1]==1)&&(networks[i1,i2,2]==1))
         {
           temp_count = temp_count+1;
          }
        }
       }

   bipart[p1_num+1,p2_num+1] = dim(networks)[1]+2*temp_count; 
#*********************************************************************
   }



#print(bipart);
#************************************************

#**** step 3: mapping the modules across two time points ********
#*** we obtain the module evolutionary by using the mutual information **
print("step 3: heuristic method to discover evolution gene module .....");
source("./cluster_map.R")
cluster_map(bipart)

}
#*****************************************************************

}
