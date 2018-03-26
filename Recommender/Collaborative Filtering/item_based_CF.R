rm(list=ls(all = T))

# 設定隨機種子
set.seed(999)

# item-based 協同推薦法function
item_CF = function(data,colnames_of_uid,rnum){

  # Input : 
  # data 輸入資料
  # colnames_of_uid 用戶ID欄位名
  # rnum 推薦數
  
  data[,colnames_of_uid] = as.character(data[,colnames_of_uid])
  
  # 去除 column of user id
  data.s <- (data[,!(names(data) %in% c(colnames_of_uid))])
  
  # 使用cosine計算item相似度
  similarity_func <- function(x,y) 
  {
    return(sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y))))
  }
  similarity  <- matrix(NA, nrow=ncol(data.s),ncol=ncol(data.s))
  for(i in 1:ncol(data.s)) {
    for(j in 1:ncol(data.s)) {
      similarity[i,j] <- similarity_func(as.matrix(data.s[i]),as.matrix(data.s[j]))
    }
  }
  
  # Item_based recommendation
  item_similarity = list()
  for (u in 1:nrow(data.s)){
    item_similarity[[u]] = numeric(ncol(data.s))
    for (k in 1:ncol(data.s)){
      item_similarity[[u]][k] = as.matrix(data.s[u,])%*%as.matrix(similarity[,k])/sum(similarity[,k])
    }
  }
  item_similarity <- as.data.frame(do.call('rbind',item_similarity))
  colnames(item_similarity) <- colnames(data.s)
  neighbours <- matrix(NA, nrow=nrow(item_similarity),ncol=rnum)
  for(i in 1:nrow(item_similarity)) 
  {
    neighbours[i,] <- colnames(item_similarity)[order(item_similarity[i,],decreasing=TRUE)[1:rnum]]
  }
  output <- data.frame(data[,1],neighbours)
  colnames(output)[1] <- colnames_of_uid
  
  return(output)
  
}

# eg.
# 產生資料
data <- matrix(NA, nrow = 100, ncol = 26)
for (i in 1:100){
            data[i,] <- sample(5,26,replace = T)
}
colnames(data) <- LETTERS
data = data.frame(uid = 1:100, data)

# 產生推薦
item_CF(data,"uid",5)