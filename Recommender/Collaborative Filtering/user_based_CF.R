rm(list=ls(all = TRUE))

# 設定隨機種子
set.seed(999)

# user-based 協同推薦法function
user_based_CF = function(data,colnames_of_uid,rnum){
  
  # Input : 
  # data 輸入資料
  # colnames_of_uid 用戶ID欄位名
  # rnum 推薦數
  
  data[,colnames_of_uid] = as.character(data[,colnames_of_uid])
  # 去除 column of user id
  data.s <- (data[,!(names(data) %in% c(colnames_of_uid))])
  
  # 使用cosine計算user相似度
  similarity_func <- function(x,y) 
  {
    return(sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y))))
  }
  similarity  <- matrix(NA, nrow=nrow(data.s),ncol=nrow(data.s))
  for(i in 1:nrow(data.s)) {
    for(j in 1:nrow(data.s)) {
      similarity[i,j] <- similarity_func(as.matrix(data.s[i,]),as.matrix(data.s[j,]))
    }
  }
  
  # User_based recommendation
  user_similarity = list()
  for (u in 1:nrow(data.s)){
    user_similarity[[u]] = numeric(ncol(data.s))
    for (i in 1:ncol(data.s)){
      s = data.frame(data[-u, colnames_of_uid],similarity[-u,u])
      index = head(sort(s[,2], index.return = TRUE, decreasing = TRUE)$ix,n = 3)
      similar_user = s[index,1]
      v = data.frame(colnames_of_uid = data[-u, colnames_of_uid], data.s[-u,])
      v = v[v$colnames_of_uid %in% similar_user,-1]
      v = v[,i]
      user_similarity[[u]][i] = v%*%similarity[-u,u][index]/sum(similarity[-u,u][index])
    }
  }
  user_similarity <- as.data.frame(do.call('rbind',user_similarity))
  colnames(user_similarity) <- colnames(data.s)
  neighbours <- matrix(NA, nrow=nrow(user_similarity),ncol=rnum)
  for(i in 1:nrow(user_similarity)) 
  {
    neighbours[i,] <- colnames(user_similarity)[order(user_similarity[i,],decreasing=TRUE)[1:rnum]]
  }
  output <- data.frame(data[,1],neighbours)
  colnames(output)[1] <- colnames_of_uid
  
  return(output)
  
}

# 產生資料
data <- matrix(NA, nrow = 100, ncol = 26)
for (i in 1:100){
  data[i,] <- sample(10,26,replace = T)
}
colnames(data) <- LETTERS
data = data.frame(uid = 1:100, data)

# Run
user_based_CF(data,"uid",3)
