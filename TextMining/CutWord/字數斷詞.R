system.time({
library(RMySQL)
con = dbConnect(RMySQL::MySQL(), dbname="",host = "",user = "",password = "")
dbSendQuery(con,'SET NAMES utf8')
sql="SELECT content.subject,content.content FROM content"
tab=dbGetQuery(con,sql)
ct<-function (y){
##清理資料
y<-gsub("\\s+|[[:punct:]]|[0-9]|[A-Za-z]","",y)
}
k1<-ct(tab$subject)
k2<-ct(tab$content)
library(Rcpp)
sourceCpp(code='
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List cpp_str_split( std::vector< std::string > strings, int n ) {
  int num_strings = strings.size();
  List out(num_strings);
  for( int i=0; i < num_strings; i++ ) {
    int num_substr = strings[i].length() / n;
    std::vector< std::string > tmp;
    for( int j=0; j < num_substr; j++ ) {
      tmp.push_back( strings[i].substr( j*n, n ) );
    }
    out[i] = tmp;
  }
  return out;
}'
)
words1<-lapply(c(4,6,8,10,12,14,16,18,20,22,24,26,28,30,32),function(n) cpp_str_split(iconv(k1,"UTF-8","BIG5"),n))
words2<-lapply(c(4,6,8,10,12,14,16,18,20,22,24,26,28,30,32),function(n) cpp_str_split(iconv(k2,"UTF-8","BIG5"),n))
newwords<-c(unlist(words1),unlist(words2))
newwords<-unique(iconv(newwords,"BIG5","UTF-8"))
write.table(newwords,"",fileEncoding="UTF-8",row.names=F)
})