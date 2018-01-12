

##-------------------------------------과자 '오사쯔' 네이버 뉴스 크롤링--------------------------------


#---------------------------------------네이버 Api 받기------------------------------------------------

library(rvest)
library(httr)
library(pspline)

client_id = 'V2CJrR4jZt_ZFdTS6Svo';
client_secret = 'BzewTURJ2K';
header = httr::add_headers(
  'X-Naver-Client-Id' = client_id,
  'X-Naver-Client-Secret' = client_secret)


query = iconvquery = '오사쯔'
# encoding 변화
query = iconv(query, to = 'UTF-8', toRaw = T)
# iconv(query, to = "UTF-8", toRaw = F)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)



end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)
i = 1
url = paste0('https://openapi.naver.com/v1/search/news.xml?query=',
             query,'&display=',display_num,'&start=',
             start_point[i],'&sort=sim')

url_body = read_xml(GET(url, header))

title = url_body %>% xml_nodes('item title') %>% 
  xml_text()
pubdate = url_body %>% xml_nodes('item pubDate') %>%
  xml_text()
link = url_body %>% xml_nodes('item link') %>%
  xml_text()
description = url_body %>% xml_nodes('item description') %>%
  html_text()


final_dat = NULL
for(i in 1:length(start_point))
{
  # request xml format
  url = paste0('https://openapi.naver.com/v1/search/news.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim')
  #option header
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  pubdate = url_body %>% xml_nodes('item pubDate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  temp_dat = cbind(title, pubdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
final_dat = data.frame(final_dat, stringsAsFactors = F)







#---------------------------------------형태소 분석----------------------------------




final_dat[10,4] # 10번째 데이터의 discription 확인 ,  temp_dat에서 총 행의 개수가 4개이므로 여기에 맞추어야한다. 
a = gsub(pattern = "<[/?A-Za-z]*>",
         replace = "",final_dat[10,4])
a

dat_tmp <- final_dat
for (i in 1:nrow(final_dat))
{
  dat_tmp[i,4]<-   gsub(pattern = "<[/|A-Za-z]*>", 
                        replace = "", final_dat[i,4])
}



library(KoNLP)


useSejongDic()
extractNoun(a)


library(tm)
text = dat_tmp[,4]
cps = Corpus(VectorSource(text))
dtm = tm::DocumentTermMatrix(cps, 
                             control = list(tokenize = extractNoun, 
                                            removeNumber = T,
                                            removePunctuation = T))

library(Matrix)

str(dtm)




rmat <- as.matrix(dtm)
str(rmat)

rmat <-spMatrix(dtm$nrow,dtm$ncol, i=dtm$i, j=dtm$j, x=dtm$v)
head(rmat)

library(rvest)
wcount<-colSums(rmat)
wname <- dtm$dimnames$Terms
wname <- rvest::repair_encoding(dtm$dimnames$Terms)
colnames(rmat)<- wname
str(wname)


sort.var <- sort(wcount,decreasing = T)[100]
idx <- !( grepl(query, wname)| (wcount<=sort.var) )
wname.rel <- wname[idx]
wcount.rel <- wcount[idx]
#------------------------------------------------------빈도 분석-----------------------

library(wordcloud)
pal <- brewer.pal(9, "Set1")
wordcloud(wname.rel,freq = wcount.rel, colors = pal)

#-------------------------------------------------------상관 분석------------------------

bb <- rmat
bb.freq <- sort(colSums(bb), decreasing = T)
plot(log(bb.freq), pch = 19, type = 'l')


bb.freq <- bb.freq[bb.freq>quantile(bb.freq,0.99)]
idx <- match(names(bb.freq),  colnames(bb))
bb.r <- bb[,idx]
dim(bb.r)

bb.r <- as.matrix(bb.r)
cor.mat <- cor(bb.r)
image(cor.mat, col =terrain.colors(100))


sort(cor.mat[1,], decreasing = T)[1:10]


# 상관분석에 4종과 5종 같은 단어가 등장하는 이유는 해태제과의 끼워팔기로 인한 공정거래법 위반에 대한 기사에 
# 오사쯔도 같이 끼워팔리는 과자 종류 중 하나로 분류되어 언급되었기 때문이다.
# 뉴스에 언급된 과자 '오사쯔'의 이미지는 해태제과에서 '끼워팔리는 과자'인 것 같다.



# 블로거를 통한 크롤링의 결과는 '고구마'와 '과자'라는 단어의 빈도가 가장 큰데, 
# 이것은 오사쯔가 '고구마과자'로 인식 되어 있다고 해석할 수 있다.  


