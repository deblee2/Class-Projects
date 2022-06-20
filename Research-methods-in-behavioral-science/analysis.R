library(tibble)
library(dplyr)
library(lubridate)
library(stringr)

#### 파일 읽기 및 data aggregation
setwd("/Users/mc/R/term project/data_all") #directory 설성

tesla.eu <- as_tibble(read.csv("tesla_eu.csv", na.strings=c("")))  #tesla 데이터 불러오기
tesla.su <- as_tibble(read.csv("tesla_su.csv", na.strings=c("")))
tesla.gu <- as_tibble(read.csv("tesla_gu.csv", na.strings=c("")))

tesla.esg <- bind_rows(tesla.eu, tesla.su, tesla.gu)
tesla.esg$Date <- as_date(mdy(tesla.esg$Date))
summary(tesla.esg)

volkswagen.eu <- as_tibble(read.csv("volkswagen_eu.csv", na.strings=c("")))  #volkswagen 데이터 불러오기
volkswagen.su <- as_tibble(read.csv("volkswagen_su.csv", na.strings=c("")))
volkswagen.gu <- as_tibble(read.csv("volkswagen_gu.csv", na.strings=c("")))

volkswagen.esg <- bind_rows(volkswagen.eu, volkswagen.su, volkswagen.gu)
volkswagen.esg$Date <- as_date(mdy(volkswagen.esg$Date))
summary(volkswagen.esg)

## tesla/ volkswagen 데이터 합치기
esg.all <- bind_rows(tesla.esg, volkswagen.esg) 
esg.all$Company[esg.all$Company=="tesla"] <- "Tesla"
esg.all$Company[esg.all$Company=="volkswagen"] <- "Volkswagen"
esg.all$text <- str_c(esg.all$Title, esg.all$Text, sep=" ")
esg.all <- esg.all %>%
  select(-Title, -Text)
head(esg.all)
tail(esg.all)
summary(esg.all)

#### 시간에 따른 뉴스기사 빈도 추이
## 전체 뉴스기사 빈도 그래프
library(ggplot2)
library(lattice)
news.count <- esg.all %>%
  mutate(Month=floor_date(esg.all$Date, unit="month")) %>%  #월 단위로 변환
  filter(Month >= "2015-01-01" & Month < "2022-01-01") %>%  #2015~2021년 데이터 
  count(Month) #월간 뉴스기사 수 count
summary(news.count)

ggplot(news.count, aes(x=Month, y=n)) +
  geom_line(size=0.8, color="dimgray") +
  geom_smooth()+
  geom_vline(xintercept=ymd("2020-01-01"),color="dimgray", linetype=2) +  #코로나 발발 시점 
  labs(title="Total News Counts over Time (Monthly)", y="Count") +
  theme_bw() +
  theme(plot.title=element_text(face="bold", size=16, hjust=0.5),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks=seq(ymd('2015-01-01'), ymd('2022-01-01'), by ='years'),
                     labels=as.character(seq(2015, 2022, by=1)))

## 회사별 뉴스기사 빈도 그래프 
news.count <- esg.all %>%
  mutate(Month=floor_date(esg.all$Date, unit="month")) %>%  #월 단위로 변환
  filter(Month >= "2015-01-01" & Month < "2022-01-01") %>%  #2015~2021년 데이터 
  count(Company, Month) #월간 회사별 뉴스기사 수 count

ggplot(news.count, aes(x=Month, y=n, color=Company)) +
  geom_line(size=1) +
  geom_vline(xintercept=ymd("2020-01-01"),color="dimgray", linetype=2) +  #코로나 발발 시점 
  labs(title="News Counts by Tesla vs Volkswagen over Time (Monthly)", y="Count") +
  theme_bw() +
  theme(plot.title=element_text(face="bold", size=16, hjust=0.5),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") +
  scale_x_continuous(breaks=seq(ymd('2015-01-01'), ymd('2022-01-01'), by ='years'),
                     labels=as.character(seq(2015, 2022, by=1)))

## ESG 별 뉴스기사 빈도 그래프
esg.count <- esg.all %>%
  mutate(Month=floor_date(esg.all$Date, unit="month")) %>%
  filter(Month >= "2015-01-01" & Month < "2022-01-01") %>% 
  count(Esg, Month)  #월간 ESG 별 뉴스기사 수 count
esg.count

ggplot(esg.count, aes(x=Month, y=n, fill=Esg, color=Esg)) +
  geom_line() +
  geom_vline(xintercept=ymd("2020-01-01"), color="dimgray", linetype=2) +
  labs(title="News Counts by ESG over Time (Monthly)", y="Count") +
  theme_bw() +
  theme(plot.title=element_text(face="bold", size=16, hjust=0.5),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") +
  labs(fill="ESG", color="ESG")+
  scale_x_continuous(breaks=seq(ymd('2015-01-01'), ymd('2022-01-01'), by ='years'),
                     labels=as.character(seq(2015, 2022, by=1))) 

## ESG 별, 회사별 뉴스기사 빈도 그래프
esg.count <- esg.all %>%
  mutate(Month=floor_date(esg.all$Date, unit="month")) %>%
  filter(Month >= "2015-01-01" & Month < "2022-01-01") %>% 
  count(Company, Esg, Month)  #월간 ESG 별 뉴스기사 수 count
esg.count

ggplot(esg.count, aes(x=Month, y=n, fill=Esg, color=Esg)) +
  geom_line() +
  geom_vline(xintercept=ymd("2020-01-01"), color="dimgray", linetype=2) +
  labs(title="News Counts by ESG over Time (Monthly)", y="Count") +
  theme_bw() +
  theme(plot.title=element_text(face="bold", size=16, hjust=0.5),
        panel.grid.minor.x = element_blank(),
        legend.position = "top") +
  labs(fill="ESG", color="ESG")+
  scale_x_continuous(breaks=seq(ymd('2015-01-01'), ymd('2022-01-01'), by ='years'),
                     labels=as.character(seq(2015, 2022, by=1))) +
  facet_wrap(~ Company)



#### 감성 분석
library(tidytext) 
library(textdata)
library(stringr)
library(SnowballC)
esg.all

## 데이터 전처리 및 토큰화
esg.words <- esg.all %>% 
  mutate(Month=floor_date(esg.all$Date, unit="month")) %>%
  filter(Month >= "2015-01-01" & Month < "2022-01-01") %>%
  mutate(text=str_replace_all(text, pattern="[^[:alpha:]]", replacement=" ")) %>%  #알파벳이 아닌 경우 space로 대체 
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%  #불용어 제거  
  mutate(word=wordStem(word)) %>%       #어간 추출
  mutate(word=gsub(pattern="can|could|will|would|should|must|may|might|trump",
                   replacement="", word)) %>%   #조동사+trump(빈도 높지만 긍정, 부정으로 분류하기 어려운 사람이름) 제거 
  filter(nchar(word) > 2) %>%           #단어 길이 2 넘는 경우로 filter
  count(Company, ESG=Esg, word, sort=TRUE, name="count") %>%
  ungroup()

esg.words  #Company, ESG 별 단어 수 테이블


###### tf- idf
tesla.words <- esg.words  %>%
  filter(Company=="Tesla")

tesla.tf_idf <- tesla.words %>%
  bind_tf_idf(term=word, document=ESG, n=count)

## bing 감성사전 이용
e.sentiment <- tesla.tf_idf %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(tf_idf > 0) %>%   #tf_idf가 0이 아닌 단어만 추출 
  group_by(sentiment) %>%
  slice_max(order_by = count, n=10) %>%
  ungroup()

e.sentiment

ggplot(e.sentiment,
       aes(reorder_within(x=word, by=count, within=sentiment), 
           count, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  scale_x_reordered() +
  facet_wrap(~ sentiment, ncol=2, dir="v", scales="free_y") +
  labs(title="Tesla's sentiment word counts (tf-idf>0)", x=NULL, y="count") +
  coord_flip()


###### 절대적 단어 빈도수로 감성분석 (tf-idf X)
##### ESG 각각에 대한 감성분석

## Environment 감성분석
e.sentiment <- esg.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%  #감성사전 "bing" 이용
  filter(ESG=="Environment") %>%
  ungroup() %>%
  group_by(Company, sentiment) %>%
  slice_max(order_by = count, n=10) %>%
  ungroup()
e.sentiment

ggplot(e.sentiment,
       aes(reorder_within(x=word, by=count, within=Company), count, 
           fill=factor(sentiment, levels=c("positive", "negative")))) +
  geom_col(show.legend=FALSE) +
  theme_bw() +
  theme(plot.title=element_text(face="bold", size=16))+
  facet_wrap(~ Company+factor(sentiment, levels=c("positive", "negative")), ncol=2, dir="v", scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(labels=c("Positive", "Negative"),
                     values=c("#619CFF","#F8766D")) +
  labs(title="Sentiment Analysis in Environment", x=NULL, y="Count") +
  coord_flip()

e.sentiment <- esg.words %>%
  inner_join(get_sentiments("loughran"), by="word") %>%  #감성사전 "loughran" 이용
  filter(ESG=="Environment") %>%
  ungroup() %>%
  group_by(Company, sentiment) %>%
  slice_max(order_by = count, n=3) %>%
  ungroup()

ggplot(e.sentiment,
       aes(reorder_within(x=word, by=count, within=Company), count, 
           fill=factor(sentiment, levels=c("positive", "negative", "litigious","uncertainty", "constraining")))) +
  geom_col(show.legend=FALSE) +
  theme_bw() +
  facet_wrap(~ Company+factor(sentiment, levels=c("positive", "negative", "litigious","uncertainty", "constraining")), ncol=2, dir="v", scales="free_y") +
  scale_x_reordered() +
  labs(title="Sentiment Analysis in Environment", x=NULL, y="Count",
       subtitle="Dictionary: loughran") +
  coord_flip()

## Social 감성분석
s.sentiment <- esg.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(ESG=="Social") %>%
  ungroup() %>%
  group_by(Company, sentiment) %>%
  slice_max(order_by = count, n=10) %>%
  ungroup()

ggplot(s.sentiment,
       aes(reorder_within(x=word, by=count, within=Company), count, 
           fill=factor(sentiment, levels=c("positive", "negative")))) +
  geom_col(show.legend=FALSE) +
  theme_bw() +
  theme(plot.title=element_text(face="bold", size=16))+
  facet_wrap(~ Company+factor(sentiment, levels=c("positive", "negative")), ncol=2, dir="v", scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(labels=c("Positive", "Negative"),
                    values=c("#619CFF","#F8766D")) +
  labs(title="Sentiment Analysis in Social", x=NULL, y="Count") +
  coord_flip()

s.sentiment <- esg.words %>%
  inner_join(get_sentiments("loughran"), by="word") %>%
  filter(ESG=="Social") %>%
  ungroup() %>%
  group_by(Company, sentiment) %>%
  slice_max(order_by = count, n=3) %>%
  ungroup()

ggplot(s.sentiment,
       aes(reorder_within(x=word, by=count, within=Company), count, 
           fill=factor(sentiment, levels=c("positive", "negative", "litigious","uncertainty", "constraining", "superfluous")))) +
  geom_col(show.legend=FALSE) +
  theme_bw() +
  facet_wrap(~ Company+factor(sentiment, levels=c("positive", "negative", "litigious","uncertainty", "constraining", "superfluous")),
             ncol=2, dir="v", scales="free_y") +
  scale_x_reordered() +
  labs(title="Sentiment Analysis in Social", x=NULL, y="Count",
       subtitle="Dictionary: loughran") +
  coord_flip()

## Governance 감성 분석
g.sentiment <- esg.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(ESG=="Governance") %>%
  ungroup() %>%
  group_by(Company, sentiment) %>%
  slice_max(order_by = count, n=10) %>%
  ungroup()

ggplot(g.sentiment,
       aes(reorder_within(x=word, by=count, within=Company), count, 
           fill=factor(sentiment, levels=c("positive", "negative")))) +
  geom_col(show.legend=FALSE) +
  theme_bw() +
  theme(plot.title=element_text(face="bold", size=16))+
  facet_wrap(~ Company+factor(sentiment, levels=c("positive", "negative")), ncol=2, dir="v", scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(labels=c("Positive", "Negative"),
                    values=c("#619CFF","#F8766D")) +
  labs(title="Sentiment Analysis in Governance", x=NULL, y="Count") +
  coord_flip()

g.sentiment <- esg.words %>%
  inner_join(get_sentiments("loughran"), by="word") %>%
  filter(ESG=="Governance") %>%
  ungroup() %>%
  group_by(Company, sentiment) %>%
  slice_max(order_by = count, n=3) %>%
  ungroup()

ggplot(g.sentiment,
       aes(reorder_within(x=word, by=count, within=Company), count, 
           fill=factor(sentiment, levels=c("positive", "negative", "litigious","uncertainty", "constraining", "superfluous")))) +
  geom_col(show.legend=FALSE) +
  theme_bw() +
  facet_wrap(~ Company+factor(sentiment, levels=c("positive", "negative", "litigious","uncertainty", "constraining", "superfluous")),
             ncol=2, dir="v", scales="free_y") +
  scale_x_reordered() +
  labs(title="Sentiment Analysis in Governance", x=NULL, y="Count",
       subtitle="Dictionary: loughran") +
  coord_flip()

#### Social+Governance 감성분석
esg.words2 <- esg.words %>%
  mutate(ESG=str_replace_all(ESG, "Environment", replacement="E")) %>%
  mutate(ESG=str_replace_all(ESG, "Social|Governance", replacement="SG")) %>%
  aggregate(count~.,data=., FUN=sum)
esg.words2

sg.sentiment <- esg.words2 %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(ESG=="SG") %>%
  ungroup() %>%
  group_by(Company, sentiment) %>%
  slice_max(order_by = count, n=10) %>%
  ungroup()

ggplot(sg.sentiment,
       aes(reorder_within(x=word, by=count, within=Company), count, 
           fill=factor(sentiment, levels=c("positive", "negative")))) +
  geom_col(show.legend=FALSE) +
  theme_bw() +
  theme(plot.title=element_text(face="bold", size=16))+
  facet_wrap(~ Company+factor(sentiment, levels=c("positive", "negative")), 
             ncol=2, dir="v", scales="free_y") +
  scale_x_reordered() +
  scale_fill_manual(labels=c("Positive", "Negative"),
                    values=c("#619CFF","#F8766D")) +
  labs(title="Sentiment Analysis in Social and Governance", x=NULL, y="Count") +
  coord_flip()

sg.sentiment <- esg.words2 %>%
  inner_join(get_sentiments("loughran"), by="word") %>%
  filter(ESG=="SG") %>%
  ungroup() %>%
  group_by(Company, sentiment) %>%
  slice_max(order_by = count, n=3) %>%
  ungroup()

ggplot(sg.sentiment,
       aes(reorder_within(x=word, by=count, within=Company), count, 
           fill=factor(sentiment, levels=c("positive", "negative", "litigious","uncertainty", "constraining", "superfluous")))) +
  geom_col(show.legend=FALSE) +
  theme_bw() +
  facet_wrap(~ Company+factor(sentiment, levels=c("positive", "negative", "litigious","uncertainty", "constraining", "superfluous")),
             ncol=2, dir="v", scales="free_y") +
  scale_x_reordered() +
  labs(title="Sentiment Analysis in Social and Governance", x=NULL, y="Count",
       subtitle="Dictionary: loughran") +
  coord_flip()



#### 워드클라우드를 통한 시각화 및 분석 
## 긍정, 부정단어 워드클라우드 
library(wordcloud)
library(reshape2)
set.seed(123)

par(mar=c(0,0,0,0), mfrow = c(2, 2)) 

## Environment & Tesla 워드클라우드
esg.words2 %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(ESG=="E"&Company=="Tesla") %>%
  ungroup() %>%
  acast(word ~ factor(sentiment, levels=c("positive", "negative")), value.var="count", fill=0) %>%  
  comparison.cloud(colors=c("cornflowerblue", "tomato"),
                   title.size = 1.5,
                   title.colors = c("cornflowerblue", "tomato"),
                   title.bg.colors = "gray90",
                   scale=c(3.5, 0.3), max.words=60, random.order=FALSE, rot.per=0) 

colors()
## Environment & Volkswagen 워드클라우드
esg.words2 %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(ESG=="E"&Company=="Volkswagen") %>%
  ungroup() %>%
  acast(word ~ factor(sentiment, levels=c("positive", "negative")), value.var="count", fill=0) %>%  
  comparison.cloud(colors=c("cornflowerblue", "tomato"),
                   title.size = 1.5,
                   title.colors = c("cornflowerblue", "tomato"),
                   title.bg.colors = "gray90",
                   scale=c(3.5, 0.3), max.words=60, random.order=FALSE, rot.per=0) 

## Social+Government & Tesla 워드클라우드
esg.words2 %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(ESG=="SG"&Company=="Tesla") %>%
  ungroup() %>%
  acast(word ~ factor(sentiment, levels=c("positive", "negative")), value.var="count", fill=0) %>%  
  comparison.cloud(colors=c("cornflowerblue", "tomato"),
                   title.size = 1.5,
                   title.colors = c("cornflowerblue", "tomato"),
                   title.bg.colors = "gray90",
                   scale=c(3.5, 0.3), max.words=60, random.order=FALSE, rot.per=0) 

## Social+Government & Volkswagen 워드클라우드
esg.words2 %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(ESG=="SG"&Company=="Volkswagen") %>%
  ungroup() %>%
  acast(word ~ factor(sentiment, levels=c("positive", "negative")), value.var="count", fill=0) %>%  
  comparison.cloud(colors=c("cornflowerblue", "tomato"),
                   title.size = 1.5,
                   title.colors = c("cornflowerblue", "tomato"),
                   title.bg.colors = "gray90",
                   scale=c(3.5, 0.3), max.words=60, random.order=FALSE, rot.per=0) 


## lougran 감성사전 이용 워드클라우드 추가 생성
# Environment & Tesla
par(mar=c(0,0,0,0), mfrow = c(1, 2)) 
esg.words2 %>%
  inner_join(get_sentiments("loughran"), by="word") %>%
  filter(ESG=="E"&Company=="Tesla") %>%
  ungroup() %>%
  acast(word ~ sentiment, value.var="count", fill=0) %>%  
  comparison.cloud(title.size = 1.2,
                   title.bg.colors = "gray90",
                   scale=c(5, 0.5), max.words=80, random.order=FALSE, rot.per=0) 
# Environment & Volkswagen
esg.words2 %>%
  inner_join(get_sentiments("loughran"), by="word") %>%
  filter(ESG=="E"&Company=="Volkswagen") %>%
  ungroup() %>%
  acast(word ~ sentiment, value.var="count", fill=0) %>%  
  comparison.cloud(title.size = 1.2,
                   title.bg.colors = "gray90",
                   scale=c(5, 0.5), max.words=80, random.order=FALSE, rot.per=0) 

#### 긍부정 단어 빈도 추이
esg.all %>% 
  mutate(Date=floor_date(esg.all$Date, unit="month")) %>%
  filter(Date >= "2015-01-01" & Date < "2022-01-01") %>%
  mutate(text=str_replace_all(text, pattern="[^[:alpha:]]", replacement=" ")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by="word") %>%
  mutate(word=wordStem(word)) %>%
  mutate(word=gsub(pattern="can|could|will|would|should|must|may|might|trump",
                   replacement="", word)) %>%
  filter(nchar(word) > 2) %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(Company, sentiment, Date, sort=TRUE, name="count") %>%  #월별 긍부정 단어 수 카운트
  group_by(Company, sentiment) %>%
  ungroup() %>%
  ggplot(aes(x=Date, y=count, 
             fill=factor(sentiment, levels=c("positive", "negative")), 
             color=factor(sentiment, levels=c("positive", "negative")))) +
  geom_area(position="identity", alpha=0.3) +
  geom_line(size=1) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  facet_wrap(~ factor(Company, labels=c("Tesla", "Volkswagen")), nrow=2) +
  scale_fill_manual(labels=c("Positive", "Negative"),
                    values=c("cornflowerblue", "coral")) +
  scale_color_manual(labels=c("Positive", "Negative"),
                     values=c("cornflowerblue", "coral")) +
  scale_x_continuous(breaks=seq(ymd('2015-01-01'), ymd('2022-01-01'), by ='years'),
                     labels=as.character(seq(2015, 2022, by=1))) +
  labs(title="Positive/Negative Word Counts over Time (Monthly)",
       x=NULL, y="Count") +
  theme(legend.position = "right", legend.title = element_blank(),
        axis.text.x = element_text(size=8),
        plot.title=element_text(face="bold", size=16, hjust=0.5)) 

