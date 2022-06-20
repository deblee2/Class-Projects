#### 스크래핑

# 오류 발생 방지
Sys.setenv("http_proxy"="")
Sys.setenv("no_proxy"=TRUE)
Sys.setenv("no_proxy"=1)

# 패키지
library(rvest)
library(stringr)
library(XML)
library(tidyverse)
library(openxlsx)

# 인코딩 오류 방지 
fileEncoding = "UTF-8"

# 회사 리스트
company_list <- c("tesla", "volkswagen")

# ESG 딕셔너리
esg_dic <- read.csv(file="ESG_Dictionary.csv", header=T)
esg_dic


# 기업별 
scraper <- function() {
  for (i in c(1:2)) {
    # ESG 클래스별 
    for (k in c(1:3)) {
      # 통합할 빈 데이터프레임 만들기
      df <- tibble()
      # ESG 서브클래스별(각각의 단어)
      for (j in c(1:17)) {
        start_pg <- 1
        
        url <- paste0("https://www.ft.com/search?q=", company_list[i], "%2B", esg_dic[k][j,], "&page=", as.character(start_pg),"&sort=date")
        html <- read_html(url)
        html.parsed <- htmlParse(html)
        pg <- xpathSApply(html.parsed, "//span[@class='search-pagination__page']", fun=xmlValue) %>%
          str_extract_all("([0-9]+)")
        end_pg <- pg[[1]][2]
  
        # 페이지별로
        for (n in c(1:as.numeric(end_pg))){
          url <- paste0("https://www.ft.com/search?q=", company_list[i], "%2B", esg_dic[k][j,], "&page=", as.character(start_pg),"&sort=date")
          html <- read_html(url)
          html.parsed <- htmlParse(html)
          print(url)
          Sys.sleep(3)
          
          # 1페이지 증가
          start_pg <- start_pg + 1
          print('start_pg')
          print(start_pg)
          
          # 시작 페이지 숫자가 마지막 페이지 숫자보다 크면 break
          if (start_pg > as.numeric(end_pg)) {
            print('Page number has exceeded')
            break
          }
          # 시작 페이지 숫자가 40 이상이면 break(오류 방지)
          if (start_pg >= 40) {
            print('Page number has exceeded')
            break
          }
          
          # 제목
          title <- xpathSApply(html.parsed, "//div[@class='o-teaser__heading']/a", fun=xmlValue)
          
          # 본문
          text <- xpathSApply(html.parsed, "//p[@class='o-teaser__standfirst']/a", fun=xmlValue)
          
          # 날짜
          date <- xpathSApply(html.parsed, "//div[@class='o-teaser__timestamp']/time", fun=xmlValue)

          # 오류 방지용 코드
          if (length(title) != length(text)) {
            next
          }
          
          # 데이터프레임에 스크래핑할 데이터화 함께 추가할 회사명, esg 클래스, esg 서브클래스
          company <- company_list[i]
          comp_list <- rep(company, length(text))
          esg <- colnames(esg_dic)[k]
          esg_list <- rep(esg, length(text))
          subclass <- esg_dic[k][j,]
          subclass_list <- rep(subclass, length(text))
          Sys.sleep(2)
          
          # 해당 페이지에 대한 데이터프레임 만들기
          news_df <- tibble(Company=unlist(comp_list), ESG=esg_list, Subclass=subclass_list, Title=title, Text=text, Date=date)
          # 최종 데이터프레임에 추가하기
          df <- rbind(df, news_df)
        }
      }
      strn <- paste(company, "_", tolower(substr(esg, 1, 1)), ".xlsx", sep="")
      print(strn)
      # 엑셀 파일로 저장
      write.xlsx(df, file=strn) 
      print('엑셀파일로 저장')
    }
  }
}


#### 함수 실행
scraper()


#### 중복 데이터 제거
library(readxl)

# 파일 리스트 
total_list <- c('tesla_e.xlsx', 'tesla_s.xlsx', 'tesla_g.xlsx', 
              'volkswagen_e.xlsx', 'volkswagen_s.xlsx', 
              'volkswagen_g.xlsx')

for (file in total_list) {
  # 데이터 읽기 
  data <- read_excel(path = file, sheet=1)
  # 중복 제거 
  data <- data[!duplicated(data$Title),]
  new_name <- gsub(".{5}$", "", file)
  new_name <- paste0("C:\\Users\\user\\Documents\\data_all\\",new_name, 'u.csv')
  # csv 파일로 export
  write.csv(data, new_name) 
}

