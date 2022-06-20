#### ��ũ����

# ���� �߻� ����
Sys.setenv("http_proxy"="")
Sys.setenv("no_proxy"=TRUE)
Sys.setenv("no_proxy"=1)

# ��Ű��
library(rvest)
library(stringr)
library(XML)
library(tidyverse)
library(openxlsx)

# ���ڵ� ���� ���� 
fileEncoding = "UTF-8"

# ȸ�� ����Ʈ
company_list <- c("tesla", "volkswagen")

# ESG ��ųʸ�
esg_dic <- read.csv(file="ESG_Dictionary.csv", header=T)
esg_dic


# ����� 
scraper <- function() {
  for (i in c(1:2)) {
    # ESG Ŭ������ 
    for (k in c(1:3)) {
      # ������ �� ������������ �����
      df <- tibble()
      # ESG ����Ŭ������(������ �ܾ�)
      for (j in c(1:17)) {
        start_pg <- 1
        
        url <- paste0("https://www.ft.com/search?q=", company_list[i], "%2B", esg_dic[k][j,], "&page=", as.character(start_pg),"&sort=date")
        html <- read_html(url)
        html.parsed <- htmlParse(html)
        pg <- xpathSApply(html.parsed, "//span[@class='search-pagination__page']", fun=xmlValue) %>%
          str_extract_all("([0-9]+)")
        end_pg <- pg[[1]][2]
  
        # ����������
        for (n in c(1:as.numeric(end_pg))){
          url <- paste0("https://www.ft.com/search?q=", company_list[i], "%2B", esg_dic[k][j,], "&page=", as.character(start_pg),"&sort=date")
          html <- read_html(url)
          html.parsed <- htmlParse(html)
          print(url)
          Sys.sleep(3)
          
          # 1������ ����
          start_pg <- start_pg + 1
          print('start_pg')
          print(start_pg)
          
          # ���� ������ ���ڰ� ������ ������ ���ں��� ũ�� break
          if (start_pg > as.numeric(end_pg)) {
            print('Page number has exceeded')
            break
          }
          # ���� ������ ���ڰ� 40 �̻��̸� break(���� ����)
          if (start_pg >= 40) {
            print('Page number has exceeded')
            break
          }
          
          # ����
          title <- xpathSApply(html.parsed, "//div[@class='o-teaser__heading']/a", fun=xmlValue)
          
          # ����
          text <- xpathSApply(html.parsed, "//p[@class='o-teaser__standfirst']/a", fun=xmlValue)
          
          # ��¥
          date <- xpathSApply(html.parsed, "//div[@class='o-teaser__timestamp']/time", fun=xmlValue)

          # ���� ������ �ڵ�
          if (length(title) != length(text)) {
            next
          }
          
          # �����������ӿ� ��ũ������ ������ȭ �Բ� �߰��� ȸ���, esg Ŭ����, esg ����Ŭ����
          company <- company_list[i]
          comp_list <- rep(company, length(text))
          esg <- colnames(esg_dic)[k]
          esg_list <- rep(esg, length(text))
          subclass <- esg_dic[k][j,]
          subclass_list <- rep(subclass, length(text))
          Sys.sleep(2)
          
          # �ش� �������� ���� ������������ �����
          news_df <- tibble(Company=unlist(comp_list), ESG=esg_list, Subclass=subclass_list, Title=title, Text=text, Date=date)
          # ���� �����������ӿ� �߰��ϱ�
          df <- rbind(df, news_df)
        }
      }
      strn <- paste(company, "_", tolower(substr(esg, 1, 1)), ".xlsx", sep="")
      print(strn)
      # ���� ���Ϸ� ����
      write.xlsx(df, file=strn) 
      print('�������Ϸ� ����')
    }
  }
}


#### �Լ� ����
scraper()


#### �ߺ� ������ ����
library(readxl)

# ���� ����Ʈ 
total_list <- c('tesla_e.xlsx', 'tesla_s.xlsx', 'tesla_g.xlsx', 
              'volkswagen_e.xlsx', 'volkswagen_s.xlsx', 
              'volkswagen_g.xlsx')

for (file in total_list) {
  # ������ �б� 
  data <- read_excel(path = file, sheet=1)
  # �ߺ� ���� 
  data <- data[!duplicated(data$Title),]
  new_name <- gsub(".{5}$", "", file)
  new_name <- paste0("C:\\Users\\user\\Documents\\data_all\\",new_name, 'u.csv')
  # csv ���Ϸ� export
  write.csv(data, new_name) 
}
