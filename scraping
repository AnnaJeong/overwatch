library(tidyverse)
library(rvest)
library(stringr)

## 2019/09/25일에 수집한 데이터
##각 티어별 중간 10페이지(천명)을 추출해 비교



overwatch_doc<-list()
url<-"http://overlog.gg/leaderboards/global/rank/"

overwatch_doc<-url %>%
  read_html()


##function으로 만들기
scraping <- function(page_number){
  url_base<-"http://overlog.gg/leaderboards/kr/rank/"
  url <- paste0(url_base, page_number)
  overwatch_doc<-url %>%
    read_html
  
# ##자료넣기
 #id
   id <- overwatch_doc %>%
   html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-Player > a > b") %>%
   html_text %>%
   str_replace_all('(?<=<b>).+?(?=</b>)',"")
 #level
   level <- overwatch_doc %>%
     html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-Player > a > em") %>%
     html_text() %>%
     str_replace_all('Lv. ',"") %>%
     as.numeric()  
 #score
   score <- overwatch_doc %>%
     html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr >td.ContentCell.ContentCell-SkillRating") %>%
     html_text() %>%
     str_replace_all('\\s+',"") %>%
   as.numeric()
 #killdeath
 killdeath <- overwatch_doc %>%
   html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-KD > b") %>%
   html_text() %>%
   str_replace_all(' : 1',"")  %>%
   as.numeric()
 #winratio
 winratio <- overwatch_doc %>%
   html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-WinRatio > div > div.WinRatio > b") %>%
   html_text() %>%
   as.numeric()
 #playtime
 playtime <- overwatch_doc%>%
   html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-PlayTime") %>%
   html_text() %>%
   str_replace_all(" hours","") %>%
   str_replace_all(" hour", "") %>%
   as.numeric()
 #mostheroes
most1 <- overwatch_doc %>%
  html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-MostHeros > img:nth-child(1)") %>%
  html_attr("alt")
  
most2 <- overwatch_doc %>%
  html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-MostHeros > img:nth-child(2)") %>%
  html_attr("alt") 
most3 <- overwatch_doc %>%
  html_nodes("#LeaderBoardsLayoutContent > table > tbody > tr > td.ContentCell.ContentCell-MostHeros > img:nth-child(3)") %>%
  html_attr("alt") 


list(id,level,score,killdeath,winratio,playtime,most1,most2,most3)

}


ID <- LEVEL <- SCORE <- KILLDEATH <- WINRATIO <- PLAYTIME <- MOST1 <- MOST2 <- MOST3 <-NULL
##1_80은 그랜드마스터 
##81부터 300은 마스터
##301부터 1010은 다이이몬드
##1011부터 2554은 플래티넘
##2555부터 4418은 골드
##4419부터 5515는 실버
##5746부터 5755는 브론 
for (i in 5746:5747){
ID <- unlist(c(ID,scraping(i)[1]))
LEVEL <- unlist(c(LEVEL,scraping(i)[2]))
SCORE <- unlist(c(SCORE, scraping(i)[3]))
KILLDEATH <- unlist(c(KILLDEATH, scraping(i)[4]))
WINRATIO <- unlist(c(WINRATIO, scraping(i)[5]))
PLAYTIME <- unlist(c(PLAYTIME, scraping(i)[6]))
MOST1 <- unlist(c(MOST1,scraping(i)[7]))
MOST2 <- unlist(c(MOST2, scraping(i)[8]))
MOST3 <- unlist(c(MOST3, scraping(i)[9]))
}
#MOST2와 MOST3가 존재하지만 결측치가 많아 제외하기로 한다 
overwatch <- data.frame(ID,LEVEL,SCORE,KILLDEATH,WINRATIO,PLAYTIME,MOST1)
