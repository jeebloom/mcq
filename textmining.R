c(
  "feather", "rio", "textdata", "text2vec",  # import data 
  "tidyverse", "tidytable", "tidytext", "lubridate", # cleaning data
  "psych", "skimr", "janitor", "broom", # data analysis
  "gt", "GGally"  # communicate data
) -> pkg

sapply(pkg, function(x) {
  if(!require(x, ch = T)) install.packages(x, dependencies = T) 
  library(x, ch = T)
})


# (mac os 에서만 실행) ggplot 한글 호환
par(family="NanumGothic")
theme_set(theme_gray(base_family= "NanumGothic"))

# (mac os 에서만 실행) 상관관계 그래프 한글 호환 
library(showtext)
font_add("NanumGothic", "NanumGothic.ttf")
showtext_auto()
theme(text = element_text(family = "NanumGothic"))


import("news_20190101_20220923_M.xlsx") -> df

glimpse(df)# 구조 확인

df %>% select(일자, 제목, 본문, 언론사, cat = `통합 분류1`, 키워드) -> df

set.seed(37)
sample_df <- df %>% sample_n(size = 100)
sample_df %>% glimpse()


df2 <- 
  df %>% 
  # 중복기사 제거
  distinct(제목, .keep_all = T) %>% 
  # 기사별 ID부여
  mutate(ID = factor(row_number())) %>% 
  # 월별로 구분한 열 추가(lubridate 패키지)
  mutate(week = week(ymd(일자))) %>%       
  # 기사 제목과 본문 결합
  unite(제목, 본문, col = "text", sep = " ") %>% 
  # 중복 공백 제거
  mutate(text = str_squish(text)) %>% 
  # 언론사 구분: 야당지, 여당지 %>% 
  mutate(press = case_when(
    언론사 == "조선일보" ~ "야당지",
    언론사 == "중앙일보" ~ "야당지",
    언론사 == "경향신문" ~ "여당지",
    TRUE ~ "여당지") ) %>% 
  # 기사 분류 구분 
  separate(cat, sep = ">", into = c("cat", "cat2")) %>% 
  # IT_과학, 경제, 사회 만 선택
  select(-cat2) %>% 
  # 분류 구분: 사회, 비사회
  mutate(catSoc = case_when(
    cat == "정치" ~ "정치면",
    TRUE ~ "비정치면") )

df2 %>% glimpse()


df2 %>% count(cat, sort = T)
df2 %>% count(catSoc, sort = T)
df2 %>% count(press, sort = T)

"!@#$... 전각ㆍㅣ문자 %^&*()" %>% str_remove("\\w+")

fullchar_v <- "ㆍ|ㅣ|‘|’|“|”|○|●|◎|◇|◆|□|■|△|▲|▽|▼|〓|◁|◀|▷|▶|♤|♠|♡|♥|♧|♣|⊙|◈|▣"

df_tk <- 
  df2 %>% 
  mutate(키워드 = str_remove_all(키워드, "[^(\\w+|\\d+|,)]")) %>% 
  mutate(키워드 = str_remove_all(키워드, fullchar_v)) %>% 
  unnest_tokens(word, 키워드, token = "regex", pattern = ",") 

df2 %>% arrange(ID) %>% head(30)


df_tk %>% arrange(ID) %>% tail(30)


count_df <-
  df_tk %>% count(word, sort =T)

count_df %>% tail(20)




combined_df <-
  df_tk %>%
  group_by(ID) %>%
  summarise(text2 = str_flatten(word, " ")) %>%
  ungroup() %>% 
  inner_join(df2, by = "ID")


combined_df %>% glimpse()


library(stm)

processed <-
  combined_df %>% textProcessor(
    documents = combined_df$text2,
    metadata = .,
    wordLengths = c(2, Inf)
  )

summary(processed)

out <-
  prepDocuments(processed$documents,
                processed$vocab,
                processed$meta,
                lower.thresh = 0)
summary(out)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

### 분석 

## topic 수 설정 

topicN <- c(3, 9, 100)
storage <- searchK(docs, vocab, K = topicN)
storage
plot(storage)


# 모형 구성

t1 <- Sys.time()
meta_fit <-
  stm(
    documents = docs,
    vocab = vocab,
    data = meta,
    K = 9,         
    prevalence =~ press + s(week, 6), # 투입하는 공변인
    max.em.its = 75,                # 최대 반복계산 회수 
    verbose = F,                    # 반복계산결과 화면출력 여부
    init.type = "Spectral",
    seed = 37 
  )
t2 <- Sys.time()
t2-t1

summary(meta_fit)

## A topic model with 9 topics, 695 documents and a 20050 word dictionary.
##Topic 1 Top Words:
##  Highest Prob: 여성, 남성, 이대남, 페미니즘, 폐지, 여가부, 갈등 
## FREX: 성범죄, 성폭력, 여가부, 무고죄, 폐지, 페미니즘, 가해자 
## Lift: eb, 가상통화, 가해자들, 갈등구조, 개념녀, 개새끼, 갤러리아백화점 
## Score: 여가부, 페미니즘, 성평등, 게이, 무고죄, 응답자군, 공약 
## Topic 2 Top Words:
##  Highest Prob: 후보, 대선, 이재명, 조사, 여성, 국민, 윤석열 
##FREX: 멸치, 멸공, 심상정, 후보, 지상파, 개표, tv조선 
##Lift: ar, ct, poll, rdd, tv조선, wwwnesdcgokr, 강북 
##Score: 후보, 이재명, 멸공, 출구조사, 포인트, 개표, 출구 
##Topic 3 Top Words:
##  Highest Prob: 여성, 남성, 대선, 후보, 선거, 투표, 정치 
## FREX: 후반, 기권, 투표율, 정호영, 집회, 투표, 초반 
##Lift: shoutout, 개인사, 기타후보, 김진아, 정치성향, 기권자, antifeminist 
##Score: 기권율, 기권, 민금, 투표율, 예빈, 은설, 감별 
##Topic 4 Top Words:
##  Highest Prob: 여성, 남성, 사회, 정책, 여가부, 청년, 남녀 
##FREX: 여성징병제, 그래프, 여성부, 제대, 할머니, 성비, 채용 
##Lift: agestibeecom, aouux, blognavercom, buy, career, central, crush 
##Score: 그래프, 응답자군, 성비, 제대, 여가부, 할머니들, 성별격차 
##Topic 5 Top Words:
##  Highest Prob: 세대, 사회, 청년, 생각, 정치, 공정, 사람 
##FREX: 리터러시, 우영우, 구원, 권민우, 강성태, 달팽이, 오수재 
##Lift: great, ox, 가족자유주의, 강남규, 강의록, 개마고원, 개발주의 
##Score: 리터러시, 우영우, 텍스트, 구원, 권민우, 오수재, 기안 
##Topic 6 Top Words:
##  Highest Prob: 남성, 혐오, 여성, 페미니즘, 이준석, 논란, 젠더 
##FREX: gs, 일베, 박나래, 포스터, 남혐, 최고위원, 모양 
##Lift: bj, camp, emot, gs리테일, item, megal, musthav 
##Score: gs, 일베, 포스터, 박나래, 최고위원, 남혐, 혐오표현 
##Topic 7 Top Words:
##  Highest Prob: 대통령, 국민, 후보, 대선, 의원, 윤석열, 민주당 
##FREX: 정권교체, 노무현, 경선, 교체, 상임고문, 정권, 개딸 
##Lift: 대깨윤, 연기자, 의원내각제, 지사님, 친칠라, ars조사, bbk 
##Score: 개딸, 엠팍, mb정부, 상임고문, 교체, 이재명, 단일화 
##Topic 8 Top Words:
##  Highest Prob: 공약, 정부, 병사, 청년, bts, 예산, 검찰 
##FREX: 급식, 병역법, 이휘, 사극, 연모, 남장, 공정위 
##Lift: alookso, appeal, armi, a후보, bill, bl, bp 
##Score: bts, 급식, 병사, 탈모, 봉급, 특례, 이휘 
##Topic 9 Top Words:
##  Highest Prob: 대표, 이준석, 정치, 청년, 국민, 생각, 민주당 
##FREX: 비대위, 청년정치, 이정인, 비서관, 징계, 공천, 위원장 
##Lift: ppat, 가석방, 개인택시, 거취, 공천권, 권력투쟁, 그물 
##Score: 윤리위, 비대위, 이정인, 징계, 비서관, 이준석, 청년정치 


# 주제 이름 짓기 
# 주제별 단어와 원문 결합 

findThoughts(
  model = meta_fit,     # 구성한 주제모형
  texts = df2$text,  # 문서 본문 문자 벡터
  topics = c(1, 2),     # 찾고자 하는 주제의 값. 기본값은 모든 주제
  n = 3                 # 찾고자 하는 문서의 수
)

td_gamma <- meta_fit %>% tidy(matrix = "gamma")
td_gamma$document <- as.integer(td_gamma$document)
combined_df$ID <- as.integer(combined_df$ID) 

text_gamma <- 
  combined_df %>% 
  select(ID, text2, text, 키워드) %>% 
  left_join(td_gamma, by = c("ID" = "document")) %>% 
  pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "tGamma",
    values_fill = 0
  )

text_gamma %>% glimpse()  


text_gamma %>% 
  arrange(-tGamma7) %>% 
  pull(text) %>% head(9)

text_gamma %>% 
  arrange(-tGamma7) %>% 
  pull(키워드) %>% .[6]

text_gamma %>% 
  arrange(-tGamma2) %>% 
  filter(str_detect(text, "페미니즘")) %>% 
  mutate(text = str_replace_all(text, "페미니즘", "**페미니즘**")) %>% 
  pull(text) %>% 
  head(5)


labelTopics(meta_fit)

topic_name <- tibble(topic = 1:9,
                     name = c("1. 여가부 폐지",
                              "2. 대통령 후보자",
                              "3. 대통령 선거",
                              "4. 청년 정책",
                              "5. 청년 세대",
                              "6. 혐오",
                              "7. 정치 관련",
                              "8. 청년 공약",
                              "9. 청년 정치") )


td_beta <- meta_fit %>% tidy(matrix = 'beta') 

term_topic_name <- 
  td_beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 7) %>% 
  left_join(topic_name, by = "topic")

term_topic_name

# 주제별 단어 분포도 
term_topic_name %>% 
  
  ggplot(aes(x = beta, 
             y = reorder_within(term, beta, name),  # 각 주제별로 재정렬
             fill = name)) +
  geom_col(show.legend = F) +
  facet_wrap(~name, scales = "free") +
  scale_y_reordered() +                             # 재정렬한 y축의 값 설정
  labs(x = expression("단어 확률분포: "~beta), y = NULL,
       title = "주제별 단어 확률 분포",
       subtitle = "주제별로 다른 단어들로 군집") +
  theme(plot.title = element_text(size = 20))



# 주제별 문서 분포도

td_gamma <- meta_fit %>% tidy(matrix = 'gamma') 

doc_topic_name <- 
  td_gamma %>% 
  group_by(topic) %>% 
  left_join(topic_name, by = "topic")

doc_topic_name

doc_topic_name %>% 
  ggplot(aes(x = gamma, fill = name)) +
  geom_histogram(bins = 50, show.legend = F) +
  facet_wrap(~name) + 
  labs(title = "주제별 문서 확률 분포",
       y = "문서(기사)의 수", x = expression("문서 확률분포"~(gamma))) +
  theme(plot.title = element_text(size = 20))



# 주제별 단어-문서 분포

# 주제별 상위 7개 단어 추출
top_terms <- 
  td_beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 7) %>% 
  select(topic, term) %>% 
  summarise(terms = str_flatten(term, collapse = ", ")) 

# 주제별 감마 평균 계산  
gamma_terms <- 
  td_gamma %>% 
  group_by(topic) %>% 
  summarise(gamma = mean(gamma)) %>% 
  left_join(top_terms, by = 'topic') %>%  # 주제별 단어 데이터프레임과 결합
  left_join(topic_name, by = 'topic')     # 주제 이름 데이터프레임과 결합


gamma_terms

gamma_terms %>% 
  
  ggplot(aes(x = gamma, y = reorder(name, gamma), fill = name)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = round(gamma, 2)), # 소수점 2자리 
            hjust = 1.15) +                # 라벨을 막대도표 안쪽으로 이동
  geom_text(aes(label = terms), 
            hjust = -0.05) +              # 단어를 막대도표 바깥으로 이동
  scale_x_continuous(expand = c(0, 0),    # x축 막대 위치를 Y축쪽으로 조정
                     limit = c(0, .8)) +   # x축 범위 설정
  labs(x = expression("문서 확률분포"~(gamma)), y = NULL,
       title = "코로나19와 백신 관련보도 상위 주제어",
       subtitle = "주제별로 기여도가 높은 단어 중심") +
  theme(plot.title = element_text(size = 20))


# 공변인 분석

out$meta$rating <- as.factor(out$meta$press)
prep <- estimateEffect(formula = 1:9 ~ press + s(week, 6), 
                       stmobj = meta_fit,
                       metadata = out$meta,
                       uncertainty = "Global")

summary(prep, topics= 1:9)


combined_df %>% names()
combined_df %>% 
  left_join(td_gamma, by = c("ID" = "document")) %>% 
  pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "tGamma",
    values_fill = 0
  ) %>% 
  
  arrange(-tGamma1) %>% 
  filter(str_detect(text, "백신")) %>% 
  mutate(text = str_replace_all(text, "백신", "**백신**")) %>% 
  head(30)

combined_df %>% names()
combined_df %>% 
  left_join(td_gamma, by = c("ID" = "document")) %>% 
  pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "tGamma",
    values_fill = 0
  ) %>% 
  
  arrange(-tGamma1) %>% 
  filter(str_detect(text, "백신")) %>% 
  mutate(text = str_replace_all(text, "백신", "**백신**")) %>% 
  pull(text) %>% .[1:10]



# 정치성향에 따른 주제분포
plot.estimateEffect(
  prep,
  covariate = "press",
  topics = c(1, 2, 4),
  model = meta_fit,
  method = "difference",
  cov.value1 = "여당지",
  cov.value2 = "야당지",
  xlab = "문서당 주제 분포 비율(야당지 대 여당지)",
  main = "언론사 정치성향에 따른 문서별 주제 분포",
  xlim = c(-.1, .1),
  labeltype = "custom",
  custom.labels = c("주제1", "주제2", "주제4")
)

# 주제 이름
topic_name


# 공변인 계수
coef_df <- 
  prep %>% tidy() %>% 
  filter(term == "press여당지")
coef_df


# 주제별 상위 10개 단어 추출
top_terms <- 
  meta_fit %>% tidy(matrix = "beta")  %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 7) %>% 
  select(topic, term) %>% 
  summarise(terms = str_flatten(term, " "))


top_terms

# 데이터프레임 결합
term_coef_name <- 
  top_terms %>% 
  left_join(topic_name, by = "topic") %>% 
  left_join(coef_df, by = "topic") 

term_coef_name %>% glimpse()

term_coef_name %>% 
  
  ggplot(aes(x = estimate,
             y = reorder(name, estimate),
             fill = name)) +
  geom_col(show.legend = F) +
  geom_errorbar(aes(xmin = estimate - std.error,
                    xmax = estimate + std.error), 
                width = .9, size = .4, color = "grey10",
                show.legend = F) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-.75, .15),
                     breaks = 0) +
  geom_text(aes(x =-.4, label = terms), show.legend = F) +
  geom_text(aes(label = round(estimate, 3)),
            hjust = -.2) +
  
  labs(x = "문서당 주제 분포 비율(야당지 대 여당지)",
       y = NULL,
       title = "언론사 정치성향에 따른 문서별 주제 분포") +
  theme(plot.title = element_text(size = 20))

plot.estimateEffect(
  prep,
  covariate = "week",    
  topics = c(1, 8),
  model = meta_fit,
  method = "continuous", # 시간대 연속적으로 표시
  xlab = "기간 (1월 ~ 3월)",
  main = "시간대별 주제 분포"
)


# 공변인 계수
coef_time <- 
  prep %>% tidy() %>% 
  filter(str_detect(term, "^s"))
coef_time


# 데이터프레임 결합
term_coef_time <- 
  coef_time %>% 
  left_join(topic_name, by = "topic") 

term_coef_time %>% glimpse()



term_coef_time %>% 
  mutate(term = str_extract(term, "\\d$")) %>% 
  mutate(term = as.integer(term)) %>% 
  mutate(term = term * 2 - 1) %>% 
  mutate(term = as.factor(term)) %>% 
  
  filter(str_detect(name, "^1|^2|^8")) %>% 
  
  ggplot(aes(x = term,
             y = estimate,
             color = name)) +
  geom_line(aes(group = name), size = 1.2) +
  geom_point(aes(shape = name), size = 3,) +
  geom_errorbar(aes(ymin = estimate - std.error, 
                    ymax = estimate + std.error), 
                width = .4, size = 1,
                position = position_dodge(.01)) +
  labs(x = "기간(1월 ~ 3월)",
       y = "문서당 주제 분포 비율",
       title = "시간대별 주제 분포") +
  theme(plot.title = element_text(size = 20))

library(reshape2)

get_lower_tri <- function(x){
  x[upper.tri(x)] <- NA
  return(x)
}

topicCorr(meta_fit) %>% .$cor %>% 
  get_lower_tri() %>% 
  melt(na.rm = T) %>% 
  
  ggplot(aes(x = factor(Var1), 
             y = factor(Var2), 
             fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0,
                       limit = c(-1, 1), space = "Lab") +
  geom_text(aes(Var1, Var2, label = round(value, 3)), color = "black", size = 3) +
  theme_minimal()

