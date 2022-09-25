attitude %>% str()

attitude %>% 
  lm(rating ~ complaints + privileges + learning, .) -> fit_at
tidy(fit_at)


attitude %>% 
  lm(rating ~ privileges, .) -> fit_at

attitude %>% 
  lm(rating ~ privileges + complaints, .) -> fit_at

tidy(fit_at)


df %>% 
  lm(행복 ~ GDP + 사회지지, .) ->df_1

tidy(df_1)

df %>% 
  lm(행복 ~ GDP + 사회지지 + 자율성 + 부패지각, .) -> df_2

tidy(df_2)



df %>% 
  lm(vigor ~ dose + sex, .) -> df

tidy(df)