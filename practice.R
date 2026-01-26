library(lubridate)

df <- readxl::read_xlsx("/Users/yd/Documents/Statistic/강의자료만들기/2026/education/data/sample_data_sports100.xlsx")
df_name <- readxl::read_xlsx("/Users/yd/Documents/Statistic/강의자료만들기/2026/education/data/column_info.xlsx")

df_new <- df %>% 
  rename(키 = ITEM_F001,
         몸무게 = ITEM_F002,
         성별 = TEST_SEX) %>% 
  select(성별, TEST_AGE, TEST_MD, 키, 몸무게)

df_new <- df_new %>% 
  mutate(TEST_MD = ymd(TEST_MD)) %>% 
  mutate(year = year(TEST_MD))


## BMI 지수 계산 공식: 체중 / 신장^2
df_new <- df_new %>% 
  mutate(height_adj = 키 / 100,
         BMI = 몸무게 / height_adj^2
         )

## 연령 별로 그룹화
df_new <- df_new %>% 
  mutate(
    age_group = case_when(
      TEST_AGE < 20    ~ "1",
      TEST_AGE < 40   ~ "2",
      TEST_AGE < 60   ~ "3",
      TRUE       ~ "4"
    )
  )

## 연령 별 / 성별 테이블 생성
df_new %>% 
  group_by(성별, age_group) %>% 
  summarise(n = n())

## BMI 지수가 10 이상인 사람들에 대해서 연령별로 키 평균
df_new %>% 
  filter(BMI >= 10) %>% 
  group_by(age_group) %>% 
  summarise(mean(키))

## 연도별로 남/녀 비율
df_new %>% 
  group_by(year, 성별) %>% 
  summarise(n = n()) %>% 
  mutate(n / sum(n))

df_new %>% 
  group_by(year, 성별) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(n / sum(n))


## 성별/연령별 BMI 중위수 계산
df_new %>% 
  group_by(성별, age_group) %>% 
  summarise(median(BMI))


df_summary <- df %>% 
  mutate(TEST_MD = ymd(TEST_MD)) %>% 
  mutate(year = year(TEST_MD))

summary_stat <- df_summary %>%
  group_by(year, TEST_SEX) %>%
  summarise(
    across(c(ITEM_F001, ITEM_F002, ITEM_F003), 
           list(mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE),
                min = ~ min(.x, na.rm = TRUE),
                max = ~ max(.x, na.rm = TRUE)
           ),
           .names = "{.col}-{.fn}"
    ),
    .groups = "keep"
  )

summary_stat <- summary_stat %>%
  pivot_longer(
    cols = starts_with("ITEM"),
    names_sep = "-", 
    names_to = c("varname_eng", "stat"),
    values_to = "value"
  )


df_var_names <- df_name %>%
  select( varname_eng , varname_kor)

summary_stat %>%
  left_join(df_var_names,
             by = "varname_eng")

