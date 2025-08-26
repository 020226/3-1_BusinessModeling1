# 비즈니스 모델링 HW3 조민선 과제 제출
# 필요한 패키지 로드
library(ggplot2)  # midwest 데이터셋 포함
library(dplyr)    # 데이터 처리용

#---------------------------------------------------------------
# 3. midwest 데이터 분석
#---------------------------------------------------------------

# 3.1 midwest 데이터 불러오기 및 특징 파악
data("midwest")  # ggplot2 패키지에서 midwest 데이터 로드

# 데이터 구조 확인
str(midwest)

# 요약 통계 확인
summary(midwest)

# 데이터 차원 확인
dim(midwest)

# 변수명 확인
names(midwest)

# 데이터 앞부분 확인
head(midwest)

# 3.1에 대한 데이터 특징 설명
# midwest 데이터는 미국 동북중부 437개 지역의 인구통계 정보를 담고 있습니다.
# 총 28개의 변수(컬럼)를 포함하고 있으며, 주요 변수로는:
# - PID: 개체 식별자
# - county, state: 지역명과 주명
# - area: 면적
# - poptotal: 전체 인구수
# - popdensity: 인구 밀도
# - popwhite, popblack, popamerindian, popasian, popother: 인종별 인구수
# - percwhite, percblack, percamerindan, percasian, percother: 인종별 인구 비율
# - popadults: 성인 인구수
# - perchsd, percollege: 고등학교/대학 졸업 비율
# - percprof: 전문직 비율
# - poppovertyknown: 빈곤 상태가 알려진 인구
# - percpovertyknown: 빈곤층 비율
# - percbelowpoverty: 빈곤선 이하 인구 비율
# - percchildbelowpovert: 빈곤선 이하 아동 비율
# - percadultpoverty: 빈곤 성인 비율
# - percelderlypoverty: 빈곤 노인 비율
# 데이터는 인구통계적 특성과 사회경제적 지표를 포함하고 있어 지역 간 비교 분석에 유용합니다.

# 3.2 변수명 수정 (poptotal -> total, popasian -> asian)
midwest <- midwest %>%
  rename(total = poptotal, asian = popasian)

# 변수명 변경 확인
names(midwest)

# 3.3 전체 인구 대비 아시아 인구 백분율 파생변수 생성 및 히스토그램 작성
midwest <- midwest %>%
  mutate(asian_pct = (asian / total) * 100)

# 아시아 인구 백분율 히스토그램
hist(midwest$asian_pct, 
     main = "아시아 인구 백분율 분포", 
     xlab = "아시아 인구 백분율", 
     col = "skyblue", 
     breaks = 20)

# 3.3에 대한 설명 
# 히스토그램을 통해 아시아 인구 백분율의 분포를 살펴보면,
# 대부분의 지역에서 아시아 인구 비율이 매우 낮은 것을 확인할 수 있습니다.
# 히스토그램이 왼쪽으로 크게 치우친 형태(right-skewed)로 나타나며,
# 대부분의 지역에서 아시아 인구 비율이 1% 미만인 것으로 보입니다.
# 일부 소수 지역만 상대적으로 높은 아시아 인구 비율을 보이고 있습니다.

# 3.4 아시아 인구 백분율 평균 계산 및 large/small 분류 변수 생성
asian_pct_mean <- mean(midwest$asian_pct)
cat("아시아 인구 백분율 평균:", asian_pct_mean, "\n")

midwest <- midwest %>%
  mutate(asian_category = ifelse(asian_pct > asian_pct_mean, "large", "small"))

# 3.5 large/small 분류에 따른 빈도표 및 막대 그래프 생성
asian_category_freq <- table(midwest$asian_category)
asian_category_freq  # 빈도표 출력

# 막대 그래프 작성
barplot(asian_category_freq, 
        main = "아시아 인구 비율 분류에 따른 지역 수", 
        xlab = "분류", 
        ylab = "지역 수", 
        col = c("coral", "skyblue"))

# 3.5에 대한 설명
# 아시아 인구 비율이 평균보다 높은 "large" 지역과 평균 이하인 "small" 지역으로 구분했을 때,
# "small" 지역이 "large" 지역보다 훨씬 많은 것으로 나타났습니다.
# 이는 아시아 인구 비율이 평균보다 높은 일부 지역이 전체 평균을 상향시키고 있음을 의미합니다.
# 즉, 아시아 인구 비율이 매우 높은 소수의 지역이 존재하고, 대부분의 지역은 상대적으로 낮은 비율을 보이는
# 오른쪽으로 치우친(right-skewed) 분포를 가지고 있음을 확인할 수 있습니다.

#---------------------------------------------------------------
# 5. dplyr 함수를 활용한 midwest 데이터 분석
#---------------------------------------------------------------

# 5.1 전체인구대비 미성년인구 백분율 변수 추가
midwest <- midwest %>%
  mutate(minor_pct = ((total - popadults) / total) * 100)

# 5.2 미성년 인구 백분율이 가장 높은 상위 5개 county 출력
top5_minor <- midwest %>%
  select(county, minor_pct) %>%
  arrange(desc(minor_pct)) %>%
  head(5)

print(top5_minor)  # 상위 5개 county 출력

# 5.3 미성년 비율 등급 변수 추가 및 등급별 지역 수 확인
midwest <- midwest %>%
  mutate(minor_grade = case_when(
    minor_pct >= 40 ~ "large",
    minor_pct >= 30 & minor_pct < 40 ~ "middle",
    TRUE ~ "small"
  ))

# 등급별 지역 수 확인 (빈도표)
minor_grade_freq <- table(midwest$minor_grade)
print(minor_grade_freq) # 지역 수 출력

# 5.4 아시아인 인구 백분율 하위 10개 지역 출력
bottom10_asian <- midwest %>%
  select(state, county, asian_pct) %>%
  arrange(asian_pct) %>%
  head(10)

print(bottom10_asian)  # 하위 10개 지역 출력
