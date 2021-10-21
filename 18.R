# 추천시스템 : 비지도학습
# 평점행렬 (Ratings matrix)
# 유사도와 거리값
# 콘텐츠 기반 필터링 CB , Term Document matrix 동질적 데이터 , knn, kmeans, bayes (python에서)
# item IBCF, UBCF (CF(Collaborative Filtering) : 협업필터링) 

# CF 유사도 척도
# 고유값 분해 : 정방행렬, 대칭행렬, (decompositio)
# 정방행렬을 보장할 수 없다. 비정방행렬
# 비정방행렬에 고유값 분해를 하는 것 : SVD
# sparce matrix (희소행렬) => matrix factorization

# 가중 유클리디안 거리(정규화된 유클리디안 거리)
# Mahalanobis => Tracking에서 사용
# 주성분 분석

# 유사도(similarity)
# pearson similarity
# cosine similarity
# jaccard similarity

# 데이터 추출, 변환, 분석
# - 사용자 평균 평점
# - 사용자 수별 제품 평점의 분산/분포
# - 사용자별 평가 아이템 수
# 추천 모델 개발 및 추천 수행
# - Recommender
# - Predict
# 추천 모델 평가
# - evaluationSchema : 평가안
# - Evaluate
# - Recommender
# - Predict
# - calcPredictionAccuracy (RMSE, MSE, MAE)


################################################################################ 실습 1

#install.packages("recommenderlab")
library(recommenderlab)
m <- matrix(sample(c(NA, 0:5), 100, replace = T, prob = c(.7, rep(.3/6,6))),
            nrow = 10, ncol = 10, dimnames = list(
              user = paste('u', 1:10, sep=''),
              item = paste('i', 1:10, sep='')
            ))

m # 희소행렬(sparce matrix) -> matrix factorization
dim(m)

r <- as(m, "realRatingMatrix") # "realRatingMatrix" => graph 형태로 표현
r
class(r)

dim(r)
dimnames(r)
print(rowCounts(r))
colCounts(r)
rowMeans(r)
getRatings(r)
getRatingMatrix(r)
image(r)
nratings(r)
hist(getRatings(r)) # 도수분포표
as(r, "list")
as(r, "data.frame")
head(as(r, "data.frame"))

#이진행렬 (이름으로 구분하는 것이 아닌 순서에 의해서 열과 행을 결정(처음 열=>행, 두번째 열=>열))
df <- data.frame(user = c(1, 1, 2, 2, 2, 3), items = c(1, 4, 1, 2, 3, 5))
df
b2 <- as(df, "binaryRatingMatrix") # 행 : user, 열 : items # 값이 TRUE , FALSE로 나타남
b2
as(b2, "matrix")

################################################################################ 실습2-1

movie <- read.csv("movie.csv", header = T)
movie
str(movie)
# user a가 moive 4,5 영화를 보지 않음 => 4, 5번 중 추천 받기
movie_t <- t(movie[, -c(1,5,6)]) # t 전치 , 전치 시킨 이유 : 사용자 별로 유사도를 재기 위해서
# cor 상관계수는 열 별로 상관계수를 계산함
movie_t
class(movie_t)
colnames(movie_t) <- c("a", "b", "c", "d", "e")
movie_t
cor(movie_t) # a와 유사도가 가장 높은게 c, d 
movie # => 유사도 가장 높은 c, d가 movie5점수가 제일 높으니까 a에게 movie5를 추천



################################################################################ 문제 1
recomm_w <- read.csv("sf_recomm.csv")
dim(recomm_w)
head(recomm_w)
recomm_df <- recomm_w[c(1:8)]
head(recomm_df)
# NOWAGE REQ_H            REQ_M                REQ_EDU   REQ_R      REQ_ETC 
# 나이   집마련 월적립금  결혼자금 월적립금    교육자금  은퇴자금   기타목돈

# 나와 비슷한 패턴을 보이는 사람의 연금 상품을 추천
# 추천 받을 사람의 데이터
inputdata <- c(0, 208, 55, 60, 35, 0, 25, 183)
# data.frame 생성 (추천대상을 첫번째 데이터로 입력)
recomm_df2 <- rbind(inputdata, recomm_df)
head(recomm_df2)
# 유사도 계산을 위한 데이터 변형
recomm_data <- recomm_df2[2:8] # CODE 제거
recomm_data
str(recomm_data)
# euclidean 거리값 계산 (대상자를 6명으로 제한해서 거리값을 구하고)
# cor() : 열 중심과 상관계수 계산
# dist() : 행 중심과 거리값 계산
# 거리 행렬도 정방행렬, 대칭행렬
test_dist <- dist(recomm_data, method = "euclidean", by_rows = TRUE) # dist는 행간 거리값을 구하고 있음
test_dist
# matrix로 구조 변경
test_dist_m <- as.matrix(test_dist)
test_dist_m[,1]
# 거리 계산값 정렬 (순서를 정렬한 다음) 첫번쨰 데이터를 중심으로한 거리값
test_dist_m_sort <- sort(test_dist_m[,1])
test_dist_m_sort
# 두번째 label반환 (자기자신을 제외한 2번째 있는 데이터를 선택)
result_index <- names(test_dist_m_sort[2]) # INDEX를 추출
class(result_index)
result_index
real_index <- as.numeric(result_index)
real_index
# 추천 데이터 세트에서 5번째 관측치 code 추출 (추천된 사람의 내용 비교)
recomm_df2[real_index, 1]
recomm_df2[real_index, ]
recomm_w[real_index, 9:13]
# 추천 상품 : CODE가 14480743인 사람의 연금상품

################################################################################ 실습 2-2

# 모델을 이용한 추천 (Recommender 모델)
library(reshape2)
movie <- read.csv("movie.csv", header = T)
movie # data.frame

# 거래데이터 -> 평점행렬
movie_long <- melt(id = 1, movie)  # melt 거래데이터 형식으로 변환
movie_long

names(movie_long) <- c("user", "movie", "rating")
movie_long

movie_long <- subset(movie_long, rating != 0)
movie_long

length(movie_long$rating) # 25 => 21 로

# 추천 Recommender 모델 생성
moview_real <- as(movie_long, "realRatingMatrix")
dim(moview_real)
as(moview_real, "matrix")
# a에 대하여 추천 
trainSet <- moview_real[c(2:5), ] # a만 제외
as(trainSet, "matrix")

recommTarget <- moview_real[1,] # a만
# 행간 유사도 : Pearson 상관계수를 이용한 유사도 -> 방향을 중시
recomm_model <- Recommender(trainSet, method = "UBCF", parameter = "Pearson") # 학습
recomm_model

# 훈련에 참여한 데이터와 추천대상자와의 유사도가 구해지고 비어있는 것에 대하여 유사성을 추천
recomm_list <- predict(recomm_model, recommTarget, n = 3)
recomm_list # topNList

recomm_result <- as(recomm_list, "list")
recomm_result # 추천 (높은 순서로 출력됨)

as(recomm_list, 'matrix')


################################################################################ 문제 2
# 3가지 이상 음식 주문한 사람들만 학습자로 지정하고, 음식추천
gloseri <- read.csv("gloseri.csv", header = T)

# 추천 Recommender 모델 생성
gloseri_real <- as(gloseri, "realRatingMatrix") # NA에 대하여 추천

dim(gloseri_real)
dimnames(gloseri_real)
print(rowCounts(gloseri_real))
colCounts(gloseri_real)
rowMeans(gloseri_real)
getRatings(gloseri_real)
getRatingMatrix(gloseri_real)
image(gloseri_real)
nratings(gloseri_real)
hist(getRatings(gloseri_real)) # 도수분포표

as(gloseri_real, "matrix")


trainData <- subset(gloseri_real, rowCounts(gloseri_real) >= 3)
str(trainData)
as(trainData, "matrix")

recomm_model <- Recommender(trainData, method = "UBCF", parameter = "Cosine") # 코사인으로
recomm_model


recommTarget <- gloseri_real[7]
as(recommTarget, 'matrix')
recomm_list <- predict(recomm_model, recommTarget, n = 5)

recomm_result <- as(recomm_list, "list")
recomm_result 

as(recomm_list, 'matrix')




# 7에 대하여 추천 
trainSet <- gloseri_real[c(1:6), ] # 7만 제외
as(trainSet, "matrix")

recommTarget <- gloseri_real[7,] # a만
# 행간 유사도 : Pearson 상관계수를 이용한 유사도 -> 방향을 중시
recomm_model <- Recommender(trainSet, method = "UBCF", parameter = "Pearson") # 학습
recomm_model

# 훈련에 참여한 데이터와 추천대상자와의 유사도가 구해지고 비어있는 것에 대하여 유사성을 추천
recomm_list <- predict(recomm_model, recommTarget)
recomm_list # topNList

recomm_result <- as(recomm_list, "list")
recomm_result 

as(recomm_list, 'matrix')
# 7에게 달걀후라이를 추천한다.

################################################################################ 실습3
# IBCF(열변수) 아이템 기반 추천 시스템
# TRUE/FALSE binaryRatingMatrix를 활용해서 작업
# 이진화 함수 : binarize =>  jaccard similarity(자카드 유사도) 활용

realData <- as(gloseri, 'realRatingMatrix')
as(realData, 'matrix')
realData_b <- binarize(realData, minRating = 1)

trainData <- sample(1:7, 6)
trainSet <- realData_b[trainData]
as(trainSet, 'matrix')


recommTarget <- realData_b[-trainData] 
recommTarget
as(recommTarget, 'matrix')

# 열간에 척도를 적용용
recomm_model <- Recommender(trainSet, method = "IBCF", parameter = "Jaccard") # 학습
recomm_model

recomm_list <- predict(recomm_model, recommTarget, n = 5)
recomm_list # topNList

recomm_result <- as(recomm_list, "list")
recomm_result 

as(recomm_list, 'matrix')

################################################################################ 실습4

data("MovieLense")
MovieLense
head(as(MovieLense, 'matrix')[,c(1:5)])
str(MovieLense)
MovieLense@data

getRatingMatrix(MovieLense)

print(rowCounts(MovieLense)) # 사용자 별로 영화를 본 횟수
colCounts(MovieLense) # 영화 별로 시청한 사용자 명 수

table(rowCounts(MovieLense)>=50) # 565 명
table(colCounts(MovieLense)>=50) # 601개
image(MovieLense[1:100, 1:100])

# NA 정리
ratings_movies <- MovieLense[rowCounts(MovieLense)>=50, colCounts(MovieLense)>=100]
ratings_movies # 565 * 336

idx <- sample(1:nrow(ratings_movies), nrow(ratings_movies)* 0.9)
trainSet <- ratings_movies[idx,]
recomm_target <- ratings_movies[-idx,]
dim(trainSet)
dim(recomm_target)

recomm_model <- Recommender(trainSet, method="UBCF", parameter="Cosine")
recomm_list <- predict(recomm_model, recomm_target, n = 5)
as(recomm_list, 'list')
length(as(recomm_list, 'list'))

head(as(recomm_list, 'list'))
rec_list <- as(recomm_list, 'list')
rec_df <- as.data.frame(rec_list)
rec_df # 사용자가 열로 출력된다.

#전치
result_df <- t(rec_df)
result_df
head(result_df)

colnames(result_df) <- c("추천1", 
                         "추천2",
                         "추천3",
                         "추천4",
                         "추천5")
head(result_df)
str(result_df)

write.csv(result_df, "recommend_movie.csv", row.names = T)


################################################################################문제3
# 아이템별 영화 평가 점수 데이터세트(BINARY DATA)로 추천 모델 생성(ibcf)
# 조건 1) BINARY포멧으로 생성
# 조건 2) 영화를 50회 이상 보고 영화별 100회 이상 본 사용자를 대상으로 하시오
# 조건 3) 사용자는 90% 훈련 참여, 10% 추천으로 데이터 분할
# 10% 데이터에 대하여 영화 5개씩 추천


data("MovieLense")
MovieLense

MOVIES <- MovieLense[rowCounts(MovieLense)>=50, colCounts(MovieLense)>=100]
movieData <- as(MOVIES, 'realRatingMatrix')
as(movieData, 'matrix')
movieData_b <- binarize(movieData, minRating = 1)
as(movieData_b, 'matrix')

trainData <- sample(1:nrow(movieData_b), 0.9 * nrow(movieData_b))
trainSet <- movieData_b[trainData]
recommTarget <- movieData_b[-trainData] 
recommTarget
dim(trainSet)
dim(recommTarget)

recomm_model <- Recommender(trainSet, method = "IBCF", parameter = "Jaccard") # 학습
recomm_model
recomm_list <- predict(recomm_model, recommTarget, n = 5)
recomm_list # topNList

recomm_result <- as(recomm_list, "list")
recomm_result 

# 추천 모델 평가 (evaluate)

# 평가 계획 세우기
eval_scheme <- evaluationScheme(data = MOVIES,
                                method = "split", # cross_validation(교차평가), bootstrap
                                train = 0.8, # 학습데이터를 0.8로
                                given = 15, # 사용자별 평가를 위한 아이템 수 
                                goodRating = 5, # 좋은 평가 등급 지정
                                k = 5) # simulation 횟수

eval_scheme

55832 / (565 * 336) # 0.2941003 약 30퍼의 데이터존재

# 협업 필터링 방식
cf_method <- list("UBCF_Cosine" = list(name="UBCF", parm = list(method = "Cosine")),
                    "UBCF_Pearson"= list(name="UBCF", parm = list(method = "Pearson")),
                    "IBCF_Jaccard" = list(name = "IBCF", parm = list(method = "Jaccard")))

result <- evaluate(eval_scheme, cf_method, type = "topNList", n = c(1, 3, 5))
result
avg(result)

# result UBCF_Cosine
(0.09911504 + 298.8673)/(0.09911504 + 0.900885 + 21.13274 + 298.8673 ) # 93퍼
(0.27079646 + 297.0389)/(0.27079646 + 2.729204 + 20.96106 + 297.0389)  # 92.6퍼
(0.46017699 + 295.2283)/(0.46017699 + 4.539823 + 20.77168 + 295.2283)  # 92퍼

# result UBCF_Pearson
(0.04955752 + 298.8177)/(0.04955752 + 0.9504425 + 21.18230 + 298.8177)
(0.21061947 + 296.9788)/(0.21061947 + 2.7893805 + 21.02124 + 296.9788)
(0.38761062 + 295.1558)/(0.38761062 + 4.6123894 + 20.84425 + 295.1558)

# result IBCF_Jaccard
(0.0920354 + 298.9416)/(0.0920354 + 0.8265487 + 21.13982 + 298.9416)
(0.2619469 + 297.3327)/(0.2619469 + 2.4353982 + 20.96991 + 297.3327)
(0.4743363 + 295.8018)/(0.4743363 + 3.9663717 + 20.75752 + 295.8018)

################################################################################
# HybridRecommender

MovieLense100 <- MovieLense[rowCounts(MovieLense) > 100,]
str(MovieLense100)

train <- MovieLense100[1:100]
test <- MovieLense100[101:103]
# registry mechanism
recommenderRegistry$get_entry_names()


