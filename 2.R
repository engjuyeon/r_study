# vector : 1차원 배열 + 선형대수

x <- c(3, 4, 6, 4, 5, 8, 1)
x
sort(x)
sort(x, decreasing = TRUE)

order(x) # 정렬 순서 값
order(x, decreasing = TRUE)
x[order(x)]

sum(2, 7, 5)
x <- c(2, NA, 3, 1, 4)
sum(x) # 결측치 처리
sum(x, na.rm = TRUE)
mean(x, na.rm = TRUE)
# 1000, 10, 15, 30
median(x, na.rm = TRUE) # 중위수 - 중시 : 이상치를 제거할 수 있음

vectorA <- c(1, 2, 3, 4)
(names(vectorA) <- c("국어", "영어", "수학", "과학"))
vectorA["국어"]
vectorA[2]
vectorA[-1]
vectorA[c(1, 3)]
vectorA[vectorA > 5]
vectorA[c(FALSE, FALSE, TRUE)]
append(vectorA, c(3, 4, 5)) # 값에 의한 전달 : 주소를 전달하는데도 불구하고 복사
vectorA
(vectorB <- append(vectorA, c(3, 4, 5)))
vectorB

# 집합연산
# 합집합, 교집합, 차집합
union(vectorA, vectorB)
intersect(vectorA, vectorB) # 교집합
setdiff(vectorA, vectorB)
setdiff(vectorB, vectorA)
setequal(vectorA, vectorB)


# subset과 which
( x <- c(3, 2, 6, 4, 5, 8, 1))
subset(x, x > 3)
which(x * x > 8) # 인덱스 위치값


# 문제
(vectorA <- c(1, 2, 3))
(names(vectorA) <- c("aaa", "bbb", "ccc"))

# 2보다 큰 값 모으시오
# 요소를 제곱했을 때 8보다 큰 위치값을 출력하시오
subset(vectorA, vectorA > 2)
which(vectorA * vectorA > 8)

# 가중치
(x <- sample(1:10, 3, replace = F)) # 1에서 10 중 랜덤으로 3개, replace는 복원추출 (뽑고 다시 1:10 중에서 고름)
?sample


x <- c(1, 2, 3)
y <- c(4, 5, 6)
x %*% y # 사이즈를 고려한 사이 각 |A||B| cos theta의 의미

nums <- c(5, 8, 10, NA, 3, 11)
nums
which.min(nums) # index
which.max(nums)
nums[which.min(nums)]
nums[which.max(nums)]

vector1 <- rep("R", 5)
vector2 <- seq(1, 20, by = 3)
vector3 <- rep(seq(1, 10, by = 3) , 3)
vector4 <- c(vector2, vector3)

seq(25, -15, by=-5)
vector5 <- vector4[seq(1, length(vector4), 2)]
vector6 <- vector5[seq(2, length(vector5), 2)]

vector7 <- as.character(vector5)
vector5 + vector6

vector5 + as.numeric(vector7)

# for 벡터화 연산, if 인덱싱
# 내적 = |A| |B| cos theta
# cos theta = (|A||B|)/내적
# theta = acos((|A| |B|)/내적)
# 축을 표현 => normalize해서 표현 (방향 표현)
# 직교 : 컴퓨터에서는 radian을 사용해야 함
install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
x <- c(1, 0, 0)
y <- c(0, 1, 0)
xq <- sqrt(1^2 + 0^2 + 0^2) # norm
yq <- sqrt(0^2 + 1^2 + 0^2)
x = x /xq # recycling => nomalize
y = y/ yq
angle = x %*% y # 크기값이 고려된 사이각
NISTradianTOdeg(acos(angle))


# factor
( x <- factor(c("single", "married", "married", "single")))
class(x)
levels(x)

( x <-factor(c("single", "married", "married", "single"), levels = c("single", "married", "divorced")))

str(x)
levels(x)

x[3]
x[c(2,4)]
x[-1]
x[c(T,F,F,T)]
x[2] = "divorced"
x
x[3] = "widowed"
levels(x) <- c(levels(x), "widowed")
levels(x)


# Matrix 행렬 : 데이터 표현, 행렬변환, 차원축소
# 행렬곱 : 내적의 연속
x = matrix(1:9, nrow = 3, byrow = TRUE) # ncol은 자동 3
colnames(x)
colnames(x) <- c("C1", "C2", "C3") # 열이름
rownames(x) <- c("R1", "R2", "R3") # 행이름
x
(x <- matrix(1:9, nrow = 3, dimnames = list(c("X","Y","Z"), c("A","B","C"))))

y <- matrix(nrow = 2, ncol = 2)
y[1,2]
y[1,1] <- 1
y[2,1] <- 2
y[1,2] <- 3
y[2,2] <- 4
mode(y)
class(y)
y


cbind(c(1, 2, 3), c(4, 5, 6)) # column 
rbind(c(1, 2, 3), c(4, 5, 6)) # row 

(x <- matrix(1:9, nrow = 3, byrow = TRUE))
(y <- matrix(11:19, nrow = 3, byrow = TRUE))
(c <- cbind(x, y))
(r <- rbind(x, y))
dim(y)

(x <- c(1,2,3,4,5,6))
x
dim(x)
dim(x) <- c(2,3)
dim(x)
x

x <- matrix(1:9, nrow = 3, byrow = TRUE)
x
x[c(1, 2), c(2,3)]
x[c(2,3), ]
x[ , ]
x[-1, ]
a = x[1, ]
a

x[c(TRUE, FALSE, TRUE), c(TRUE, TRUE, FALSE)]
x[c(TRUE, FALSE), c(2,3)] # recycling
x[c(TRUE, FALSE)] # 데이터도 1차원으로 봐라

x[x > 5]
x[x %% 2 == 0]


(x[2,2] <- 10)
(x[x < 5] <- 0)
x

(mdat <- matrix(seq(20, 4, -2), nrow = 3, ncol = 3, byrow = TRUE, 
                  dimnames = list(c("a", "b", "c"), c("x", "y", "z"))))
mdat
t(mdat)
nrow(mdat)
ncol(mdat)
dim(mdat)
mdat
rowMeans(mdat)
rowSums(mdat)
colMeans(mdat)
colSums(mdat)


# 단위 행렬(대각 방향) 벡터의 사이즈 결정
diag(mdat)
diag(20, 4, 4) # 대각행렬　

#　정직교　하는　축이　아니면　상관관계　－　영향력
eigen(mdat)  # 고유값 분해 = 고유치( 크면 주성분 -> 분산이 크면) + 고유백터 (축을 의미)=> 주성분분석(차원축소, 노이즈제거) nomalize 돼잇음

solve(10, 20) # y = ax
solve(20, 10)

# 2X + 3Y = 5
# 3X + 5Y = 6

mdat <- matrix(c(2,3,3,5), nrow = 2, byrow = T)
c <- c(5,6)
solve(mdat, c)

# 2x + y + z = 1
# 4x + 3y + 4z = 2
# -4x + 2y + 2z = -6
# 위 방정식의 해를 푸시오
equation1 <- matrix(c(2,1,1,4,3,4,-4,2,2), nrow = 3, byrow = T)
c <- c(1, 2, -6)
solve(equation1, c)

#문제 1~15를 요소로 하는 5X3행렬을 만들고 16~30을 요소로 하는 5X3행렬을 만들고
# 1) 두 행렬을 rbind와 cbind로 묶어서 출력하시요
# 2) 위 두 행렬의 사칙 연산을 수행하시요

vec <- matrix(1:15, nrow = 5, ncol = 3, byrow = T)
vec2 <- matrix(16:30, nrow = 5, ncol = 3, byrow = T)

vec %*% vec2
rbind(vec, vec2)
cbind(vec, vec2)
vec + vec2
vec - vec2
vec * vec2
vec / vec2
t <- vec %*% t(vec2)

# Data.frame
x <- c(10, 20, 30, 40)
y <- c(6, 7, 8, 9)
data <- data.frame(width = x, height = y) # 열 이름이 키, 열을 변수라고 하고 행을 데이터 포인트
data
str(data)
data$width
data[,1]  
head(data)


(L3 <- LETTERS[1:3])
d <- data.frame(cbind(x = 1, y = 1:10), fac = sample(L3, 10, replace = TRUE)) # 복원추출 == 중복
d
d$fac
names(d)
(d$yhat <- d$fac) # 파생변수
d

rownames(d) <- c("일", "이", "삼", "사", "오", "육" , "칠", "팔", "구", "십")

d

#수정
x <- data.frame("SN" = 1:2, "Age" = c(21,15), "Name" = c("에어콘", "삼성SDS"))
str(x)

x["Name"] # 결과가 데이터 프레임
x$Name
class(x)
class(x$Name) # vector
class(x["Name"])
x[["Name"]]
class(x[["Name"]])

library(help = "datasets")
data()
str(trees)
head(trees, n=3)
tail(trees, n=3)
trees[2:3,]
trees[trees$Height > 82,]
summary(trees)# min, max, 1st quantile, media, mean, 3rd quantile
# range = max - min
boxplot(trees) # 이상치 유무 1.5 * IQR
pairs(trees) # 정방행렬이고 대칭행렬 고유값 분해를 하면 고유치와 고유벡터가 출력


# 다음을 데이터 프레임에 입력
#     영어점수 등급
# 퀴즈  67     "C"
# 중간  92     "A"
# 기말  89     "B"
# 수학점수 50, 100, 80점으로 추가입력
# 보충이라는 이름으로 data.frame 만들어 rbind


x <- c(67, 92, 89)
y <- c("C", "A", "B")
score <- data.frame("영어점수" = x, "등급" = y)
rownames(score) <- c("퀴즈", "중간", "기말")
score

score$"수학점수" <- c(50, 100, 80)
score
data <- data.frame("영어점수" = c(90), "등급" = c("A"), "수학점수" = c(100), row.names = "보충")
data2 <- rbind(score, data)
data2

# sep : 분리자
write.table(data2, file = "sungjuk.csv", sep =",", col.names = NA)
(data3 <- read.table("sunjuk.csv", header = TRUE, sep = ",", row.names = 1))

# b : both - line + point
plot(c(data3$수학), main = "그래픽", col = "red", ylab = "수학", ylim = c(0, 100), type = "b")

name <- c("대한이", "민국이", "만세야", "희망이", "다함께")
age <- c(55, 45, 45, 53, 15)
gender <- c(1, 1, 1, 2, 1) # 1 남자 2 여자 # factor
job <- c("연예인", "주부", "군인", "직장인", "학생") # factor
sat <- c(3, 4, 2, 5, 5)
grade <- c("C", "C", "A", "D", "A") # factor
total <- c(44.4, 38.5, 43.5, NA, 27.1)

user <- data.frame(name, age, gender, job, sat, grade, total)
str(user)

user$gender <- as.factor(gender)
user$job <- as.factor(job)
user$grade <- as.factor(grade)
user

?sd
plot
plot(user$sat, main = "만족도 산점도", col = "red", ylab = "만족도", ylim=c(0,5),type = "p")
mean(user$total, na.rm = "T") # 평균
var(user$total, na.rm = "T")  # 분산 variance
sd(user$total, na.rm = "T")   # 표준편차 standard deviation 표준편차

# list : 값을 유지한다.
y <- list(2.5, TRUE, 1:3)
y
# vector 
# z <- c(2.5, TRUE, 1:3)

(x <- list("name" = "에어콘", "age" = 19, "speaks" = c("한국어", "영어")))
x
x$name
x[c(1:2)]
x[-2]
x[c(T,F,F)]
x[c("age","speaks")]
x["age"]
typeof(x["age"])
x[["age"]]
typeof(x[["age"]])
x$speaks[1]
x$speaks[2]
x[["speaks"]][2]
x[["age"]] <- NULL

# Array
vector1 <- c(5,9,3)
vector2 <- c(10, 11, 12, 13,14)
# 면행렬 => R에서는 행, 열, 면으로 지정
result <- array(c(vector1, vector2), dim = c(4, 3, 2)) # 24
print(result)

vector1 <- c(5,9,3)
vector2 <- c(10, 11, 12, 13, 14, 15)
column.names <- c("COL1", "COL2", "COL3")
row.names <- c("ROW1", "ROW2", "ROW3")
matrix.names <- c("MATRIX1", "MATRIX2", "MATRIX3", "MATRIX4", "MATRIX5")

result <- array(c(11:19, 21:29, 31:39, 41:49, 51:59), dim = c(3, 3, 5), dimnames = list(row.names, column.names, matrix.names))

print(result)
print(result[3,,2])
print(result[,,"MATRIX1"])
print(result[,"COL1", "MATRIX1"])


print(result[,,"MATRIX2"])
print(result["ROW3","COL2", "MATRIX2"])
print(result["ROW2","COL3", "MATRIX1"])
print(result["ROW1","COL3", "MATRIX2"])
print(result["ROW2","COL2", "MATRIX3"])
print(result["ROW2","COL3", "MATRIX3"])
print(result["ROW3","COL2", "MATRIX4"])
print(result["ROW3","COL1", "MATRIX5"])

print(result[,"COL1", "MATRIX2"])
print(result["ROW3",,"MATRIX2"])
print(result[1:2, 2:3, "MATRIX4"])
print(result[c("ROW1","ROW2"), c("COL2", "COL3"), "MATRIX4"])
