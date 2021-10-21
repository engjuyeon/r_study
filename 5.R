library(dplyr)
library(tidyverse)



# Q1 ?쑀?겢由щ뵒?븞 嫄곕━ 怨꾩궛 ?븿?닔
a = c(1,2)
b = c(4,5)

func <- function(a, b){
  xvar <- ((a[1] - b[1])^2)
  yvar <- ((a[2] - b[2])^2)
  sqrt( ((a[1] - b[1])^2) + ((a[2] - b[2])^2) )
}

func(a,b)


# Q
library(readr)
election_2012 <- read_csv("election_2012.csv")
head(election_2012)
names(election_2012)

# Q1
attach(election_2012)  #?썑蹂댁옄 ID, ?썑蹂댁옄 ?씠由?, ?썑?썝?옄 ?씠由?, ?썑?썝?옄 吏곸뾽援?, ?썑?썝湲?
candidate <- data.frame(cand_id, cand_nm, contbr_nm, contbr_occupation, contb_receipt_amt)
head(candidate)
detach(election_2012)


romney <- subset(sample, election$cand_nm=='Romney, Mitt')

obama <- subset(sample, election$cand_nm=='Obama, Barack')

rommey <- subset(election_2012, cand_nm == "Bachmann, Michelle")
rommey
Obama <- subset(election_2012, cand_nm == "Obama, Barack")
Obama
# Q2


romney <- subset(candidate, cand_nm == "Romney, Mitt")
obama <- subset(candidate, cand_nm == "Obama, Barack")

nrow(romney)
nrow(obama)

head(romney)
head(obama, 10)

# Q3
romney_6000_over <- subset(romney, contb_receipt_amt >= 600)
obama_6000_over <- subset(obama, contb_receipt_amt >= 600)

nrow(distinct(romney_6000_over))
nrow(distinct(obama_6000_over))

romney_6000_over
max(as.numeric(romney_6000_over$contb_receipt_amt))

rmax <- max(as.numeric(romney_6000_over$contb_receipt_amt))
rmax
select(romney_6000_over, contbr_nm, contbr_occupation) %>% filter(contb_receipt_amt == rmax)
reach

romney_6000_over %>% filter(contb_receipt_amt == rmax) %>% select(contbr_nm, contbr_occupation)
omax <- max(as.numeric(obama_6000_over$contb_receipt_amt))
obama_6000_over %>% filter(contb_receipt_amt == omax) %>% select(contbr_nm, contbr_occupation)

# Q4
romney2 <- subset(romney, !is.na(contbr_occupation) | contbr_occupation != " ")
obama2 <- subset(obama, !is.na(contbr_occupation) | contbr_occupation != " ")
nrow(romney)
nrow(romney2)
cat("?쟾?썑李⑥씠 : " , nrow(romney) - nrow(romney2))
nrow(obama)
nrow(obama2)
cat("?쟾?썑李⑥씠 : " , nrow(obama) - nrow(obama2))

romney_6000_over %>% filter(contb_receipt_amt == rmax) %>% select(contbr_nm, contbr_occupation, contb_receipt_amt)
rrmax <- max(as.numeric(romney2$contb_receipt_amt))
romney2 %>% filter(contb_receipt_amt == rrmax) %>% select(contbr_nm, contbr_occupation, contb_receipt_amt)

write.csv(romney2, file = "romney2.csv", row.names = F)
write.csv(obama2, file = "obama2.csv", row.names = F)

# Q5
library(readr)
romney3 <- read_csv("romney2.csv")
obama3 <- read_csv("obama2.csv")
nrow(romney3)
nrow(obama3)



romney3 %>% filter(contbr_occupation == "RETIRED") %>% select(contb_receipt_amt) %>% sum()
obama3 %>% filter(contbr_occupation == "RETIRED") %>% select(contb_receipt_amt) %>% sum()

# Q6
romneyOrder <- romney3 %>% group_by(contbr_occupation) %>% count()
names(romneyOrder)
romneySort <- sort(as.numeric(romneyOrder$n), decreasing = T)
head(romneySort, 3)


obama3

library(dplyr)
romney3 %>% group_by(contbr_occupation) %>% count() %>% arrange(desc(n)) %>% head(3)



romney3 %>% filter(contbr_occupation == "ENGINEER") %>% select(contb_receipt_amt) %>% sum()
obama3 %>% filter(contbr_occupation == "ENGINEER") %>% select(contb_receipt_amt) %>% sum()

sumEN <- 0
for(i in 1:nrow(romney3)){
  if(romney3[i, "contbr_occupation"] == "ENGINEER"){
  
    sumEN <- sum(sumEN + romney3$contb_receipt_amt[i])
  }
}
sumEN <- 0
for(i in 1:nrow(obama3)){
  if(obama3[i, "contbr_occupation"] == "ENGINEER"){
    
    sumEN <- sum(sumEN + obama3$contb_receipt_amt[i])
  }
}
sumEN

sum(romney3$contb_receipt_amt[romney3$contbr_occupation == 'ENGINEER'])


