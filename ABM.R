#simulate nests
nests <- matrix(as.integer(rbinom(10000, 1, .05)), nrow = 100, byrow = T)
#simulate initial predators
preds <- matrix(as.integer(rbinom(10000, 1, .05)), nrow = 100, byrow = T)

#list of cells with predators
predCell <- which(preds == 1)
#list of cells with nests
nestCell <- which(nests == 1)

nestHist <- data.frame(nestID=nestCell)
nestHist$day1 = as.integer(nestHist$nestID == nestCell)

#check if each predator is at a nest
for (i in predCell) {
  if (any(nestCell == predCell[i], na.rm = T)) {
    nests[i] = 0 #if yes, then kill the nest (set it to a zero)
  }
}

#update nest cell
nestCell <- which(nests == 1)
#add data to dataframe
nestHist$day2 <- as.integer(nestHist$nestID == nestCell)

#then make pred move and repeat
move <- function (pos) {
  dir <- sample(c(1:5), 1)
  switch(dir,
         if (pos-100 > 0) {pos-100} else {10000+pos-100},
         if (pos %% 100 == 0) {pos-99} else {pos+1},
         if (pos+100 < 10000) {pos+100} else {pos+100-10000},
         if (pos %% 100 == 1)  {pos+99} else {pos-1},
         pos
  )
}

predCell <- sapply(predCell, move)
