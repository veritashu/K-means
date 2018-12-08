rm(list=ls())

#############################
set.seed(1) ### Set seed here
#############################

wines = read.csv("wines_hw3.csv")

words = c("sweet", "acid", "earthy", "fruit", "tannin", "herb",
          "tart", "spice", "smooth", "full", "intense",
          "wood", "soft", "dry", "apple", "pear", "cherry",
          "berry", "aroma", "citrus", "lemon", "lime", "peach", "blossom",
          "sugar", "simple", "cinnamon", "ripe",
          "crisp", "honey", "brisk", "fresh", "sour", "floral", 
          "dark", "complex", "oak", "balance", "caramel", "plum", "mint",
          "apricot", "cream", "vanilla", "butter", "sharp")

X = wines[, words]
X = as.matrix(X)
X = scale(X)

n = dim(X)[1]
p = dim(X)[2]

k = 3

# K-means++
cent <- rep(0, k) # Centers index in X
cent[1] <- sample(1:n,1) # Random choose c1

for(i in 2:k){
  dist <- matrix(nrow = i-1, ncol = n) # distance matrix, from each data (n) to existed center (i-1)
  D <- rep(0,n) # D(x), defined in paper, minimum of each column
  
  for(l in 1:n){
    for(j in 1:i-1){
      dist[j,l] = sqrt( sum( (X[l, ]-X[cent[j], ])^2 ) )
      # = dist(rbind(X[l, ], X[cent[j], ]), method = "euclidean")
    }
    D[l] = min(dist[,l])
  } # Compute distance matrix, choose minimum for each n 

  pr <- rep(0,n)
  for(m in 1:n){
    pr[m] = D[m]^2/sum(D^2)
  } # Probability of choosing new center, defined in paper
  
  cent[i] = sample(1:n, 1, prob = pr) # Sample i-th center with prob, choose exited center w/ pr=0
}

fit_kplus <- kmeans(X, centers = X[cent,])

cluster1 = which(fit_kplus$cluster == 1)
cluster2 = which(fit_kplus$cluster == 2)
cluster3 = which(fit_kplus$cluster == 3)

C1 = table(wines[cluster1, "variety"])
C2 = table(wines[cluster2, "variety"])
C3 = table(wines[cluster3, "variety"])
print(cbind(C1,C2,C3))


# Old K-means
fit_k <- kmeans(X, 3)

cluster1 = which(fit_k$cluster == 1)
cluster2 = which(fit_k$cluster == 2)
cluster3 = which(fit_k$cluster == 3)

C1 = table(wines[cluster1, "variety"])
C2 = table(wines[cluster2, "variety"])
C3 = table(wines[cluster3, "variety"])
print(cbind(C1,C2,C3))
