#Clear previous variables
rm(list=ls())

h = function(k){
    return((3*k+5) %% 11)
}

lin_prob = function(vec, hash, elem){
    if (hash(elem) %in% vec){
        lin_prob(vec, hash, elem+1)
    }
    else{
        return(c(vec, hash(elem)))
    }
}

k = c(12, 44, 13, 88, 23, 94, 11, 39, 20, 16, 5)
h_k = c()

for (i in 1:length(k)){
    h_k = lin_prob(h_k, h, k[i])
}
