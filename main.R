#Clear previous variables
rm(list=ls())

# Hash function
h = function(k){
    return((3*k+5) %% 11)
}

## Second hash function
h_s = function(k){
	return(7- (k %% 7))
}

## Linear probing for collision handling
lin_prob = function(vec, hash, elem){
    if (hash(elem) %in% vec){
        lin_prob(vec, hash, elem+1)
    }
    else{
        return(c(vec, hash(elem)))
    }
	
	#for (i in 1:length(vec)){
	#	if (!((hash(elem)+i-1) %in% vec) || i==length(vec)){
	#		return(c(vec, hash(elem)+i-1))
	#	}
	#}
}

## Double hashing for collision handling
hashx2 = function(vec, hash, hash2, elem, n){
	#if (hash(elem) %in% vec){
	#	prob(vec, hash, hash2, h(elem) + hash2(1))
	#}
	#else{
	#	return(c(vec, hash(elem)))
	#}
	
	for (i in 1:n){
		if(!(((hash(elem)+(i-1)*hash2(elem))%%n) %in% vec)){
			return(c(vec, (hash(elem) + (i-1)*hash2(elem))%%n))
			break
		}
	}
}

k = c(12, 44, 13, 88, 23, 94, 11, 39, 20, 16, 5)
h_k = c()

for (i in 1:length(k)){
    #h_k = lin_prob(h_k, h, k[i])
	h_k = hashx2(h_k, h, h_s, k[i], length(k))
}

df = data.frame(k, h_k)

print(df)