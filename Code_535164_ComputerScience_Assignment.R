install.packages(c("dplyr", "jsonlite", "readxl", "stringdist", "stringi", "tokenizers"))
library(dplyr)
library(jsonlite)
library(readxl) 
library(stringdist) 
library(stringi) 
library(tokenizers)

#Load the data and extract model IDs
data <- fromJSON("C:/Users/johan/OneDrive/Documenten/Werkstukken uni/Jaar 4/Blok2/Computer Science/Exercise/TVs-all-merged.json")
model_ids <- names(data)


##################################Data cleaning###################################
#Store the data
combine_data <- list()

#Bring all data together
for (model_id in model_ids) {
  model_data <- data[[model_id]]
  one_product <- as.data.frame(model_data, row.names = NULL)
  one_product$model_id <- model_id
  combine_data[[model_id]] <- one_product
}

combine_data <- bind_rows(combine_data)
combine_data_new <- combine_data[, -which(names(combine_data) == "featuresMap")]
combine_data_new[] <- lapply(combine_data_new, tolower)

#Clean certain phrases of the data
combine_data_new$title <- gsub('["”]', 'inch', combine_data_new$title)
combine_data_new$title <- gsub('-inch', 'inch', combine_data_new$title)  
combine_data_new$title <- gsub(' hz', 'hz', combine_data_new$title)
combine_data_new$title <- gsub(' / ', ' ', combine_data_new$title)
combine_data_new$title <- gsub(' - ', ' ', combine_data_new$title)       
combine_data_new$title <- gsub(' & ', '', combine_data_new$title)
combine_data_new$title <- gsub('[()]', '', combine_data_new$title)

#####################################Create list of TVs and brands################
#Create an overview of all unique TVs (containing all 1624 TVs)
overview_tvs <- list()
for (tv in 1:length(data)){
  if (length(data[[tv]]$shop)==1){
    new_tv <- list()
    fMap <- list()
    new_tv <- c(new_tv, data[[tv]]$shop, data[[tv]]$url, data[[tv]]$title, data[[tv]]$modelID)
    fMap <- c(fMap, data[[tv]]$featuresMap)
    new_tv <- c(new_tv, list(fMap))
    overview_tvs <-c(overview_tvs,list(new_tv))
  }
  else{
    for (shop in 1:length(data[[tv]]$shop)){
      new_tv <- list()
      fMap <- list()
      new_tv <- c(new_tv, data[[tv]]$shop[shop], data[[tv]]$url[shop], data[[tv]]$title[shop], data[[tv]]$modelID[shop])
      fMap <- c(fMap, data[[tv]]$featuresMap[shop, !is.na(data[[tv]]$featuresMap[shop,])])
      new_tv <- c(new_tv, list(fMap))
      overview_tvs <-c(overview_tvs,list(new_tv))
    }
  }
}

tv_brands <- read_excel("C:/Users/johan/OneDrive/Documenten/Werkstukken uni/Jaar 4/Blok2/Computer Science/Exercise/tv_brands.xlsx")
tv_brands <- list(tv_brands)

###############################Calculate binary matrix, set variables for bootstrapping#####################
#Extract model words, choose which definition
model_words <- unique(unlist(stri_extract_all_regex(
  as.character(combine_data_new$title),
  #"[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*" #Model words as defined in the papers
  #"[a-zA-Z0-9]*([0-9]{2,}[^0-9, ]+|[^0-9, ]+[0-9]{2,})[a-zA-Z0-9]*" #Model words that contain 2 consecutive digits
  "[a-zA-Z0-9]*([0-9]{2,}[^0-9, ]{2,}|[^0-9, ]{2,}[0-9]{2,})[a-zA-Z0-9]*" #Model words that contain 2 consecutive digits and 2 consecutive non-numerics
)))

#Create binary matrix
binary_vectors <- sapply(model_words, function(word) {
  as.integer(stri_detect(combine_data_new$title, fixed = word))
})
binary_vectors <- t(data.frame(binary_vectors))


# Number of bootstraps and bands
bootstrap_n <- 5
bands_n <- c(20, 25, 32, 40, 50, 80, 100, 160, 200, 400)

# Initialize frames to store the bootstrapping results
results <- data.frame(Bands = numeric(), F1_star = numeric())
Comparison_fraction <- matrix(0, nrow = length(bands_n), ncol = bootstrap_n)
LSH_Pair_Quality <- matrix(0, nrow = length(bands_n), ncol = bootstrap_n)
LSH_Pair_Completeness <- matrix(0, nrow = length(bands_n), ncol = bootstrap_n)
F1_stored <- matrix(0, nrow = length(bands_n), ncol = bootstrap_n)

#################################Perform bootstrapping########################################
# Perform bootstrapping
for (bootstrap in 1:bootstrap_n) {
  set.seed(10*bootstrap+500)
  
  #Initialize bootstrap sample of 63%, create storage for F1 values
  bootstrap_samples <- sample(ncol(binary_vectors), size = round(0.63 * ncol(binary_vectors)), replace = FALSE)
  bootstrap_data <- binary_vectors[, bootstrap_samples, drop = FALSE]
  colnames(bootstrap_data) <- bootstrap_samples
  f1_score <- numeric()
  
  # LSH
  for (bands in bands_n) {
    permutations = 800
    set.seed(123)
    
    #Use the permutations to construct a signature matrix
    signature_matrix <-matrix(NA, nrow = permutations, ncol = ncol(bootstrap_data))
    colnames(signature_matrix) <- colnames(bootstrap_data)
    
    for (i in 1:permutations) {
      binary_vectors_perm <-bootstrap_data[sample(nrow(bootstrap_data)), , drop = FALSE]
      first_1_rows <-apply(binary_vectors_perm, 2, function(col) which(col == 1)[1])
      signature_matrix[i,] <- first_1_rows
    }
    
    #Initialize buckets
    buckets <- list()
    r = nrow(signature_matrix) / bands
    
    # Place vectors in buckets, store band buckets
    for (band in 1:bands) {
      set.seed(10*bootstrap+501)
      start <- (band - 1) * r + 1
      end <- band * r
      band_matrix <- signature_matrix[start:end, ]
      
      band_buckets <- sapply(1:ncol(band_matrix), function(col) {
        hash_value <- paste(band_matrix[, col], collapse = "")
        return(hash_value)
      })
      buckets[[band]] <- band_buckets
    }
    
    candidate_pairs <- list()
    for (band in 1:bands) {
      unique_buckets <- unique(buckets[[band]])
      
      # Find duplicates by scanning the buckets
      for (bucket in unique_buckets) {
        col_indices <- which(buckets[[band]] == bucket)
        
        # A column length greater than 2 indicates a candidate pair
        if (length(col_indices) > 1) {
          
          # Check the tvs that belong to the candidate pair
          candidate_pairs[[length(candidate_pairs) + 1]] <-
            list(Band = band, Bucket = bucket, TVs = col_indices)
        }
      }
    }
    
    # Create a matrix for the candidate pairs
    n <- ncol(bootstrap_data)
    candidate_pair_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(bootstrap_samples, bootstrap_samples))
    
    # Fill the matrix with candidate pairs
    for (pair_list in 1:length(candidate_pairs)) {
      for (candidate1 in 1:length(candidate_pairs[[pair_list]]$TVs)) {
        for (candidate2 in 1:length(candidate_pairs[[pair_list]]$TVs)) {
          if (candidate1 != candidate2)
          {
            candidate_pair_matrix[candidate_pairs[[pair_list]]$TVs[candidate1], candidate_pairs[[pair_list]]$TVs[candidate2]] <-1
          }
        }
      }
    }
    
    # Set diagonal elements and elements below back to 0
    diag(candidate_pair_matrix) <- 0
    candidate_pair_matrix[lower.tri(candidate_pair_matrix)] <- 0
    
    # Create counters for the evaluation measures
    true_positives <- 0
    real_duplicates <- 0
    
    # Iterate over the candidate pair matrix and update the counters
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        if (overview_tvs[[i]][[4]] == overview_tvs[[j]][[4]]) {
          real_duplicates <- real_duplicates + 1
        }
        if (candidate_pair_matrix[i, j] == 1) {
          if (overview_tvs[[as.integer(rownames(candidate_pair_matrix)[i])]][[4]] == overview_tvs[[as.integer(colnames(candidate_pair_matrix)[j])]][[4]]) {
            true_positives <- true_positives + 1
          }
        }
      }
    }
    
    #Function to determine the total number of possible candidates
    total_possible_duplicates <- function(matrix) {
      n <- nrow(matrix)
      sum(upper.tri(matrix))
    }
    
    #Create the evaluation values
    comparisons_made <- sum(candidate_pair_matrix)
    comparison_frac <- comparisons_made/total_possible_duplicates(candidate_pair_matrix)
    PQ_lsh <- true_positives / comparisons_made
    PC_lsh <- true_positives / real_duplicates
    F1_star <- (2 * PQ_lsh * PC_lsh) / (PQ_lsh + PC_lsh)
    f1_score <- c(f1_score, F1_star)
    
    #Store the evaluation values
    band_index <- which(bands_n == bands)
    Comparison_fraction[band_index, bootstrap] <- comparison_frac
    F1_stored[band_index, bootstrap] <- F1_star
    LSH_Pair_Completeness[band_index, bootstrap] <- PC_lsh
    LSH_Pair_Quality[band_index, bootstrap] <- PQ_lsh
  }
  
  # Find the optimal F1 score
  best_bands <- bands_n[which.max(f1_score)]
  best_f1 <- max(f1_score)
  
  # Store the results in the data frame
  results <- rbind(results, data.frame(Bands = best_bands, F1_star = best_f1))
}


##################################Print and plot results###############################
# Average the bootstrap results
average_comparison_frac <- rowMeans(Comparison_fraction)
average_F1<-rowMeans(F1_stored)
average_PC_lsh <- rowMeans(LSH_Pair_Completeness)
average_PQ_lsh <- rowMeans(LSH_Pair_Quality)

# Store the average bootstrap results
average_results2 <- data.frame(
  Bands_2 = bands_n,
  Avg_comparison_frac_2 = average_comparison_frac,
  AVG_F1_2<-average_F1,
  Avg_PC_LSH_2 = average_PC_lsh,
  Avg_PQ_LSH_2 = average_PQ_lsh
)

#Import overviews of the 3 different average results matrices, drawing graphs of them. In these overviews, store the fraction of comparisons,
#respectively the F1 measure per model word specification, the PQ values per model word specification and the PC values per model word specification.
f1 <- read_excel("C:/Users/johan/OneDrive/Documenten/Werkstukken uni/Jaar 4/Blok2/Computer Science/Graphs/f1.xlsx")
pq <- read_excel("C:/Users/johan/OneDrive/Documenten/Werkstukken uni/Jaar 4/Blok2/Computer Science/Graphs/pq.xlsx")
pc <- read_excel("C:/Users/johan/OneDrive/Documenten/Werkstukken uni/Jaar 4/Blok2/Computer Science/Graphs/pc.xlsx")

#Plot three plots for the different evaluation measures, in order to compare the different model word specifications
f1plot<-plot(f1$`Fraction of comparisons`, f1$ModelwordTitleReg,  type = "l", col = "blue", xlab = "Average comparison fraction", ylab = "Average F1", main = "Average F1 vs. average comparison fraction")
lines(f1$`Fraction of comparisons`, f1$ModelwordTitle2, type = "l", col = "red")
lines(f1$`Fraction of comparisons`, f1$ModelwordTitle3, type = "l", col = "black")

PQplot<-plot(pq$`Fraction of comparisons`, pq$ModelwordTitleReg,  type = "l", col = "blue", xlab = "Average comparison fraction", ylab = "Average PQ LSH", main = "Average PQ vs. average comparison fraction")
lines(pq$`Fraction of comparisons`, pq$ModelwordTitle2, type = "l", col = "red")
lines(pq$`Fraction of comparisons`, pq$ModelwordTitle3, type = "l", col = "black")

PCplot<-plot(pc$`Fraction of comparisons`, pc$ModelwordTitleReg,  type = "l", col = "blue", xlab = "Average comparison fraction", ylab = "Average PC LSH", main = "Average PC vs. average comparison fraction")
lines(pc$`Fraction of comparisons`, pc$ModelwordTitle2, type = "l", col = "red")
lines(pc$`Fraction of comparisons`, pc$ModelwordTitle3, type = "l", col = "black")

########################################MSM#########################################
#This should be initialized with a full run LSH, using the entire dataset and its optimal number of bands

#Number of optimal parameters, extracted from the paper of Bezu et al
alpha <- 0.602
beta <- 0
gamma <- 0.756
mu <- 0.650
epsilon<-0.522

n <- nrow(combine_data)
dist <- n*n

#Create a list of the webshops
webshops <- c("amazon.com", "newegg.com", "bestbuy.com", "thenerds.net")
webshops <- as.list(webshops)

#Set stringsize for q to 3
q <- 3

#Calculate the q-gram similarity for strings q and r
calcSim <- function(s1, s2) {
  # Calculate q gram distance
  qGramDistance <- stringdist(s1,s2,method='qgram',q=q)
  
  # Calculate amount of tokens and similarity value
  n1 <- length(tokenize_character_shingles(s1, n = 3, n_min = 3, strip_non_alphanum = FALSE)[[1]])
  n2 <- length(tokenize_character_shingles(s2, n = 3, n_min = 3, strip_non_alphanum = FALSE)[[1]])
  qgram_similarity_value <- (n1 + n2 - qGramDistance) / (n1 + n2)
  return(qgram_similarity_value)
}

#Function that returns true if shop is same for products i and j
sameShop <- function(tv_i, tv_j) {
  return(overview_tvs[[tv_i]][[1]]==overview_tvs[[tv_j]][[1]])
}

#Function that returns true if brands of products i and j are different
diffBrand <- function(tv_i, tv_j) {
  if (!is.null(brand(tv_i)) && !is.null(brand(tv_j)) && brand(tv_i) != brand(tv_j)) 
  {
    return(TRUE)
  } 
  else 
  {
    return(FALSE)
  }
}

#Return the key values of a tv
kv <- function(tv) {
  featuresMap <- overview_tvs[[tv]][[5]]
  pairs_list <- as.list(featuresMap)
  return(pairs_list)
}

#All model words from the values of the attributes from product p
exMW <- function(cleandata) {
  cleandata <- lapply(cleandata, function(x) gsub('["”]', "inch", x))
  cleandata <- lapply(cleandata, function(x) gsub('-inch', "inch", x))
  cleandata <- lapply(cleandata, function(x) gsub(' hz', 'hz', x))
  cleandata <- lapply(cleandata, function(x) gsub(' - ', ' ', x))
  cleandata <- lapply(cleandata, function(x) gsub(' / ', ' ', x))
  cleandata <- lapply(cleandata, function(x) gsub('[()]', '', x))
  cleandata <- lapply(cleandata, function(x) gsub(' & ', '', x))
  model_cleandata <- unique(unlist(stri_extract_all_regex(as.character(cleandata),"[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*"))) #Model words as defined in the papers
  #model_cleandata <- unique(unlist(stri_extract_all_regex(as.character(cleandata),"[a-zA-Z0-9]*([0-9]{2,}[^0-9, ]+|[^0-9, ]+[0-9]{2,})[a-zA-Z0-9]*"))) #Model words that contain 2 consecutive digits
  #model_cleandata <- unique(unlist(stri_extract_all_regex(as.character(cleandata),"[a-zA-Z0-9]*([0-9]{2,}[^0-9, ]{2,}|[^0-9, ]{2,}[0-9]{2,})[a-zA-Z0-9]*"))) #Model words that contain 2 consecutive digits and 2 consecutive non-numerics
  model_cleandata <- model_cleandata[!sapply(model_cleandata, is.na)]
  return(model_cleandata)
}

#Function that gives the percentage of matching model words from two sets of model words
mw <- function(list1,list2){
#Calculate Jaccard similarity measure
  intsect <- length(intersect(list1, list2))
  union <- length(union(list1, list2))
  jaccard_similarity <- intsect / union
  percentage <- jaccard_similarity
  return(percentage)
}

#Function that calculates the TMWM similarity between the tvs i and j using the parameters alpha and beta
TMWMSim <- function(tvi,tvj,alpha,beta){
  tmw_i <- exMW(overview_tvs[[tvi]][[3]])
  tmw_j <- exMW(overview_tvs[[tvj]][[3]])
  
  cosSim <- length(intersect(tmw_i, tmw_j))/(sqrt(length(tmw_i))*sqrt(length(tmw_j)))
  
  if(cosSim<=beta)
  {
    return(-1)
  }
  if(cosSim>=alpha)
  {
    return(1)
  }
  return(cosSim)
}

#Function that returns the brand if it is found in the title
brand <- function(tv) {
  tv_title <- overview_tvs[[tv]][[3]]
  common_text <- intersect(unlist(tv_brands), unlist(strsplit(tv_title, " ")))
  if (length(common_text) > 0) 
  {
    return(common_text[1])
  } 
  else 
  {
    return(NULL)
  }
}

#Application of pseudocode given by Bezu et. al to construct the distance matrix
distmat <- matrix(Inf, n, n)
for (webshop in webshops){
  for (tvi in 1:nrow(combine_data)){ 
    if (combine_data[tvi,1]==webshop){
      for (tvj in 1:nrow(combine_data)){
        if(candidate_pairs[tvi, tvj]==1) {
          if (combine_data[tvj,1] != webshop){
            if (sameShop(tvi,tvj)||diffBrand(tvi,tvj)){
              distmat[tvi,tvj] <- Inf
            }
            else {
              sim <- 0
              avgSim <- 0
              m <- 0
              w <- 0
              nmk_i <- kv(tvi)
              nmk_j <- kv(tvj)
              for (pair_i in 1:length(kv(tvi))){
                for (pair_j in 1:length(kv(tvj))){
                  keySim <- calcSim(names(overview_tvs[[tvi]][[5]])[pair_i],names(overview_tvs[[tvj]][[5]])[pair_j])
                  if (keySim > gamma){
                    valueSim <- calcSim(overview_tvs[[tvi]][[5]][[pair_i]],overview_tvs[[tvj]][[5]][[pair_j]])
                    weight <- keySim
                    sim <- sim + weight * valueSim
                    m <- m + 1
                    w <- w + weight
                    nmk_i[names(kv(tvi)[pair_i])] <- NULL 
                    nmk_j[names(kv(tvj)[pair_j])] <- NULL
                  }
                }
              }
              if (w>0){
                avgSim <- sim/w
              }
              mwPerc <- mw(exMW(nmk_i),exMW(nmk_j))
              titleSim=TMWMSim(tvi, tvj, alpha, beta)
              minFeatures <- min(length(overview_tvs[[tvi]][[5]]),length(overview_tvs[[tvj]][[5]]))
              if (titleSim==-1) {
                theta1 <- m/minFeatures
                theta2 <- 1 - theta1
                hSim <- theta1*avgSim + theta2*mwPerc
              } else {
                theta1 <- (1 - mu)*(m/minFeatures)
                theta2 <- 1 - mu - theta1
                hSim <- theta1*avgSim + theta2*mwPerc + mu*titleSim
              }
              distmat[tvi,tvj] <- 1 - hSim
              if (is.na(distmat[tvi, tvj])){
                print(tvi)
                print(tvj)
              }
            }
          }
        }
      }
    }
  }
}

#################################Print the clusters################################################

# Create a list of clusters, initially with each tv in a separate cluster
clusters <- lapply(1:dim(distmat)[1], function(i) c(i))

# Function to check if a cluster contains at least one tv with infinite distance
containsInfinity <- function(cluster1,cluster2, distmat) {
  for (i in cluster1) {
    for (j in cluster2) {
      if(i<j){
        if (is.infinite(distmat[i, j])) {
          return(TRUE)
        }
      }
      else{
        if (is.infinite(distmat[j, i])) {
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

# Function to calculate the dissimilarity between two clusters using single linkage
calculateDissimilarity <- function(cluster1, cluster2, distmat) {
  submat <- distmat[cluster1, cluster2]
  return(min(submat))
}

# Main loop for hierarchical clustering
while (length(clusters) > 1) {
  minDissimilarity <- Inf
  mergeClusters <- c(0, 0)
  
  # Find the two clusters with the minimum dissimilarity
  for (i in 1:(length(clusters)-1)) {
    for (j in (i + 1):length(clusters)) {
      # Check if either cluster contains at least one tv with infinite distance
      if (!containsInfinity(clusters[[i]], clusters[[j]], distmat)) {
        dissimilarity <- calculateDissimilarity(clusters[[i]], clusters[[j]], distmat)
        if (dissimilarity < minDissimilarity) {
          minDissimilarity <- dissimilarity
          mergeClusters <- c(i, j)
        }
      }
    }
  }
  
  # Check if the minimum dissimilarity is above the epsilon threshold, if not, escape loop
  if (minDissimilarity > epsilon) {
    break
  }
  print(length(clusters))
  
  # Merge the two closest clusters
  newCluster <- c(clusters[[mergeClusters[1]]], clusters[[mergeClusters[2]]])
  clusters <- append(clusters[-mergeClusters], list(newCluster))
}

# Print the final clusters after the entire loop is done
cat("Final Clusters:\n")
for (i in 1:length(clusters)) {
  cat("Cluster", i, ":", clusters[[i]], "\n")
}

##############################Print true clusters##############################
# Function to generate true clusters from overview_tvs
generateTrueClusters <- function(overview_tvs) {
  True_Clusters <- list()
  
  for (i in 1:(length(overview_tvs) - 1)) {
    for (j in (i + 1):length(overview_tvs)) {
      if (overview_tvs[[i]][[4]] == overview_tvs[[j]][[4]]) {
        True_Clusters <- append(True_Clusters, list(c(i, j)))
      }
    }
  }
  
  # Merge clusters with more than two tvs
  merged_True_Clusters <- list()
  
  while (length(True_Clusters) > 0) {
    current_cluster <- True_Clusters[[1]]
    True_Clusters <- True_Clusters[-1]
    
    i <- 1
    while (i <= length(True_Clusters)) {
      if (any(True_Clusters[[i]] %in% current_cluster)) {
        current_cluster <- union(current_cluster, True_Clusters[[i]])
        True_Clusters <- True_Clusters[-i]
        i <- 0  # restart loop to check against updated True_Clusters
      }
      i <- i + 1
    }
    merged_True_Clusters <- append(merged_True_Clusters, list(current_cluster))
  }
  return(merged_True_Clusters)
}

# Generate true clusters with merged clusters
merged_True_Clusters <- generateTrueClusters(overview_tvs)

# Print the merged true clusters
cat("Merged Ground Truth:\n")
for (i in 1:length(merged_True_Clusters)) {
  cat("Cluster", i, ":", merged_True_Clusters[[i]], "\n")
}


#############################Calculate and print evaluation measures##########################################
# Function to calculate pair quality and pair completeness
calculatePairQualityCompleteness <- function(clusters, True_Clusters, overview_tvs) {
  true_positives <- 0
  number_of_comparisons <- sum(sapply(clusters, function(pair) choose(length(pair), 2)))
  total_duplicates <- sum(sapply(True_Clusters, function(pair) choose(length(pair), 2)))
  
  for (cluster in clusters) {
    cluster_size <- length(cluster)
    
    # Compare each pair of tvs within the cluster
    if(cluster_size==1)
    {
      
    }
    else{
      for (i in 1:(cluster_size - 1)) {
        for (j in (i + 1):cluster_size) {
          # Check if the pair is a true duplicate based on model_id
          print(cluster)
          if (overview_tvs[[cluster[i]]][[4]] == overview_tvs[[cluster[j]]][[4]]) {
            true_positives <- true_positives + 1
          }
        }
      }
    }
  }
  print(true_positives)
  print(total_duplicates)
  print(number_of_comparisons)
  cat("True positives:", true_positives, "\n")
  cat("Total duplicates:", total_duplicates, "\n")
  cat("Number of Comparisons:", number_of_comparisons, "\n")
  
  # Calculate pair quality and pair completeness
  pair_quality <- true_positives / number_of_comparisons
  pair_completeness <- true_positives / total_duplicates
  
  return(list(pair_quality = pair_quality, pair_completeness = pair_completeness))
}

# Function to calculate F1* measure
calculateF1StarMeasure <- function(pair_quality, pair_completeness) {
  f1_star <- 2 * (pair_quality * pair_completeness) / (pair_quality + pair_completeness)
  return(f1_star)
}

# Calculate pair quality and pair completeness
pair_quality_completeness <- calculatePairQualityCompleteness(clusters, merged_True_Clusters, overview_tvs)
pair_quality <- pair_quality_completeness$pair_quality
pair_completeness <- pair_quality_completeness$pair_completeness

# Calculate F1* measure
f1_star <- calculateF1StarMeasure(pair_quality, pair_completeness)

# Print the results
cat("Pair Quality:", pair_quality, "\n")
cat("Pair Completeness:", pair_completeness, "\n")
cat("F1* Measure:", f1_star, "\n")