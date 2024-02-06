
library(tidyverse)

current_path_history <- vector("list",30000)
Cooperation_pattern <- function(num_all, expected_all, num_players = 9000, num_generations =30000 ){
  for ( i in seq_along(1:num_generations)){
    
    ## Below code chunk ensures that the strategy with the highest expected payoff is played by one
    ## more player in the next round while the strategy with the lowest expected payoff is played
    ## by one less player. If any strategy is not by played by any player, then it is removed from
    ## the international system.Any strategy played by zero number of players simply dies out. Once
    ## the strategy dies out it cannot reproduce. Therefore, the dead strategy cannot increase or
    ## decrease in number.
    if (any(num_all <=0)){
      zero_index <- which(num_all<=0)
      zero_element_name <-names(num_all[zero_index])
      num_all <- num_all[-zero_index]
      expected_all  <- expected_all[-which(names(expected_all)==zero_element_name)]
    }
    
    current_path_history[[i]] <<- num_all  ## list data structure to store path histories
    
    max_index <- which.max(expected_all)
    num_all[max_index] <- num_all[max_index] + 1
    
    min_index <- which.min(expected_all)
    num_all[min_index]<- num_all[min_index] - 1
    
    ## EU1(M) = Bm(Pm)2 ??? Cm 
    Expected_payoffs_m <-  sum( (22*(((num_all["Multilateral"])/num_players)^2)),-5, na.rm = T) + rnorm(n = 1, mean = 0, sd = 2)
    
    ## EU1(B) = Bb(2PbPm + 2PbPu + 2PbPb) ??? Cb 
    
    Expected_payoffs_b <-  sum((2*10*((num_all["Multilateral"])/num_players)*(num_all["Bilateral"]/num_players) ) ,
                               (2*10*(num_all["Bilateral"]/num_players)*(num_all["Unilateral"]/num_players)),        
                               (2*10*(num_all["Bilateral"]/num_players)^2), -3 ,na.rm = T) + rnorm(n = 1, mean = 0, sd = 2)
    
    Expected_payoffs_u <-  0 + rnorm(n = 1, mean = 0, sd = 2)
    
    
    if(sum(num_all) > num_players){
      break
    }
    
  }
  print(num_all) 
}


num_players = 9000
payoffs <- matrix(c(60,-5,-5,-3, 7,-3,0,0,0), nrow = 3, byrow = T)

## "M" stands for multilateral "B" stands for bilateral and "U" stands for "unilateral strategy"
rownames(payoffs) <- c("Multilateral","Bilateral","Unilateral") 
colnames(payoffs) <- c("Multilateral","Bilateral","Unilateral")

output_storage <- vector("list", 1000) ## list data structure to store output distribution of strategies
storage <- vector("list",1000)         ## list data structure to store initial distribution of strategies
current_path_history <- vector("list",300)
path_histories <- vector("list",1000)
for (k in seq_along(1:1000)){
  sample_to_fixed_sum <- function(fixed_sum){
    sample_vector <- numeric(3)
    sample_vector[1] <- sample(1:(fixed_sum-2), 1)
    sample_vector[2] <- sample(1:(fixed_sum-sample_vector[1]-1), 1)
    sample_vector[3] <- fixed_sum - sum(sample_vector)
    names(sample_vector) <- c("Multilateral","Bilateral","Unilateral")
    return(sample_vector)
  }
  
  storage[[k]] <-sample_to_fixed_sum(9000)
  num_all      <- storage[[k]]
  
  #num_all <- c(200, 0, 8800)
  #names(num_all) <-c("Multilateral","Bilateral","Unilateral")
  
  
  Expected_payoffs_m <-  sum( (22*(((num_all["Multilateral"])/num_players)^2)),-5, na.rm = T) + rnorm(n = 1, mean = 0, sd = 2)
  
  Expected_payoffs_b <-  sum((2*10*((num_all["Multilateral"])/num_players)*(num_all["Bilateral"]/num_players) ) ,
                             (2*10*(num_all["Bilateral"]/num_players)*(num_all["Unilateral"]/num_players)),        
                             (2*10*(num_all["Bilateral"]/num_players)^2), -3 ,na.rm = T) + rnorm(n = 1, mean = 0, sd = 2)
  
  
  Expected_payoffs_u <-  0 + rnorm(n = 1, mean = 0, sd = 2)
  
  
  
  expected_all <- c("Multilateral" = Expected_payoffs_m,"Bilateral"= Expected_payoffs_b, "Unilateral"= Expected_payoffs_u)
  
  output_storage[[k]] <-Cooperation_pattern(num_all = num_all, expected_all= expected_all, num_players = 9000, num_generations =30000 )
  
  path_histories[[k]] = current_path_history
  
}




multilateral_inital <- map_dbl(storage, function(x) x[[1]] ) 
bilateral_initial   <- map_dbl(storage, function(x) x[[2]] )
unilateral_initial  <- map_dbl(storage, function(x) x[[3]] )

Cooperation_outcome <- map_chr(output_storage, function(x) names(x))

df <- data.frame(m_inital = multilateral_inital,
                 b_initial   = bilateral_initial,
                 u_initial  = unilateral_initial,
                 coop_outcome = Cooperation_outcome)

df_fdi <- as_tibble(df)


write_csv(df_fdi, "Stag_elephant_fdi_learning_Oct06.csv")

##************************************Storing path histories in separate csv files**************************##
##*********************************************************************************************************##

for (n in seq_along(1:1000)){
  
  Multilateral_history <- map_dbl(path_histories[[n]], function(x) x["Multilateral"])
  Bilateral_history <- map_dbl(path_histories[[n]], function(x) x["Bilateral"])
  Unilateral_history <- map_dbl(path_histories[[n]], function(x) x["Unilateral"])
  
  path_history_df <- data.frame(Multilateral_history = Multilateral_history,
                                Bilateral_history   = Bilateral_history,
                                Unilateral_history = Unilateral_history)
  
  file_path <-  file.path("~/Health equity/coop_path_histories", paste0("path_history", "_", n, ".csv"))                
  
  write_csv(path_history_df, file_path)
  
}
