iterateCFA <- function(data, n_itr, filter, sample_size=500) {
  
  # n_itr = 100 # set number of iterations
  results_con = matrix(nrow = n_itr, ncol = 6)#[1:n_itr, 6]
  # set filter (here we take all with freq lower than 10)
  if (filter=='>') {
    print('More than')
    dataN_Con <- filter(data, freq > 10)  
  } else {
    print ('Less than')
    dataN_Con <- filter(data, freq <= 10)  
  }
  
  for (i in 1:n_itr) {
    # filter and sample data
    dfSamp <- sample_n(dataN_Con, sample_size) 
    pcl5Model <- cfa(dsm5Fac_Con, data = dfSamp[,1:20], estimator = "WLSMV")
    results_con[i,] <- fitMeasures(pcl5Model, c("chisq","df","pvalue","srmr","cfi","rmsea"))
    
  }
  # create a dataframe from the matrix
  results <- data.frame(chisq = results_con[,1], df = results_con[,2],
                        pvalue = results_con[,3], srmr = results_con[,4],
                        cfi = results_con[,5], rmsea = results_con[,6])
  return(results)
}
