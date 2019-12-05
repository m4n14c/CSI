csi_v2 <- function(array_1,array_2,count_bands){
  array_1 <- as.numeric(array_1)
  array_2 <- as.numeric(array_2)
  df_out <- as.data.frame(matrix(nrow = count_bands,ncol = 7))
  colnames_data_out <- c("Score_bands","Array1","Array2","diff_Array1_Array2","ln_Old_by_New","Index","CSI")
  names(df_out) <- colnames_data_out
  quantiles <- as.numeric(quantile(array_1,probs = seq(0, 1, 1/(count_bands)), na.rm = TRUE))
  df_out$Score_bands <- quantiles[2:length(quantiles)]
  df_out$Array1[1] <- length(subset(array_1,array_1<=as.numeric(df_out$Score_bands[1])))/length(array_1)
  df_out$Array2[1] <- length(subset(array_2,array_2<=as.numeric(df_out$Score_bands[1])))/length(array_2)
  for (i in 2:count_bands) {
    df_out$Array1[i] <- length(subset(array_1,array_1 <= as.numeric(df_out$Score_bands[i]) & array_1 > as.numeric(df_out$Score_bands[i-1])))/length(array_1)
    df_out$Array2[i] <- length(subset(array_2,array_2 <= as.numeric(df_out$Score_bands[i]) & array_2 > as.numeric(df_out$Score_bands[i-1])))/length(array_2)
  }
  df_out$diff_Array1_Array2 <- as.numeric(df_out$Array1) - as.numeric(df_out$Array2)
  df_out$ln_Old_by_New <- as.numeric(log(as.numeric(df_out$Array1)/as.numeric(df_out$Array2)))
  df_out$Index <- as.numeric(df_out$diff_Array1_Array2)*as.numeric(df_out$ln_Old_by_New)
  df_out$CSI <- sum(df_out$Index)
  return(df_out)
}
