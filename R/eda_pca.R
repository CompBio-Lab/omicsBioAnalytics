#' @export
#' @rdname eda_pca
eda_pca = function(omicsData, metadata, group, reference){
  lapply(omicsData, function(omicData){
    pcs_analytics <- prcomp(
      x = omicData,
      scale. = TRUE,
      center = TRUE,
      rank. = 5
    )
    return(list(var_exp = summary(pcs_analytics),
                pcs = pcs_analytics$x))
  })
}
