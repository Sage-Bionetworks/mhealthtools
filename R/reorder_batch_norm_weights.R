#incorrect_gammas <- c(3, 9, 15, 21, 27, 33, 39, 45)
#incorrect_betas <- incorrect_gammas + 1
#
#guanlab_nn_weights <- purrr::map(1:length(guanlab_nn_weights), function(i) {
#  w <- guanlab_nn_weights[[i]]
#  w_new <- purrr::map(1:length(w), function(j) {
#    if (j %in% incorrect_gammas) {
#      incorrect_beta_weights <- w[[j+1]]
#      return(incorrect_beta_weights) 
#    } else if (j %in% incorrect_betas) {
#      incorrect_gamma_weights <- w[[j-1]]
#      return(incorrect_gamma_weights)
#    }
#    return(w[[j]])
#  })
#  return(w_new)
#})