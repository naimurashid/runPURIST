#' @export
ind_fun = function(train_sub, classifier){
  indmat = matrix(-1, ncol(train_sub), nrow(classifier$TSPs))
  for(i in 1:nrow(classifier$TSPs)){
    p1 = which(rownames(train_sub) == classifier$TSPs[i,1])
    p2 = which(rownames(train_sub) == classifier$TSPs[i,2])
    if(length(p1) == 0) stop(sprintf("%s is not found in input matrix rownames",classifier$TSPs[i,1]))
    if(length(p2) == 0) stop(sprintf("%s is not found in input matrix rownames",classifier$TSPs[i,2]))
    indmat[,i] = (train_sub[p1,] > train_sub[p2,])^2
  }
  indmat = t(indmat)
  colnames(indmat) = colnames(train_sub)
  return(indmat)
}

#' @export
apply_classifier = function(data, classifier){
  
  # drop TSPs with 0 weight
  classifier$TSPs = classifier$TSPs[classifier$fit$beta[-1]!=0,]
  
  # create TSP indicator matrix
  indmat = t(ind_fun(train_sub = data, classifier = classifier))
  
  # name columns
  colnames(indmat) = paste("indmat", 1:ncol(indmat), sep = "")
  
  # add intercept column
  X=cbind(rep(1, nrow(indmat)), indmat)
  
  # make prediction 
  beta = classifier$fit$beta
  beta = beta[beta!=0]
  Pred_prob_basal = exp(X%*%beta)/(1+exp(X%*%beta))
  
  # get subtype
  Subtype = c("classical","basal-like")[(Pred_prob_basal > 0.5)^2 + 1]
  
  # get graded subtype
  Subtype_graded = rep(1, length(Pred_prob_basal))
  Subtype_graded[Pred_prob_basal < .1] = 1
  Subtype_graded[Pred_prob_basal > .1 & Pred_prob_basal < .4] = 2
  Subtype_graded[Pred_prob_basal > .4 & Pred_prob_basal < .5] = 3
  Subtype_graded[Pred_prob_basal > .5 & Pred_prob_basal < .6] = 4
  Subtype_graded[Pred_prob_basal > .6 & Pred_prob_basal < .9] = 5
  Subtype_graded[Pred_prob_basal > .9 ] = 6
  
  # graded categories
  grades = c("strong classical","likely classical","lean classical","lean basal-like","likely basal-like", "strong basal-like")
  Subtype_graded = grades[Subtype_graded]
  
  # final matrix
  final = data.frame(Pred_prob_basal= Pred_prob_basal, Subtype = Subtype, Subtype_graded = Subtype_graded)
  rownames(final) = make.names(colnames(data), unique = any(table(colnames(data)) > 1) )
  
  return(final)
}
ind_fun = function(train_sub, classifier){
  indmat = matrix(-1, ncol(train_sub), nrow(classifier$TSPs))
  for(i in 1:nrow(classifier$TSPs)){
    p1 = which(rownames(train_sub) == classifier$TSPs[i,1])
    p2 = which(rownames(train_sub) == classifier$TSPs[i,2])
    if(length(p1) == 0) stop(sprintf("%s is not found in input matrix rownames",classifier$TSPs[i,1]))
    if(length(p2) == 0) stop(sprintf("%s is not found in input matrix rownames",classifier$TSPs[i,2]))
    indmat[,i] = (train_sub[p1,] > train_sub[p2,])^2
  }
  indmat = t(indmat)
  colnames(indmat) = colnames(train_sub)
  return(indmat)
}

# compare within TSP expression for a given matrix of samples
compare_tsp_gene_expr = function(dat, classifier, title = "", basal = NULL){
  
  # get col vector
  if(is.null(basal)){
    col = rep(1, ncol(dat))
  }else{
    col = rep("blue", ncol(dat))
    col[grep("asal", basal)] = "orange"
  }
  
  # drop TSPs with 0 weight
  classifier$TSPs = classifier$TSPs[classifier$fit$beta[-1]!=0,]
  
  # set up matrices
  diff = rdiff = matrix(NA,nrow(classifier$TSPs), ncol(dat))
  rownames(diff) = rownames(rdiff) = paste(classifier$TSPs[,1],classifier$TSPs[,2], sep = "_")
  colnames(diff) = colnames(rdiff) = colnames(dat)
  
  # get ranked matrix too
  rdat = apply(dat, 2, rank)
  
  for(i in 1:nrow(diff)){
    b = dat[rownames(dat) == classifier$TSPs[i,1],]
    c = dat[rownames(dat) == classifier$TSPs[i,2],]
    diff[i,] = as.numeric(log2(b+.1)) - as.numeric(log2(c+.1))
    
    b = rdat[rownames(dat) == classifier$TSPs[i,1],]
    c = rdat[rownames(dat) == classifier$TSPs[i,2],]
    rdiff[i,] = b - c 
  }
  
  # plot
  par(mfrow = c(2,2))
  boxplot(t(abs(diff)), ylab = "Raw differences", las = 2, main = title, outline = F)
  abline(h = 0)
  boxplot(t(abs(rdiff)), ylab = "Rank differences", las = 2, main = title, outline = F)
  abline(h = 0)
  boxplot((diff), ylab = "Raw differences", las = 2, main = title, outline = F, border = col)
  abline(h = 0)
  boxplot((rdiff), ylab = "Rank differences", las = 2, main = title, outline = F, border = col)
  abline(h = 0)
  return(list(diff = diff, rdiff = rdiff))
}


