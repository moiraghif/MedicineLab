library(dplyr)

features <- read.csv("./feature_dataset.csv")
features <- features %>% mutate_at(setdiff(colnames(features),
                                           c("y")),
                                   ~(scale(.) %>% as.vector))

round(head(features), 3)

score <- c()
for (i in seq(1,dim(features)[2]-1)){
  formula <- paste0(colnames(features)[i]," ~ y")
  t_score <- wilcox.test(formula=as.formula(formula),
                    data=features)$p.value
  score <- c(score, round(t_score,3))
}
score_df <- data.frame(t(score))
colnames(score_df) <- colnames(features)[1:dim(features)[2]-1]
accepted <- colnames(score_df[,score_df < 0.05])

features <- features[,c(accepted, "y")]

score_df[,accepted]

new_cols <- setdiff(colnames(features),
                    c("Maximum","Variance",
                      "Maximum3DDiameter",
                      "MinorAxisLength",
                      "Contrast", "Sphericity"))

features <- features[,new_cols]

library(MASS)


accuracy <- function(y_true, y_hat) {
  return(mean(y_true == y_hat))
}

precision <- function(y_true, y_hat) {
  tp <- mean(y_hat == 1 & y_true == 1)
  fp <- mean(y_hat == 1 & y_true == 0)
  return(tp / (tp + fp))
}

recall <- function(y_true, y_hat) {
  tp <- mean(y_hat == 1 & y_true == 1)
  fn <- mean(y_hat == 0 & y_true == 1)
  if (fn == 0) return(1)
  return(tp / (tp + fn))
}

f1 <- function(y_true, y_hat) {
  p <- precision(y_true, y_hat)
  r <- recall(y_true, y_hat)
  return(2 * p * r / (p + r))
}


k <- 30
dim_fold <- 9
out <- list(accuracy = c(),
            precision = c(),
            recall = c(),
            f_1 = c())
features$y <- as.factor(features$y)
for (i in seq(1, k)) {
  set.seed(i)
  test_index <- sample(seq(1,dim(features)[1]), dim_fold)
  train_set <- features[-test_index, ]
  test_set  <- features[ test_index, ]

  mod <- glm(y ~ SurfaceArea + Kurtosis + Skewness,
             data = train_set,
             family = binomial("logit"))

  y_hat <- ifelse(predict(mod, test_set)>0.5, 1, 0)
  y_true <- test_set$y
  out$accuracy <- c(out$accuracy, accuracy(y_true, y_hat))
  out$precision <- c(out$precision, precision(y_true, y_hat))
  out$recall <- c(out$recall, recall(y_true, y_hat))
  out$f_1 <- c(out$f_1, f1(y_true, y_hat))
}

summary(mod)
