rm(list = ls())
set.seed(201920)

library(RNifti)
library(tidyverse)


basic_path <- "./code__esempi/lesions"
classes <- c("heterogeneous", "homogeneous")
paths <- paste(basic_path, classes, "nifti/", sep="/")

files_heterogeneous <- paste0(paths[1], list.files(paths[1]))
files_homogeneous   <- paste0(paths[2], list.files(paths[2]))
files_class <- rep.int(c(1, 0),
                       times = purrr::map_int(list(files_heterogeneous,
                                                   files_homogeneous),
                                              length))
files <- tibble::tibble(
  filename = c(files_heterogeneous, files_homogeneous),
  heterogeneous = files_class)
files <- files[sample(1:nrow(files)), ]


read_nii <- function(file_path) {
  clean <- function(im) {
    clean_x <- function(im) {
      return(im[purrr::keep(1:dim(im)[1],
                            ~!all(is.na(im[.x, , ]))), ,])
    }
    clean_y <- function(im) {
      return(im[, purrr::keep(1:dim(im)[2],
                              ~!all(is.na(im[, .x, ]))), ])
    }
    clean_z <- function(im) {
      return(im[, , purrr::keep(1:dim(im)[3],
                                ~!all(is.na(im[, , .x])))])
    }
    clean_image <- purrr::compose(clean_x, clean_y, clean_z)
    out <- clean_image(im)
    pixdim(out) <- pixdim(im)
    return(out)
  }
  image <- RNifti::readNifti(file_path)
  image[image == 0] <- NA
  return(clean(image))
}

get_area <- function(image) {
  xyz <- dim(image)
  voxel_dim <- prod(pixdim(image))
  out <- 0
  for (x in 1:xyz[1])
    for (y in 1:xyz[2])
      for (z in 1:xyz[3])
        if (!is.na(image[x, y, z])) {
          lim_x <- c(max(x - 1, 0), min(x + 1, xyz[1]))
          lim_y <- c(max(y - 1, 0), min(y + 1, xyz[2]))
          lim_z <- c(max(z - 1, 0), min(z + 1, xyz[3]))
          intorno <- image[seq(lim_x[1], lim_x[2]),
                           seq(lim_y[1], lim_y[2]),
                           seq(lim_z[1], lim_z[2])]
          if (anyNA(intorno) || (x %in% lim_x || y %in% lim_y || z %in% lim_z))
            out <- out + voxel_dim
        }
  return(out)
}

skewness <- function(image) {
  image_clean <- image[!is.na(image)]
  return(mean(((image_clean - mean(image_clean)) / sd(image_clean))^3))
}

kurtosis <- function(image) {
  image_clean <- image[!is.na(image)]
  return(mean(((image_clean - mean(image_clean)) / sd(image_clean))^4))
}

extract_features <- function(image_path) {
  image <- read_nii(image_path)
  voxel_dim <- pixdim(image)
  image.mean <- mean(image, na.rm = TRUE)
  image.sd   <- sd(image, na.rm = TRUE)
  image.sk   <- skewness(image)
  image.kurt <- kurtosis(image)
  image.volume <- prod(voxel_dim) * sum(! is.na(image))
  image.sphere <- pi * 4/3 * (max(voxel_dim * dim(image)) / 2)^3
  return(c(image.mean, image.sd, image.sk, image.kurt, image.volume, image.sphere, get_area(image[,,])))
}

features <- tibble::as.tibble(t(purrr::map_dfc(files$filename, extract_features)))
names(features) <- c("mean", "sd", "sk", "kurt", "volume", "sphere", "area")
features$y <- files$heterogeneous

library(MASS)

train_index <-  1:35
test_index  <- 36:nrow(features)

train_set <- features[train_index, ]
test_set  <- features[test_index,  ]


predict_model <- function(mod, data) {
  y_hat <- ifelse(predict(mod, data) > 0.5, 1, 0)
  return(mean(y_hat == data$y))
}


mod_full <- glm(y ~ 1 + mean + sk + kurt + volume,
           data = train_set,
           family = binomial("logit"))
mod <- stepAIC(mod_full, direction = "both")

summary(mod)
