library(purrr)
library(magrittr)
library(dplyr)
library(stats)
library(factor)
library(tidyverse)
library(tidyr)
library(knitr)
library(readr)
#install.packages()
dat <- tibble(
  x = c(2.0, 2.0, 2.0, 2.5, 2.5, 3.0, 4.0, 4.0, 4.5, 4.5, 4.5 , 4.5),
  y = c(1.0, 1.5, 2.0, 1.0, 2.0, 4.0, 1.0, 2.5, 1.0, 1.5, 2.5 , 3.0)
)

#install.packages('purr',repos='http://cran.us.r-project.org')
dat
#--------------------------------------------
k <- 3
centroids <- dat %>% slice(1:k)
new_centroids <- centroids

??slice()
data

#-------------------------------------------
iteration <- 1
while(TRUE) {
  print(str_c("Iteration ", iteration))
  cat('\n')
  dist_to_centroids <- as.matrix(dist(bind_rows(new_centroids, dat)))[1:k, (k+1):(nrow(dat)+k)] print(knitr::kable(dist_to_centroids))
  assignments <- dist_to_centroids %>% map_dbl(~which.min(.)) %>% as_tibble() %>% mutate(row = print(knitr::kable(assignments))
  cat('\n')
  new_centroids <- nest(assignments, row)$data %>% map(~colMeans(dat[.$row,])) %>% do.call("rbind", print(knitr::kable(new_centroids))
  cat('\n')
  dat_clu <- dat %>% bind_cols(tibble(cluster = factor(assignments$value)))
  
  print(ggplot(dat_clu, aes(x, y, color = cluster, fill = cluster)))
  
  geom_point(pch = 16, size = 10) +
  geom_point(data = dat_clu %>%
  group_by(cluster) %>%
  summarize_all(mean), shape = "+", size = 10) +
  guides(fill = FALSE, color = FALSE) +
  labs(title = str_c("Iteration", iteration)) +
  theme_bw())
                                                                                                                                                                                
  dat <- tibble(
        x = c(7.25, 5.25, 2.8, 4.25, 5.1, 5.75, 2.30, 1.10, 4.0, 2.1, 3.8 , 5.9),
        y = c(1.00, 3.60, 3.8, 4.80, 3.8, 0.60, 1.65, 2.50, 4.0, 2.1, 1.6 , 1.4)
  )
  
  print(ggplot(dat, aes(x, y)) +
  geom_point(pch = 16, size = 8) +
  guides(fill = FALSE, color = FALSE) +
  theme_bw())
  
  if(identical(centroids, new_centroids)) {
  break
  }
  centroids <- new_centroids
  iteration <- iteration + 1
}                                                                                                                                                                                  