test <- resights %>% group_by(seaotter) %>% nest()


trans2 <- trans.mat(bathy_bathy, min.depth = 0, max.depth = -30)
trans3 <- trans.mat(bathy_bathy, min.depth = 5)

dist2_1 <- lc.dist(trans2, otter809_short1, res = "dist")


testmap_dist <- test %>% mutate(distance = map(data, function(x) {lc.dist(trans2, x[, c("longitude", "latitude")], res = "dist")} ))
testmap_dist2 <- test[1:3,] %>% mutate(distance = map(data, function(x) {lc.dist(trans3, x[, c("longitude", "latitude")], res = "dist")} ),
                                 #diag = map(distance, function(x) {})
                                  diag = map(distance, function(x) {as.matrix(x, nrow = attr(x, "Size")-1, ncol = attr(x, "Size"))})
                          )

#testmap_path <- test %>% mutate(distance = map(data, function(x) {lc.dist(trans2, x[, c("longitude", "latitude")], res = "path")} ))

# test$data[,c("longitude", "latitude")]
# test$data[1]
# 

testmap_dist2$distance[1]


