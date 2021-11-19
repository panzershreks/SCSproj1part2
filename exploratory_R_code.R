
load(file="foresthealth.rda")

dat$tree_sp_eu <- factor(dat$tree_sp_eu)
levels(dat$tree_sp_eu)

summary(dat)
print(nrow(dat))
print(nrow(na.omit(dat)))
(9447-8105)/9447 # 14% of data missing from just removing NAs

dd <- na.omit(dat)
cor(dat$H_spec, dat$Es, use="pairwise.complete.obs")
cor(dat$spei_12_oct, dat$spei_24_oct, use="pairwise.complete.obs")
cor(dat$spei_3_may, dat$spei_3_aug, use="pairwise.complete.obs")
cor(dat$spei_12_oct, dat$spei_3_aug, use="pairwise.complete.obs")
dat$id <- sapply(dat$id, as.numeric)
drop_cols <- dat[!(names(dat) %in% c("Es", ""))] # omitted Es data
(9447-9241)/9447 # now only 2% of data loss
data <- na.omit(drop_cols) # initial processed data stores as `data`