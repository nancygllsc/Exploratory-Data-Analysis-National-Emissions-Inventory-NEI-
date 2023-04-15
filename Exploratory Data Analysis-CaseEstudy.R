cnames <- strsplit(cnames, "|", fixed = TRUE) 
names(pm0)<- make.names(cnames[[1]][wcol])
mean(is.na(x0)) #as an argument to see what percentage of values are missing (NA) in x0.
dim(pm1)
summary(x0)
boxplot(log10(x0),log10(x1))

mean(negative, na.rm = TRUE)
dates <- as.Date(as.character(dates), "%Y%m%d")
both <- intersect(site0, site1)
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
abline(h = median(x0sub, na.rm = TRUE),lwd=2)
rng <- range(x0sub,x1sub,na.rm=TRUE)
mn1 <- with(pm1,tapply(Sample.Value,State.Code,mean,na.rm=TRUE))
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
with(mrg, plot(rep(1, 52), mrg[, 2], xlim = c(.5, 2.5)))
with(mrg, points(rep(2, 52), mrg[, 3]))
segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])






