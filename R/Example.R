hotels <- read.csv("data/NightlyHotel.csv", sep=";",fileEncoding="UTF-8-BOM")


cost <- ts(hotels[, 2])

lcost <- log(cost)


n <- length(lcost)

n.hold <- 6    
n.train <- n - n.hold
test.lcost <- tail(lcost, n.hold)
train.lcost <- lcost[1:n.train]
y <- train.lcost
y.append <- c(y, rep(NA, n.hold))
n.seas <- 12
trend <- 1:length(y.append)
lcost.dat <- cbind.data.frame(y = y.append,
                              trend = trend,
                              seasonal = trend)




traffic <-
  read.csv(file = "data/Metro_Interstate_94 Traffic_Volume_Hourly.csv",
           header = TRUE,
           stringsAsFactors = FALSE, sep=";")


traffic <- as_tibble(traffic)

traffic <- traffic %>%
  mutate(date_time = as.POSIXct(
    as.character(date_time),
    # origin = "1899-12-30",
    format = "%d.%m.%Y %H",
    tz = "UTC"
  ))


traffic <- traffic %>%
  mutate(
    month = month(date_time, label = TRUE),
    day = day(date_time),
    year = year(date_time),
    dayofweeek = weekdays(date_time),
    hourofday = hour(date_time),
    weeknum = week(date_time)
  )


n <- 720 * 3
y.3mth <- traffic$traffic_volume[1:n] / 1000
par(mfrow = c(2, 2))
ts.plot(y.3mth)
acf(y.3mth, 200, main = "")
spectral <- mvspec(y.3mth , fast = FALSE, main = "")
amplitude <- 2 * spectral$spec
period <- 1 / spectral$freq
plot(
  period,
  amplitude,
  type = "h",
  col = "blue",
  lwd = 2,
  main = ""
)
harmonics <- n / period
all <- cbind(period, harmonics, amplitude)
#order periodogram amplitudes
samp <- all[order(-all[, 3]),]


# Harmonics
id <- 1:n
k <- c(90, 11, 20, 47, 62, 23, 9, 34, 29, 13)
sin.mat <- cos.mat <- matrix(0, nrow = n, ncol = length(k))
for (i in 1:length(k)) {
  sin.mat[, i] <- sin(2 * pi * id * k[i] / n)
  colnames(sin.mat) <- paste("S", c(1:(length(k))), sep = "")
  cos.mat[, i] <- cos(2 * pi * id * k[i] / n)
  colnames(cos.mat) <- paste("C", c(1:(length(k))), sep = "")
}