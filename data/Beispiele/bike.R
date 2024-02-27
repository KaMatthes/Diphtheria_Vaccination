bike <- read.csv("data/Beispiele/bikeusage_daily.csv", sep=";", fileEncoding = 'UTF-8-BOM')
n <- nrow(bike)
n.hold <- 7
n.train <- n - n.hold
test.data <- tail(bike$Total.Users, n.hold)
train.data <- bike[1:n.train,]
# Index and dataframe
n.seas <- s <-  7
y <- train.data$Total.Users
y.append <- c(y, rep(NA, n.hold))
id <- id.trend <-  id.seas <- 1:length(y.append)
bike.dat <-
  cbind.data.frame(id,
                   id.trend,
                   id.seas,
                   y.append,
                   select(bike, Temperature.F:Wind.Speed))
# Model formula and fit
bike.formula <- y.append ~  
  f(id.trend, model = "rw1") +
  f(id.seas, model = "seasonal", season.length = n.seas)
bike.model <- inla(
  bike.formula,
  family = "poisson",
  data = bike.dat,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE
  ),
  control.family = list(link = "log")
)
summary(bike.model)
# Code for Figure 9.5
fit.bike <- bike.model$summary.fitted.values$mean
fit.bike.df <-
  cbind.data.frame(
    time = 1:n,
    obs.bike = y.append,
    fit.bike = fit.bike
  )

ggplot()+
  geom_line(data=fit.bike.df, aes(x=time, y=fit.bike, col="fitting values"), lwd=1) +
  geom_line(data=fit.bike.df, aes(x=time, y= obs.bike, col="notified cases"),lwd= 1)


yfore <- tail(fit.bike, n.hold)
mae(test.data, round(yfore))