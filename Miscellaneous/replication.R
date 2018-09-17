## STANDALONE REPLICATION SCRIPT 3
## for installing the DepthProc
## developer versions are available under the adress
## library(devtools)
## install_github("zzawadz/DepthProc")

library(DepthProc)

# Set seed for reproducible results
set.seed(123)

## Illustrations and plots appearing in text (ordered)

data(inf.mort, maesles.imm)
data2011 = na.omit(cbind(inf.mort[, 22], maesles.imm[, 22]))

### Figure 1:
depthContour(
  data2011,
  n = 50,
  points = TRUE,
  depth_params = list(method = "Projection"),
  graph_params = list(
    xlab = "infant mortality rate per 1000 live births",
    ylab = "against measles immunized percentage",
    main = 'Projection depth,
    UN Fourth Goal 2011'
  )
  )

### Figure 2:
depthContour(
  data2011,
  n = 50,
  points = TRUE,
  depth_params = list(method = "Tukey"),
  graph_params = list(
    xlab = "infant mortality rate per 1000 live births",
    ylab = "against measles immunized percentage",
    main = 'Tukey depth,
    UN Fourth Goal 2011'
  )
  )

### Figure 3:
depthContour(
  data2011,
  n = 50,
  points = TRUE,
  depth_params = list(method = "Mahalanobis"),
  graph_params =  list(
    xlab = "infant mortality rate per 1000 live births",
    ylab = "against measles immunized percentage",
    main = 'Mahalanobis depth,
    UN Fourth Goal 2011'
  )
  )

### Figure 4:
depthContour(
  data2011,
  n = 50,
  points = TRUE,
  depth_params = list(method = "LP", pdim = 2),
  graph_params = list(
    xlab = "infant mortality rate per 1000 live births",
    ylab = "against measles immunized percentage",
    main = 'L2 depth,
    UN Fourth Goal 2011'
  ),
  levels = 30
  )

### Figure 5 -- EXAMPLE 3
### Figure 6 -- EXAMPLE 4

### Figure 7
library(colorspace)

dcont = lsdSampleDepthContours(rnorm(200, 0, 1))
plot(dcont, col = heat_hcl(4))

### Figure 8
dcont = lsdSampleDepthContours(rt(200, 1))
plot(dcont, col = heat_hcl(4))

### Figure 9 - 10

Sigma1 = matrix(c(10, 3, 3, 2), 2, 2)
X1 = mvrnorm(n = 8500, mu = c(0, 0), Sigma1)
Sigma2 = matrix(c(10, 0, 0, 2), 2, 2)
X2 = mvrnorm(n = 1500, mu = c(-10, 6), Sigma2)
BALLOT = rbind(X1, X2)
train <- sample(1:10000, 100)
data <- BALLOT[train, ]
depthContour(data,
             depth_params = list(
               method = "Local",
               beta = 0.2,
               depth_params1 = list(method = "LP")
             ), points = TRUE)
depthContour(
  data,
  depth_params = list(
    method = "Local",
    beta = 0.6,
    depth_params1 = list(method = "LP")
  ),
  points = TRUE
)

### Figure 11
data("cracow.airpollution")
attach(cracow.airpollution)
library(zoo)
NO.fst= rollapply(NO,
                  width = 24,
                  by = 24,
                  window,
                  by.column = FALSE) #functional time series
NOx.fst= rollapply(NOx,
                   width = 24,
                   by = 24,
                   window,
                   by.column = FALSE) #functional time series

PM10.fst= rollapply(PM10,
                    width = 24,
                    by = 24,
                    window,
                    by.column = FALSE) #functional time series
PM2.5.fst= rollapply(PM2.5,
                     width = 24,
                     by = 24,
                     window,
                     by.column = FALSE) #functional time series

w1 = fncBoxPlot(NO.fst, bands=c(0,0.05,0.1,0.2,0.3,0.5,0.7),
                method = "MBD")
print(w1 + ggtitle("Air pollution in Cracow in December  2016") +
        labs(y = "NO ", x = "hour "))
w2 = fncBoxPlot(NOx.fst, bands=c(0,0.05,0.1,0.2,0.3,0.5,0.7),
                method = "MBD")
print(w2 + ggtitle("Air pollution in Cracow in December  2016") +
        labs(y = "NOx ", x = "hour "))
w3 = fncBoxPlot(PM10.fst, bands=c(0,0.05,0.1,0.2,0.3,0.5,0.7),
                method = "MBD")
print(w3 + ggtitle("Air pollution in Cracow in December  2016") +
        labs(y = "PM10", x = "hour "))
w4 = fncBoxPlot(PM2.5.fst, bands=c(0,0.05,0.1,0.2,0.3,0.5,0.7),
                method = "MBD")
print(w4 + ggtitle("Air pollution in Cracow in December  2016") +
        labs(y = "zanieczyszczenie PM2.5 ", x = "hour "))


### Figures 12 - 15

## example the Internet users activities monitoring
data("internet.users")
users <- internet.users[1:17280, 5]
views <- internet.users[1:17280, 6]

ind_1 = which(internet.users[, 1] == 1)
DATA_1 = internet.users[ind_1,] #the first Internet service
ind_2 = which(internet.users[, 1] == 2)
DATA_2 = internet.users[ind_2,]

users_1 <- DATA_1[1:8759, 5] #the number of unique users in the service 1#
views_1 <- DATA_1[1:8759, 6] #the number of page views in the service 1#

users_2 <- DATA_2[1:8759, 5] #the number of unique users in the service 2#
views_2 <- DATA_2[1:8759, 6] #the number of page views in the service 2


library(zoo)
window <- function(x) { x }
users.m.1 = rollapply(users_1,
                      width = 24,
                      by = 24,
                      window,
                      by.column = FALSE)
views.m.1 = rollapply(views_1,
                      width = 24,
                      by = 24,
                      window,
                      by.column = FALSE)
users.m.2 = rollapply(users_2,
                      width = 24,
                      by = 24,
                      window,
                      by.column = FALSE)
views.m.2 = rollapply(views_2,
                      width = 24,
                      by = 24,
                      window)
# FUNCTIONAL BOXPLOTS
fncBoxPlot(users.m.1, bands = c(0, 0.5, 1), method = "MBD") #the modified band depth
fncBoxPlot(users.m.1, bands = c(0, 0.5, 1), method = "FM") #Frainman and Muniz depth

fncBoxPlot(views.m.1, bands = c(0, 0.5, 1), method = "MBD") #the modified band depth
fncBoxPlot(views.m.1, bands = c(0, 0.5, 1), method = "FM") #Frainman and Muniz depth

### Figures 16 - 17

ddPlot(users.m.1, users.m.2, depth_params = list(method = "Local", depth_params1 = list(method = "MBD", beta = 0.25)))

ddPlot(users.m.1, users.m.2, depth_params = list(method = "Local", depth_params1 = list(method = "MBD", beta = 0.45)))


### Figures 18 - 19
x <- matrix(rnorm(2000), ncol = 2)
depthContour(x, depth_params = list(method = "Euclidean"))
depthPersp(x, depth_params = list(method = "Euclidean"))

### Figure 20 - DD PLOT AND LOCATION DIFFERENCE
standard = mvrnorm(1000, c(0, 0), diag(2))
shift    =  mvrnorm(1000, c(0.5, 0), diag(2))
ddPlot(x = standard, y = shift, title = "Difference in position")

### Figure 21 - EXAMPLE: DD PLOT AND SCALE DIFFERENCE
standard <- mvrnorm(1000, c(0, 0), diag(2))
scale <- mvrnorm(1000, c(0, 0), 4 * diag(2))
ddPlot(x = standard, y = scale)

### Figure 22
## comparing of two scale curves - normal distribution and mixture of normal distributions
x = mvrnorm(100, c(0, 0), diag(2))
y = mvrnorm(80, c(0, 0), diag(2))
z = mvrnorm(20, c(5, 5), diag(2))
scaleCurve(
  x,
  rbind(y, z),
  depth_params = list(method = "Projection"),
  name = "N",
  name_y = "Mixture of N"
)



### Figure 23
# ASYMMETRY CURVE
require(sn)
xi = c(0, 0)
alpha <- c(2,-5)
Omega <- diag(2) * 5
n = 500
X = mvrnorm(n, xi, Omega)  # normal distribution
Y = rmst(n, xi, Omega, alpha, nu = 1)
asymmetryCurve(X, Y, name = "NORM", name_y = "S_T(2,-5,10)")

### Figure 24 - 25
# See Example 6

### Figure 26
gl_1 = depth(
  users.m.1,
  method = "Local",
  beta = 0.2,
  depth_params1 = list(method = "MBD")
)

gl_2 = depth(
  views.m.1,
  method = "Local",
  beta = 0.2,
  depth_params1 = list(method = "MBD")
) #depths

# Local functional depth for Internet users data

par(mfrow = c(1, 2))
require("RColorBrewer")
cols_1 = colorRampPalette(c('red', 'blue'))(length(gl_1))[rank(gl_1)]
matplot(
  t(users.m.1),
  type = "l",
  col = cols_1,
  lwd = 3 * gl_1,
  main = 'Local depth, beta=0.2',
  xlab = 'hour',
  ylab = 'users'
)
med_ind_1 = which(gl_1 == max(gl_1))
lines(users.m.1[med_ind_1, ], lwd = 4, col = 1)

cols_2 = colorRampPalette(c('red', 'blue'))(length(gl_2))[rank(gl_2)]
matplot(
  t(views.m.1),
  type = "l",
  col = cols_2,
  lwd = 3 * gl_2,
  main = 'Local depth, beta=0.2',
  xlab = 'hour',
  ylab = 'views'
)
med_ind_2 = which(gl_2 == max(gl_2))
lines(views.m.1[med_ind_2, ], lwd = 4, col = 1)

### Figure 27 - UML diagram - created in Dia
### Figure 28 - UML diagram - created in Dia

### Figures 29 - 32

library(DepthProc)
library(dplyr)
library(reshape2)

## Data preparation (both for scale curve and student depth examples)

# load all data
data(inf.mort)
data(maesles.imm)
data(under5.mort)

# rescale maesles.imm into occurrences/1000
maesles.imm[,3] = (100 - maesles.imm[,3])*10

dt1990 <-
  na.omit(cbind(under5.mort[, "1990"], inf.mort[, "1990"], maesles.imm[, "1990"]))
dt2011 <-
  na.omit(cbind(under5.mort[, "1990"], inf.mort[, "1990"], maesles.imm[, "1990"]))

depthContour(
  dt1990[, c(1, 3)],
  points = TRUE,
  depth_params = list(
    method = "Local",
    depth_params1 = list(method = "LP"),
    beta = 0.2
  ), graph_params = list(xlab = "under five months mortality rate per 1000 births", ylab = "against measles immunized percentage", main = "Local L2 depth 1990, locality = 50%")
)

depthContour(
  dt2011[, c(1, 3)],
  points = TRUE,
  depth_params = list(
    method = "Local",
    depth_params1 = list(method = "LP"),
    beta = 0.2
  ), graph_params = list(xlab = "under five months mortality rate per 1000 births", ylab = "against measles immunized percentage", main = "Local L2 depth 2011, locality = 50%")
)

depthContour(
  dt1990[, c(2, 3)],
  points = TRUE,
  depth_params = list(
    method = "Local",
    depth_params1 = list(method = "LP"),
    beta = 0.2
  ), graph_params = list(xlab = "infant mortality rate per 1000 births", ylab = "against measles immunized percentage", main = "Local L2 depth 1990, locality = 50%")
)

depthContour(
  dt2011[, c(2, 3)],
  points = TRUE,
  depth_params = list(
    method = "Local",
    depth_params1 = list(method = "LP"),
    beta = 0.2
  ), graph_params = list(xlab = "infant mortality rate per 1000 births", ylab = "against measles immunized percentage", main = "Local L2 depth 2011, locality = 50%")
)

### Figures 33 - 34

dt1990 <- na.omit(cbind(under5.mort[,"1990"], inf.mort[,"1990"], maesles.imm[,"1990"]))
dt2011 <- na.omit(cbind(under5.mort[,"2011"], inf.mort[,"2011"], maesles.imm[,"2011"]))


ddPlot(dt1990, dt2011, depth_params = list(method = "LP"), scale = TRUE)

ddPlot(dt1990, dt2011, depth_params = list(method = "LP"), location = TRUE)

### Figure 35 - 36

# prepare data for melting with country name as a variable
inf.mort = cbind(name = rownames(inf.mort), inf.mort)
maesles.imm = cbind(name = rownames(maesles.imm), maesles.imm)
under5.mort = cbind(name = rownames(under5.mort), under5.mort)

# melt all data with reshape2
inf.mort = melt(inf.mort)
maesles.imm = melt(maesles.imm)
under5.mort = melt(under5.mort)

all_data = merge(maesles.imm, under5.mort, by = c("name","variable"))
all_data = merge(all_data, inf.mort, by = c("name","variable"))

# set proper column names
colnames(all_data) = c("country","year","maesles.imm", "under5.mort", "inf.mort")

## Scale Curve

# prepare function for dplyr "do"
# it computes scale-curve, and set proper name
scWrap = function(x) scaleCurve(x[,3:5], name = as.character(x[1,2] %>% unlist))

# estimate scale-curve for every year
# we use new dplyr syntax with pipe opertor %>%
sc_curves = all_data %>% group_by(year) %>%
  do(scale_curves = scWrap(.))

# extract scale curves, and merging into one plot with %+% and reduce
scale_curves = sc_curves$scale_curves
scurves = Reduce(combineDepthCurves, scale_curves)
getPlot(scurves) + scale_color_manual(
  values = heat_hcl(length(scale_curves)),
  name = "Year")

### Figure 36

lsd_by_years = all_data %>% group_by(year) %>%
  do(lsd = lsdSampleDepthContours(.$inf.mort, depth = 0.1)) %>%
  "$"("lsd") # extract lsd column as list
years = all_data$year %>% unique

colors = heat_hcl(length(lsd_by_years))
plot(lsd_by_years[[1]],cont = 0.1, border = "red", lwd = 2)

# add other years into plot
sapply(seq_along(lsd_by_years),
       function(i)
       {
         lsdAddContour(lsd_by_years[[i]],cont = 0.1, lwd = 4, border = colors[i])
       })

legend("topleft",legend = years, ncol = 6, lwd = 4, col = colors)

# Figures 37 -38

data(under5.mort)
data(inf.mort)
data(maesles.imm)
data2011 = na.omit(cbind(under5.mort[, 22], inf.mort[, 22], maesles.imm[, 22]))
x <- data2011[, 3]
y <- data2011[, 2]
plot(x, y, cex = 1.2,
  ylab = "infant mortality rate per 1000 live births",
  xlab = "against measles immunized percentage",
  main = 'Deepest regressions vs. LS regressions'
)

abline(lm(x ~ y, data = pension), lwd = 2, col = 'black') #lm
abline(deepReg2d (x, y), lwd = 2, col = 'red') #trimmed reg#
legend(
  "bottomleft",
  c("LS", "DeepReg"),
  fill = c("black", "red"),
  cex = 1.4,
  bty = "n"
)


plot(x, y, cex = 1.2,
     ylab = "infant mortality rate per 1000 live births",
     xlab = "against measles immunized percentage",
     main = 'Projection Depth Trimmed vs. LS regressions'
)

abline(lm(x ~ y, data = pension), lwd = 2, col = 'black') #lm
abline(trimProjReg2d(x, y), lwd = 2, col = 'red') #trimmed reg#
legend(
  "bottomleft",
  c("LS", "TrimReg"),
  fill = c("black", "red"),
  cex = 1.4,
  bty = "n"
)


################## Other code ###########
## example
data(under5.mort)
data(inf.mort)
data(maesles.imm)
data2011 = na.omit(cbind(under5.mort[, 22], inf.mort[, 22], maesles.imm[, 22]))
data1990 = na.omit(cbind(under5.mort[, 1], inf.mort[, 1], maesles.imm[, 1]))
mWilcoxonTest(data2011, data1990)

# NUMBERED EXAMPELS APPEARING IN THE TEXT

set.seed(123)

### EXAMPLE 1
library(DepthProc)
require(MASS)

x = mvrnorm(100, c(0, 0), diag(2))
y = mvrnorm(100, c(0, 0), diag(2) * 1.4)
mWilcoxonTest(x, y)


### EXAMPLE 2
x  <- mvrnorm(1000, c(0,0), diag(2))
s1 <- scaleCurve(x, name = "Curve 1")
s2 <- scaleCurve(x*2, x*3, name = "Curve 2", name_y = "Curve 3")
w  <- getPlot(combineDepthCurves(s1, s2)) + ggtitle("Plot")
w + theme(text = element_text(size = 25))
xx <- mvrnorm(1000, c(0,0), diag(2))
yy <- mvrnorm(1000, c(0,0), diag(2))
p  <- asymmetryCurve(xx, yy)
getPlot(p) + ggtitle("Plot")

xx <- mvrnorm(1000, c(0, 0), diag(2))
yy <- mvrnorm(1000, c(0, 0), diag(2))
p <- asymmetryCurve(xx, yy)
getPlot(p) + ggtitle("Plot")

### EXAMPLE 3
### FIGURE 5
plot(starsCYG, cex=1.4)
deepreg  <- deepReg2d(starsCYG$log.Te, starsCYG$log.light)
trimreg  <- trimProjReg2d(starsCYG$log.Te, starsCYG$log.light)
least.sq <- lm(starsCYG$log.Te ~ starsCYG$log.light)
abline(deepreg,  lwd = 3, col = "red")
abline(trimreg,  lwd = 3, col = "brown")
abline(least.sq, lwd = 3, col = "blue")
  legend("bottomleft", c("DeepReg", "TrimReg", "LS"), col = c("red", "brown", "blue"), lwd = 2)

### EXAMPLE 4
### FIGURE 6
library("MASS")
library("quantreg")
library("DepthProc")
data("france")
attach(france)
plot(MW, UR, cex=2)
RES1 <- lm(UR ~ MW)
abline(RES1, lwd=2, cex=3, col='red')
summary(RES1)
RES2 <- rlm(UR ~ MW)
summary(RES2)
abline(RES2, lwd=5, col="blue")
deviance(RES2)
(RES4 <- lqs(UR ~ MW,method = "lms"))
(RES5 <- lqs(UR ~ MW, method = "lts"))
abline(RES4, lwd = 2, col = "green")
abline(RES5, lwd = 2, col = "pink")
lines(lowess(MW, UR, f=0.5, iter = 0), lwd = 2)
RES6 <- trimProjReg2d(MW, UR)
abline(RES6, lwd = 3, col = 'darkgreen')

### EXAMPLE 5

require("MASS")
Sigma1 <- matrix(c(10, 3, 3, 2), 2, 2)
X1 <- mvrnorm(n = 8500, mu = c(0, 0), Sigma1)
Sigma2 <- matrix(c(10, 0, 0, 2), 2, 2)
X2 <- mvrnorm(n = 1500, mu = c(-10, 6), Sigma2)
BALLOT <- rbind(X1,X2)
train  <- sample(1:10000, 500)
data   <- BALLOT[train, ]
cov_x  <- CovLP(data, 1, 1, 1)
cov_x

### EXAMPLE 6

require("MASS")
Sigma1 <- matrix(c(10, 3, 3, 2), 2, 2)
X1     <- mvrnorm(n = 8500, mu= c(0, 0), Sigma1)
Sigma2 <- matrix(c(10,0,0,2), 2, 2)
X2     <- mvrnorm(n = 1500, mu = c(-10, 6), Sigma2)
BALLOT <- rbind(X1, X2)
train  <- sample(1:10000, 500)
data   <- BALLOT[train, ]
plot(data)

b1 <- binningDepth2D(data, remove_borders = FALSE, nbins = 12, k = 1)
b2 <- binningDepth2D(data, nbins = 12, k = 1, remove_borders = TRUE)
plot(b1)
plot(b2)

### EXAMPLE 7
data("under5.mort")
data("maesles.imm")
data2011 <- cbind(under5.mort[, 22], maesles.imm[, 22])
plot(binningDepth2D(data2011, nbins = 8, k = 0.5,
                    remove_borders = TRUE ))


### EXAMPLE 8

data("katowice.airpollution")
katowice.raw <- as.matrix(katowice.airpollution)
matplot(t(katowice.raw), type = "l",
        col  = terrain.colors(181), main = 'KATOWICE',
        xlab = 'hour', xlim = c(0, 24), ylab = 'pollution')
w1 <- fncBoxPlot(katowice.airpollution,
             bands = c(0, 0.05, 0.10, 0.5, 0.90, 0.95), method = "MBD")
print(w1 + ggtitle("Air pollution in Katowice 2016 - 2017") +
             labs(y = "pollution ", x = "hour "))

data("cracow.airpollution")
cracow.pm10 <- matrix(cracow.airpollution[,"PM10"], ncol = 24, byrow = TRUE)

w1 <- fncBoxPlot(cracow.pm10,
             bands = c(0, 0.05, 0.10, 0.5, 0.90, 0.95), method = "MBD")
print(w1 + ggtitle("Air pollution in December 2016") +
             labs(y = "pollution", x = "hour"))

### EXAMPLE 9
data("internet.users")
users<-internet.users[1:17280,5]
views<-internet.users[1:17280,6]
library("zoo")
window <- function(x) { x }
users.m <- rollapply(users, width = 24, by = 24, window, by.column = FALSE)
views.m <- rollapply(views, width = 24, by = 24, window, by.column = FALSE)
depths_1 <- depthLocal(users.m, beta=0.45, depth_params1 = list(method = "MBD"))
depths_2 <- depthLocal(views.m, beta=0.45, depth_params1 = list(method = "MBD"))
par(mfrow = c(1, 2))
plot(depths_1, xlab = "hour", ylab = "users", main = "local depth, beta = 0.45")
plot(depths_2, xlab = "hour", ylab = "views", main = "local depth, beta = 0.45")

### EXAMPLE 10
data("internet.users")
ind_1 <- which(internet.users[, 1] == 1)
DATA_1 <- internet.users[ind_1, ] # the first Internet service
ind_2  <- which(internet.users[, 1] == 2)
DATA_2 <- internet.users[ind_2, ] # the second Internet service
users_1 <- DATA_1[1:8759, 5]
# the number of unique users in the service 1
views_1 <- DATA_1[1:8759, 6]
# the number of page views in the service 1
users_2 <- DATA_2[1:8759, 5]
# the number of unique users in the service 2
views_2 <- DATA_2[1:8759, 6]
# the number of page views in the service 2

library(zoo)
window<-function(x){x}
users.m.1 <- rollapply(users_1, width=24, by=24, window, by.column=FALSE)
views.m.1 <- rollapply(views_1, width=24, by=24, window, by.column=FALSE)
users.m.2 <- rollapply(users_2, width=24, by=24, window, by.column=FALSE)
views.m.2 <- rollapply(views_2, width=24, by=24, window, by.column=FALSE)

### EXAMPLE 11

fncBoxPlot(users.m.1, bands = c(0, 0.05, 0.5, 0.95,1), method = "MBD")
fncBoxPlot(users.m.1, bands = c(0, 0.05, 0.5, 0.95,1), method = "FM")
fncBoxPlot(views.m.1, bands = c(0, 0.05, 0.5, 0.95,1), method = "MBD")
fncBoxPlot(views.m.1, bands = c(0, 0.05, 0.5, 0.95,1), method = "FM")
fncBoxPlot(users.m.2, bands = c(0, 0.05, 0.5, 0.95,1), method = "MBD")
fncBoxPlot(users.m.2, bands = c(0, 0.05, 0.5, 0.95,1), method = "FM")
fncBoxPlot(views.m.2, bands = c(0, 0.05, 0.5, 0.95,1), method = "MBD")
fncBoxPlot(views.m.2, bands = c(0, 0.05, 0.5, 0.95,1), method = "FM")

### EXAMPLE 12

ddPlot(x = users.m.1, y = users.m.2, depth_params = list(method = "Local",
                           beta = 0.45, depth_params1 = list(method = "MBD")))
ddPlot(x = views.m.1, y = users.m.2, depth_params = list(method = "Local",
                            beta = 0.45, depth_params1 = list(method = "MBD")))
ddPlot(x = views.m.1, y = users.m.2, depth_params = list(method = "Local",
                            beta = 0.25, depth_params1 = list(method = "MBD")))
par(mfrow=c(1, 1))

### EXAMPLE 13

wrapMBD = function(x) {
  depthMedian(x, depth_params = list(
    method="Local",
    beta=0.45,
    depth_params1 = list(method = "MBD")))
}

SV  <- function(n, gamma, phi, sigma, delta) {
  epsilon <- rnorm(n)
  eta <- rnorm(2 * n, 0, delta)
  h <- rnorm(1)
  for(t in 2:(2 * n)) {
    h[t] <- exp(gamma + phi * (h[t - 1] - gamma) + sigma * eta[t])
  }
  Z <- sqrt(utils::tail(h, n)) * epsilon
  return(Z)
}
example <- SV(100, 0, 0.2, 0.5, 0.1)
plot(ts(example))

### EXAMPLE 14

m.data1 <- function(n,a,b) {
  M <- matrix(nrow = n, ncol = 120)
  for(i in 1:n) M[i,]<- a*SV(120, 0, 0.3, 0.5, 0.1) + b
  M
}

m.data.out1 <- function(eps, m, n, a, b, c, d) {
  H <- rbind(m.data1(m, a, b), m.data1(n, c, d))
  ind <- sample((m+n),eps)
  H1  <- H[ind,]
  H1
}

### EXAMPLE 15

require("DepthProc")
require("RColorBrewer")
require("zoo")
m <- matrix(c(1, 0, 1, 3, 2, 3, 2, 0), nrow = 2, ncol = 4)
m[2,] <- c(2,2,3,3)
m[1,] <- c(0,1,1,0)
M2A <- m.data.out1(150, 3000, 7000, 5, 0, 1, 25)
M2B <- m.data.out1(150, 3000, 7000, 2, 0, 1, 15)
M2C <- m.data.out1(150, 3000, 7000, 3, 0, 1, 10)
matplot(t(M2A), type="l", col = topo.colors(151), xlab = "time", main = "FTS")
matplot(t(M2B), type="l", col = topo.colors(151), xlab = "time", main = "FTS ")
matplot(t(M2C), type="l", col = topo.colors(151), xlab = "time", main = "FTS")

result4A = rollapply(t(M2A),width = 15, wrapMBD, by.column = FALSE)
result4B = rollapply(t(M2B),width = 15, wrapMBD, by.column = FALSE)
result4C = rollapply(t(M2C),width = 15, wrapMBD, by.column = FALSE)
matplot(result4A,type = "l",col = topo.colors(87), xlab = "time",
                     main="15-obs mov. med.")
matplot(result4B,type = "l",col = topo.colors(87), xlab = "time",
                     main="15-obs mov. med.")
matplot(result4C,type = "l",col = topo.colors(87), xlab = "time",
                     main="15-obs mov. med.")

### EXAMPLE 16
Md1 = m.data1(100, 1, 2)
Md2 = m.data1(100, 1, 7)
mWilcoxonTest (t(Md1), t(Md2), depth_params = list(method = "MBD"))
mWilcoxonTest(t(Md1), t(Md2),
  depth_params  = list(method="Local", beta=0.25,
  depth_params1 = list(method = "MBD")))

### EXAMPLE 17

movwilcox <- function(x) {
   res <- mWilcoxonTest(x, t(ref), depth_params = list(method = "Local",
       beta = 0.25, depth_params1 = list(method = "MBD")))
   as.numeric(res[1])
}

ref <- m.data1(50,1,2)
trajectory <- function(n, m) {
   ref <- m.data1(50, 1, 2)
   # ref is a reference sample, here a 120 x 50 matrix,
   # MAA is a data frame, to which we apply a moving window.
   # The MAA has dimension 120 x (m+n).
   M_1A <- m.data1(n, 1, 2)
   M_1B <- m.data1(m, 1, 7)
   MAA <- cbind(M_1A, M_1B)
   results <- c()
   for(i in 50:150) { results[i] <- movwilcox(MAA[, i:(i + 49)]) }
   na.omit(results)
}
example <- trajectory(100,100)
plot(example)

### EXAMPLE 18

x = matrix(rnorm(200000), ncol = 5)
system.time(depth(x))

### EXAMPLE 19

system.time(depth(x, threads = 1))

### EXAMPLE 20

system.time(depth(x, threads = -10))

### EXAMPLE 21

x = matrix(rnorm(1e5), ncol = 2)
dep = depth(x)
max(dep)

### EXAMPLE 22

system.time(dx <- depthMedian(x))

### EXAMPLE 23

system.time(dm <- depthMedian(dep))
all.equal(dm, dx)

### EXAMPLE 24

x <- matrix(rnorm(1e2), ncol = 2)
y <- matrix(rnorm(1e2), ncol = 2)
ddplot <-  ddPlot(x,y)
p <- getPlot(ddplot)
# In order to modify a title
p + ggtitle("X vs Y")
scplot <- scaleCurve(x,y)
p <- getPlot(scplot)
# In order to change a color palette
p + scale_color_brewer(palette = "Set1")

### EXAMPLE 25

data("under5.mort")
data("maesles.imm")
data2011 <- cbind(under5.mort[,"2011"],maesles.imm[,"2011"])
data2000 <- cbind(under5.mort[,"2000"],maesles.imm[,"2000"])
data1995 <- cbind(under5.mort[,"1995"],maesles.imm[,"1995"])
sc2011 <- scaleCurve(data2011, name = "2011")
sc2000 <- scaleCurve(data2000, name = "2000")
# In order to create ScaleCurveList
sclist <- combineDepthCurves(sc2000,sc2011)
sclist
# In order to add another Curve
sc1995 <- scaleCurve(data1995, name = "1995")
combineDepthCurves(sclist, sc1995)

### EXAMPLE 26

n <- 200
mat_list <- replicate(n, matrix(rnorm(200), ncol = 2), simplify = FALSE)
scurves <- lapply(mat_list, scaleCurve)
scurves <- Reduce(combineDepthCurves, scurves)
p <- getPlot(scurves)
p + theme(legend.position="none") +
  scale_color_manual(values = rep("black",n))


