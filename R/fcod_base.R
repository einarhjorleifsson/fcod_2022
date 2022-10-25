# run as: nohup R < R/fcod_run.R --vanilla > NULL &
library(tidyverse)
library(stockassessment)
cn <- read.ices("data/cn2.dat")
cw <- lw <- dw <- read.ices("data/cw2.dat")
lf <- read.ices("data/lf2.dat")
mo <- read.ices("data/mo2.dat")
nm <- read.ices("data/nm2.dat")
pf <- read.ices("data/pf2.dat")
pm <- read.ices("data/pm2.dat")
sw <- read.ices("data/sw2.dat")
surveys <- read.ices("data/survey_age2plus.dat")

dat <- setup.sam.data(surveys=surveys,
                      residual.fleet=cn,
                      prop.mature=mo,
                      stock.mean.weight=sw,
                      catch.mean.weight=cw,
                      dis.mean.weight=dw,
                      land.mean.weight=lw,
                      prop.f=pf,
                      prop.m=pm,
                      natural.mortality=nm,
                      land.frac=lf)
conf <- defcon(dat)
conf$fbarRange <- c(3, 7)
par <- defpar(dat, conf)

# note: get a Warning message:
#  In sqrt(diag(object$cov.fixed)) : NaNs produced
fit <- sam.fit(dat, conf, par)
fit |> write_rds("run/fit.rds")

retro.years <- 28
retro <- retro(fit, year = retro.years, ncores = retro.years)
retro |> write_rds("run/retro.rds")
