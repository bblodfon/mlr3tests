# https://cran.r-project.org/web/packages/survival/vignettes/compete.pdf
library(survival)

# dataset
?mgus2

head(mgus2)
#' `ptime` => time until progression to a plasma cell malignancy (PCM) or last contact, in months
#' `pstatus` => occurrence of PCM: 0=no, 1=yes
#' `futime` => time until death or last contact, in months
#' `death` => occurrence of death: 0=no, 1=yes

oldpar <- par(mfrow=c(1,2))
hist(mgus2$age, nclass=30, main='', xlab="Age")
with(mgus2, tapply(age, sex, mean))
# 71.32171 69.67065

# KM per sex group ----
mfit1 <- survfit(Surv(futime, death) ~ sex, data=mgus2)
mfit1

plot(mfit1, col=c(1,2), xscale=12, mark.time=FALSE, lwd=2,
     xlab="Years post diagnosis", ylab="Survival")
legend("topright", c("female", "male"), col=1:2, lwd=2, bty='n')

# COMPETING RISKS: death vs PCM ----
#' (ptime, pstat) => time to progression
#' (futime, status) => time to death or last known alive
#' The code below creates the necessary `etime` and `event` variables

mgus2$etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
mgus2$event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))
table(mgus2$event)
levels(mgus2$event) # SOS: first level is censoring
#censor pcm death
#409 115 860

#' `event` is a factor, the first level of which MUST BE CENSORING
mfit2 <- survfit(Surv(etime, event) ~ sex, data = mgus2)

# rmean = 240 => 240/12 => 10 years
print(mfit2, rmean = 240, scale=12) # etime in months, this `/scale` makes it years
# male subject will spend, on average, 8.7 of his first 20 years post
# diagnosis in the entry state, 1.1 years in the PCM state and 10.3 of those
# 20 in the death state.

mfit2$transitions
plot(mfit2, col=c(1,2,1,2), lty=c(2,2,1,1),
     mark.time=FALSE, lwd=2, xscale=12,
     xlab="Years post diagnosis", ylab="Probability in State")
legend(240, .6, c("death:female", "death:male", "pcm:female", "pcm:male"),
      col=c(1,2,1,2), lty=c(1,1,2,2), lwd=2, bty='n')

# doing KM to estimate the expected occurrence of plasma cell malignancy (PCM) if
# all other causes of death were to be disallowed (OVERESTIMATION, WRONG!)
pcmbad <- survfit(Surv(etime, pstat) ~ sex, data=mgus2)
plot(pcmbad[2], lwd=2, fun="event", conf.int=FALSE, xscale=12,
       xlab="Years post diagnosis", ylab="Fraction with PCM")
lines(mfit2[2,"pcm"], lty=2, lwd=2, mark.time=FALSE, conf.int=FALSE)
legend(0, .25, c("Males, PCM, incorrect curve", "Males, PCM, competing risk"),
         col=1, lwd=2, lty=c(1,2), bty='n')

# mstate AJ estimation ----
## Illness-death model with no going back from illness to alive. Transitions:
## Alive => PCM => Death
## Alive => Death

## SOS => TRANSFORM the dataset so that:
## 1) One row per transition (per id)
## so multiple rows per id
## id, tstart, tstop, (from, to, trans, event in {0,1}) in general

## Deal with ties (9 subjects had PCM and death declared at the same month!)
## we assume PCM was there a little bit before as this was discovered at the time
## of death
ptemp <- with(mgus2, ifelse(ptime==futime & pstat==1, ptime-.1, ptime))
data3 <- tmerge(mgus2, mgus2, id=id, death=event(futime, death),
                pcm = event(ptemp, pstat))
data3 <- tmerge(data3, data3, id, enum=cumtdc(tstart))
with(data3, table(death, pcm))
# fix event column
temp <- with(data3, ifelse(death==1, 2, pcm))
data3$event <- factor(temp, 0:2, labels=c("censor", "pcm", "death"))
data3$event_integer = temp

head(data3) # `enum` counts rows per subject id
head(data3$event) # factor
table(data3$event_integer) # factor

# Surv is `mcounting` type
Surv(data3$tstart, data3$tstop, data3$event)
Surv(data3$tstart, data3$tstop, data3$event_integer, type = "mstate")

mfit3 <- survfit(Surv(tstart, tstop, event) ~ sex, data = data3, id = id)
mfit4 <- survfit(Surv(tstart, tstop, event_integer, type = "mstate") ~ sex, data = data3, id = id) # exactly the same

mfit3$transitions
mfit4$transitions

# mfit3$pstate

print(mfit3, rmean=240, digits=2)
print(mfit4, rmean=240, digits=2)

print(mfit3)

plot(mfit3[,"pcm"], mark.time=FALSE, col=1:2, lty=1:2, lwd=2,
     xscale=12,
     xlab="Years post MGUS diagnosis", ylab="Fraction in the PCM state")
legend(40, .4, c("female", "male"), lty=1:2, col=1:2, lwd=2, bty='n')
