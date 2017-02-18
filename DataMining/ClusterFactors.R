library(data.table)
library(mclust)
library(psych)

box = fread("box2015.csv")
height_weight = fread("height-weight.csv")
setnames(height_weight, c("PLAYER","HEIGHT","WEIGHT"))
### Pre-process fucntion
pre_process_NBA = function(x){
        ###Remove if game played = 0
        box_test = subset(x, GAME_PLAYED != 0)
        
        ### Creating FGM, FGA, TPA, TPM, FTA, FTM, GP (Should we only use players that plpayed more than 10 games?)
        temp = box_test[, ':=' (FGM=sum(FIELD_GOALS_MADE), FGA=sum(FIELD_GOALS_ATT), TPA=sum(THREE_POINT_ATT),
                              TPM = sum(THREE_POINT_MADE), FTA=sum(FREE_THROWS_ATT), FTM=sum(FREE_THROWS_MADE)
                              ,GP=sum(GAME_PLAYED)), by=PLAYER]
        temp = box_test[, ':=' (TWPA = FIELD_GOALS_ATT - THREE_POINT_ATT, TWPM = FIELD_GOALS_MADE - THREE_POINT_MADE)]
        temp = temp[, c("PLAYER", "FGM","FGA","TPA","TPM","FTA","FTM","GP"), with = FALSE]
        
        temp = unique(temp)
        
        temp$eFG = ((((temp$FGM) + (0.5 * temp$TPM))/(temp$FGA)))
        
        temp$FTP = (temp$FTM/temp$FTA)
        
        #### Noted, need to fix rows that have Nan
        #table(is.nan(temp$FTP))  ### 14 players
        #table(is.nan(temp$eFG))  ### 1 payer
        
        
        
        #### REBOUNDING PERCENTAGE
        
        test <- box_test[, ':=' (DREB = sum(DEFENSIVE_REBOUNDS), OREB = sum(OFFENSIVE_REBOUNDS)), by = GAME_CODE]
        test$DREBPCT <- test$DEFENSIVE_REBOUNDS / test$DREB
        test$OREBPCT <- test$OFFENSIVE_REBOUNDS / test$OREB
        
        
        test_avg <- test[, lapply(.SD,mean, na.rm = TRUE), by = PLAYER, .SDcols = c('DREBPCT', 'OREBPCT')]
        
        
        ### Per 36 stats
        
        box_test = box_test[, ':=' (TWPA = FIELD_GOALS_ATT - THREE_POINT_ATT, TWPM = FIELD_GOALS_MADE - THREE_POINT_MADE)]
        
        box_test$PPM_36 = 36 * ((box_test$TOTAL_POINTS)/box_test$MINUTES_PLAYED)
        
        box_test$APM_36 = 36 * ((box_test$ASSISTS)/box_test$MINUTES_PLAYED)
        
        box_test$BPM_36 = 36 * ((box_test$BLOCKED_SHOTS)/box_test$MINUTES_PLAYED)
        
        box_test$SPM_36 = 36 * ((box_test$STEALS)/box_test$MINUTES_PLAYED)
        
        box_test$TPM_36 = 36 * ((box_test$TURNOVERS)/box_test$MINUTES_PLAYED)
        
        box_test$FPM_36 = 36 * ((box_test$PERSONAL_FOULS)/box_test$MINUTES_PLAYED)
        
        box_test$THPM_36 = 36 * ((box_test$THREE_POINT_MADE)/box_test$MINUTES_PLAYED)
        
        box_test$THPA_36 = 36 * ((box_test$THREE_POINT_ATT)/box_test$MINUTES_PLAYED)
        
        box_test$TWPM_36 = 36 * ((box_test$TWPM)/box_test$MINUTES_PLAYED)
        
        box_test$TWPA_36 = 36 * ((box_test$TWPA)/box_test$MINUTES_PLAYED)
        
        box_test$FTA_36 = 36 * ((box_test$FREE_THROWS_ATT)/box_test$MINUTES_PLAYED)
        
        box_test$OR_36 = 36 * ((box_test$OFFENSIVE_REBOUNDS)/box_test$MINUTES_PLAYED)
        
        box_test$DR_36 = 36 * ((box_test$DEFENSIVE_REBOUNDS)/box_test$MINUTES_PLAYED)
        
        
        ### Average per 36 stats per player 
        box_test_avg = box_test[, lapply(.SD, mean, na.rm = TRUE), by = PLAYER, .SDcols = c('PPM_36',
                                                                              'APM_36', 'BPM_36','SPM_36','TPM_36',
                                                                              'FPM_36','THPM_36','THPA_36',
                                                                              'TWPM_36','TWPA_36','FTA_36',
                                                                              'OR_36','DR_36')]
        ### Combining per 36 stats, eFG, FTP, and Rebounding percentages
        box_test_final = cbind(box_test_avg, temp$eFG, temp$FTP, test_avg$DREBPCT, test_avg$OREBPCT, temp$GP)
        setnames(box_test_final, "V2", "eFG")
        setnames(box_test_final, "V3", "FTP")
        setnames(box_test_final, "V4", "DREBPCT")
        setnames(box_test_final, "V5", "OREBPCT")
        setnames(box_test_final, "V6", "GP")
        setkey(box_test_final, PLAYER)
        setkey(height_weight, PLAYER)
        box_test_final = merge(box_test_final,height_weight)
        box_test_final = subset(box_test_final, GP > 10)
        box_test_final[which(box_test_final$PLAYER == 'Mike Miller'),]$FTP <- .769
        box_test_final = box_test_final[complete.cases(box_test_final),]
        box_test_final = box_test_final[, GP := NULL]
        return(box_test_final)
}
### Preparing data
nba_test_data = pre_process_NBA(box)
nba_data_PCA = pre_process_NBA(box)
nba_data_K = pre_process_NBA(box)
nba_data_Gaus = pre_process_NBA(box)

### PCA
PCA_test = data.table(scale(nba_data_PCA[,-1]))
PCA_test = PCA_test[,c("HEIGHT","WEIGHT") := NULL,]
fit1 = principal(PCA_test, rotate = "none")

fit1$values

fit2 = principal(PCA_test, nfactors = 4)
fit2$loadings

#### Can only the following factors:

#### Scorer PPM, TWPM_36, TWPA_36, FTA_36

#### REBOUNDER DREBPCT, OREBPCT

#### PG Score APM, SPM,TPM,


summary.kmeans = function(fit)
{
        p = ncol(fit$centers)
        k = nrow(fit$centers)
        n = sum(fit$size)
        sse = sum(fit$withinss)
        xbar = t(fit$centers)%*%fit$size/n
        ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2)
        print(data.frame(
                n=c(fit$size, n),
                Pct=(round(c(fit$size, n)/n,2)),
                round(rbind(fit$centers, t(xbar)), 2),
                RMSE = round(sqrt(c(fit$withinss/(p*fit$size-1), sse/(p*(n-k)))), 4)
        ))
        cat("SSE = ", sse, "; SSB = ", ssb, "\n")
        cat("R-Squared = ", ssb/(ssb+sse), "\n")
        cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
        invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}

plot.kmeans = function(fit,boxplot=F)
{
        require(lattice)
        p = ncol(fit$centers)
        k = nrow(fit$centers)
        plotdat = data.frame(
                mu=as.vector(fit$centers),
                clus=factor(rep(1:k, p)),
                var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
        )
        print(dotplot(var~mu|clus, data=plotdat,
                      panel=function(...){
                              panel.dotplot(...)
                              panel.abline(v=0, lwd=.1)
                      },
                      layout=c(k,1),
                      xlab="Cluster Mean"
        ))
        invisible(plotdat)
}

## Kmeans

### Creating the factors

nba_data_K$scorer = nba_data_K$PPM_36 + nba_data_K$TWPM_36 + nba_data_K$TWPA_36 + nba_data_K$FTA_36
nba_data_K$rebounder = nba_data_K$DREBPCT + nba_data_K$OREBPCT
nba_data_K$pg = nba_data_K$APM_36 + nba_data_K$TPM_36 + nba_data_K$SPM_36

### Removiing Factors

nba_data_K = nba_data_K[, c("PPM_36","TWPM_36","TWPA_36","FTA_36","DREBPCT","OREBPCT","APM_36","TPM_36","SPM_36") := NULL]

### Standardize

nba_data_K[,2:14] = data.table(scale(nba_data_K[,2:14]))

## SSE check clust

SSE = double(30)
for(i in 2:31){
        SSE[i-1] = kmeans(nba_data_K[,-1], i, iter.max = 1000, nstart = 100)$tot.withinss
}

plot(2:31,SSE, type= "b")

## 6 - 10 clusters based on SSE
cluster6 = kmeans(nba_data_K[,-1], 6, iter.max = 1000, nstart = 100)

###Checking who is group 4
nba_test_data$clust6 = cluster6$cluster
clust6 = subset(nba_test_data, clust6 == 4)


plot(cluster6)

cluster8 = kmeans(nba_data_K[,-1], 8, iter.max = 1000, nstart = 100)

plot(cluster8)



### Gaussian mixtures since data sets are small

bic = double(15)
for(i in 2:16){
        bic[i-1] = Mclust(nba_data_K[,-1], G = i)$bic
}

plot(2:16,bic)

### Either 6 to 8 with BIC
gauss1 = Mclust(test_nba, G = 6)
gauss2 = Mclust(test_nba, G = 8)

summary(gauss1)

gauss1$parameters$mean


gauss2$parameters$mean

