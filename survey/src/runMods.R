runMod <- function(forms,
                   fam,
                   ys,
                   dats,
                   return.sum=FALSE){
    ## runs models based on a formula, family, response variable and data
    print(ys)
    if(fam =="poisson" | fam =="binomial"){
        mod <- glmer(forms,
                     family=fam,
                     data=dats,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=1e9)))
        ifelse(return.sum,
               return(summary(mod)),
               return(mod))
    }else if(fam =="nbinom"){
        mod <- glmer.nb(forms,
                        data=dats,
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=1e9),
                                             tolPwrss=1e-3))
    }else if(fam =="gaussian"){
        mod <- do.call(lmer,
                       list(formula=forms,
                            data=dats))
    }
    ifelse(return.sum,
           return(summary(mod)),
           return(mod))
}


getOutput <- function(x){
    out <- data.frame(importance(x,
                                 type=2))
    out$Variables <- row.names(out)
    out <- out[order(out$MeanDecreaseGini, decreasing = TRUE),]
    return(out)
}



checkRandomForest <- function(dev.data, val.data, rf, plot.main, y){
    quartz()
    plot(rf)
    quartz(height=11, width=8.5)
    varImpPlot(rf,
               sort = TRUE,
               main=plot.main,
               n.var=10)
    ## importance.vars <- getOutput(rf)

    dev.data$pr1 <- predict(rf,
                            dev.data)
    val.data$pr1 <- predict(rf,
                            val.data)

    if(class(dev.data[,y]) == "numeric" | class(dev.data[,y]) == "integer"){
        RMSE.forest <- sqrt(mean((val.data$pr1-val.data[,y])^2, na.rm=TRUE))
        MAE.forest <- mean(abs(val.data$pr1-val.data[,y]), na.rm=TRUE)
        return(list(RMSE.forest, MAE.forest))
    } else if(class(dev.data[,y]) == "factor"){
        c.mat.dev <- confusionMatrix(data=
                                         dev.data$pr1,
                                     reference=dev.data[,y])

        c.mat.val <- confusionMatrix(data=
                                         val.data$pr1,
                                     reference=val.data[,y])

        return(list(c.mat.dev, c.mat.val))
    }
}
