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
