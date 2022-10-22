library("glmmTMB")
library("bbmle") ## for AICtab
library("ggplot2")
## cosmetic
theme_set(theme_bw()+
            theme(panel.spacing=grid::unit(0,"lines")))


Owls <- transform(Owls,
                  Nest=reorder(Nest,NegPerChick),
                  NCalls=SiblingNegotiation,
                  FT=FoodTreatment)
class(Owls)
dim(Owls)
head(Owls)
help("Owls")
summary(Owls)


fit_zipoisson <- glmmTMB(NCalls~(FT+ArrivalTime)*SexParent+
                           offset(log(BroodSize))+(1|Nest),
                         data=Owls,
                         ziformula=~1,
                         family=poisson)
summary(fit_zipoisson)
