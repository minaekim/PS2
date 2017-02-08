# PS2
MBA 2017 - Minae Kim 

# 1) Calculating violations
x <- sample(1:1000000,100)
violations <- function(x, m=TRUE,d=TRUE)
{ Prop <- table(as.integer(substring(as.character(x),1,1)))/length(x)
if(m==TRUE & d==TRUE){
    m <- max(Prop-log10(1+(1/c(1:9))))
    d <- sqrt(sum(Prop-log10(1+(1/c(1:9)))^2))
    return(list("M Statistic"=m,"D Statistic"=d, "Dirstribution"=Prop))
}}

  
# 2) Critical Values
x <- sample(1:1000000,100)
print.benfords <- function(){
    m<-violations(x)[[1]]
    d<-violations(x)[[2]]
    
    if(m >=1.212 & d >=1.569){return(list("M Statistic"=m,"D Statistic"=d,
                                          as.character(Significant0.01)))}
    else if((m >=0.967& m< 1.212) & (d >=1.330 & m<1.212))
            {return(list("M Statistic"=m,"D Statistic"=d,
                         as.character(Significant0.05)))}
    else if((m >=0.851&m<0.967) & (d >=1.212&d<1.330))
            {return(list("M Statistic"=m,"D Statistic"=d,
                         as.character(Significant0.1)))}
            else if (m< 0.851 & d< 1.212)
            {return(list("M Statistic"=m,"D Statistic"=d,
                         as.character("cannot reject null hypothesis")))}
    }
print.benfords()
