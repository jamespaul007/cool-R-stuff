library(data.table)
set.seed(45L)
DT <- data.table(V1=c(1L,2L),
                 V2=LETTERS[1:3],
                 V3=round(rnorm(4),4),
                 V4=1:12)
DT
DT[3:5,] #or DT[3:5]
DT[V2 =='A']
DT[V2 %in% c('A','C')]
DT[,V2]
DT[, .(V2,V3)]
DT[,sum(V1)]
DT[,.(sum(V1),sd(V3))] 
DT[,.(Aggregate = sum(V1),
      Sd.V3 = sd(V3))]
DT[,.(V1, Sd.V3 = sd(V3))] 
DT[,{print(V2)
     plot(V3)
     NULL}]
DT[,.(V4.Sum = sum(V4)),by=V1] 
DT[,.(V4.Sum = sum(V4)),by=.(V1,V2)] 
DT[,.(V4.Sum = sum(V4)),by=sign(V1-1)] 
DT[,.(V4.Sum = sum(V4)),
   by=.(V1.01 = sign(V1-1))]
DT[1:5,.(V4.Sum = sum(V4)),by=V1]
DT[,.N,by=V1] 
DT[, V1 := round(exp(V1),2)]
DT[, c("V1","V2") := list
   (round(exp(V1),2), LETTERS
    [4:6])]
DT
DT[, ':=' (V1 =
             round(exp(V1),2),
           V2 = LETTERS[4:6])][]
setkey(DT,V2)
DT["A"] 
DT[c("A","C")]
DT["A", mult ="first"]
DT["A", mult = "last"]
DT[c("A","D")] 
DT[c("A","D"), nomatch= 0]
DT[c("A","C"),
   sum(V4)]

DT[c("A","C"),
   sum(V4), by=.EACHI]
setkey(DT,V1,V2) 
DT[.(2,"C")] 
DT[.(2, c("A","C"))]

DT[,sum(V1)] 
DT[,.(sum(V1),sd(V3))] 
DT[,.(Aggregate = sum(V1),
      Sd.V3 = sd(V3))]
DT[,.(V1, Sd.V3 = sd(V3))] 
DT[,{print(V2)
     plot(V3)
     NULL}]

DT[.N-1]
DT[,.N]
DT[,.(V2,V3)] #or
DT[,list(V2,V3)] 
DT[, (meanV3 = mean(V3)),
   by=.(V1,V2)]

DT[, print(.SD), by=V2] 
DT[,.SD[c(1,.N)], by=V2]
DT[, lapply(.SD, sum), by=V2]

DT[, lapply(.SD,sum), by=V2,
   .SDcols = c("V3","V4")]
DT[, lapply(.SD,sum), by=V2,
   .SDcols = paste0("V",3:4)]
DT<-DT[, .(V4.Sum = sum(V4)),by=V1]
DT
DT[V4.Sum > 40]
DT[, .(V4.Sum = sum(V4)),
   by=V1][V4.Sum > 40 ]
DT[, .(V4.Sum = sum(V4)),
   by=V1][order(-V1)]

#set(): for (i in from:to) set(DT, row, column, new value). 
rows = list(3:4,5:6)
cols = 1:2
for (i in seq_along(rows))
{ set(DT,
      i=rows[[i]],
      j = cols[i],
      value = NA) }
setnames(DT,"V2","Rating")

