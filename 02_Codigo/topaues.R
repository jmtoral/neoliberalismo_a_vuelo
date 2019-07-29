
authorProdOverTime2 <- function(M,k=10, graph=TRUE){
  
  M$TC=as.numeric(M$TC)
  M$PY=as.numeric(M$PY)
  AU=names(tableTag(M,"AU"))
  k=min(k,length(AU))
  AU=AU[1:k]
  #AU=names(AU)
  df=data.frame("Author"="NA","year"=NA, "TI"="NA","SO"="NA","DOI"="NA", "TC"=NA,"TCpY"=NA,stringsAsFactors = FALSE)
  Y=as.numeric(substr(Sys.time(),1,4))
  if (!("DI" %in% names(M))){M$DI="NA"}
  for (i in 1:length(AU)){
    
    ind=which(regexpr(AU[i],M$AU)>-1)
    TCpY=M$TC[ind]/(Y-M$PY[ind]+1)
    dfAU=data.frame("Author"=rep(AU[i],length(ind)),"year"=M$PY[ind],"TI"=M$TI[ind],"SO"=M$SO[ind],"DOI"=M$DI[ind],"TC"=M$TC[ind], "TCpY"=TCpY,stringsAsFactors = TRUE)
    df=rbind(df,dfAU)
  }
  df=df[-1,]
  
  df2<-dplyr::group_by(df, .data$Author,.data$year) %>%
    dplyr::summarise(freq=length(.data$year),TC=sum(.data$TC),TCpY=sum(.data$TCpY))
  
  df2=as.data.frame(df2)
  df2$Author=factor(df2$Author,levels=AU[1:k])
  #theme_set(theme_bw())
  
  
  g <- ggplot(df2, aes(x=df2$Author, y=df2$year, text = paste("Autor: ", df2$Author,"\nAño: ",df2$year ,"\nN. of Articles: ",df2$freq ,"\nTotal Citations per Year: ", round(TCpY,2))))+
    geom_point(aes(alpha=df2$TCpY,size = df2$freq), color="dodgerblue4")+ 
    scale_size(range=c(2,6))+
    scale_alpha(range=c(0.3,1))+
    scale_y_continuous(breaks = seq(min(df2$year),max(df2$year), by=2))+
    guides(size = guide_legend(order = 1, "Número de\nartículos"), alpha = guide_legend(order = 2, "Citas por año"))+
    theme(legend.position = 'right'
          ,text = element_text(color = "#444444")
          ,panel.background = element_rect(fill = 'gray97')
          ,panel.grid.minor = element_line(color = '#FFFFFF')
          ,panel.grid.major = element_line(color = '#FFFFFF')
          ,plot.title = element_text(size = 24)
          ,axis.title = element_text(size = 14, color = '#555555')
          ,axis.title.y = element_text(vjust = 1, angle = 0, face="bold")
          ,axis.title.x = element_text(hjust = .95, face="bold")
          ,axis.text.x = element_text(face="bold")
          ,axis.text.y = element_text(face="bold")
    )+
    labs(title="Producción académica de los autores", 
         x="Autor",
         y="Año")+
    geom_line(aes(x = df2$Author, y = df2$year, group=df2$Author),size=1.0, color="firebrick", alpha=0.3 )+
    scale_x_discrete(limits = rev(levels(df2$Author)))+
    coord_flip()+
    theme_ipsum(grid="X")
  
  df$DOI=as.character(df$DOI)
  res <- list(dfAU=df2,dfPapersAU=df,graph=g)
  if (isTRUE(graph)){plot(g)}
  return(res)
}