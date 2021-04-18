#working
#Dep. de f\'isica
#hidroclimatolog\'ia-2_2020
#datos medidos en tierra-estaciones en Bolivia
#ruiz.22112012@gmail.com
#-----------------#
#' CLIMATOLOGIA
#' @export
#' @param est : es el nombre de la estacion (ej: '2_Abapo.xlsx')
#' @param sheet : es un numero entero (cada hoja es una varible meteorologica)
#' @author Nihel Ruiz Atanacio
#' @description Este es un algoritmo para las estaciones de SENAMHI-BOLIVIA, el fin de este trabajo es para
#' tener un mejor manejo con los datos que monitorea las estaciones que estan en el dominio de Bolivia.
#' @details Originalmente los datos vienen en una extension .xlsx, una extension que sismet tiene almacenado los
#' datos monitoreados por las estaciones senamhi-Bolivia; el algoritmo simplifica considerablemente el proceso de transformar
#' y ordenar los datos a una extension mas frecuente por la comunidad de analistas de datos.
#'
#'  Este trabajo fue posible gracias a mis compa\~neros de climatologia de la carrera de fisica 1/2021.
#'
#'
#'
station<-function(est,sheet){
  pacman::p_load(lubridate,openxlsx)
  #old<-getwd()
  options(warn=-1)
  #setwd('2021-datos.diarios_BOLIVIA')
  ########################################
  ############ Nombre de estaci\'on con extensi\'on .xlsx
  ########################################
  #sheet<-1################################ Numero de hoja 1-->prcp,2--->Tmax,3--->Tmin,4--->Tmean,5--->Amplitud termica,....
  #10--->tensi\'on de vapor,....etc
  ########################################
  #------------------------#
  a<-read.xlsx(est,sheet=sheet,colNames=F)#
  yr<-5
  if(is.na(a[yr,1])==T)break
  year<-c();n<-0
  repeat{
    if(is.na(a[(yr+n*38),1])==T)break
    year<-append(year,substr(a[(yr+n*38),1],6,10));n=n+1
  }
  yearN<-seq(from=min(year),to=max(year),by=1)
  N<-1
  date<-c()
  pt<-c()
  i<-1
  repeat{
    if(year[i]==yearN[N]){
      y0<-ymd(paste(year[i],'01-01',sep='-'))
      y<-ymd(paste(year[i],'12-31',sep='-'))
      b<-seq(from=y0,to=y,by=1)
      a0<-30
      if(i==1){
        day0<-7
        c<-a[day0:(day0+a0),2:13]
      }else{
        day0<-day0+a0+8
        c<-a[day0:(day0+a0),2:13]
      }
      d<-c()
      e<-c()
      j=1
      k=1
      for(r in 1:366){
        d<-as.integer(append(d,substr(b[r],start=9,stop=10)))
      }
      if((as.numeric(year[i])%%4)==0){
        for(r in 1:366){
          d<-as.integer(append(d,substr(b[r],start=9,stop=10)))
        }
        for (l in 1:366){
          e<-append(e,c[j,k])
          j=j+1
          if(l+1==367) break
          if(d[l+1]<d[l]){
            k=k+1
            j=1
          }
        }
      }else{
        for(r in 1:365){
          d<-as.integer(append(d,substr(b[r],start=9,stop=10)))
        }
        for (l in 1:365){
          e<-append(e,c[j,k])
          j=j+1
          if(l+1==366) break
          if(d[l+1]<d[l]){
            k=k+1
            j=1
          }
        }
      }

      i=i+1
      N=N+1
    }else{
      y0<-ymd(paste(yearN[N],'01-01',sep='-'))
      y<-ymd(paste(yearN[N],'12-31',sep='-'))
      b<-seq(from=y0,to=y,by=1)
      e<-rep(NA,times=length(b))
      N=N+1
    }
    options(warn=-1)
    date<-append(date,b)
    pt<-append(pt,as.numeric(e))
    options(warn=-1)
    if(year[i-1]==max(year))break
  }
  df<-data.frame(date,pt)
  #setwd(old)
  write.table(df,gsub('.xlsx$','.dat',est),row.names = F)
  #print(df)
  cat('\n')
  cat('COMPLETADO EXITOSAMENTE     :)  \n')
  cat('UNIVERSIDAD MAYOR DE SAN SIMON - Facultad de Ciencias y Tecnologia \n')
  cat('Depto. Fisica-Variables Climaticas \n')
}
