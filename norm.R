
##### Elaborado por Pedro José Farias Fernandes                                         
##### Laboratório de Geografia Física - Universidade Federal Fluminense 
##### Contato: pj_fernandes@id.uff.br                                                         


#SCRIPT DE NORMALIZAÇÃO RADIOMÉTRICA A PARTIR DOS PIFs, LEMBRAR QUE AS BARRAS DO DIRETÓRIO DEVEM ESTAR ASSIM "/"

norm<-function(directory=getwd()) {

#CARREGAR OS PACOTES RASTER E RGDAL, SE NÃO ESTIVER INSTALADO, ABRIR O R, IR NO MENU PACOTES>INSTALAR PACOTES
library("raster")

#DEFINIR O DIRETORIO DAS IMAGENS E COM OS PONTOS
setwd(directory)

#EXTRAINDO AS IMAGENS DO USGS
dir.create("IMAGENS_BRUTAS")
untar(dir(directory,pattern="tar"),exdir="IMAGENS_BRUTAS")

#DEFININDO OS OBJETOS COM AS IMAGENS A SEREM NORMALIZADAS E COM OS PONTOS
imagem_a_ser_normalizada1<-raster(dir(paste(directory,"/IMAGENS_BRUTAS",sep=""),pattern="B1.TIF",full.names=T,ignore.case=T))
imagem_a_ser_normalizada2<-raster(dir(paste(directory,"/IMAGENS_BRUTAS",sep=""),pattern="B2.TIF",full.names=T,ignore.case=T))
imagem_a_ser_normalizada3<-raster(dir(paste(directory,"/IMAGENS_BRUTAS",sep=""),pattern="B3.TIF",full.names=T,ignore.case=T))
imagem_a_ser_normalizada4<-raster(dir(paste(directory,"/IMAGENS_BRUTAS",sep=""),pattern="B4.TIF",full.names=T,ignore.case=T))
imagem_a_ser_normalizada5<-raster(dir(paste(directory,"/IMAGENS_BRUTAS",sep=""),pattern="B5.TIF",full.names=T,ignore.case=T))
imagem_a_ser_normalizada7<-raster(dir(paste(directory,"/IMAGENS_BRUTAS",sep=""),pattern="B7.TIF",full.names=T,ignore.case=T))

imagem_ref1<-raster(dir(directory,pattern="B1.*?*C.*?*.tif",ignore.case=T))
imagem_ref2<-raster(dir(directory,pattern="B2.*?*C.*?*.tif",ignore.case=T))
imagem_ref3<-raster(dir(directory,pattern="B3.*?*C.*?*.tif",ignore.case=T))
imagem_ref4<-raster(dir(directory,pattern="B4.*?*C.*?*.tif",ignore.case=T))
imagem_ref5<-raster(dir(directory,pattern="B5.*?*C.*?*.tif",ignore.case=T))
imagem_ref7<-raster(dir(directory,pattern="B7.*?*C.*?*.tif",ignore.case=T))

pontos<-shapefile(dir(directory,pattern="shp",ignore.case=T)[1])

#NORMALIZAÇÃO
imagem_normalizada1 = ((lm(extract(imagem_ref1, pontos) ~ extract(imagem_a_ser_normalizada1, pontos))$coefficients[2])*imagem_a_ser_normalizada1)+((lm(extract(imagem_ref1, pontos) ~ extract(imagem_a_ser_normalizada1,pontos))$coefficients[1]))
imagem_normalizada2 = ((lm(extract(imagem_ref2, pontos) ~ extract(imagem_a_ser_normalizada2, pontos))$coefficients[2])*imagem_a_ser_normalizada2)+((lm(extract(imagem_ref2, pontos) ~ extract(imagem_a_ser_normalizada2,pontos))$coefficients[1]))
imagem_normalizada3 = ((lm(extract(imagem_ref3, pontos) ~ extract(imagem_a_ser_normalizada3, pontos))$coefficients[2])*imagem_a_ser_normalizada3)+((lm(extract(imagem_ref3, pontos) ~ extract(imagem_a_ser_normalizada3,pontos))$coefficients[1]))
imagem_normalizada4 = ((lm(extract(imagem_ref4, pontos) ~ extract(imagem_a_ser_normalizada4, pontos))$coefficients[2])*imagem_a_ser_normalizada4)+((lm(extract(imagem_ref4, pontos) ~ extract(imagem_a_ser_normalizada4,pontos))$coefficients[1]))
imagem_normalizada5 = ((lm(extract(imagem_ref5, pontos) ~ extract(imagem_a_ser_normalizada5, pontos))$coefficients[2])*imagem_a_ser_normalizada5)+((lm(extract(imagem_ref5, pontos) ~ extract(imagem_a_ser_normalizada5,pontos))$coefficients[1]))
imagem_normalizada7 = ((lm(extract(imagem_ref7, pontos) ~ extract(imagem_a_ser_normalizada7, pontos))$coefficients[2])*imagem_a_ser_normalizada7)+((lm(extract(imagem_ref7, pontos) ~ extract(imagem_a_ser_normalizada7,pontos))$coefficients[1]))

#SALVAR A IMAGEM NORMALIZADA NO FORMATO TIFF
imagem_normalizada<-stack(imagem_normalizada1,imagem_normalizada2,imagem_normalizada3,imagem_normalizada4,imagem_normalizada5,imagem_normalizada7)
writeRaster(imagem_normalizada,"imagem_normalizada",format="GTiff")

#GERANDO A TABELA COM OS VALORES DA IMAGEM DE REFERENCIA, IMAGEM A SER NORMALIZADA, IMAGEM NORMALIZADA, ERRO CALCULADO E OS COEFICIENTES DE DETERMINAÇÃO
antes1<-summary(lm(extract(imagem_ref1,pontos)~extract(imagem_a_ser_normalizada1,pontos)))
depois1<-summary(lm(extract(imagem_ref1,pontos)~extract(imagem_normalizada1,pontos)))
antes2<-summary(lm(extract(imagem_ref2,pontos)~extract(imagem_a_ser_normalizada2,pontos)))
depois2<-summary(lm(extract(imagem_ref2,pontos)~extract(imagem_normalizada2,pontos)))
antes3<-summary(lm(extract(imagem_ref3,pontos)~extract(imagem_a_ser_normalizada3,pontos)))
depois3<-summary(lm(extract(imagem_ref3,pontos)~extract(imagem_normalizada3,pontos)))
antes4<-summary(lm(extract(imagem_ref4,pontos)~extract(imagem_a_ser_normalizada4,pontos)))
depois4<-summary(lm(extract(imagem_ref4,pontos)~extract(imagem_normalizada4,pontos)))
antes5<-summary(lm(extract(imagem_ref5,pontos)~extract(imagem_a_ser_normalizada5,pontos)))
depois5<-summary(lm(extract(imagem_ref5,pontos)~extract(imagem_normalizada5,pontos)))
antes7<-summary(lm(extract(imagem_ref7,pontos)~extract(imagem_a_ser_normalizada7,pontos)))
depois7<-summary(lm(extract(imagem_ref7,pontos)~extract(imagem_normalizada7,pontos)))
dir.create("RELATORIOS")
write.table(list(IMAGEM_REF=extract(imagem_ref1,pontos),IMAGEM_A_SER_NORMALIZADA=extract(imagem_a_ser_normalizada1,pontos),IMAGEM_NORMALIZADA=extract(imagem_normalizada1,pontos),R_quadrado_antes=antes1$r.squared,R_quadrado_depois=depois1$r.squared,ERRO=sqrt (sum(((extract(imagem_ref1,pontos)-extract(imagem_normalizada1,pontos))^2))/(summary(pontos)$npoints-2))),file="RELATORIOS/RELATORIO_B1.xls",sep="\t",row.names=FALSE)
write.table(list(IMAGEM_REF=extract(imagem_ref2,pontos),IMAGEM_A_SER_NORMALIZADA=extract(imagem_a_ser_normalizada2,pontos),IMAGEM_NORMALIZADA=extract(imagem_normalizada2,pontos),R_quadrado_antes=antes2$r.squared,R_quadrado_depois=depois2$r.squared,ERRO=sqrt (sum(((extract(imagem_ref2,pontos)-extract(imagem_normalizada2,pontos))^2))/(summary(pontos)$npoints-2))),file="RELATORIOS/RELATORIO_B2.xls",sep="\t",row.names=FALSE)
write.table(list(IMAGEM_REF=extract(imagem_ref3,pontos),IMAGEM_A_SER_NORMALIZADA=extract(imagem_a_ser_normalizada3,pontos),IMAGEM_NORMALIZADA=extract(imagem_normalizada3,pontos),R_quadrado_antes=antes3$r.squared,R_quadrado_depois=depois3$r.squared,ERRO=sqrt (sum(((extract(imagem_ref3,pontos)-extract(imagem_normalizada3,pontos))^2))/(summary(pontos)$npoints-2))),file="RELATORIOS/RELATORIO_B3.xls",sep="\t",row.names=FALSE)
write.table(list(IMAGEM_REF=extract(imagem_ref4,pontos),IMAGEM_A_SER_NORMALIZADA=extract(imagem_a_ser_normalizada4,pontos),IMAGEM_NORMALIZADA=extract(imagem_normalizada4,pontos),R_quadrado_antes=antes4$r.squared,R_quadrado_depois=depois4$r.squared,ERRO=sqrt (sum(((extract(imagem_ref4,pontos)-extract(imagem_normalizada4,pontos))^2))/(summary(pontos)$npoints-2))),file="RELATORIOS/RELATORIO_B4.xls",sep="\t",row.names=FALSE)
write.table(list(IMAGEM_REF=extract(imagem_ref5,pontos),IMAGEM_A_SER_NORMALIZADA=extract(imagem_a_ser_normalizada5,pontos),IMAGEM_NORMALIZADA=extract(imagem_normalizada5,pontos),R_quadrado_antes=antes5$r.squared,R_quadrado_depois=depois5$r.squared,ERRO=sqrt (sum(((extract(imagem_ref5,pontos)-extract(imagem_normalizada5,pontos))^2))/(summary(pontos)$npoints-2))),file="RELATORIOS/RELATORIO_B5.xls",sep="\t",row.names=FALSE)
write.table(list(IMAGEM_REF=extract(imagem_ref7,pontos),IMAGEM_A_SER_NORMALIZADA=extract(imagem_a_ser_normalizada7,pontos),IMAGEM_NORMALIZADA=extract(imagem_normalizada7,pontos),R_quadrado_antes=antes7$r.squared,R_quadrado_depois=depois7$r.squared,ERRO=sqrt (sum(((extract(imagem_ref7,pontos)-extract(imagem_normalizada7,pontos))^2))/(summary(pontos)$npoints-2))),file="RELATORIOS/RELATORIO_B7.xls",sep="\t",row.names=FALSE)

#GRÁFICOS DE DISPERSÃO
bmp(file="graficos.bmp",width=1600,height=1000,res=130)
par(mfrow=c(3,4))
plot(extract(imagem_a_ser_normalizada1,pontos),extract(imagem_ref1,pontos),ylab="Imagem de referência",xlab="Imagem a ser normalizada",main="Banda 1 (antes)",pch=22,bg="black")
abline(lm(extract(imagem_ref1,pontos)~extract(imagem_a_ser_normalizada1,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(antes1$r.squared, digits=2),"\n","y=",format(antes1$coefficients[2],digits=2),"x+",format(antes1$coefficients[1],digits=2)))
plot(extract(imagem_normalizada1,pontos),extract(imagem_ref1,pontos),ylab="Imagem de referência",xlab="Imagem normalizada",main="Banda 1 (depois)",pch=22,bg="black")
abline(lm(extract(imagem_ref1,pontos)~extract(imagem_normalizada1,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(depois1$r.squared, digits=2),"\n","y=",format(depois1$coefficients[2],digits=2),"x+",format(depois1$coefficients[1],digits=2)))
plot(extract(imagem_a_ser_normalizada2,pontos),extract(imagem_ref2,pontos),ylab="Imagem de referência",xlab="Imagem a ser normalizada",main="Banda 2 (antes)",pch=22,bg="black")
abline(lm(extract(imagem_ref2,pontos)~extract(imagem_a_ser_normalizada2,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(antes2$r.squared, digits=2),"\n","y=",format(antes2$coefficients[2],digits=2),"x+",format(antes2$coefficients[1],digits=2)))
plot(extract(imagem_normalizada2,pontos),extract(imagem_ref2,pontos),ylab="Imagem de referência",xlab="Imagem normalizada",main="Banda 2 (depois)",pch=22,bg="black")
abline(lm(extract(imagem_ref2,pontos)~extract(imagem_normalizada2,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(depois2$r.squared, digits=2),"\n","y=",format(depois2$coefficients[2],digits=2),"x+",format(depois2$coefficients[1],digits=2)))
plot(extract(imagem_a_ser_normalizada3,pontos),extract(imagem_ref3,pontos),ylab="Imagem de referência",xlab="Imagem a ser normalizada",main="Banda 3 (antes)",pch=22,bg="black")
abline(lm(extract(imagem_ref3,pontos)~extract(imagem_a_ser_normalizada3,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(antes3$r.squared, digits=2),"\n","y=",format(antes3$coefficients[2],digits=2),"x+",format(antes3$coefficients[1],digits=2)))
plot(extract(imagem_normalizada3,pontos),extract(imagem_ref3,pontos),ylab="Imagem de referência",xlab="Imagem normalizada",main="Banda 3 (depois)",pch=22,bg="black")
abline(lm(extract(imagem_ref3,pontos)~extract(imagem_normalizada3,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(depois3$r.squared, digits=2),"\n","y=",format(depois3$coefficients[2],digits=2),"x+",format(depois3$coefficients[1],digits=2)))
plot(extract(imagem_a_ser_normalizada4,pontos),extract(imagem_ref4,pontos),ylab="Imagem de referência",xlab="Imagem a ser normalizada",main="Banda 4 (antes)",pch=22,bg="black")
abline(lm(extract(imagem_ref4,pontos)~extract(imagem_a_ser_normalizada4,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(antes4$r.squared, digits=2),"\n","y=",format(antes4$coefficients[2],digits=2),"x+",format(antes4$coefficients[1],digits=2)))
plot(extract(imagem_normalizada4,pontos),extract(imagem_ref4,pontos),ylab="Imagem de referência",xlab="Imagem normalizada",main="Banda 4 (depois)",pch=22,bg="black")
abline(lm(extract(imagem_ref4,pontos)~extract(imagem_normalizada4,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(depois4$r.squared, digits=2),"\n","y=",format(depois4$coefficients[2],digits=2),"x+",format(depois4$coefficients[1],digits=2)))
plot(extract(imagem_a_ser_normalizada5,pontos),extract(imagem_ref5,pontos),ylab="Imagem de referência",xlab="Imagem a ser normalizada",main="Banda 5 (antes)",pch=22,bg="black")
abline(lm(extract(imagem_ref5,pontos)~extract(imagem_a_ser_normalizada5,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(antes5$r.squared, digits=2),"\n","y=",format(antes5$coefficients[2],digits=2),"x+",format(antes5$coefficients[1],digits=2)))
plot(extract(imagem_normalizada5,pontos),extract(imagem_ref5,pontos),ylab="Imagem de referência",xlab="Imagem normalizada",main="Banda 5 (depois)",pch=22,bg="black")
abline(lm(extract(imagem_ref5,pontos)~extract(imagem_normalizada5,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(depois5$r.squared, digits=2),"\n","y=",format(depois5$coefficients[2],digits=2),"x+",format(depois5$coefficients[1],digits=2)))
plot(extract(imagem_a_ser_normalizada7,pontos),extract(imagem_ref7,pontos),ylab="Imagem de referência",xlab="Imagem a ser normalizada",main="Banda 7 (antes)",pch=22,bg="black")
abline(lm(extract(imagem_ref7,pontos)~extract(imagem_a_ser_normalizada7,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(antes7$r.squared, digits=2),"\n","y=",format(antes7$coefficients[2],digits=2),"x+",format(antes7$coefficients[1],digits=2)))
plot(extract(imagem_normalizada7,pontos),extract(imagem_ref7,pontos),ylab="Imagem de referência",xlab="Imagem normalizada",main="Banda 7 (depois)",pch=22,bg="black")
abline(lm(extract(imagem_ref7,pontos)~extract(imagem_normalizada7,pontos)),col="red")
legend("topleft", bty="n", legend=paste("r² =", format(depois7$r.squared, digits=2),"\n","y=",format(depois7$coefficients[2],digits=2),"x+",format(depois7$coefficients[1],digits=2)))
dev.off()

print("Normalização radiométrica finalizada. Confira o relatório e os gráficos gerados.")

}
