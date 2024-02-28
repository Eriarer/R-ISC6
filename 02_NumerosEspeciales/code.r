#Pseudoclase dado
dado<-function(caras,nombre){
    d<-list("caras"=caras,"nom"=nombre)
    class(d)<-"dado"
    return(d)
}

print.dado<-function(d){
    cat(d$nom,"\n")
    nc<-length(d$caras)
    cat("╔",rep("═╦",nc-1),"═╗\n",sep="")
    for(i in 1:nc) cat("║",d$caras[i],sep="")
    cat("║\n")
    cat("╚",rep("═╩",nc-1),"═╝\n",sep="")
}

tirar<-function(d,n=1){
    s<-sample(d$caras,size=n,replace=TRUE)
    t<-sum(s)
    r<-list("s"=s,"t"=t,"nom"=d$nom)
    return(r)
}

gana<-function(s1,s2){
    res<-ifelse(s1$t<s2$t,s2$nom,
	ifelse(s1$t>s2$t,s1$nom,"white"))
    return(res)
}

ciclos<-10000

#--------------------
#Comienza simulación
#--------------------

# Dados normales
dado1<-dado(1:6,"red")
dado2<-dado(1:6,"blue")
dado3<-dado(1:6,"green")

r1vs2<-character()
r1vs3<-character()
r2vs3<-character()

for(i in 1:ciclos){
    d1<-tirar(dado1)
    d2<-tirar(dado2)
    d3<-tirar(dado3)
    r1vs2<-c(r1vs2,gana(d1,d2))
    r1vs3<-c(r1vs3,gana(d1,d3))
    r2vs3<-c(r2vs3,gana(d2,d3))
}

t1vs2<-table(r1vs2)
t1vs3<-table(r1vs3)
t2vs3<-table(r2vs3)

pdf("dadosNormales.pdf")
par(mfrow=c(1,3))
barplot(t1vs2,main="Red vs Blue",col=names(t1vs2),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs2))
barplot(t1vs3,main="Red vs Green",col=names(t1vs3),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs3))
barplot(t2vs3,main="Blue vs Green",col=names(t2vs3),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t2vs3))
dev.off()

# Dados no transitivos
dado1<-dado(c(2,2,4,4,9,9),"red")
dado2<-dado(c(1,1,6,6,8,8),"blue")
dado3<-dado(c(3,3,5,5,7,7),"green")

r1vs2<-character()
r1vs3<-character()
r2vs3<-character()

for(i in 1:ciclos){
    d1<-tirar(dado1)
    d2<-tirar(dado2)
    d3<-tirar(dado3)
    r1vs2<-c(r1vs2,gana(d1,d2))
    r1vs3<-c(r1vs3,gana(d1,d3))
    r2vs3<-c(r2vs3,gana(d2,d3))
}

t1vs2<-table(r1vs2)
t1vs3<-table(r1vs3)
t2vs3<-table(r2vs3)

pdf("dadosNoTransitivos.pdf")
par(mfrow=c(1,3))
barplot(t1vs2,main="Red vs Blue",col=names(t1vs2),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs2))
barplot(t1vs3,main="Red vs Green",col=names(t1vs3),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs3))
barplot(t2vs3,main="Blue vs Green",col=names(t2vs3),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t2vs3))
dev.off()


dado1<-dado(c(1,1,5,5,9,9),"red")
dado2<-dado(c(2,2,6,6,7,7),"blue")
dado3<-dado(c(3,3,4,4,8,8),"green")

# Imprime el dado
#print(dado1)

d1<-tirar(dado1,2)
d2<-tirar(dado2,2)
d3<-tirar(dado3,2)


# Ver que dado es mayor
r1vs2<-character()
r1vs3<-character()
r2vs3<-character()

for(i in 1:ciclos){
    d1<-tirar(dado1)
    d2<-tirar(dado2)
    d3<-tirar(dado3)
    r1vs2<-c(r1vs2,gana(d1,d2))
    r1vs3<-c(r1vs3,gana(d1,d3))
    r2vs3<-c(r2vs3,gana(d2,d3))
}

#ver estructura de tiradas
#str(table$r1vs2)
#str(table$r1vs3)
#str(table$r2vs3)

t1vs2<-table(r1vs2)
t1vs3<-table(r1vs3)
t2vs3<-table(r2vs3)

# Preparar la pantalla para guardar 3 gráficos, lo que salga a la pantalla guardarlo en un archivo pdf
pdf("dadosNoTransitivos_2Tiradas.pdf")
par(mfrow=c(1,3))
#crear un barplot el cual tenga de color los nombres, un titulo deacuerdo a la tabla
# y nombres en los ejes x y y de acuerdo a la tabla
# en el tope de las barras se debe mostrar la frecuencia
barplot(t1vs2,main="Red vs Blue",col=names(t1vs2),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs2))
barplot(t1vs3,main="Red vs Green",col=names(t1vs3),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs3))
barplot(t2vs3,main="Blue vs Green",col=names(t2vs3),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t2vs3))
#cerrar el archivo pdf
dev.off()

# Dados de efron
dado1<-dado(c(4,4,4,4,0,0),"red")
dados2<-dado(c(3,3,3,3,3,3),"blue")
dado3<-dado(c(6,6,6,2,2,2),"green")
dado4<-dado(c(5,5,5,5,1,1),"yellow")

r1vs2<-character()
r1vs3<-character()
r1vs4<-character()
r2vs3<-character()
r2vs4<-character()
r3vs4<-character()

for(i in 1:ciclos){
    d1<-tirar(dado1)
    d2<-tirar(dado2)
    d3<-tirar(dado3)
    d4<-tirar(dado4)
    r1vs2<-c(r1vs2,gana(d1,d2))
    r1vs3<-c(r1vs3,gana(d1,d3))
    r1vs4<-c(r1vs4,gana(d1,d4))
    r2vs3<-c(r2vs3,gana(d2,d3))
    r2vs4<-c(r2vs4,gana(d2,d4))
    r3vs4<-c(r3vs4,gana(d3,d4))
}

t1vs2<-table(r1vs2)
t1vs3<-table(r1vs3)
t1vs4<-table(r1vs4)
t2vs3<-table(r2vs3)
t2vs4<-table(r2vs4)
t3vs4<-table(r3vs4)

pdf("dadosNoTransitivosEfron.pdf")
par(mfrow=c(2,3))
barplot(t1vs2,main="Red vs Blue",col=names(t1vs2),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs2))
barplot(t1vs3,main="Red vs Green",col=names(t1vs3),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs3))
barplot(t1vs4,main="Red vs Yellow",col=names(t1vs4),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t1vs4))
barplot(t2vs3,main="Blue vs Green",col=names(t2vs3),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t2vs3))
barplot(t2vs4,main="Blue vs Yellow",col=names(t2vs4),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t2vs4))
barplot(t3vs4,main="Green vs Yellow",col=names(t3vs4),ylab="Frecuencia",xlab="Ganador p/ tirada",
        names.arg=names(t3vs4))
dev.off()

# -----
