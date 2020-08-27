z_J_par <- 0.05 #1/day
z_par <- 0.03 #1/day
alp_par <- 25.0 #days, alpha
b_pk_par <- 10.0 #individuals/(individual * day)
kap_par <- 0.1 #1/day, kappa, K

#arbitrary values
k_par <- 8.617*1e-5
E_par <- 0.55
E_D_par <- 4
T_pk_par <- 25.3+273.15
T_ref_par <- 10+273.15
B_0_par <- 0.5


B_0_alp =  1/alp_par 
T_pk_alp = 25.3
E_alp = 0.55
E_D_alp = 3

B_0_bpk = b_pk_par #individuals/(individual * day)
T_pk_bpk = 25.3
E_bpk = 0.55
E_D_bpk = 3

B_0_z = 1/z_par  #1/day; inverse because we are using inverse of the S-S TPC model
T_pk_z = 25.3
E_z = 0.55
E_D_z = 3

B_0_zJ = 1/z_J_par  #1/day; inverse because we are using inverse of the S-S TPC model
T_pk_zJ = 25.3
E_zJ = 0.55
E_D_zJ = 3

B_0_kap = kap_par #1/day
T_pk_kap = 25.3
E_kap = 0.55
E_D_kap = 3

z_J_par <- 0.05 #1/day
z_par <- 0.03 #1/day
alp_par <- 25.0 #days, alpha
b_pk_par <- 10.0 #individuals/(individual * day)
kap_par <- 0.1 #1/day, kappa, K

r_SP_app <- function(alp,b_pk,k,z,z_J)  -(1/(alp*k+alp*z+1))*(alp*k*z_J+alp*z*z_J+
                                                                log10((k+z)^(k+z)/b_pk^(k+z)))


B <- function(B_0,E,E_D,T,T_ref,T_pk,k)  B_0 * exp(-E * ((1/(k*(T))) - (1/(k*T_ref)))) /
  (1 + (E / (E_D - E)) * exp((E_D / k)* ((1/T_pk) - (1/T))))


B_inv <- function(B_0,E,E_D,T,T_ref,T_pk,k) 1/ B(B_0,E,E_D,T,T_ref,T_pk,k)

## Vectors setting

T_vec <-  273.15+seq(0,40,0.05)

alp_vec <- 40.8-2.65 *(T_vec-273.15)+0.0476*(T_vec-273.15)^2

bpk_vec <-  -35.5 + 4.05*(T_vec-273.15)-0.0894* (T_vec-273.15)^2

z_vec <- 0.138- 0.0093*(T_vec-273.15)+0.000277*(T_vec-273.15)^2


zJ_vec <- 5.3+ 1.23*(T_vec-273.15)-0.0358*(T_vec-273.15)^2

kap_vec <- B(B_0_kap,E_kap,E_D_kap,T_vec,T_ref_par,T_pk_kap+273.15,k_par)







par(mfrow=c(2,3))  
plot(T_vec-273.15,alp_vec,type = "l",col="blue",ylab = expression(alpha),xlab = "Temperature(C)")
plot(T_vec-273.15,bpk_vec,type = "l",col="red",ylab = expression(b[pk]),xlab = "Temperature(C)")
plot(T_vec-273.15,z_vec,type = "l",col="cyan",ylab = expression(z),xlab = "Temperature(C)")
plot(T_vec-273.15,zJ_vec,type = "l",col="orange",ylab = expression(z[J]),xlab = "Temperature(C)")
plot(T_vec-273.15,kap_vec,type = "l",col="purple",ylab = expression(k),xlab = "Temperature(C)")
dev.off()

r_m_vec <- r_SP_app(alp_vec,bpk_vec,kap_vec,z_vec,zJ_vec)

plot(T_vec-273.15,r_m_vec,type = "l",col="blue",lwd=1.5,ylab = expression(r[m]),xlab = "Temperature(C)")



nicheW <- function(r_m,Temp){
  dataframe<- data.frame(Temperature=Temp,Rate=r_m)
  values <- subset(dataframe,dataframe$Rate>0)
  width<-max(values$Temperature)-min(values$Temperature)
  print(width)
}

#Vectors
alp_vec<- #Values
  T_pk_par <- 25.3+273.15
E_par <- 0.65
ED_par <- 3
k_par <- 8.617*1e-5
T_ref_par <- 10+273.15

#B0
B0_alp<- 1/alp_par 
B0_bpk<-b_pk_par 
B0_z<-1/z_par 
B0_zJ <-1/z_J_par 
B0_kap <- kap_par


T_vec <- 273.15+seq(0,50,0.05)

response_mm <- (exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/T_vec))))


T_vec <- 273.15+seq(0,50,0.05)

response_mm <- (exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/T_vec))))

#B0
B0_alp<- 1/alp_par 
B0_bpk<-b_pk_par 
B0_z<-1/z_par 
B0_zJ <-1/z_J_par 
B0_kap <- kap_par

#Vectors
alp_vec<- 40.8-2.65 *(T_vec-273.15)+0.0476*(T_vec-273.15)^2
bpk_vec<- -35.5 + 4.05*(T_vec-273.15)-0.0894* (T_vec-273.15)^2
z_vec<- 0.138- 0.0093*(T_vec-273.15)+0.000277*(T_vec-273.15)^2
zJ_vec <- 5.3+ 1.23*(T_vec-273.15)-0.0358*(T_vec-273.15)^2
kap_vec <- response_mm*B0_kap



r_m_vector <- ((kap_vec+z_vec)*(log10(bpk_vec/(kap_vec+z_vec))-(alp_vec*zJ_vec)))/((alp_vec*(kap_vec+z_vec))+1)

plot(T_vec-273.15,r_m_vector,type = "l",col="blue",ylab = expression(r[m]),
     xlab = "Temperature(C)",ylim = c(-3,1))
NicheW <-c()


T_pk_mm <- T_pk_par +  seq(-10,10.5,0.5)
T_pk_par <- 25.3+273.15
for(i in 1:(length(T_pk_mm)/2)){
  a_vec_mm <-  40.8-2.65 *(T_vec-273.15)+0.0476*(T_vec-273.15)^2
  
  zJ_vec_mm <- 5.3+ 1.23*(T_vec-273.15)-0.0358*(T_vec-273.15)^2
  
  #these are the same
  bpk_vec_mm <- -35.5 + 4.05*(T_vec-273.15)-0.0894* (T_vec-273.15)^2
  kap_vec_mm <- B0_kap*((exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/T_vec)))))
  z_vec_mm <- 0.138- 0.0093*(T_vec-273.15)+0.000277*(T_vec-273.15)^2
  
  rm_vec_mm<-((kap_vec_mm+z_vec_mm)*(log10(bpk_vec_mm/(kap_vec_mm+z_vec_mm))-(a_vec_mm*zJ_vec_mm)))/((a_vec_mm*(kap_vec_mm+z_vec_mm))+1) 
  
  lines(T_vec-273.15,rm_vec_mm,col="royalblue1")
  NicheW[[i]]<- nicheW(rm_vec_mm,T_vec) 
  
}

for(i in (length(T_pk_mm)/2):length(T_pk_mm)){
  a_vec_mm <-  40.8-2.65 *(T_vec-273.15)+0.0476*(T_vec-273.15)^2
  
  zJ_vec_mm <- 5.3+ 1.23*(T_vec-273.15)-0.0358*(T_vec-273.15)^2
  
  #these are the same
  bpk_vec_mm <- -35.5 + 4.05*(T_vec-273.15)-0.0894* (T_vec-273.15)^2
  kap_vec_mm <- B0_kap*((exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/T_vec)))))
  z_vec_mm <- 0.138- 0.0093*(T_vec-273.15)+0.000277*(T_vec-273.15)^2
  
  rm_vec_mm<-((kap_vec_mm+z_vec_mm)*(log10(bpk_vec_mm/(kap_vec_mm+z_vec_mm))-(a_vec_mm*zJ_vec_mm)))/((a_vec_mm*(kap_vec_mm+z_vec_mm))+1) 
  
  lines(T_vec-273.15,rm_vec_mm,col="tomato")
  NicheW[[i]]<- nicheW(rm_vec_mm,T_vec) 
  
}

f <- expression(-(1/(alp*k+alp*z+1))*(alp*k*z_J+alp*z*z_J+
                                        log((k+z)^(k+z)/b_pk^(k+z))))

D(f,"alpha")
D(f,"b_pk")
D(f,"z")
D(f,"z_J")
D(f,"k")


dr_dalp <- function(alp,b_pk,k,z,z_J)  (k + z)/(alp * k + alp * z + 1)^2 * (alp * k * z_J +                                  alp *  z * z_J + log10((k + z)^(k + z)/b_pk^(k + z))) - (1/(alp * 
                                                                                                                                                                                           k + alp * z + 1)) * (k * z_J + z * z_J)

dr_dbpk <- function(alp,b_pk,k,z) (1/(alp * k + alp * z + 1)) * ((k + z)^(k + z) * (b_pk^((k + 
                                                                                             z) - 1) * (k + z))/(b_pk^(k + z))^2/((k + z)^(k + z)/b_pk^(k +  z) * log(10)))


dr_dz <- function( alp,b_pk,k,z,z_J)  alp/(alp * k + alp * z + 1)^2 * (alp * k * z_J + alp *
                                                                         z * z_J + log10((k + z)^(k + z)/b_pk^(k + z))) - (1/(alp * k + alp * z + 1)) * (alp * 
                                                                                                                                                           z_J + (((k + z)^((k + z) -  1) * (k + z) + (k + z)^(k + z) * log((k + z)))/b_pk^(k + z) - (k + z)^(k + z) * (b_pk^(k + z) * log(b_pk))/(b_pk^(k + z))^2)/((k + z)^(k + z)/b_pk^(k + z) * log(10)))


dr_dzJ <- function(alp,b_pk,k,z )  -((1/(alp * k + alp * z + 1)) * (alp * k + alp * z))


dr_dkap <-  function(alp,b_pk,k,z,z_J ) alp/(alp * k + alp * z + 1)^2 * (alp * k * z_J + alp *z * z_J + log10((k + z)^(k + z)/b_pk^(k + z))) - (1/(alp * k + alp * z + 1)) * (alp *  z_J + (((k + z)^((k + z) -  1) * (k + z) + (k + z)^(k + z) * log((k + z)))/b_pk^(k + z) - (k + z)^(k + z) * (b_pk^(k + z) * log( abs( b_pk) ))/(b_pk^(k + z))^2)/((k + z)^(k + z)/b_pk^(k + z) * log(10)))


f2 <- expression(B_0 * exp(-E * ((1/(k*(T))) - (1/(k*T_ref)))) /
                   (1 + (E / (E_D - E)) * exp((E_D / k)* ((1/T_pk) - (1/T)))))

D(f2,"T")

dBdT <- function(B_0,E,E_D,T,T_ref,T_pk,k ) B_0 * (exp(-E * ((1/(k * (T))) - (1/(k * T_ref)))) * (E * (k/(k * (T))^2)))/(1 + (E/(E_D - E)) * exp((E_D/k) * ((1/T_pk) - (1/T)))) - B_0 * exp(-E * ((1/(k * (T))) - (1/(k * T_ref)))) *  ((E/(E_D - E)) * (exp((E_D/k) * ((1/T_pk) - (1/T))) * ((E_D/k) * (1/T^2))))/(1 + (E/(E_D - E)) * exp((E_D/k) * ((1/T_pk) -  (1/T))))^2



f3 <- expression(1/ (B_0 * exp(-E * ((1/(k*(T))) - (1/(k*T_ref)))) /
                       (1 + (E / (E_D - E)) * exp((E_D / k)* ((1/T_pk) - (1/T))))))

D(f3,"T")

dB_invdT <- function( B_0,E,E_D,T,T_ref,T_pk,k ) -((B_0 * (exp(-E * ((1/(k * (T))) - (1/(k * T_ref)))) * (E * (k/(k * (T))^2)))/(1 + (E/(E_D - E)) * exp((E_D/k) * ((1/T_pk) - (1/T)))) - B_0 * exp(-E * ((1/(k * (T))) - (1/(k * T_ref)))) *  ((E/(E_D - E)) * (exp((E_D/k) * ((1/T_pk) - (1/T))) * ((E_D/k) *  (1/T^2))))/(1 + (E/(E_D - E)) * exp((E_D/k) * ((1/T_pk) - (1/T))))^2)/(B_0 * exp(-E * ((1/(k * (T))) - (1/(k * T_ref))))/(1 +   (E/(E_D - E)) * exp((E_D/k) * ((1/T_pk) - (1/T)))))^2)



dr_dalp_vec <- dr_dalp(alp_vec,bpk_vec,kap_vec,z_vec,zJ_vec)
dr_dbpk_vec <- dr_dbpk(alp_vec,bpk_vec,kap_vec,z_vec)
dr_dz_vec <- dr_dz(alp_vec,bpk_vec,kap_vec,z_vec,zJ_vec)
dr_dzJ_vec <- dr_dzJ (alp_vec,bpk_vec,kap_vec,z_vec)
dr_dkap_vec <- dr_dkap(alp_vec,bpk_vec,kap_vec,z_vec,zJ_vec)

dalp_dT_vec <- dB_invdT(B_0_alp,E_alp,E_D_alp,T_vec,T_ref_par,T_pk_alp+273.15,k_par)
dbpk_dT_vec <- dBdT(B_0_bpk,E_bpk,E_D_bpk,T_vec,T_ref_par,T_pk_bpk+273.15,k_par)
dz_dT_vec <-  dB_invdT(B_0_z,E_z,E_D_z,T_vec,T_ref_par,T_pk_z+273.15,k_par)
dzJ_dT_vec <- dB_invdT(B_0_zJ,E_zJ,E_D_zJ,T_vec,T_ref_par,T_pk_zJ+273.15,k_par)
dkap_dT_vec <- dBdT(B_0_kap,E_kap,E_D_kap,T_vec,T_ref_par,T_pk_kap+273.15,k_par)


par(mfrow=c(1,2))
plot(T_vec-273.15,dalp_dT_vec,type = "l",col="blue",ylab = expression(paste("d",alpha/dT)),xlab = "Temprature")
plot(T_vec-273.15,dr_dalp_vec,type = "l",col="blue",ylab = expression(paste("d",r[m]/"d",alpha)),xlab = "Temprature")


par(mfrow=c(1,2))
plot(T_vec-273.15,dbpk_dT_vec,type = "l",col="red",ylab = expression(db[pk]/dT),xlab = "Temprature" )
plot(T_vec-273.15,dr_dbpk_vec,type = "l",col="red",ylab = expression(drm/db[pk]),xlab = "Temprature")


par(mfrow=c(1,2))
plot(T_vec-273.15,dz_dT_vec,type = "l",col="cyan",ylab = "dz/dT",xlab = "Temprature")
plot(T_vec-273.15,dr_dz_vec,type = "l",col="cyan",ylab = "drm/dz",xlab = "Temprature")



par(mfrow=c(1,2))
plot(T_vec-273.15,dzJ_dT_vec,type = "l",col="orange",ylab = expression(dz[J]/dT),xlab = "Temprature")
plot(T_vec-273.15,dr_dzJ_vec,type = "l",col="orange",ylab = expression(drm/dz[J]),xlab = "Temprature")



par(mfrow=c(1,2))
plot(T_vec-273.15,dkap_dT_vec,type = "l",col="purple",ylab = "dk/dT",xlab = "Temprature")
plot(T_vec-273.15,dr_dkap_vec,type = "l",col="purple",ylab = "drm/dk",xlab = "Temprature")


alp_vec <- 40.8-2.65 *(T_vec-273.15)+0.0476*(T_vec-273.15)^2

bpk_vec <-  -35.5 + 4.05*(T_vec-273.15)-0.0894* (T_vec-273.15)^2

z_vec <- 0.138- 0.0093*(T_vec-273.15)+0.000277*(T_vec-273.15)^2


zJ_vec <- 5.3+ 1.23*(T_vec-273.15)-0.0358*(T_vec-273.15)^2

kap_vec <- B(B_0_kap,E_kap,E_D_kap,T_vec,T_ref_par,T_pk_kap+273.15,k_par)


r_m_vec <- r_SP_app(alp_vec,bpk_vec,kap_vec,z_vec,zJ_vec)
r_alp_cons_vec <- r_SP_app(alp_par,bpk_vec,kap_vec,z_vec,zJ_vec)
r_bpk_cons_vec <- r_SP_app(alp_vec,b_pk_par,kap_vec,z_vec,zJ_vec)
r_z_cons_vec <- r_SP_app(alp_vec,bpk_vec,kap_vec,z_par,zJ_vec)
r_zJ_cons_vec <- r_SP_app(alp_vec,bpk_vec,kap_vec,z_vec,z_J_par)
r_kap_cons_vec <- r_SP_app(alp_vec,bpk_vec,kap_par,z_vec,zJ_vec)
r_J_cons_vec <- r_SP_app(alp_par,bpk_vec,kap_vec,z_vec,z_J_par)
r_A_cons_vec <- r_SP_app(alp_vec,b_pk_par,kap_par,z_par,zJ_vec)

dev.off()

plot(T_vec-273.15,r_m_vec,type = "l",col="black",ylab=expression(dr[m]/dT),xlab = "Temperature(C)")
lines(T_vec-273.15,r_alp_cons_vec,type = "l",col="blue" )
lines(T_vec-273.15,r_bpk_cons_vec,type = "l",col="red" )
lines(T_vec-273.15,r_z_cons_vec,type = "l",col="cyan" )
lines(T_vec-273.15,r_zJ_cons_vec ,type = "l",col="orange" )
lines(T_vec-273.15,r_kap_cons_vec ,type = "l",col="purple" )
legend(10,-4,
       c("full",expression(b[pk]),expression(alpha),expression(z),expression(z[J]),expression(k)),
       col = c("black", "blue","red","cyan","orange","purple"),
       lwd = c("2.5","2.5","2.5","2.5","2.5","2.5"),lty = c(1,1,1,1,1,1))


plot(T_vec-273.15,r_m_vec,type = "l",col="black",lwd=1.5,ylab=expression(dr[m]/dT),xlab = "Temperature(C)")
lines(T_vec-273.15,r_J_cons_vec,lty=2,col="blue",lwd=1.5)
lines(T_vec-273.15,r_A_cons_vec,lty=2,col="red",lwd=1.5)
legend(30,-8,c("Full","Juvenile traits constant","Adult traits constant"),
       col = c("black", "blue","red"),
       lwd = c("1.5","1.5","1.5"),lty = c(1,2,2))













