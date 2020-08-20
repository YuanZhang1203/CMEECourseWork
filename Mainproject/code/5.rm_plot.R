#!usr/bin/env R
#################################################
# Title: 4. rm plot in R
# MSc CMEE 
# Auguest 2020 
# Author: YUAN ZHANG 
# refer to: Ves_Mismatch_Alex.ipynb
#################################################

rm(list = ls())
graphics.off()

#------------------------------plug real Thermal performance curves into the  r_max model-----------------------------------------------------------------#

#### put TPC parameters

a <- 197-15.7*x+0.445*x^2-0.0042*x^3
z = -0.313 + 0.0462 *x - 0.00207*x +3.07 * 10^(-5)*x^3

T_pk_par <- 25.3+273.15
E_par <- 0.65
ED_par <- 3
k_par <- 8.617*1e-5
T_ref_par <- 10+273.15

X_Temp <- 273.15+seq(0,40,0.05)
response <- (exp((-E_par/k_par)*((1/X_Temp)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/X_Temp))))

B0_alp<- 1/alp_par 
B0_bpk<-b_pk_par 
B0_z<-1/z_par 
B0_zJ <-1/z_J_par 
B0_kap <- kap_par

Y_alp<- 1/(response*B0_alp) 
Y_bpk<- response*B0_bpk
Y_z<- 1/(response*B0_z)
Y_zJ <-1/(response*B0_zJ)
Y_kap <- response*B0_kap


####Find r_m (growth rate)
r_m_vector <- ((Y_kap+Y_z)*(log10(Y_bpk/(Y_kap+Y_z))-(Y_alp*Y_zJ)))/((Y_alp*(Y_kap+Y_z))+1)

plot(X_Temp-273.15,r_m_vector, ylim=c(-0.3,0.4), type="l",col="Blue")
abline(h=0,lty=2)



# calculate niche width

nicheW <- function(r_m,Temp){
  dataframe<- data.frame(Temperature=Temp,Rate=r_m)
  values <- subset(dataframe,dataframe$Rate>0)
  width<-max(values$Temperature)-min(values$Temperature)
  print(width)
}

nicheW(r_m_vector,X_Temp)

#Values
T_pk_par <- 25.3+273.15
E_par <- 0.65
ED_par <- 3
k_par <- 8.617*1e-5
T_ref_par <- 10+273.15


T_vec <- 273.15+seq(0,50,0.05)

response_mm <- (exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/T_vec))))

z_J_par <- 0.05 
z_par <- 0.03 
alp_par <- 25.0 
b_pk_par <- 10.0 
kap_par <- 0.1 

#B0
B0_alp<- 1/alp_par 
B0_bpk<-b_pk_par 
B0_z<-1/z_par 
B0_zJ <-1/z_J_par 
B0_kap <- kap_par

#Vectors
alp_vec<- 1/(response_mm*B0_alp)
bpk_vec<- response_mm*B0_bpk
z_vec<- 1/(response_mm*B0_z)
zJ_vec <-1/(response_mm*B0_zJ)
kap_vec <- response_mm*B0_kap

r_m_vector <- ((kap_vec+z_vec)*(log10(bpk_vec/(kap_vec+z_vec))-(alp_vec*zJ_vec)))/((alp_vec*(kap_vec+z_vec))+1)

#set mistmatch temps, only change juvenile traits (alp,zJ)
mm <- seq(-10,10.5,0.5)
T_pk_mm <- T_pk_par + mm

plot(T_vec-273.15,r_m_vector, ylim=c(-0.5,0.5), type="l",col="blue",lwd=2)
abline(h=0,lty=2)
NicheW <-c()

for(i in 1:length(T_pk_mm)){
  #these two different
  alp_vec_mm <- 1/(B0_alp*((exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_mm[i])-(1/T_vec)))))) 
  zJ_vec_mm <- 1/(B0_zJ*((exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_mm[i])-(1/T_vec))))))
  
  #these are the same
  bpk_vec_mm <- B0_bpk*((exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/T_vec)))))
  kap_vec_mm <- B0_kap*((exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/T_vec)))))
  z_vec_mm <- 1/(B0_z*((exp((-E_par/k_par)*((1/T_vec)-(1/T_ref_par))))/(1+(E_par/(ED_par-E_par))*exp((ED_par/k_par)*((1/T_pk_par)-(1/T_vec))))))    
  
  
  rm_vec_mm<-((kap_vec_mm+z_vec_mm)*(log10(bpk_vec_mm/(kap_vec_mm+z_vec_mm))-(alp_vec_mm*zJ_vec_mm)))/((alp_vec_mm*(kap_vec_mm+z_vec_mm))+1)  
  
  lines(T_vec-273.15,rm_vec_mm)
  NicheW[[i]]<- nicheW(rm_vec_mm,T_vec) 
}

plot(mm,NicheW,type="l")



