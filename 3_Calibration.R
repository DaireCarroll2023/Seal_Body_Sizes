#Daire Carroll, Gothenburg University, 2023, carrolldaire@gmail.com
#3: Approaching a population level assessment of body size in pinnipeds using drones, an early warning of environmental degradation.
#Process and calibrate summarized harbor seal measurements

rm(list = ls())
graphics.off() 

trans2 = FALSE #transformation based on true mass to estimate regression
trans3 = TRUE #transformation based on true volume to estimate regression
interv = "confidence" #confidence or prediction this choice needs to be made
CI1 = 0.99
CI2 = 0.99

#################################################################
library(ggplot2)
#library(cobs)

cleanup_grid = theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(color = "black"),)

cleanup_text = theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
                     axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = 0.8, vjust = 0.4, face = "plain"),  
                     axis.title.x = element_text(color = "black", size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                     axis.title.y = element_text(color = "black", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"))			
#################################################################

#summer growth: data based on Harding et al. 2005, the daily increase in pup mass

my.dir = paste("  ")
setwd(my.dir)

my.data = read.csv("Pup_growth.csv", sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
attach(my.data)

filt.data = my.data[Days < 105,] #up to the 1st of October
attach(filt.data)

m1 = lm(Mass~Days)
m2 = lm(Mass~log(Days))
AIC(m1)
AIC(m2)
cofs_spring = coefficients(m1)

filt.data = data.frame(cbind(filt.data,predict(m1)))
if(interv == "confidence"){
  er = predict(m1, interval="confidence", level = CI1)
}else{
  er = predict(m1, interval="prediction")
}

names(filt.data)[3]    = "Model"                  

ggplot(filt.data , aes(Days)) +	
  labs(x="Estimated age (days)", y=expression("Mass (kg)")) +
  cleanup_grid + cleanup_text +
  geom_line(aes(y = Model), size = 1, color = "#619CFF", alpha = 1)+
  geom_ribbon(aes(ymin= er[,"lwr"], ymax = er[,"upr"]), fill = "#619CFF", alpha = 0.2)+
  geom_point(aes(y = (Mass    )), size = 1.3, color = "black")+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))

attach(my.data)
filt.data = my.data[Days < 105 & Days > 75,] #between september and the 1st of october
attach(filt.data)

m1 = lm(Mass~Days)
m2 = lm(Mass~log(Days))
AIC(m1)
AIC(m2)
cofs1 = coefficients(m1)

filt.data = data.frame(cbind(filt.data,predict(m1)))
if(interv == "confidence"){
  er = predict(m1, interval="confidence", level = CI1)
}else{
  er = predict(m1, interval="prediction")
}
names(filt.data)[3]    = "Model"                  

ggplot(filt.data , aes(Days)) +	
  labs(x="Estimated age (days)", y=expression("Mass (kg)")) +
  cleanup_grid + cleanup_text +
  geom_line(aes(y = Model), size = 1, color = "#619CFF", alpha = 1)+
  geom_ribbon(aes(ymin= er[,"lwr"], ymax = er[,"upr"]), fill = "#619CFF", alpha = 0.2)+
  geom_point(aes(y = (Mass    )), size = 1.3, color = "black")+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))

#################################################################

#analyse seals of known mass and compare true and drone based measurments

my.data = read.csv("Known_Seals.csv", sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
attach(my.data)
head(my.data)

m1 = lm(True_Length~Est_Length)
m2 = lm(True_Width~Est_Width)
if(interv == "confidence"){
  er1 = predict(m1, interval="confidence", level = CI1)
  er2 = predict(m2, interval="confidence", level = CI1)
}else{
  er1 = predict(m1, interval="prediction")
  er2 = predict(m2, interval="prediction")
}
my.data = data.frame(cbind(my.data,
                           predict(m1,newdata = data.frame(Est_Length = Est_Length)),
                           predict(m2,newdata = data.frame(Est_Width = Est_Width))
))

names(my.data)[((ncol(my.data))-1):ncol(my.data)]    = c("Model_L", "Model_W")                  
cofs_L = coefficients(m1)
cofs_W = coefficients(m2)

ggplot(my.data, aes(x = Model_L, y = True_Length)) +
  labs(x=expression("Estimated length (m)"), y=expression("True length (m)")) +
  #geom_line(aes(y = Model_L), size = 1, color = "#619CFF", alpha = 1)+
  #geom_ribbon(aes(ymin= er1[,"lwr"], ymax = er1[,"upr"]), fill = "#619CFF", alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0, size = 2, color = "#619CFF", alpha = 1, linetype ="dashed")+ #change this to a segment
  
  cleanup_grid + cleanup_text +
  ylim(c(0.8,1.6))+
  xlim(c(0.8,1.6))+
  #geom_point(aes(color = Condition))+
  geom_point(size = 2)+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25)) 

cleanup_grid2 = theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(color = "black"),)


ggplot(my.data, aes(x = Model_W, y = True_Width)) +
  labs(x=expression("Estimated width (m)"), y=expression("True width (m)")) +
  #geom_line(aes(y = Model_W), size = 1, color = "#619CFF", alpha = 1)+
  #geom_ribbon(aes(ymin= er2[,"lwr"], ymax = er2[,"upr"]), fill = "#619CFF", alpha = 0.2)+
  
  geom_abline(slope = 1, intercept = 0, size = 2, color = "#619CFF", alpha = 1, linetype ="dashed")+ #change this to a segment
  cleanup_grid + cleanup_text +
  ylim(c(0.15,0.38))+
  xlim(c(0.15,0.38))+
  geom_point(size = 2)+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25)) 

Model_Vol = (4/3)*pi*(my.data$Model_L/2)*(my.data$Model_W/2)*(my.data$Model_W/2)
m3 = lm(True_Vol~Model_Vol)

if(interv == "confidence"){
  er3 = predict(m3, interval="confidence", level = CI1)
}else{
  er3 = predict(m3, interval="prediction")
}

mvol = m3

if(trans3 == TRUE){
  newx = data.frame(Est_Vol = Model_Vol)
  Model_Vol = predict(mvol,newdata = newx)
}

my.data = data.frame(my.data,Model_Vol)

ggplot(my.data, aes(x = Model_Vol, y = True_Vol)) +
  labs(x=bquote('Estimated volume '(m^3)), y=bquote('True volume '(m^3))) +
  #geom_line(aes(y = Model_Vol), size = 1, color = "#619CFF", alpha = 1)+
  #geom_ribbon(aes(ymin= er3[,"lwr"], ymax = er3[,"upr"]), fill = "#619CFF", alpha = 0.2)+
  geom_abline(slope = 1, intercept = 0, size = 1, color = "#619CFF", alpha = 1)+ #change this to a segment
  
  cleanup_grid + cleanup_text +
  ylim(c(0.010,0.101))+
  xlim(c(0.010,0.101))+
  geom_point()+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25)) 

write.csv(my.data, "Known_Seals_Mass_Index.csv")

my.data = read.csv("measurments.csv", sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
attach(my.data)
head(my.data)

Modle_L = my.data$Length.m. * cofs_L[2] + cofs_L[1]
Modle_W = my.data$Width.m. * cofs_W[2] + cofs_W[1]
Model_Vol = (4/3)*pi*(Modle_L/2)*(Modle_W/2)*(Modle_W/2)

if(trans3 == TRUE){
  newx = data.frame(Est_Vol = Model_Vol)
  Model_Vol = predict(mvol,newdata = newx)
}

my.data = data.frame(my.data,Modle_L,Modle_W,Model_Vol)

write.csv(my.data,"measurments_mass_index.csv")

#################################################################

#regression of known mass against known ellipsoid volume

my.data = read.csv("Summarised_Weights.csv", sep = ",", header = TRUE)
my.data = my.data[which(my.data$Location != "Slottskogen"),]
attach(my.data)

m1 = lm(Mass.kg.~Elipsoid_Vol.m3.)
m2 = lm(Mass.kg. ~ 0 + Elipsoid_Vol.m3.)
m3 = nls(Mass.kg. ~ SSlogis(Elipsoid_Vol.m3., Asym, xmid, scal))          

nk = NA #choice of number of knots can be NA

if(is.numeric(nk) == TRUE ){
  m4 = smooth.spline(Elipsoid_Vol.m3.,Mass.kg.,nknots = nk) #spline fit through origin
}else{
  m4 = smooth.spline(Elipsoid_Vol.m3.,Mass.kg.) #spline fit through origin
}

con = rbind(c(0,0,0))

mcho = m2 #
erRSE = sigma(mcho)

spl1 = 0 #0 is not a spline, 1 smooth.spline() object, 2 cobs object

if(spl1 == 1){
  my.data = data.frame(cbind(my.data,predict(mcho, data.frame(Elipsoid_Vol.m3. = Elipsoid_Vol.m3.))$y))
  names(my.data)[ncol(my.data)]   = "Mass_Index.kg."                  
  er = sqrt(   sum((my.data$Model - my.data$Mass.kg.)^2) / length(my.data$Model)-m4$df )
}else if(spl1 == 2){
  my.data = data.frame(cbind(my.data,predict(mcho,Elipsoid_Vol.m3.)[,2]))
  names(my.data)[ncol(my.data)]   = "Mass_Index.kg."                  
  er = sqrt(   sum((my.data$Model - my.data$Mass.kg.)^2) / length(my.data$Model)-5.608966 ) #this formula should be changed
}else{
  my.data = data.frame(cbind(my.data,predict(mcho))) #m2 forces line through 0
  names(my.data)[ncol(my.data)]   = "Mass_Index.kg."                  
  
  if(interv == "confidence"){
    er = predict(mcho, interval="confidence", level = CI2) 
  }else{
    er = predict(mcho, interval="prediction")
  }
  
  lwr = er[,"lwr"]
  upr = er[,"upr"]
  my.data = data.frame(cbind(my.data,upr,lwr))
}

if(spl1 == 2){
  ggplot(my.data , aes(Elipsoid_Vol.m3.)) +	
    labs(x=bquote('Volume '(m^3)), y=expression("Mass (kg)")) +
    cleanup_grid + cleanup_text +
    geom_line(aes(y = Mass_Index.kg.), size = 1, color = "#619CFF", alpha = 1)+
    geom_ribbon(aes(ymin= Mass_Index.kg.-er, ymax = Mass_Index.kg.+er), fill = "#619CFF", alpha = 0.2)+
    geom_point(aes(y = (Mass.kg.    )), size = 1.3, color = "black") +
    theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))
}else{
  ggplot(my.data , aes(Elipsoid_Vol.m3.)) +	
    labs(x=bquote('True volume '(m^3)), y=expression("True mass (kg)")) +
    cleanup_grid + cleanup_text +
    geom_line(aes(y = Mass_Index.kg.), size = 2, color = "#619CFF", alpha = 1)+
    geom_ribbon(aes(ymin= lwr, ymax = upr), fill = "#619CFF", alpha = 0.2)+
    geom_point(aes(y = (Mass.kg.    )), size = 2, color = "black") +
    theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))
}

#Now mass is estimated for all seals

my.data2 = read.csv("measurments_mass_index.csv", sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
attach(my.data2)
head(my.data2)

if(spl1 == 1){
  newX = data.frame(Elipsoid_Vol.m3. = my.data2$Model_Vol)
  Mass_Index.kg. = as.numeric(unlist(c(predict(mcho, newX)$y))) 
}else if(spl1 == 2){
  newX = my.data2$Model_Vol
  Mass_Index.kg. = predict(mcho, newX)[,2]
}else{
  newX = data.frame(Elipsoid_Vol.m3. = my.data2$Model_Vol)
  Mass_Index.kg. = c(predict(mcho, newdata = newX)) #m2 forces line through 0 alt. m3
  
  if(interv == "confidence"){
    er = predict(mcho, newdata = newX, interval="confidence", level = CI2) #bit silly?
    lwr = er[,"lwr"]
    upr = er[,"upr"]
    
  }else{
    er = predict(mcho, newdata = newX, interval="prediction")
    lwr = er[,"lwr"]
    upr = er[,"upr"]
    
  }
  
}

my.data2 = data.frame(cbind(my.data2,Mass_Index.kg.,lwr,upr)) 

my.data3 = read.csv("Known_Seals_Mass_Index.csv", sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
attach(my.data3)
head(my.data3)

if(spl1 == 1){
  newX = data.frame(Elipsoid_Vol.m3. = my.data3$Model_Vol)
  Mass_Index.kg. = as.numeric(unlist(c(predict(mcho, newX)$y))) 
}else if(spl1 == 2){
  newX = my.data3$Model_Vol
  Mass_Index.kg. = predict(mcho, newX)[,2]
}else{
  newX = data.frame(Elipsoid_Vol.m3. = my.data3$Model_Vol)
  Mass_Index.kg. = c(predict(mcho, newdata = newX)) #m2 forces line through 0 alt. m3
  
  if(interv == "confidence"){
    er = predict(mcho, newdata = newX, interval="confidence", level = CI2 ) #bit silly?
  }else{
    er = predict(mcho, newdata = newX, interval="prediction")
  }
  
  lwr = er[,"lwr"]
  upr = er[,"upr"]
}

m4 = lm(True_Mass~0 + Mass_Index.kg.)

if(trans2 == TRUE){
  Mass_Index.kg.2 = predict(m4, newdata = data.frame(Mass_Index.kg.)) #toggle second transformation
  upr = (upr- Mass_Index.kg.) + Mass_Index.kg.2
  lwr = (lwr- Mass_Index.kg.) + Mass_Index.kg.2
  Mass_Index.kg. = Mass_Index.kg.2
}

my.data3 = data.frame(cbind(my.data3,Mass_Index.kg.,upr,lwr))

ggplot(my.data3, aes(x = Mass_Index.kg., y = True_Mass)) +
  labs(x=bquote('Mass index (kg)'), y=bquote('True mass (kg)')) +
  #geom_ribbon(aes(ymin= lwr, ymax = upr), fill = "#619CFF", alpha = 0.2)+
  cleanup_grid + cleanup_text +
  ylim(c(-10,120))+ #0,100
  xlim(c(-10,120))+ #0,100
  geom_point(size = 2)+
  geom_abline(slope = 1, intercept = 0, size = 2, color = "#619CFF", alpha = 1, linetype = "dashed")+ #change this to a segment
  geom_point()+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25)) +
  geom_errorbarh(aes(y = True_Mass, xmin = (lwr),xmax = (upr)))

if(trans2 == TRUE){
  my.data$Mass_Index.kg. = my.data$Mass_Index.kg.*coef(m4)[1] #toggle second transformation
  my.data2$Mass_Index.kg. = my.data2$Mass_Index.kg.*coef(m4)[1] #toggle second transformation
}

write.csv(my.data3,"Known_Seals_Mass_Index.csv")
write.csv(my.data2,"measurments_mass_index.csv")
write.csv(my.data,"Summarised_Weights_mass_index.csv")

#################################################################

my.data = read.csv("Known_Seals_Mass_Index.csv", sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
attach(my.data)
head(my.data)

my.data = my.data[which(my.data$Condition != "Bannana" & my.data$Condition != "Compressed"),] #only a small number of seals were analysed in these poses, they have been removed from analysis
my.data = na.omit(my.data)
my.data = my.data[which(my.data$Individual != "N1" & my.data$Individual != "N2" & my.data$Individual != "N3"),]

my.data$Individual = as.factor(my.data$Individual)
my.data$Condition = as.factor(my.data$Condition)
attach(my.data)
library(lme4)

newx = data.frame(Elipsoid_Vol.m3. = my.data3$Model_Vol)
if(interv == "confidence"){
  er = predict(mcho, newdata = newX, interval="confidence", level = CI2 ) #bit silly?
}else{
  er = predict(mcho, newdata = newX, interval="prediction")
} 

library(nlme)

lm1 = lme(Mass_Index.kg.~True_Mass * Condition,random=~1|Individual,data=my.data) #group is main, data is random, mass is response

anova(lm1)

summary(lm1)

m1 = fixef(lm1)[2]   
c1 = fixef(lm1)[1]
  
m2 = fixef(lm1)[2] + fixef(lm1)[4]   
c2 = fixef(lm1)[1] + fixef(lm1)[3]

pred_front = m1*my.data$True_Mass +  c1
pred_side = m2*my.data$True_Mass +  c2

my.data = cbind(my.data,pred_front,pred_side)

ggplot(my.data, aes(x = True_Mass, y = Mass_Index.kg., color = Condition)) +
  labs(x=expression("True mass (kg)"), y=expression("Mass index (kg)")) +

  cleanup_grid + cleanup_text +
  ylim(c(5,50))+ #15,45
  xlim(c(5,50))+
  geom_point() +
  geom_line(aes(y = pred_front), size = 1, color = "#F8766D", alpha = 1)+
  #geom_ribbon(aes(ymin= pred_front-summary(lm1)$sigma, ymax = pred_front+summary(lm1)$sigma), fill = "#F8766D", alpha = 0.1, colour = NA)+
  geom_line(aes(y = pred_side), size = 1, color = "#619CFF", alpha = 1)+
  #geom_ribbon(aes(ymin= pred_side-summary(lm1)$sigma, ymax = pred_side+summary(lm1)$sigma), fill = "#619CFF", alpha = 0.1, colour = NA)+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25)) +
  #theme(legend.position = "none") +
  geom_errorbar(aes(y = Mass_Index.kg., ymin = lwr,ymax = upr)) #view by pose

ggplot(data.frame(cbind(my.data,pred_front,pred_side)), aes(x = Mass_Index.kg., y = True_Mass, pch = Condition, color = Individual)) +
  labs(x=expression("Mass index (kg)"), y=expression("True mass (kg)")) +
  geom_abline(slope = 1, intercept = 0, size = 2, color = "#619CFF", alpha = 0.5, linetype = "dashed")+ #change this to a segment
  cleanup_grid + cleanup_text +
  ylim(c(5,50))+
  xlim(c(5,50))+
  geom_point(size = 3) +
  #geom_line(aes(y = pred_front), size = 1, color = "#F8766D", alpha = 1)+
  #geom_ribbon(aes(ymin= pred_front-summary(lm1)$sigma, ymax = pred_front+summary(lm1)$sigma), fill = "#F8766D", alpha = 0.1, colour = NA)+
  #geom_line(aes(y = pred_side), size = 1, color = "#619CFF", alpha = 1)+
  #geom_ribbon(aes(ymin= pred_side-summary(lm1)$sigma, ymax = pred_side+summary(lm1)$sigma), fill = "#619CFF", alpha = 0.1, colour = NA)+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))+
  theme(legend.position = "none") +
  geom_errorbar(aes(x = Mass_Index.kg., xmin = lwr,xmax = upr)) #view by individual

######################################################

#Now pups are 'aged' to estimated october 1st mass and survival is estimated

my.data = read.csv("measurments_mass_index.csv", sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
attach(my.data)
head(my.data)

Group = c()
for(i in 1:length(my.data$Modle_L)){
  if(my.data$Modle_L[i] < 1 & my.data$Mass_Index.kg.[i] < 32){#taking the maximum september length to be 1 based on the data from Harding et al. 2005
    Group[i] = "Pup"
  }else{
    Group[i] = "Adult"
  }
}

my.data = cbind(my.data,Group)

Mass_Index.kg.Oct.1 = my.data$Mass_Index.kg. + as.numeric((105-my.data$Days_Since_June_18)*cofs1[2]) #Day 89 after 18 June is the 15 Sept, 105 is 1st of oct
my.data = cbind(my.data,Mass_Index.kg.Oct.1)

Yr_M = paste0(my.data$Year,"_",my.data$Month)
Yr_M[which(Yr_M == "2021_6")] = "June 2021"
Yr_M[which(Yr_M == "2021_9")] = "Sept 2021"
Yr_M[which(Yr_M == "2022_6")] = "June 2022"
Yr_M[which(Yr_M == "2022_9")] = "Sept 2022"

my.data = cbind(my.data,Yr_M)
my.data = my.data[which(is.na(my.data$Year) != TRUE),]

my.data = my.data[my.data$Notes != "Eva",] #these are replicates using both the phantom and mavic systems
my.data = my.data[my.data$Modle_L < 1.8 & my.data$Modle_L > 0.5 & my.data$Mass_Index.kg. > 0,] #clean unrealistic sizes

ggplot(my.data, aes(x=as.factor(Yr_M), y=Mass_Index.kg., color = as.factor(Year))) + 
  geom_violin(trim=FALSE, position = position_dodge(0.9))+
  geom_jitter(position=position_jitter(0.2), alpha = 0.21)+
  geom_boxplot(width=0.1, color = "black", position = position_dodge(0.9),outlier.colour = "transparent")+
  labs(x=expression("Season"), y=expression("Mass index (kg)")) +
  cleanup_grid + cleanup_text +
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))+ 
  labs(color='Year') +
  theme(legend.position = "none") 

ggplot(my.data, aes(x=as.factor(Yr_M), y=Modle_L, color = as.factor(Year))) + 
  geom_violin(trim=FALSE, position = position_dodge(0.9))+
  geom_jitter(position=position_jitter(0.2), alpha = 0.21)+
  geom_boxplot(width=0.1, color = "black", position = position_dodge(0.9),outlier.colour = "transparent")+
  labs(x=expression("Season"), y=expression("Length (m)")) +
  cleanup_grid + cleanup_text +
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))+ 
  labs(color='Year') +
  theme(legend.position = "none") 

write.csv(my.data,"measurments_mass_index.csv")

mean(my.data$Mass_Index.kg.[which(my.data$Yr_M == "June 22" & my.data$Group == "Adult")])
sd(my.data$Mass_Index.kg.[which(my.data$Yr_M == "June 22" & my.data$Group == "Adult")])

my.data = my.data[my.data$Group == "Pup",]

ggplot(my.data, aes(x=as.factor(Yr_M), y=Mass_Index.kg., color = as.factor(Year))) + 
  geom_violin(trim=FALSE, position = position_dodge(0.9))+
  geom_jitter(position=position_jitter(0.2), alpha = 0.21)+
  geom_boxplot(width=0.1, color = "black", position = position_dodge(0.9),outlier.colour = "transparent")+
  labs(x=expression("Season"), y=expression("Mass index (kg)")) +
  cleanup_grid + cleanup_text +
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))+ 
  labs(color='Year') +
  theme(legend.position = "none")

mean(my.data$Mass_Index.kg.[which(my.data$Yr_M == "Sept 2022")])

#############################################

#Summarise mass and lengths between seasons

my.data = read.csv("measurments_mass_index.csv", sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
attach(my.data)
sept_d_15 = my.data[my.data$Month == 9 & my.data$Group == "Pup"  & is.na(my.data$Date) != TRUE ,]
sept_d_15$Year = as.factor(sept_d_15$Year)

test1 = t.test(sept_d_15$Mass_Index.kg.Oct.1~sept_d_15$Year)
test1

mean_21 = mean(na.omit(sept_d_15[sept_d_15$Year == 2021,]$Mass_Index.kg.Oct.1))
sd_21 = sd(na.omit(sept_d_15[sept_d_15$Year == 2021,]$Mass_Index.kg.Oct.1))
se_21 = sd_21/sqrt(length(na.omit(sept_d_15[sept_d_15$Year == 2021,]$Mass_Index.kg.Oct.1)))
mean_22 = mean(na.omit(sept_d_15[sept_d_15$Year == 2022,]$Mass_Index.kg.Oct.1))
sd_22 = sd(na.omit(sept_d_15[sept_d_15$Year == 2022,]$Mass_Index.kg.Oct.1))
se_22 = sd_21/sqrt(length(na.omit(sept_d_15[sept_d_15$Year == 2022,]$Mass_Index.kg.Oct.1)))

sum = data.frame (cbind(c(mean_21,sd_21,se_21),c(mean_22,sd_22,se_22)))
names(sum) = c(2021,2022)
row.names(sum) = c("Mean", "SD", "SE")

sum 

ggplot(sept_d_15, aes(x=as.factor(Year), y=Mass_Index.kg.Oct.1, color=Year)) + 
  geom_violin(trim=FALSE)+
  geom_jitter(position=position_jitter(0.2), alpha = 0.21)+
  geom_boxplot(width=0.1, color = "black",outlier.colour = "transparent")+
  labs(x=expression("Year"), y=expression("Mass index (kg)")) +
  cleanup_grid + cleanup_text +
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))+
  theme(legend.position = "none") 

#############################################

#Estimate overwinter survival based on Harding et al. 2005 data

Wts = c(17,	20,	23,	26,	29,	32)
Sur = c(0.6308,	0.7459,	0.8345,	0.8965,	0.937,	0.9623)
UCI = c(0.8076,	0.8399,	0.891,	0.9479,	0.9788,	0.9922)
LCI = c(0.3996,	0.613,	0.7586,	0.8073,	0.8308,	0.8413)

spl = smooth.spline(Wts,Sur)
pred = predict(spl)

my.data = data.frame(Wts,Sur,UCI,LCI,pred$y)

ggplot(my.data, aes(x = Wts, y = Sur)) +
  labs(x=expression("Mass (kg)"), y=expression("Survival probability")) +
  geom_smooth(aes(y = Sur), size = 1, color = "#619CFF", alpha = 1,se = FALSE)+
  geom_ribbon(aes(ymin= LCI, ymax = UCI), fill = "#619CFF", alpha = 0.2)+
  
  cleanup_grid + cleanup_text +
  ylim(c(0.3,1))+
  xlim(c(17,32))+
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))

pred = predict(spl, data.frame(Wts = as.numeric(na.omit(sept_d_15$Mass_Index.kg.Oct.1))))
pred2 = data.frame(predict(spl, data.frame(Wts = c(mean_21,mean_22))),c(2021,2022))
names(pred2) = c("Sur","Wt","Year")

pred$y[which(pred$y[,1]>1),1] = 1 #survival is limited to 1

dat = data.frame(pred,na.omit(sept_d_15$Year))
names(dat) = c("Wt","Sur","Year")

ggplot(dat, aes(x=Year, y=Sur, color=Year)) + 
  #geom_violin(trim=FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(x=expression("Year"), y=expression("Survival probability")) +
  cleanup_grid + cleanup_text +
  theme(panel.border = element_rect(color = "black", fill = NA,size = 1.25))+ 
  stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="BLACK")+ 
  coord_cartesian(ylim=c(0.6, 1))+
  theme(legend.position = "none")

min(dat$Sur[which(dat$Year == 2022)])

###################################################

