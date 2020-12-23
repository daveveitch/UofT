setwd('C:/Users/davev/Google Drive/Documents/UofT/PhD/Courses/1850/Project/Code')

#### Pokemon Projection ####
set.seed(1)
poke_data = read.csv('pokemon.csv')
poke_matrix = data.matrix(poke_data[,c('HP','Attack','Defense','SpAttack','SpDefense','Speed')])
rownames(poke_matrix)=poke_data[,1]

random_matrix = matrix(data=rnorm(2*6),nrow=2,ncol=6)

projected_data = t(random_matrix%*%t(poke_matrix))
colnames(projected_data)=c('ProjFeat1','ProjFeat2')

plot(ProjFeat1~ProjFeat2,data=projected_data,col='white',main='Projected Pokemon')
text(ProjFeat1~ProjFeat2, labels=rownames(projected_data),data=projected_data, cex=0.5, font=.5)

reduced_projected_data=projected_data[c('Abra','Kadabra','Alakazam',
                                        'Metapod','Kakuna',
                                        'Gastly','Haunter','Gengar',
                                        'Charmander','Squirtle','Bulbasaur'),]


plot(ProjFeat1~ProjFeat2,data=reduced_projected_data,col='white',main='Projected Pokemon',
     xlim=c(0,200))

points(x=reduced_projected_data[c('Abra','Kadabra','Alakazam'),'ProjFeat2'],
       y=reduced_projected_data[c('Abra','Kadabra','Alakazam'),'ProjFeat1'],col='purple',bg='purple',pch=21)
points(x=reduced_projected_data[c('Metapod','Kakuna'),'ProjFeat2'],
       y=reduced_projected_data[c('Metapod','Kakuna'),'ProjFeat1'],col='green',bg='green',pch=21)
points(x=reduced_projected_data[c('Gastly','Haunter','Gengar'),'ProjFeat2'],
       y=reduced_projected_data[c('Gastly','Haunter','Gengar'),'ProjFeat1'],col='red',bg='red',pch=21)
points(x=reduced_projected_data[c('Charmander','Squirtle','Bulbasaur'),'ProjFeat2'],
       y=reduced_projected_data[c('Charmander','Squirtle','Bulbasaur'),'ProjFeat1'],col='black',bg='black',pch=21)

legend('topleft', legend=c("Abra Evolution", "Ghosts","Starters",'Bad Bugs'),
       pch=c(21,21,21,21),cex=1.5,pt.bg=c("purple", "red","black",'green'))
text(ProjFeat1~ProjFeat2, labels=rownames(reduced_projected_data),data=reduced_projected_data, cex=1.5, font=.5)


#### Economic Data ####
econ_data = read.csv('EconData.csv')

econ_data=econ_data[econ_data[,'Country']%in%c('US','Brazil','India','China'),]

econ_matrix = data.matrix(econ_data[,c('GDPGrowthPct',
                                      'CPIPct',
                                      'UnemploymentPct',
                                      'GovtSpendPct',
                                      'CurrentAccountPct',
                                      'RealRatePct')])
row.names(econ_matrix)=econ_data[,1]

# Normalize Columns
for(i in seq(1,dim(econ_matrix)[2])){
  econ_matrix[,i]=(econ_matrix[,i]-mean(econ_matrix[,i]))/sqrt(var(econ_matrix[,i]))
  print(i)
}

random_matrix = matrix(data=rnorm(2*6),nrow=2,ncol=6)



projected_data = t(random_matrix%*%t(econ_matrix))
colnames(projected_data)=c('ProjFeat1','ProjFeat2')

plot(ProjFeat1~ProjFeat2,data=projected_data,col='black')
text(ProjFeat1~ProjFeat2, labels=rownames(projected_data),data=projected_data, cex=0.5, font=.5)

projected_data = as.data.frame(projected_data,row.names=row.names(econ_matrix))
projected_data[,'Country']=econ_data[,'Country']

lines(x=projected_data[projected_data$Country=='China','ProjFeat2'],
      y=projected_data[projected_data$Country=='China','ProjFeat1'],
      col='red')

lines(x=projected_data[projected_data$Country=='US','ProjFeat2'],
      y=projected_data[projected_data$Country=='US','ProjFeat1'],
      col='blue')

lines(x=projected_data[projected_data$Country=='Brazil','ProjFeat2'],
      y=projected_data[projected_data$Country=='Brazil','ProjFeat1'],
      col='green')

lines(x=projected_data[projected_data$Country=='Canada','ProjFeat2'],
      y=projected_data[projected_data$Country=='Canada','ProjFeat1'],
      col='black')

lines(x=projected_data[projected_data$Country=='India','ProjFeat2'],
      y=projected_data[projected_data$Country=='India','ProjFeat1'],
      col='orange')

lines(x=projected_data[projected_data$Country=='UK','ProjFeat2'],
      y=projected_data[projected_data$Country=='UK','ProjFeat1'],
      col='purple')



#### Human Development ####
hd_data = data.frame(read.csv('HumanDevelopmentData.csv'))
colnames(hd_data)=c('Country','HDIndex','Dependence','UrbanPop','MeanSchool','CO2PerCap','LifeExpect',
                    'InternetUsers','DrinkingWater','Unemployment')

countries_1 = hd_data[hd_data[,'HDIndex']%in%seq(1,10),'Country']
countries_2 = hd_data[hd_data[,'HDIndex']%in%seq(68,79),'Country']
countries_3 = hd_data[hd_data[,'HDIndex']%in%seq(166,189),'Country']

hd_data=hd_data[hd_data[,'HDIndex']%in%c(seq(1,10),seq(68,79),seq(166,189)),]

hd_matrix = data.matrix(hd_data[,3:10])

hd_matrix[,'DrinkingWater']=hd_data[,'DrinkingWater']
hd_matrix[,'InternetUsers']=hd_data[,'InternetUsers']
hd_matrix=apply(hd_matrix, 2, as.numeric)
rownames(hd_matrix)=hd_data[,1]

# Delete NA values
hd_matrix=hd_matrix[rowSums(is.na(hd_matrix))==00,]

# Normalize Data #
for(i in c('Dependence','UrbanPop','MeanSchool','LifeExpect',
           'InternetUsers','Unemployment','CO2PerCap')){
  hd_matrix[,i]=(hd_matrix[,i]-mean(hd_matrix[,i]))/sqrt(var(hd_matrix[,i]))
  print(i)
}

hd_matrix[,'DrinkingWater']=log((100-hd_matrix[,'DrinkingWater'])+1)
hd_matrix[,'DrinkingWater']=(hd_matrix[,'DrinkingWater']-mean(hd_matrix[,'DrinkingWater']))/sqrt(var(hd_matrix[,'DrinkingWater']))

set.seed(2)
random_matrix = matrix(data=rnorm(2*8),nrow=2,ncol=8)

projected_data = t(random_matrix%*%t(hd_matrix))
colnames(projected_data)=c('ProjFeat1','ProjFeat2')

plot(ProjFeat1~ProjFeat2,data=projected_data,col='white',
     xlab='Projected Feature 1',
     ylab='Projected Feature 2',
     xlim=c(-6,5),
     main='Randomly Projected Human Development Indicators ')

points(x=projected_data[countries_1,'ProjFeat2'],y=projected_data[countries_1,'ProjFeat1'],col='red',bg='red',pch=21)
points(x=projected_data[countries_2,'ProjFeat2'],y=projected_data[countries_2,'ProjFeat1'],col='blue',bg='blue',pch=22)
points(x=projected_data[countries_3,'ProjFeat2'],y=projected_data[countries_3,'ProjFeat1'],col='purple',bg='purple',pch=24)
text(ProjFeat1~ProjFeat2, labels=rownames(projected_data),data=projected_data, cex=1, font=.5)

legend('bottomleft', legend=c("Top 10", "Middle 10","Bottom 10"),
   pch=c(21,22,24),cex=1,pt.bg=c("red", "blue","purple"))

