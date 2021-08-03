library(rworldmap)

## Create multiple color codes
# 1=heat, 2=precip, 3=both
malDF <- data.frame(country = c("BGD", "BEN", "BFA",
                                "KHM","CAF","TCD",
                                "GMB","GIN","GNB",
                                "MLI","NER","UGA"),
                    malaria = c("Both", "Precipitation", "Both",
                                "Heat","Both","Precipitation",
                                "Heat","Both","Precipitation",
                                "Both","Heat","Heat"))

## Re-merge
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")

## Specify the colourPalette argument
mapCountryData(malMap, nameColumnToPlot="malaria",mapTitle='Least Developed Countries\nAdversely Impacted By Climate Change', 
               catMethod = "categorical",
               missingCountryCol = gray(.8), colourPalette = c("purple", "red","blue"),
               xlim=c(10,80),ylim=c(-40,40))

# MAP FOR PARAMETRI CTRENDS
## Create multiple color codes
# 1=heat, 2=precip, 3=both
malDF <- data.frame(country = c('COD',
                                'ETH',
                                'LSO',
                                'LBR',
                                'BGD',
                                'BEN',
                                'KMH',
                                'CAF',
                                'ERI',
                                'GIN',
                                'HTI',
                                'MDG',
                                'MLI',
                                'NER',
                                'BFA',
                                'TCD',
                                'GMB',
                                'NPL',
                                'TGO',
                                'UGA',
                                'AFG',
                                'AGO',
                                'BTN',
                                'BDI',
                                'DJI',
                                'GNB',
                                'LAO',
                                'MWI',
                                'MZI',
                                'RWA',
                                'SOM',
                                'SDN',
                                'TZA',
                                'ZMB',
                                'YEM'
                                ),
                    malaria = c('constant',
                                'constant',
                                'constant',
                                'constant',
                                'linear',
                                'linear',
                                'linear',
                                'linear',
                                'linear',
                                'linear',
                                'linear',
                                'linear',
                                'linear',
                                'linear',
                                'quadratic',
                                'quadratic',
                                'quadratic',
                                'quadratic',
                                'quadratic',
                                'quadratic',
                                rep('uncertain',15)
                              
                                ))

## Re-merge
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")

## Specify the colourPalette argument
mapCountryData(malMap, nameColumnToPlot="malaria",mapTitle='Parametric Trends in Extreme Heat in LDC', 
               catMethod = "categorical",
               missingCountryCol = gray(.8), colourPalette = c("yellow", "orange",'red','black'),
               ylim=c(-25,35),xlim=c(-80,120))



