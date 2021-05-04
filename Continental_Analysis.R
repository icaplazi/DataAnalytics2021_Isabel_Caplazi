# Continental Analysis of Electricity usage

######### Geothermal #########  
# download data set and export as csv file
# call Geo electricity dataset as csv
Geo <- read.csv("C:/users/caplam/Downloads/UNdata_Geothermal/UNdata_Geothermal.csv")
View(Geo) 

# dplyr package for filter function
library(dplyr)


### Africa ###
g_Africa <- filter(Geo, Country.or.Area %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
                                                 "Cabo Verde", "Cameroon", "Central Africa", "Congo", "Dem. Rep. of the Congo", 
                                                 "Egypt", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea-Bissau",
                                                 "Kenya", "Liberia", "Libya", "Madagascar", "Mali", "Mauritania", "Mauritius",
                                                 "Mayotte", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
                                                 "Reunion","Réunion", "Rwanda", "Sao Tome and Principe", "Senegal",
                                                 "Seychelles", "Sierra Leone", "South Africa", "South Sudan", "St. Helena and Depend.",
                                                 "Tunisia", "Uganda", "United Rep. of Tanzania", "Zambia")) # creation of Africa data set
View(g_Africa)
fivenum(g_Africa$Quantity,na.rm = TRUE)
mean(g_Africa$Quantity) # Africa Quantity column mean
median((g_Africa$Quantity)) # Africa Quantity column median
y <- table(g_Africa$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # g_Africa Quantity mode(s)
summary(g_Africa)


### Asia ###
g_Asia <- filter(Geo, Country.or.Area %in% c("Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", "China", "India", "Indonesia",
                                               "Japan", "Kazakhstan", "Korea, Dem.Ppl's.Rep.", "Korea, Republic of", "Lao People's Dem. Rep.",
                                               "Malaysia" , "Maldives", "Myanmar", "Nepal", "Other Asia", "Palau", "Philippines", 
                                               "Russian Federation", "Singapore", "Sri Lanka", "Thailand", "Timor-Leste", "Vietnam")) # creation of Asia data set
View(g_Asia)
fivenum(g_Asia$Quantity,na.rm = TRUE)
mean(g_Asia$Quantity) # Asia Quantity column mean
median((g_Asia$Quantity)) # Asia Quantity column median
y <- table(g_Asia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Asia Quantity mode(s)
summary(g_Asia)


### Caribbean ###
g_Caribbean <- filter(Geo, Country.or.Area %in% c("Anguilla", "Antigua and Barbuda","Barbados", "British Virgin Islands", "Cuba", 
                                                    "Dominican Republic","Grenada" , "Guadeloupe", "Haiti", "Jamaica", "Martinique", 
                                                    "Puerto Rico", "St. Kitts-Nevis", "St. Lucia", "St. Vincent-Grenadines", 
                                                    "Trinidad and Tobago", "Turks and Caicos Islands")) # creation of Caribbean data set
View(g_Caribbean)
fivenum(g_Caribbean$Quantity,na.rm = TRUE)
mean(g_Caribbean$Quantity) # Caribbean Quantity column mean
median((g_Caribbean$Quantity)) # Caribbean Quantity column median
y <- table(g_Caribbean$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Caribbean Quantity mode(s)
summary(g_Caribbean)


### Central America ###
g_C_America <- filter(Geo, Country.or.Area %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua",
                                                    "Panama")) # creation of Central America data set

View(g_C_America)
fivenum(g_C_America$Quantity,na.rm = TRUE)
mean(g_C_America$Quantity) # Central America Quantity column mean
median((g_C_America$Quantity)) # Central America Quantity column median
y <- table(g_C_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Central America Quantity mode(s)
summary(g_C_America)


### Europe ###
g_Europe <- filter(Geo, Country.or.Area %in% c("Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                                                 "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Faeroe Islands", 
                                                 "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                                                 "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
                                                 "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
                                                 "Republic of Moldova", "Romania", "Serbia","Slovakia", "Slovenia", "Spain", "Sweden",
                                                 "Switzerland", "Ukraine", "United Kingdom")) # creation of Europe data set
View(g_Europe)
fivenum(g_Europe$Quantity,na.rm = TRUE)
mean(g_Europe$Quantity) # Europe Quantity column mean
median((g_Europe$Quantity)) # Europe Quantity column median
y <- table(g_Europe$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Europe Quantity mode(s)
summary(g_Europe)


### Middle East ###
g_Middle_E <- filter(Geo, Country.or.Area %in% c("Afghanistan","Bahrain","Iran (Islamic Rep. of)", "Iraq", "Israel", "Jordan", "Kuwait",
                                                   "Lebanon", "Oman", "Pakistan", "Saudi Arabia", "State of Palestine", "Turkey",
                                                   "United Arab Emirates", "Uzbekistan", "Yemen")) # creation of Middle East data set
View(g_Middle_E)
fivenum(g_Middle_E$Quantity,na.rm = TRUE)
mean(g_Middle_E$Quantity) # Middle East Quantity column mean
median((g_Middle_E$Quantity)) # Middle East Quantity column median
y <- table(g_Middle_E$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Middle East Quantity mode(s)
summary(g_Middle_E)


### North America ###
g_N_America <- filter(Geo, Country.or.Area %in% c("Canada", "Mexico", "St. Pierre-Miquelon", "United States")) # creation of North America data set

View(g_N_America)
fivenum(g_N_America$Quantity,na.rm = TRUE)
mean(g_N_America$Quantity) # North America Quantity column mean
median((g_N_America$Quantity)) # North America Quantity column median
y <- table(g_N_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # North America Quantity mode(s)
summary(g_N_America)


### Oceania ###
g_Oceania <- filter(Geo, Country.or.Area %in% c("American Samoa","Australia", "Cook Islands", "Fiji", "French Polynesia", "Guam", 
                                                  "Kiribati", "Marshall Islands", "Micronesia (Fed. States of)", "New Caledonia", 
                                                  "New Zealand", "Nauru", "Niue", "Papua New Guinea", "Samoa", "Solomon Islands",
                                                  "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna Is.")) # creation of Oceania data set
View(g_Oceania)
fivenum(g_Oceania$Quantity,na.rm = TRUE)
mean(g_Oceania$Quantity) # Oceania Quantity column mean
median((g_Oceania$Quantity)) # Oceania Quantity column median
y <- table(g_Oceania$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Oceania Quantity mode(s)
summary(g_Oceania)


### South America ###
g_S_America <- filter(Geo, Country.or.Area %in% c("Argentina", "Aruba", "Bolivia (plur. State of)", "Bonaire, St Eustatius, Saba", 
                                                    "Brazil", "Chile", "Colombia", "Curaçao", "Ecuador", "Falkland Is. (Malvinas)", 
                                                    "French Guiana", "Guyana", "Peru", "Suriname", "Uruguay", "Venezuela (Bolivar. Rep.)")) 
# creation of South America data set
View(g_S_America)
fivenum(g_S_America$Quantity,na.rm = TRUE)
mean(g_S_America$Quantity) # South America Quantity column mean
median((g_S_America$Quantity)) # South America Quantity column median
y <- table(g_S_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # South America Quantity mode(s)
summary(g_S_America)


### EDA Continental Solar Electricity ###
Africa_Geo <- ggplot(g_Africa, aes(Country.or.Area, Quantity, color = Year))
print(Africa_Geo + geom_point(size=3))

Asia_Geo <- ggplot(g_Asia, aes(Country.or.Area, Quantity, color = Year))
print(Asia_Solar + geom_point(size=3))

Caribbean_Geo <- ggplot(g_Caribbean, aes(Country.or.Area, Quantity, color = Year))
print(Caribbean_Geo + geom_point(size=3))

C_America_Geo <- ggplot(g_C_America, aes(Country.or.Area, Quantity, color = Year))
print(C_America_Geo + geom_point(size=3))

Europe_Geo <- ggplot(g_Europe, aes(Country.or.Area, Quantity, color = Year))
print(Europe_Geo + geom_point(size=3))

Middle_E_Geo <- ggplot(g_Middle_E, aes(Country.or.Area, Quantity, color = Year))
print(Middle_E_Geo + geom_point(size=3))

N_America_Geo <- ggplot(g_N_America, aes(Country.or.Area, Quantity, color = Year))
print(N_America_Geo + geom_point(size=3))

Oceania_Geo <- ggplot(s_Oceania, aes(Country.or.Area, Quantity, color = Year))
print(Oceania_Geo + geom_point(size=3))

S_America_Geo <- ggplot(s_S_America, aes(Country.or.Area, Quantity, color = Year))
print(S_America_Geo + geom_point(size=3))


All_Geo <- ggplot(Geo, aes(Country.or.Area, Quantity)) +
  geom_point(data=g_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=g_N_America, color = "green") +
  geom_point(data=g_Middle_E, color = "orange") + 
  geom_point(data=g_Europe, color = "turquoise") +
  geom_point(data=g_Caribbean, color = "royalblue4") +
  geom_point(data=g_C_America, color = "purple") +
  geom_point(data=g_Asia, color = "deeppink1") + 
  geom_point(data=g_Africa, color = "chocolate4") +
  geom_point(data=g_S_America, color = "mediumorchid2") +
  theme(axis.text.x=element_text(angle=90, size=7.5, vjust=0.5))
print(All_Geo)

All_Geo_yr <- ggplot(Geo, aes(Year, Quantity)) +
  geom_point(data=g_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=g_N_America, color = "green") +
  geom_point(data=g_Middle_E, color = "orange") + 
  geom_point(data=g_Europe, color = "turquoise") +
  geom_point(data=g_Caribbean, color = "blue") +
  geom_point(data=g_C_America, color = "purple") +
  geom_point(data=g_Asia, color = "deeppink1") + 
  geom_point(data=g_Africa, color = "chocolate4") +
  geom_point(data=g_S_America, color = "mediumorchid2") 
print(All_Geo_yr)

Geo_yr2019 <- ggplot(Geo, aes(Country.or.Area, Quantity)) +
  geom_point(data=g_Oceania_tot2019, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=g_N_America_tot2019, color = "green") +
  geom_point(data=g_Middle_E_tot2019, color = "orange") + 
  geom_point(data=g_Europe_tot2019, color = "turquoise") +
  geom_point(data=g_Caribbean_tot2019, color = "blue") +
  geom_point(data=g_C_America_tot2019, color = "purple") +
  geom_point(data=g_Asia_tot2019, color = "deeppink1") + 
  geom_point(data=g_Africa_tot2019, color = "chocolate4") +
  geom_point(data=g_S_America_tot2019, color = "mediumorchid2") +
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5))
print(Geo_yr2019)

# by year
Continents <- c("Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America")
Years <- c("2019","2019","2019","2019","2019","2019","2019","2019","2019",
           "2018","2018","2018","2018","2018","2018","2018","2018","2018",
           "2017","2017","2017","2017","2017","2017","2017","2017","2017",
           "2016","2016","2016","2016","2016","2016","2016","2016","2016",
           "2015","2015","2015","2015","2015","2015","2015","2015","2015")
Geo_Sums_5yrs <- c(sum(s_Oceania_tot2019$Quantity), sum(s_N_America_tot2019$Quantity), 
                     sum(s_Middle_E_tot2019$Quantity), sum(s_Europe_tot2019$Quantity),
                     sum(s_Caribbean_tot2019$Quantity),sum(s_C_America_tot2019$Quantity), 
                     sum(s_Asia_tot2019$Quantity), sum(s_Africa_tot2019$Quantity), 
                     sum(s_S_America_tot2019$Quantity), 
                     sum(s_Oceania_tot2018$Quantity), sum(s_N_America_tot2018$Quantity), 
                     sum(s_Middle_E_tot2018$Quantity), sum(s_Europe_tot2018$Quantity),
                     sum(s_Caribbean_tot2018$Quantity),sum(s_C_America_tot2018$Quantity), 
                     sum(s_Asia_tot2018$Quantity), sum(s_Africa_tot2018$Quantity), 
                     sum(s_S_America_tot2018$Quantity),
                     sum(s_Oceania_tot2017$Quantity), sum(s_N_America_tot2017$Quantity), 
                     sum(s_Middle_E_tot2017$Quantity), sum(s_Europe_tot2017$Quantity),
                     sum(s_Caribbean_tot2017$Quantity),sum(s_C_America_tot2017$Quantity), 
                     sum(s_Asia_tot2017$Quantity), sum(s_Africa_tot2017$Quantity), 
                     sum(s_S_America_tot2017$Quantity),
                     sum(s_Oceania_tot2016$Quantity), sum(s_N_America_tot2016$Quantity), 
                     sum(s_Middle_E_tot2016$Quantity), sum(s_Europe_tot2016$Quantity),
                     sum(s_Caribbean_tot2016$Quantity),sum(s_C_America_tot2016$Quantity), 
                     sum(s_Asia_tot2016$Quantity), sum(s_Africa_tot2016$Quantity), 
                     sum(s_S_America_tot2016$Quantity),
                     sum(s_Oceania_tot2015$Quantity), sum(s_N_America_tot2015$Quantity), 
                     sum(s_Middle_E_tot2015$Quantity), sum(s_Europe_tot2015$Quantity),
                     sum(s_Caribbean_tot2015$Quantity),sum(s_C_America_tot2015$Quantity), 
                     sum(s_Asia_tot2015$Quantity), sum(s_Africa_tot2015$Quantity), 
                     sum(s_S_America_tot2015$Quantity))

Solar_Cont_5yrs <- data.frame(Years, Continents, Solar_Sums_5yrs)
View(Solar_Cont_5yrs)

# not printing correctly
Solar_5yrs <- ggplot(Solar_Cont_5yrs, aes(Years, Solar_Sums_5yrs)) +
  ggtitle('Continental Solar Usage over last 5 Years') +
  geom_point(aes(color="Continents")) +
  scale_colour_manual(name='', values=c('Oceania'='gold2', 'North America'='green',
                                        'Middle East'= 'orange', 'Europe' = 'turquoise',
                                        'Caribbean' = 'blue', 'Central America' = 'purple',
                                        'Asia' = 'deeppink1', 'Africa' = 'chocolate4',
                                        'South America' = 'mediumorchid2'))
print(Solar_5yrs)


######### Hydro #########  
# download data set and export as csv file
# call Hydro electricity dataset as csv
Hydro <- read.csv("C:/users/caplam/Downloads/UNdata_Hydro/UNdata_Hydro.csv")
View(Hydro) 

# dplyr package for filter function
library(dplyr)


### Africa ###
h_Africa <- filter(Hydro, Country.or.Area %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
                                                 "Cabo Verde", "Cameroon", "Central Africa", "Congo", "Dem. Rep. of the Congo", 
                                                 "Egypt", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea-Bissau",
                                                 "Kenya", "Liberia", "Libya", "Madagascar", "Mali", "Mauritania", "Mauritius",
                                                 "Mayotte", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
                                                 "Reunion","Réunion", "Rwanda", "Sao Tome and Principe", "Senegal",
                                                 "Seychelles", "Sierra Leone", "South Africa", "South Sudan", "St. Helena and Depend.",
                                                 "Tunisia", "Uganda", "United Rep. of Tanzania", "Zambia")) # creation of Africa data set
View(h_Africa)
fivenum(h_Africa$Quantity,na.rm = TRUE)
mean(h_Africa$Quantity) # Africa Quantity column mean
median((h_Africa$Quantity)) # Africa Quantity column median
y <- table(h_Africa$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # h_Africa Quantity mode(s)
summary(h_Africa)


h_Africa_tot2019 <- filter(h_Africa, Year == "2019")
View(h_Africa_tot2019)

h_Africa_tot2018 <- filter(h_Africa, Year == "2018")
View(h_Africa_tot2018)

h_Africa_tot2017 <- filter(h_Africa, Year == "2017")
View(h_Africa_tot2017)

h_Africa_tot2016 <- filter(h_Africa, Year == "2016")
View(h_Africa_tot2016)

h_Africa_tot2015 <- filter(h_Africa, Year == "2015")
View(h_Africa_tot2015)


### Asia ###
h_Asia <- filter(Hydro, Country.or.Area %in% c("Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", "China", "India", "Indonesia",
                                               "Japan", "Kazakhstan", "Korea, Dem.Ppl's.Rep.", "Korea, Republic of", "Lao People's Dem. Rep.",
                                               "Malaysia" , "Maldives", "Myanmar", "Nepal", "Other Asia", "Palau", "Philippines", 
                                               "Russian Federation", "Singapore", "Sri Lanka", "Thailand", "Timor-Leste", "Vietnam")) # creation of Asia data set
View(h_Asia)
fivenum(h_Asia$Quantity,na.rm = TRUE)
mean(h_Asia$Quantity) # Asia Quantity column mean
median((h_Asia$Quantity)) # Asia Quantity column median
y <- table(h_Asia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Asia Quantity mode(s)
summary(h_Asia)


h_Asia_tot2019 <- filter(h_Asia, Year == "2019")
View(h_Asia_tot2019)

h_Asia_tot2018 <- filter(h_Asia, Year == "2018")
View(h_Asia_tot2018)

h_Asia_tot2017 <- filter(h_Asia, Year == "2017")
View(h_Asia_tot2017)

h_Asia_tot2016 <- filter(h_Asia, Year == "2016")
View(h_Asia_tot2016)

h_Asia_tot2015 <- filter(h_Asia, Year == "2015")
View(h_Asia_tot2015)


### Caribbean ###
h_Caribbean <- filter(Hydro, Country.or.Area %in% c("Anguilla", "Antigua and Barbuda","Barbados", "British Virgin Islands", "Cuba", 
                                                    "Dominican Republic","Grenada" , "Guadeloupe", "Haiti", "Jamaica", "Martinique", 
                                                    "Puerto Rico", "St. Kitts-Nevis", "St. Lucia", "St. Vincent-Grenadines", 
                                                    "Trinidad and Tobago", "Turks and Caicos Islands")) # creation of Caribbean data set
View(h_Caribbean)
fivenum(h_Caribbean$Quantity,na.rm = TRUE)
mean(h_Caribbean$Quantity) # Caribbean Quantity column mean
median((h_Caribbean$Quantity)) # Caribbean Quantity column median
y <- table(h_Caribbean$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Caribbean Quantity mode(s)
summary(h_Caribbean)


h_Caribbean_tot2019 <- filter(h_Caribbean, Year == "2019")
View(h_Caribbean_tot2019)

h_Caribbean_tot2018 <- filter(h_Caribbean, Year == "2018")
View(h_Caribbean_tot2018)

h_Caribbean_tot2017 <- filter(h_Caribbean, Year == "2017")
View(h_Caribbean_tot2017)

h_Caribbean_tot2016 <- filter(h_Caribbean, Year == "2016")
View(h_Caribbean_tot2016)

h_Caribbean_tot2015 <- filter(h_Caribbean, Year == "2015")
View(h_Caribbean_tot2015)


### Central America ###
h_C_America <- filter(Hydro, Country.or.Area %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua",
                                                    "Panama")) # creation of Central America data set

View(h_C_America)
fivenum(h_C_America$Quantity,na.rm = TRUE)
mean(h_C_America$Quantity) # Central America Quantity column mean
median((h_C_America$Quantity)) # Central America Quantity column median
y <- table(h_C_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Central America Quantity mode(s)
summary(h_C_America)


h_C_America_tot2019 <- filter(h_C_America, Year == "2019")
View(h_C_America_tot2019)

h_C_America_tot2018 <- filter(h_C_America, Year == "2018")
View(h_C_America_tot2018)

h_C_America_tot2017 <- filter(h_C_America, Year == "2017")
View(h_C_America_tot2017)

h_C_America_tot2016 <- filter(h_C_America, Year == "2016")
View(h_C_America_tot2016)

h_C_America_tot2015 <- filter(h_C_America, Year == "2015")
View(h_C_America_tot2015)


### Europe ###
h_Europe <- filter(Hydro, Country.or.Area %in% c("Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                                                 "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Faeroe Islands", 
                                                 "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                                                 "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
                                                 "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
                                                 "Republic of Moldova", "Romania", "Serbia","Slovakia", "Slovenia", "Spain", "Sweden",
                                                 "Switzerland", "Ukraine", "United Kingdom")) # creation of Europe data set
View(h_Europe)
fivenum(h_Europe$Quantity,na.rm = TRUE)
mean(h_Europe$Quantity) # Europe Quantity column mean
median((h_Europe$Quantity)) # Europe Quantity column median
y <- table(h_Europe$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Europe Quantity mode(s)
summary(h_Europe)


h_Europe_tot2019 <- filter(h_Europe, Year == "2019")
View(h_Europe_tot2019)

h_Europe_tot2018 <- filter(h_Europe, Year == "2018")
View(h_Europe_tot2018)

h_Europe_tot2017 <- filter(h_Europe, Year == "2017")
View(h_Europe_tot2017)

h_Europe_tot2016 <- filter(h_Europe, Year == "2016")
View(h_Europe_tot2016)

h_Europe_tot2015 <- filter(h_Europe, Year == "2015")
View(h_Europe_tot2015)


### Middle East ###
h_Middle_E <- filter(Hydro, Country.or.Area %in% c("Afghanistan","Bahrain","Iran (Islamic Rep. of)", "Iraq", "Israel", "Jordan", "Kuwait",
                                                   "Lebanon", "Oman", "Pakistan", "Saudi Arabia", "State of Palestine", "Turkey",
                                                   "United Arab Emirates", "Uzbekistan", "Yemen")) # creation of Middle East data set
View(h_Middle_E)
fivenum(h_Middle_E$Quantity,na.rm = TRUE)
mean(h_Middle_E$Quantity) # Middle East Quantity column mean
median((h_Middle_E$Quantity)) # Middle East Quantity column median
y <- table(h_Middle_E$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Middle East Quantity mode(s)
summary(h_Middle_E)


h_Middle_E_tot2019 <- filter(h_Middle_E, Year == "2019")
View(h_Middle_E_tot2019)

h_Middle_E_tot2018 <- filter(h_Middle_E, Year == "2018")
View(h_Middle_E_tot2018)

h_Middle_E_tot2017 <- filter(h_Middle_E, Year == "2017")
View(h_Middle_E_tot2017)

h_Middle_E_tot2016 <- filter(h_Middle_E, Year == "2016")
View(h_Middle_E_tot2016)

h_Middle_E_tot2015 <- filter(h_Middle_E, Year == "2015")
View(h_Middle_E_tot2015)


### North America ###
h_N_America <- filter(Hydro, Country.or.Area %in% c("Canada", "Mexico", "St. Pierre-Miquelon", "United States")) # creation of North America data set

View(h_N_America)
fivenum(h_N_America$Quantity,na.rm = TRUE)
mean(h_N_America$Quantity) # North America Quantity column mean
median((h_N_America$Quantity)) # North America Quantity column median
y <- table(h_N_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # North America Quantity mode(s)
summary(h_N_America)


h_N_America_tot2019 <- filter(h_N_America, Year == "2019")
View(h_N_America_tot2019)

h_N_America_tot2018 <- filter(h_N_America, Year == "2018")
View(h_N_America_tot2018)

h_N_America_tot2017 <- filter(h_N_America, Year == "2017")
View(h_N_America_tot2017)

h_N_America_tot2016 <- filter(h_N_America, Year == "2016")
View(h_N_America_tot2016)

h_N_America_tot2015 <- filter(h_N_America, Year == "2015")
View(h_N_America_tot2015)


### Oceania ###
h_Oceania <- filter(Hydro, Country.or.Area %in% c("American Samoa","Australia", "Cook Islands", "Fiji", "French Polynesia", "Guam", 
                                                  "Kiribati", "Marshall Islands", "Micronesia (Fed. States of)", "New Caledonia", 
                                                  "New Zealand", "Nauru", "Niue", "Papua New Guinea", "Samoa", "Solomon Islands",
                                                  "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna Is.")) # creation of Oceania data set
View(h_Oceania)
fivenum(h_Oceania$Quantity,na.rm = TRUE)
mean(h_Oceania$Quantity) # Oceania Quantity column mean
median((h_Oceania$Quantity)) # Oceania Quantity column median
y <- table(h_Oceania$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Oceania Quantity mode(s)
summary(h_Oceania)


h_Oceania_tot2019 <- filter(h_Oceania, Year == "2019")
View(h_Oceania_tot2019)

h_Oceania_tot2018 <- filter(h_Oceania, Year == "2018")
View(h_Oceania_tot2018)

h_Oceania_tot2017 <- filter(h_Oceania, Year == "2017")
View(h_Oceania_tot2017)

h_Oceania_tot2016 <- filter(h_Oceania, Year == "2016")
View(h_Oceania_tot2016)

h_Oceania_tot2015 <- filter(h_Oceania, Year == "2015")
View(h_Oceania_tot2015)


### South America ###
h_S_America <- filter(Hydro, Country.or.Area %in% c("Argentina", "Aruba", "Bolivia (plur. State of)", "Bonaire, St Eustatius, Saba", 
                                                    "Brazil", "Chile", "Colombia", "Curaçao", "Ecuador", "Falkland Is. (Malvinas)", 
                                                    "French Guiana", "Guyana", "Peru", "Suriname", "Uruguay", "Venezuela (Bolivar. Rep.)")) 
# creation of South America data set
View(h_S_America)
fivenum(h_S_America$Quantity,na.rm = TRUE)
mean(h_S_America$Quantity) # South America Quantity column mean
median((h_S_America$Quantity)) # South America Quantity column median
y <- table(h_S_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # South America Quantity mode(s)
summary(h_S_America)


h_S_America_tot2019 <- filter(h_S_America, Year == "2019")
View(h_S_America_tot2019)

h_S_America_tot2018 <- filter(h_S_America, Year == "2018")
View(h_S_America_tot2018)

h_S_America_tot2017 <- filter(h_S_America, Year == "2017")
View(h_S_America_tot2017)

h_S_America_tot2016 <- filter(h_S_America, Year == "2016")
View(h_S_America_tot2016)

h_S_America_tot2015 <- filter(h_S_America, Year == "2015")
View(h_S_America_tot2015)


### EDA Continental Hydro Electricity ###
Africa_Hydro <- ggplot(h_Africa, aes(Country.or.Area, Quantity, color = Year))
print(Africa_Hydro + geom_point(size=3))

Asia_Hydro <- ggplot(h_Asia, aes(Country.or.Area, Quantity, color = Year))
print(Asia_Hydro + geom_point(size=3))

Caribbean_Hydro <- ggplot(h_Caribbean, aes(Country.or.Area, Quantity, color = Year))
print(Caribbean_Hydro + geom_point(size=3))

C_America_Solar <- ggplot(h_C_America, aes(Country.or.Area, Quantity, color = Year))
print(C_America_Solar + geom_point(size=3))

Europe_Hydro <- ggplot(h_Europe, aes(Country.or.Area, Quantity, color = Year))
print(Europe_Hydro + geom_point(size=3))

Middle_E_Hydro <- ggplot(h_Middle_E, aes(Country.or.Area, Quantity, color = Year))
print(Middle_E_Hydro + geom_point(size=3))

N_America_Hydro <- ggplot(h_N_America, aes(Country.or.Area, Quantity, color = Year))
print(N_America_Hydro + geom_point(size=3))

Oceania_Hydro <- ggplot(h_Oceania, aes(Country.or.Area, Quantity, color = Year))
print(Oceania_Hydro + geom_point(size=3))

S_America_Hydro <- ggplot(h_S_America, aes(Country.or.Area, Quantity, color = Year))
print(S_America_Hydro + geom_point(size=3))


All_Hydro <- ggplot(Hydro, aes(Country.or.Area, Quantity)) +
  geom_point(data=h_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=h_N_America, color = "green") +
  geom_point(data=h_Middle_E, color = "orange") + 
  geom_point(data=h_Europe, color = "turquoise") +
  geom_point(data=h_Caribbean, color = "royalblue4") +
  geom_point(data=h_C_America, color = "purple") +
  geom_point(data=h_Asia, color = "deeppink1") + 
  geom_point(data=h_Africa, color = "chocolate4") +
  geom_point(data=h_S_America, color = "mediumorchid2") +
  theme(axis.text.x=element_text(angle=90, size=7.5, vjust=0.5))
print(All_Hydro)

All_Hydro_yr <- ggplot(Hydro, aes(Year, Quantity)) +
  geom_point(data=h_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=h_N_America, color = "green") +
  geom_point(data=h_Middle_E, color = "orange") + 
  geom_point(data=h_Europe, color = "turquoise") +
  geom_point(data=h_Caribbean, color = "blue") +
  geom_point(data=h_C_America, color = "purple") +
  geom_point(data=h_Asia, color = "deeppink1") + 
  geom_point(data=h_Africa, color = "chocolate4") +
  geom_point(data=h_S_America, color = "mediumorchid2") 
print(All_Hydro_yr)

Hydro_yr2019 <- ggplot(Hydro, aes(Country.or.Area, Quantity)) +
  geom_point(data=h_Oceania_tot2019, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=h_N_America_tot2019, color = "green") +
  geom_point(data=h_Middle_E_tot2019, color = "orange") + 
  geom_point(data=h_Europe_tot2019, color = "turquoise") +
  geom_point(data=h_Caribbean_tot2019, color = "blue") +
  geom_point(data=h_C_America_tot2019, color = "purple") +
  geom_point(data=h_Asia_tot2019, color = "deeppink1") + 
  geom_point(data=h_Africa_tot2019, color = "chocolate4") +
  geom_point(data=h_S_America_tot2019, color = "mediumorchid2") +
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5))
print(Hydro_yr2019)

# by year
Continents <- c("Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America")
Years <- c("2019","2019","2019","2019","2019","2019","2019","2019","2019",
           "2018","2018","2018","2018","2018","2018","2018","2018","2018",
           "2017","2017","2017","2017","2017","2017","2017","2017","2017",
           "2016","2016","2016","2016","2016","2016","2016","2016","2016",
           "2015","2015","2015","2015","2015","2015","2015","2015","2015")
Hydro_Sums_5yrs <- c(sum(h_Oceania_tot2019$Quantity), sum(h_N_America_tot2019$Quantity), 
                     sum(h_Middle_E_tot2019$Quantity), sum(h_Europe_tot2019$Quantity),
                     sum(h_Caribbean_tot2019$Quantity),sum(h_C_America_tot2019$Quantity), 
                     sum(h_Asia_tot2019$Quantity), sum(h_Africa_tot2019$Quantity), 
                     sum(h_S_America_tot2019$Quantity), 
                     sum(h_Oceania_tot2018$Quantity), sum(h_N_America_tot2018$Quantity), 
                     sum(h_Middle_E_tot2018$Quantity), sum(h_Europe_tot2018$Quantity),
                     sum(h_Caribbean_tot2018$Quantity),sum(h_C_America_tot2018$Quantity), 
                     sum(h_Asia_tot2018$Quantity), sum(h_Africa_tot2018$Quantity), 
                     sum(h_S_America_tot2018$Quantity),
                     sum(h_Oceania_tot2017$Quantity), sum(h_N_America_tot2017$Quantity), 
                     sum(h_Middle_E_tot2017$Quantity), sum(h_Europe_tot2017$Quantity),
                     sum(h_Caribbean_tot2017$Quantity),sum(h_C_America_tot2017$Quantity), 
                     sum(h_Asia_tot2017$Quantity), sum(h_Africa_tot2017$Quantity), 
                     sum(h_S_America_tot2017$Quantity),
                     sum(h_Oceania_tot2016$Quantity), sum(h_N_America_tot2016$Quantity), 
                     sum(h_Middle_E_tot2016$Quantity), sum(h_Europe_tot2016$Quantity),
                     sum(h_Caribbean_tot2016$Quantity),sum(h_C_America_tot2016$Quantity), 
                     sum(h_Asia_tot2016$Quantity), sum(h_Africa_tot2016$Quantity), 
                     sum(h_S_America_tot2016$Quantity),
                     sum(h_Oceania_tot2015$Quantity), sum(h_N_America_tot2015$Quantity), 
                     sum(h_Middle_E_tot2015$Quantity), sum(h_Europe_tot2015$Quantity),
                     sum(h_Caribbean_tot2015$Quantity),sum(h_C_America_tot2015$Quantity), 
                     sum(h_Asia_tot2015$Quantity), sum(h_Africa_tot2015$Quantity), 
                     sum(h_S_America_tot2015$Quantity))

Hydro_Cont_5yrs <- data.frame(Years, Continents, Hydro_Sums_5yrs)
View(Hydro_Cont_5yrs)

# not printing correctly
Hydro_5yrs <- ggplot(Hydro_Cont_5yrs, aes(Years, Hydro_Sums_5yrs)) +
  ggtitle('Continental Hydro Usage over last 5 Years') +
  geom_point(aes(color="Continents")) +
  scale_colour_manual(name='', values=c('Oceania'='gold2', 'North America'='green',
                                        'Middle East'= 'orange', 'Europe' = 'turquoise',
                                        'Caribbean' = 'blue', 'Central America' = 'purple',
                                        'Asia' = 'deeppink1', 'Africa' = 'chocolate4',
                                        'South America' = 'mediumorchid2'))
print(Hydro_5yrs)


######### Nuclear #########  
# download data set and export as csv file
# call Hydro electricity dataset as csv
Nuclear <- read.csv("C:/users/caplam/Downloads/UNdata_Nuclear/UNdata_Nuclear.csv")
View(Nuclear) 

# dplyr package for filter function
library(dplyr)


### Africa ###
n_Africa <- filter(Nuclear, Country.or.Area %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
                                                 "Cabo Verde", "Cameroon", "Central Africa", "Congo", "Dem. Rep. of the Congo", 
                                                 "Egypt", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea-Bissau",
                                                 "Kenya", "Liberia", "Libya", "Madagascar", "Mali", "Mauritania", "Mauritius",
                                                 "Mayotte", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
                                                 "Reunion","Réunion", "Rwanda", "Sao Tome and Principe", "Senegal",
                                                 "Seychelles", "Sierra Leone", "South Africa", "South Sudan", "St. Helena and Depend.",
                                                 "Tunisia", "Uganda", "United Rep. of Tanzania", "Zambia")) # creation of Africa data set
View(n_Africa)
fivenum(n_Africa$Quantity,na.rm = TRUE)
mean(n_Africa$Quantity) # Africa Quantity column mean
median((n_Africa$Quantity)) # Africa Quantity column median
y <- table(n_Africa$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # n_Africa Quantity mode(s)
summary(n_Africa)


n_Africa_tot2019 <- filter(n_Africa, Year == "2019")
View(n_Africa_tot2019)

n_Africa_tot2018 <- filter(n_Africa, Year == "2018")
View(n_Africa_tot2018)

n_Africa_tot2017 <- filter(n_Africa, Year == "2017")
View(n_Africa_tot2017)

n_Africa_tot2016 <- filter(n_Africa, Year == "2016")
View(n_Africa_tot2016)

n_Africa_tot2015 <- filter(n_Africa, Year == "2015")
View(n_Africa_tot2015)


### Asia ###
n_Asia <- filter(Nuclear, Country.or.Area %in% c("Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", "China", "India", "Indonesia",
                                               "Japan", "Kazakhstan", "Korea, Dem.Ppl's.Rep.", "Korea, Republic of", "Lao People's Dem. Rep.",
                                               "Malaysia" , "Maldives", "Myanmar", "Nepal", "Other Asia", "Palau", "Philippines", 
                                               "Russian Federation", "Singapore", "Sri Lanka", "Thailand", "Timor-Leste", "Vietnam")) # creation of Asia data set
View(n_Asia)
fivenum(n_Asia$Quantity,na.rm = TRUE)
mean(n_Asia$Quantity) # Asia Quantity column mean
median((n_Asia$Quantity)) # Asia Quantity column median
y <- table(n_Asia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Asia Quantity mode(s)
summary(n_Asia)


n_Asia_tot2019 <- filter(n_Asia, Year == "2019")
View(n_Asia_tot2019)

n_Asia_tot2018 <- filter(n_Asia, Year == "2018")
View(n_Asia_tot2018)

n_Asia_tot2017 <- filter(n_Asia, Year == "2017")
View(n_Asia_tot2017)

n_Asia_tot2016 <- filter(n_Asia, Year == "2016")
View(n_Asia_tot2016)

n_Asia_tot2015 <- filter(n_Asia, Year == "2015")
View(n_Asia_tot2015)


### Caribbean ###
n_Caribbean <- filter(Nuclear, Country.or.Area %in% c("Anguilla", "Antigua and Barbuda","Barbados", "British Virgin Islands", "Cuba", 
                                                    "Dominican Republic","Grenada" , "Guadeloupe", "Haiti", "Jamaica", "Martinique", 
                                                    "Puerto Rico", "St. Kitts-Nevis", "St. Lucia", "St. Vincent-Grenadines", 
                                                    "Trinidad and Tobago", "Turks and Caicos Islands")) # creation of Caribbean data set
View(n_Caribbean)
fivenum(n_Caribbean$Quantity,na.rm = TRUE)
mean(n_Caribbean$Quantity) # Caribbean Quantity column mean
median((n_Caribbean$Quantity)) # Caribbean Quantity column median
y <- table(n_Caribbean$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Caribbean Quantity mode(s)
summary(n_Caribbean)


n_Caribbean_tot2019 <- filter(n_Caribbean, Year == "2019")
View(n_Caribbean_tot2019)

n_Caribbean_tot2018 <- filter(n_Caribbean, Year == "2018")
View(n_Caribbean_tot2018)

n_Caribbean_tot2017 <- filter(n_Caribbean, Year == "2017")
View(n_Caribbean_tot2017)

n_Caribbean_tot2016 <- filter(n_Caribbean, Year == "2016")
View(n_Caribbean_tot2016)

n_Caribbean_tot2015 <- filter(n_Caribbean, Year == "2015")
View(n_Caribbean_tot2015)


### Central America ###
n_C_America <- filter(Nuclear, Country.or.Area %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua",
                                                    "Panama")) # creation of Central America data set

View(n_C_America)
fivenum(n_C_America$Quantity,na.rm = TRUE)
mean(n_C_America$Quantity) # Central America Quantity column mean
median((n_C_America$Quantity)) # Central America Quantity column median
y <- table(n_C_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Central America Quantity mode(s)
summary(n_C_America)


n_C_America_tot2019 <- filter(n_C_America, Year == "2019")
View(n_C_America_tot2019)

n_C_America_tot2018 <- filter(n_C_America, Year == "2018")
View(n_C_America_tot2018)

n_C_America_tot2017 <- filter(n_C_America, Year == "2017")
View(n_C_America_tot2017)

n_C_America_tot2016 <- filter(n_C_America, Year == "2016")
View(n_C_America_tot2016)

n_C_America_tot2015 <- filter(n_C_America, Year == "2015")
View(n_C_America_tot2015)


### Europe ###
n_Europe <- filter(Nuclear, Country.or.Area %in% c("Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                                                 "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Faeroe Islands", 
                                                 "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                                                 "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
                                                 "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
                                                 "Republic of Moldova", "Romania", "Serbia","Slovakia", "Slovenia", "Spain", "Sweden",
                                                 "Switzerland", "Ukraine", "United Kingdom")) # creation of Europe data set
View(n_Europe)
fivenum(n_Europe$Quantity,na.rm = TRUE)
mean(n_Europe$Quantity) # Europe Quantity column mean
median((n_Europe$Quantity)) # Europe Quantity column median
y <- table(n_Europe$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Europe Quantity mode(s)
summary(n_Europe)


n_Europe_tot2019 <- filter(n_Europe, Year == "2019")
View(n_Europe_tot2019)

n_Europe_tot2018 <- filter(n_Europe, Year == "2018")
View(n_Europe_tot2018)

n_Europe_tot2017 <- filter(n_Europe, Year == "2017")
View(n_Europe_tot2017)

n_Europe_tot2016 <- filter(n_Europe, Year == "2016")
View(n_Europe_tot2016)

n_Europe_tot2015 <- filter(n_Europe, Year == "2015")
View(n_Europe_tot2015)


### Middle East ###
n_Middle_E <- filter(Nuclear, Country.or.Area %in% c("Afghanistan","Bahrain","Iran (Islamic Rep. of)", "Iraq", "Israel", "Jordan", "Kuwait",
                                                   "Lebanon", "Oman", "Pakistan", "Saudi Arabia", "State of Palestine", "Turkey",
                                                   "United Arab Emirates", "Uzbekistan", "Yemen")) # creation of Middle East data set
View(n_Middle_E)
fivenum(n_Middle_E$Quantity,na.rm = TRUE)
mean(n_Middle_E$Quantity) # Middle East Quantity column mean
median((n_Middle_E$Quantity)) # Middle East Quantity column median
y <- table(n_Middle_E$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Middle East Quantity mode(s)
summary(n_Middle_E)


n_Middle_E_tot2019 <- filter(n_Middle_E, Year == "2019")
View(n_Middle_E_tot2019)

n_Middle_E_tot2018 <- filter(n_Middle_E, Year == "2018")
View(n_Middle_E_tot2018)

n_Middle_E_tot2017 <- filter(n_Middle_E, Year == "2017")
View(n_Middle_E_tot2017)

n_Middle_E_tot2016 <- filter(n_Middle_E, Year == "2016")
View(n_Middle_E_tot2016)

n_Middle_E_tot2015 <- filter(n_Middle_E, Year == "2015")
View(n_Middle_E_tot2015)


### North America ###
n_N_America <- filter(Nuclear, Country.or.Area %in% c("Canada", "Mexico", "St. Pierre-Miquelon", "United States")) # creation of North America data set

View(n_N_America)
fivenum(n_N_America$Quantity,na.rm = TRUE)
mean(n_N_America$Quantity) # North America Quantity column mean
median((n_N_America$Quantity)) # North America Quantity column median
y <- table(n_N_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # North America Quantity mode(s)
summary(n_N_America)


n_N_America_tot2019 <- filter(n_N_America, Year == "2019")
View(n_N_America_tot2019)

n_N_America_tot2018 <- filter(n_N_America, Year == "2018")
View(n_N_America_tot2018)

n_N_America_tot2017 <- filter(n_N_America, Year == "2017")
View(n_N_America_tot2017)

n_N_America_tot2016 <- filter(n_N_America, Year == "2016")
View(n_N_America_tot2016)

n_N_America_tot2015 <- filter(n_N_America, Year == "2015")
View(n_N_America_tot2015)


### Oceania ###
n_Oceania <- filter(Nuclear, Country.or.Area %in% c("American Samoa","Australia", "Cook Islands", "Fiji", "French Polynesia", "Guam", 
                                                  "Kiribati", "Marshall Islands", "Micronesia (Fed. States of)", "New Caledonia", 
                                                  "New Zealand", "Nauru", "Niue", "Papua New Guinea", "Samoa", "Solomon Islands",
                                                  "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna Is.")) # creation of Oceania data set
View(n_Oceania)
fivenum(n_Oceania$Quantity,na.rm = TRUE)
mean(n_Oceania$Quantity) # Oceania Quantity column mean
median((n_Oceania$Quantity)) # Oceania Quantity column median
y <- table(n_Oceania$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Oceania Quantity mode(s)
summary(n_Oceania)


n_Oceania_tot2019 <- filter(n_Oceania, Year == "2019")
View(n_Oceania_tot2019)

n_Oceania_tot2018 <- filter(n_Oceania, Year == "2018")
View(n_Oceania_tot2018)

n_Oceania_tot2017 <- filter(n_Oceania, Year == "2017")
View(n_Oceania_tot2017)

n_Oceania_tot2016 <- filter(n_Oceania, Year == "2016")
View(n_Oceania_tot2016)

n_Oceania_tot2015 <- filter(n_Oceania, Year == "2015")
View(n_Oceania_tot2015)


### South America ###
n_S_America <- filter(Nuclear, Country.or.Area %in% c("Argentina", "Aruba", "Bolivia (plur. State of)", "Bonaire, St Eustatius, Saba", 
                                                    "Brazil", "Chile", "Colombia", "Curaçao", "Ecuador", "Falkland Is. (Malvinas)", 
                                                    "French Guiana", "Guyana", "Peru", "Suriname", "Uruguay", "Venezuela (Bolivar. Rep.)")) 
# creation of South America data set
View(n_S_America)
fivenum(n_S_America$Quantity,na.rm = TRUE)
mean(n_S_America$Quantity) # South America Quantity column mean
median((n_S_America$Quantity)) # South America Quantity column median
y <- table(n_S_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # South America Quantity mode(s)
summary(n_S_America)


n_S_America_tot2019 <- filter(n_S_America, Year == "2019")
View(n_S_America_tot2019)

n_S_America_tot2018 <- filter(n_S_America, Year == "2018")
View(n_S_America_tot2018)

n_S_America_tot2017 <- filter(n_S_America, Year == "2017")
View(n_S_America_tot2017)

n_S_America_tot2016 <- filter(n_S_America, Year == "2016")
View(n_S_America_tot2016)

n_S_America_tot2015 <- filter(n_S_America, Year == "2015")
View(n_S_America_tot2015)


### EDA Continental Hydro Electricity ###
Africa_Nuclear <- ggplot(n_Africa, aes(Country.or.Area, Quantity, color = Year))
print(Africa_Nuclear + geom_point(size=3))

Asia_Nuclear <- ggplot(n_Asia, aes(Country.or.Area, Quantity, color = Year))
print(Asia_Nuclear + geom_point(size=3))

Caribbean_Nuclear <- ggplot(n_Caribbean, aes(Country.or.Area, Quantity, color = Year))
print(Caribbean_Nuclear + geom_point(size=3))

C_America_Nuclear <- ggplot(n_C_America, aes(Country.or.Area, Quantity, color = Year))
print(C_America_Nuclear + geom_point(size=3))

Europe_Nuclear <- ggplot(n_Europe, aes(Country.or.Area, Quantity, color = Year))
print(Europe_Nuclear + geom_point(size=3))

Middle_E_Nuclear <- ggplot(n_Middle_E, aes(Country.or.Area, Quantity, color = Year))
print(Middle_E_Nuclear + geom_point(size=3))

N_America_Nuclear <- ggplot(n_N_America, aes(Country.or.Area, Quantity, color = Year))
print(N_America_Nuclear + geom_point(size=3))

Oceania_Nuclear <- ggplot(n_Oceania, aes(Country.or.Area, Quantity, color = Year))
print(Oceania_Nuclear + geom_point(size=3))

S_America_Nuclear <- ggplot(n_S_America, aes(Country.or.Area, Quantity, color = Year))
print(S_America_Nuclear + geom_point(size=3))


All_Nuclear <- ggplot(Nuclear, aes(Country.or.Area, Quantity)) +
  geom_point(data=n_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=n_N_America, color = "green") +
  geom_point(data=n_Middle_E, color = "orange") + 
  geom_point(data=n_Europe, color = "turquoise") +
  geom_point(data=n_Caribbean, color = "royalblue4") +
  geom_point(data=n_C_America, color = "purple") +
  geom_point(data=n_Asia, color = "deeppink1") + 
  geom_point(data=n_Africa, color = "chocolate4") +
  geom_point(data=n_S_America, color = "mediumorchid2") +
  theme(axis.text.x=element_text(angle=90, size=7.5, vjust=0.5))
print(All_Nuclear)

All_Nuclear_yr <- ggplot(Nuclear, aes(Year, Quantity)) +
  geom_point(data=n_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=n_N_America, color = "green") +
  geom_point(data=n_Middle_E, color = "orange") + 
  geom_point(data=n_Europe, color = "turquoise") +
  geom_point(data=n_Caribbean, color = "blue") +
  geom_point(data=n_C_America, color = "purple") +
  geom_point(data=n_Asia, color = "deeppink1") + 
  geom_point(data=n_Africa, color = "chocolate4") +
  geom_point(data=n_S_America, color = "mediumorchid2") 
print(All_Nuclear_yr)

Nuclear_yr2019 <- ggplot(Nuclear, aes(Country.or.Area, Quantity)) +
  geom_point(data=n_Oceania_tot2019, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=n_N_America_tot2019, color = "green") +
  geom_point(data=n_Middle_E_tot2019, color = "orange") + 
  geom_point(data=n_Europe_tot2019, color = "turquoise") +
  geom_point(data=n_Caribbean_tot2019, color = "blue") +
  geom_point(data=n_C_America_tot2019, color = "purple") +
  geom_point(data=n_Asia_tot2019, color = "deeppink1") + 
  geom_point(data=n_Africa_tot2019, color = "chocolate4") +
  geom_point(data=n_S_America_tot2019, color = "mediumorchid2") +
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5))
print(Nuclear_yr2019)

# by year
Continents <- c("Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America")
Years <- c("2019","2019","2019","2019","2019","2019","2019","2019","2019",
           "2018","2018","2018","2018","2018","2018","2018","2018","2018",
           "2017","2017","2017","2017","2017","2017","2017","2017","2017",
           "2016","2016","2016","2016","2016","2016","2016","2016","2016",
           "2015","2015","2015","2015","2015","2015","2015","2015","2015")
Hydro_Sums_5yrs <- c(sum(n_Oceania_tot2019$Quantity), sum(n_N_America_tot2019$Quantity), 
                     sum(n_Middle_E_tot2019$Quantity), sum(n_Europe_tot2019$Quantity),
                     sum(n_Caribbean_tot2019$Quantity),sum(n_C_America_tot2019$Quantity), 
                     sum(n_Asia_tot2019$Quantity), sum(n_Africa_tot2019$Quantity), 
                     sum(n_S_America_tot2019$Quantity), 
                     sum(n_Oceania_tot2018$Quantity), sum(n_N_America_tot2018$Quantity), 
                     sum(n_Middle_E_tot2018$Quantity), sum(n_Europe_tot2018$Quantity),
                     sum(n_Caribbean_tot2018$Quantity),sum(n_C_America_tot2018$Quantity), 
                     sum(n_Asia_tot2018$Quantity), sum(n_Africa_tot2018$Quantity), 
                     sum(n_S_America_tot2018$Quantity),
                     sum(n_Oceania_tot2017$Quantity), sum(n_N_America_tot2017$Quantity), 
                     sum(n_Middle_E_tot2017$Quantity), sum(n_Europe_tot2017$Quantity),
                     sum(n_Caribbean_tot2017$Quantity),sum(n_C_America_tot2017$Quantity), 
                     sum(n_Asia_tot2017$Quantity), sum(n_Africa_tot2017$Quantity), 
                     sum(n_S_America_tot2017$Quantity),
                     sum(n_Oceania_tot2016$Quantity), sum(n_N_America_tot2016$Quantity), 
                     sum(n_Middle_E_tot2016$Quantity), sum(n_Europe_tot2016$Quantity),
                     sum(n_Caribbean_tot2016$Quantity),sum(n_C_America_tot2016$Quantity), 
                     sum(n_Asia_tot2016$Quantity), sum(n_Africa_tot2016$Quantity), 
                     sum(n_S_America_tot2016$Quantity),
                     sum(n_Oceania_tot2015$Quantity), sum(n_N_America_tot2015$Quantity), 
                     sum(n_Middle_E_tot2015$Quantity), sum(n_Europe_tot2015$Quantity),
                     sum(n_Caribbean_tot2015$Quantity),sum(n_C_America_tot2015$Quantity), 
                     sum(n_Asia_tot2015$Quantity), sum(n_Africa_tot2015$Quantity), 
                     sum(n_S_America_tot2015$Quantity))

Nuclear_Cont_5yrs <- data.frame(Years, Continents, Nuclear_Sums_5yrs)
View(Nuclear_Cont_5yrs)

# not printing correctly
Nuclear_5yrs <- ggplot(Nuclear_Cont_5yrs, aes(Years, Nuclear_Sums_5yrs)) +
  ggtitle('Continental Nuclear Usage over last 5 Years') +
  geom_point(aes(color="Continents")) +
  scale_colour_manual(name='', values=c('Oceania'='gold2', 'North America'='green',
                                        'Middle East'= 'orange', 'Europe' = 'turquoise',
                                        'Caribbean' = 'blue', 'Central America' = 'purple',
                                        'Asia' = 'deeppink1', 'Africa' = 'chocolate4',
                                        'South America' = 'mediumorchid2'))
print(Nuclear_5yrs)


######### Solar #########  
# download data set and export as csv file
# call Solar electricity dataset as csv
Solar <- read.csv("C:/users/caplam/Downloads/UNdata_SolarElec/UNdata_SolarElec.csv")
View(Solar) 

# dplyr package for filter function
library(dplyr)


### Africa ###
s_Africa <- filter(Solar, Country.or.Area %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
                                                 "Cabo Verde", "Cameroon", "Central Africa", "Congo", "Dem. Rep. of the Congo", 
                                                 "Egypt", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea-Bissau",
                                                 "Kenya", "Liberia", "Libya", "Madagascar", "Mali", "Mauritania", "Mauritius",
                                                 "Mayotte", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria",
                                                 "Reunion","Réunion", "Rwanda", "Sao Tome and Principe", "Senegal",
                                                 "Seychelles", "Sierra Leone", "South Africa", "South Sudan", "St. Helena and Depend.",
                                                 "Tunisia", "Uganda", "United Rep. of Tanzania", "Zambia")) # creation of Africa data set
View(s_Africa)
fivenum(s_Africa$Quantity,na.rm = TRUE)
mean(s_Africa$Quantity) # Africa Quantity column mean
median((s_Africa$Quantity)) # Africa Quantity column median
y <- table(s_Africa$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # s_Africa Quantity mode(s)
summary(s_Africa)


s_Africa_tot2019 <- filter(s_Africa, Year == "2019")
View(s_Africa_tot2019)

s_Africa_tot2018 <- filter(s_Africa, Year == "2018")
View(s_Africa_tot2018)
Africa2018_solar_tot <- sum(s_Africa_tot2018$Quantity)
Africa2018_solar_tot

s_Africa_tot2017 <- filter(s_Africa, Year == "2017")
View(s_Africa_tot2017)

s_Africa_tot2016 <- filter(s_Africa, Year == "2016")
View(s_Africa_tot2016)

s_Africa_tot2015 <- filter(s_Africa, Year == "2015")
View(s_Africa_tot2015)


### Asia ###
s_Asia <- filter(Solar, Country.or.Area %in% c("Bangladesh", "Bhutan", "Brunei Darussalam", "Cambodia", "China", "India", "Indonesia",
                                               "Japan", "Kazakhstan", "Korea, Dem.Ppl's.Rep.", "Korea, Republic of", "Lao People's Dem. Rep.",
                                               "Malaysia" , "Maldives", "Myanmar", "Nepal", "Other Asia", "Palau", "Philippines", 
                                               "Russian Federation", "Singapore", "Sri Lanka", "Thailand", "Timor-Leste", "Vietnam")) # creation of Asia data set
View(s_Asia)
fivenum(s_Asia$Quantity,na.rm = TRUE)
mean(s_Asia$Quantity) # Asia Quantity column mean
median((s_Asia$Quantity)) # Asia Quantity column median
y <- table(s_Asia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Asia Quantity mode(s)
summary(s_Asia)


s_Asia_tot2019 <- filter(s_Asia, Year == "2019")
View(s_Asia_tot2019)

s_Asia_tot2018 <- filter(s_Asia, Year == "2018")
View(s_Asia_tot2018)

s_Asia_tot2017 <- filter(s_Asia, Year == "2017")
View(s_Asia_tot2017)

s_Asia_tot2016 <- filter(s_Asia, Year == "2016")
View(s_Asia_tot2016)

s_Asia_tot2015 <- filter(s_Asia, Year == "2015")
View(s_Asia_tot2015)


### Caribbean ###
s_Caribbean <- filter(Solar, Country.or.Area %in% c("Anguilla", "Antigua and Barbuda","Barbados", "British Virgin Islands", "Cuba", 
                                                    "Dominican Republic","Grenada" , "Guadeloupe", "Haiti", "Jamaica", "Martinique", 
                                                    "Puerto Rico", "St. Kitts-Nevis", "St. Lucia", "St. Vincent-Grenadines", 
                                                    "Trinidad and Tobago", "Turks and Caicos Islands")) # creation of Caribbean data set
View(s_Caribbean)
fivenum(s_Caribbean$Quantity,na.rm = TRUE)
mean(s_Caribbean$Quantity) # Caribbean Quantity column mean
median((s_Caribbean$Quantity)) # Caribbean Quantity column median
y <- table(s_Caribbean$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Caribbean Quantity mode(s)
summary(s_Caribbean)


s_Caribbean_tot2019 <- filter(s_Caribbean, Year == "2019")
View(s_Caribbean_tot2019)

s_Caribbean_tot2018 <- filter(s_Caribbean, Year == "2018")
View(s_Caribbean_tot2018)

s_Caribbean_tot2017 <- filter(s_Caribbean, Year == "2017")
View(s_Caribbean_tot2017)

s_Caribbean_tot2016 <- filter(s_Caribbean, Year == "2016")
View(s_Caribbean_tot2016)

s_Caribbean_tot2015 <- filter(s_Caribbean, Year == "2015")
View(s_Caribbean_tot2015)


### Central America ###
s_C_America <- filter(Solar, Country.or.Area %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua",
                                                    "Panama")) # creation of Central America data set

View(s_C_America)
fivenum(s_C_America$Quantity,na.rm = TRUE)
mean(s_C_America$Quantity) # Central America Quantity column mean
median((s_C_America$Quantity)) # Central America Quantity column median
y <- table(s_C_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Central America Quantity mode(s)
summary(s_C_America)


s_C_America_tot2019 <- filter(s_C_America, Year == "2019")
View(s_C_America_tot2019)

s_C_America_tot2018 <- filter(s_C_America, Year == "2018")
View(s_C_America_tot2018)

s_C_America_tot2017 <- filter(s_C_America, Year == "2017")
View(s_C_America_tot2017)

s_C_America_tot2016 <- filter(s_C_America, Year == "2016")
View(s_C_America_tot2016)

s_C_America_tot2015 <- filter(s_C_America, Year == "2015")
View(s_C_America_tot2015)


### Europe ###
s_Europe <- filter(Solar, Country.or.Area %in% c("Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                                                 "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Faeroe Islands", 
                                                 "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                                                 "Italy", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
                                                 "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
                                                 "Republic of Moldova", "Romania", "Serbia","Slovakia", "Slovenia", "Spain", "Sweden",
                                                 "Switzerland", "Ukraine", "United Kingdom")) # creation of Europe data set
View(s_Europe)
fivenum(s_Europe$Quantity,na.rm = TRUE)
mean(s_Europe$Quantity) # Europe Quantity column mean
median((s_Europe$Quantity)) # Europe Quantity column median
y <- table(s_Europe$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Europe Quantity mode(s)
summary(s_Europe)


s_Europe_tot2019 <- filter(s_Europe, Year == "2019")
View(s_Europe_tot2019)

s_Europe_tot2018 <- filter(s_Europe, Year == "2018")
View(s_Europe_tot2018)

s_Europe_tot2017 <- filter(s_Europe, Year == "2017")
View(s_Europe_tot2017)

s_Europe_tot2016 <- filter(s_Europe, Year == "2016")
View(s_Europe_tot2016)

s_Europe_tot2015 <- filter(s_Europe, Year == "2015")
View(s_Europe_tot2015)


### Middle East ###
s_Middle_E <- filter(Solar, Country.or.Area %in% c("Afghanistan","Bahrain","Iran (Islamic Rep. of)", "Iraq", "Israel", "Jordan", "Kuwait",
                                                   "Lebanon", "Oman", "Pakistan", "Saudi Arabia", "State of Palestine", "Turkey",
                                                   "United Arab Emirates", "Uzbekistan", "Yemen")) # creation of Middle East data set
View(s_Middle_E)
fivenum(s_Middle_E$Quantity,na.rm = TRUE)
mean(s_Middle_E$Quantity) # Middle East Quantity column mean
median((s_Middle_E$Quantity)) # Middle East Quantity column median
y <- table(s_Middle_E$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Middle East Quantity mode(s)
summary(s_Middle_E)


s_Middle_E_tot2019 <- filter(s_Middle_E, Year == "2019")
View(s_Middle_E_tot2019)

s_Middle_E_tot2018 <- filter(s_Middle_E, Year == "2018")
View(s_Middle_E_tot2018)

s_Middle_E_tot2017 <- filter(s_Middle_E, Year == "2017")
View(s_Middle_E_tot2017)

s_Middle_E_tot2016 <- filter(s_Middle_E, Year == "2016")
View(s_Middle_E_tot2016)

s_Middle_E_tot2015 <- filter(s_Middle_E, Year == "2015")
View(s_Middle_E_tot2015)


### North America ###
s_N_America <- filter(Solar, Country.or.Area %in% c("Canada", "Mexico", "St. Pierre-Miquelon", "United States")) # creation of North America data set

View(s_N_America)
fivenum(s_N_America$Quantity,na.rm = TRUE)
mean(s_N_America$Quantity) # North America Quantity column mean
median((s_N_America$Quantity)) # North America Quantity column median
y <- table(s_N_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # North America Quantity mode(s)
summary(s_N_America)


s_N_America_tot2019 <- filter(s_N_America, Year == "2019")
View

s_N_America_tot2018 <- filter(s_N_America, Year == "2018")
View(s_N_America_tot2018)

s_N_America_tot2017 <- filter(s_N_America, Year == "2017")
View(s_N_America_tot2017)

s_N_America_tot2016 <- filter(s_N_America, Year == "2016")
View(s_N_America_tot2016)

s_N_America_tot2015 <- filter(s_N_America, Year == "2015")
View(s_N_America_tot2015)
N_America2015_solar_tot <- sum(s_N_America_tot2015$Quantity)
N_America2015_solar_tot


### Oceania ###
s_Oceania <- filter(Solar, Country.or.Area %in% c("American Samoa","Australia", "Cook Islands", "Fiji", "French Polynesia", "Guam", 
                                                  "Kiribati", "Marshall Islands", "Micronesia (Fed. States of)", "New Caledonia", 
                                                  "New Zealand", "Nauru", "Niue", "Papua New Guinea", "Samoa", "Solomon Islands",
                                                  "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna Is.")) # creation of Oceania data set
View(s_Oceania)
fivenum(s_Oceania$Quantity,na.rm = TRUE)
mean(s_Oceania$Quantity) # Oceania Quantity column mean
median((s_Oceania$Quantity)) # Oceania Quantity column median
y <- table(s_Oceania$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Oceania Quantity mode(s)
summary(s_Oceania)


s_Oceania_tot2019 <- filter(s_Oceania, Year == "2019")
View(s_Oceania_tot2019)

s_Oceania_tot2018 <- filter(s_Oceania, Year == "2018")
View(s_Oceania_tot2018)

s_Oceania_tot2017 <- filter(s_Oceania, Year == "2017")
View(s_Oceania_tot2017)

s_Oceania_tot2016 <- filter(s_Oceania, Year == "2016")
View(s_Oceania_tot2016)

s_Oceania_tot2015 <- filter(s_Oceania, Year == "2015")
View(s_Oceania_tot2015)


### South America ###
s_S_America <- filter(Solar, Country.or.Area %in% c("Argentina", "Aruba", "Bolivia (plur. State of)", "Bonaire, St Eustatius, Saba", 
                                                    "Brazil", "Chile", "Colombia", "Curaçao", "Ecuador", "Falkland Is. (Malvinas)", 
                                                    "French Guiana", "Guyana", "Peru", "Suriname", "Uruguay", "Venezuela (Bolivar. Rep.)")) 
                                                    # creation of South America data set
View(s_S_America)
fivenum(s_S_America$Quantity,na.rm = TRUE)
mean(s_S_America$Quantity) # South America Quantity column mean
median((s_S_America$Quantity)) # South America Quantity column median
y <- table(s_S_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # South America Quantity mode(s)
summary(s_S_America)


s_S_America_tot2019 <- filter(s_S_America, Year == "2019")
View(s_S_America_tot2019)

s_S_America_tot2018 <- filter(s_S_America, Year == "2018")
View(s_S_America_tot2018)

s_S_America_tot2017 <- filter(s_S_America, Year == "2017")
View(s_S_America_tot2017)

s_S_America_tot2016 <- filter(s_S_America, Year == "2016")
View(s_S_America_tot2016)

s_S_America_tot2015 <- filter(s_S_America, Year == "2015")
View(s_S_America_tot2015)


### EDA Continental Solar Electricity ###
Africa_Solar <- ggplot(s_Africa, aes(Country.or.Area, Quantity, color = Year))
print(Africa_Solar + geom_point(size=3))

Asia_Solar <- ggplot(s_Asia, aes(Country.or.Area, Quantity, color = Year))
print(Asia_Solar + geom_point(size=3))

Caribbean_Solar <- ggplot(s_Caribbean, aes(Country.or.Area, Quantity, color = Year))
print(Caribbean_Solar + geom_point(size=3))

C_America_Solar <- ggplot(s_C_America, aes(Country.or.Area, Quantity, color = Year))
print(C_America_Solar + geom_point(size=3))

Europe_Solar <- ggplot(s_Europe, aes(Country.or.Area, Quantity, color = Year))
print(Europe_Solar + geom_point(size=3))

Middle_E_Solar <- ggplot(s_Middle_E, aes(Country.or.Area, Quantity, color = Year))
print(Middle_E_Solar + geom_point(size=3))

N_America_Solar <- ggplot(s_N_America, aes(Country.or.Area, Quantity, color = Year))
print(N_America_Solar + geom_point(size=3))

Oceania_Solar <- ggplot(s_Oceania, aes(Country.or.Area, Quantity, color = Year))
print(Oceania_Solar + geom_point(size=3))

S_America_Solar <- ggplot(s_S_America, aes(Country.or.Area, Quantity, color = Year))
print(S_America_Solar + geom_point(size=3))


All_Solar <- ggplot(Solar, aes(Country.or.Area, Quantity)) +
  geom_point(data=s_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=s_N_America, color = "green") +
  geom_point(data=s_Middle_E, color = "orange") + 
  geom_point(data=s_Europe, color = "turquoise") +
  geom_point(data=s_Caribbean, color = "royalblue4") +
  geom_point(data=s_C_America, color = "purple") +
  geom_point(data=s_Asia, color = "deeppink1") + 
  geom_point(data=s_Africa, color = "chocolate4") +
  geom_point(data=s_S_America, color = "mediumorchid2") +
  theme(axis.text.x=element_text(angle=90, size=7.5, vjust=0.5))
print(All_Solar)

All_Solar_yr <- ggplot(Solar, aes(Year, Quantity)) +
  geom_point(data=s_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=s_N_America, color = "green") +
  geom_point(data=s_Middle_E, color = "orange") + 
  geom_point(data=s_Europe, color = "turquoise") +
  geom_point(data=s_Caribbean, color = "blue") +
  geom_point(data=s_C_America, color = "purple") +
  geom_point(data=s_Asia, color = "deeppink1") + 
  geom_point(data=s_Africa, color = "chocolate4") +
  geom_point(data=s_S_America, color = "mediumorchid2") 
print(All_Solar_yr)

Solar_yr2019 <- ggplot(Solar, aes(Country.or.Area, Quantity)) +
  geom_point(data=s_Oceania_tot2019, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=s_N_America_tot2019, color = "green") +
  geom_point(data=s_Middle_E_tot2019, color = "orange") + 
  geom_point(data=s_Europe_tot2019, color = "turquoise") +
  geom_point(data=s_Caribbean_tot2019, color = "blue") +
  geom_point(data=s_C_America_tot2019, color = "purple") +
  geom_point(data=s_Asia_tot2019, color = "deeppink1") + 
  geom_point(data=s_Africa_tot2019, color = "chocolate4") +
  geom_point(data=s_S_America_tot2019, color = "mediumorchid2") +
  theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5))
print(Solar_yr2019)

# by year
Continents <- c("Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America", 
                "Oceania", "North America", "Middle East", "Europe", "Caribbean", 
                "Central America", "Asia", "Africa", "South America")
Years <- c("2019","2019","2019","2019","2019","2019","2019","2019","2019",
           "2018","2018","2018","2018","2018","2018","2018","2018","2018",
           "2017","2017","2017","2017","2017","2017","2017","2017","2017",
           "2016","2016","2016","2016","2016","2016","2016","2016","2016",
           "2015","2015","2015","2015","2015","2015","2015","2015","2015")
Solar_Sums_5yrs <- c(sum(s_Oceania_tot2019$Quantity), sum(s_N_America_tot2019$Quantity), 
                     sum(s_Middle_E_tot2019$Quantity), sum(s_Europe_tot2019$Quantity),
                     sum(s_Caribbean_tot2019$Quantity),sum(s_C_America_tot2019$Quantity), 
                     sum(s_Asia_tot2019$Quantity), sum(s_Africa_tot2019$Quantity), 
                     sum(s_S_America_tot2019$Quantity), 
                     sum(s_Oceania_tot2018$Quantity), sum(s_N_America_tot2018$Quantity), 
                     sum(s_Middle_E_tot2018$Quantity), sum(s_Europe_tot2018$Quantity),
                     sum(s_Caribbean_tot2018$Quantity),sum(s_C_America_tot2018$Quantity), 
                     sum(s_Asia_tot2018$Quantity), sum(s_Africa_tot2018$Quantity), 
                     sum(s_S_America_tot2018$Quantity),
                     sum(s_Oceania_tot2017$Quantity), sum(s_N_America_tot2017$Quantity), 
                     sum(s_Middle_E_tot2017$Quantity), sum(s_Europe_tot2017$Quantity),
                     sum(s_Caribbean_tot2017$Quantity),sum(s_C_America_tot2017$Quantity), 
                     sum(s_Asia_tot2017$Quantity), sum(s_Africa_tot2017$Quantity), 
                     sum(s_S_America_tot2017$Quantity),
                     sum(s_Oceania_tot2016$Quantity), sum(s_N_America_tot2016$Quantity), 
                     sum(s_Middle_E_tot2016$Quantity), sum(s_Europe_tot2016$Quantity),
                     sum(s_Caribbean_tot2016$Quantity),sum(s_C_America_tot2016$Quantity), 
                     sum(s_Asia_tot2016$Quantity), sum(s_Africa_tot2016$Quantity), 
                     sum(s_S_America_tot2016$Quantity),
                     sum(s_Oceania_tot2015$Quantity), sum(s_N_America_tot2015$Quantity), 
                     sum(s_Middle_E_tot2015$Quantity), sum(s_Europe_tot2015$Quantity),
                     sum(s_Caribbean_tot2015$Quantity),sum(s_C_America_tot2015$Quantity), 
                     sum(s_Asia_tot2015$Quantity), sum(s_Africa_tot2015$Quantity), 
                     sum(s_S_America_tot2015$Quantity))

Solar_Cont_5yrs <- data.frame(Years, Continents, Solar_Sums_5yrs)
View(Solar_Cont_5yrs)

# not printing correctly
Solar_5yrs <- ggplot(Solar_Cont_5yrs, aes(Years, Solar_Sums_5yrs)) +
  ggtitle('Continental Solar Usage over last 5 Years') +
  geom_point(aes(color="Continents")) +
  scale_colour_manual(name='', values=c('Oceania'='gold2', 'North America'='green',
                                        'Middle East'= 'orange', 'Europe' = 'turquoise',
                                        'Caribbean' = 'blue', 'Central America' = 'purple',
                                        'Asia' = 'deeppink1', 'Africa' = 'chocolate4',
                                        'South America' = 'mediumorchid2'))
print(Solar_5yrs)


######### Wind ######### 
# download data set and export as csv file
# call wind electricity dataset as csv
Wind <- read.csv("C:/users/caplam/Downloads/UNdata_WindElec/UNdata_WindElec.csv")
View(Wind) 

# dplyr package for filter function
library(dplyr)


### Africa ###
w_Africa <- filter(Wind, Country.or.Area %in% c("Algeria", "Cabo Verde", "Egypt", "Eritrea", "Ethiopia", "Gambia", "Kenya",
                                              "Mauritania", "Mauritius", "Morocco", "Namibia", "Reunion","Réunion", 
                                              "Seychelles", "South Africa", "St. Helena and Depend.", "Tunisia")) # creation of Africa data set
View(w_Africa)
fivenum(w_Africa$Quantity,na.rm = TRUE)
mean(w_Africa$Quantity) # Africa Quantity column mean
median((w_Africa$Quantity)) # Africa Quantity column median
y <- table(w_Africa$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Africa Quantity mode(s)
summary(w_Africa)


### Asia ###
w_Asia <- filter(Wind, Country.or.Area %in% c("Bangladesh", "Bhutan", "China", "India", "Indonesia", "Japan", "Kazakhstan",
                                            "Korea, Republic of", "Maldives", "Nepal", "Other Asia", "Philippines", 
                                            "Russian Federation", "Sri Lanka", "Thailand", "Vietnam")) # creation of Asia data set
View(w_Asia)
fivenum(w_Asia$Quantity,na.rm = TRUE)
mean(w_Asia$Quantity) # Asia Quantity column mean
median((w_Asia$Quantity)) # Asia Quantity column median
y <- table(w_Asia$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Asia Quantity mode(s)
summary(w_Asia)


### Caribbean ###
w_Caribbean <- filter(Wind, Country.or.Area %in% c("British Virgin Islands", "Cuba", "Dominican Republic", "Guadeloupe",
                                                 "Jamaica", "Martinique", "Puerto Rico", "St. Kitts-Nevis")) # creation of Caribbean data set
View(w_Caribbean)
fivenum(w_Caribbean$Quantity,na.rm = TRUE)
mean(w_Caribbean$Quantity) # Caribbean Quantity column mean
median((w_Caribbean$Quantity)) # Caribbean Quantity column median
y <- table(w_Caribbean$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Caribbean Quantity mode(s)
summary(w_Caribbean)


### Central America ###
w_C_America <- filter(Wind, Country.or.Area %in% c("Costa Rica", "Guatemala", "Honduras", "Nicaragua", "Panama")) # creation of Central America data set

View(w_C_America)
fivenum(w_C_America$Quantity,na.rm = TRUE)
mean(w_C_America$Quantity) # Central America Quantity column mean
median((w_C_America$Quantity)) # Central America Quantity column median
y <- table(w_C_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Central America Quantity mode(s)
summary(w_C_America)


### Europe ###
w_Europe <- filter(Wind, Country.or.Area %in% c("Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina",
                                                "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Faeroe Islands", 
                                                "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                                                "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", "Malta", "Montenegro", "Netherlands",
                                                "North Macedonia", "Norway", "Poland", "Portugal", "Republic of Moldova", "Romania",
                                                "Serbia","Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine",
                                                "United Kingdom")) # creation of Europe data set
View(w_Europe)
fivenum(w_Europe$Quantity,na.rm = TRUE)
mean(w_Europe$Quantity) # Europe Quantity column mean
median((w_Europe$Quantity)) # Europe Quantity column median
y <- table(w_Europe$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Europe Quantity mode(s)
summary(w_Europe)


### Middle East ###
w_Middle_E <- filter(Wind, Country.or.Area %in% c("Bahrain","Iran (Islamic Rep. of)", "Israel", "Jordan", "Pakistan", "Saudi Arabia",
                                                "Turkey", "Uzbekistan")) # creation of Middle East data set
View(w_Middle_E)
fivenum(w_Middle_E$Quantity,na.rm = TRUE)
mean(w_Middle_E$Quantity) # Middle East Quantity column mean
median((w_Middle_E$Quantity)) # Middle East Quantity column median
y <- table(w_Middle_E$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Middle East Quantity mode(s)
summary(w_Middle_E)


### North America ###
w_N_America <- filter(Wind, Country.or.Area %in% c("Canada", "Mexico", "St. Pierre-Miquelon", "United States")) # creation of North America data set

View(w_N_America)
fivenum(w_N_America$Quantity,na.rm = TRUE)
mean(w_N_America$Quantity) # North America Quantity column mean
median((w_N_America$Quantity)) # North America Quantity column median
y <- table(w_N_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # North America Quantity mode(s)
summary(w_N_America)


### Oceania ###
w_Oceania <- filter(Wind, Country.or.Area %in% c("Australia", "Fiji", "French Polynesia", "Guam",  "Micronesia (Fed. States of)",
                                                 "New Caledonia", "New Zealand", "Papua New Guinea", "Samoa", "Tonga", "Vanuatu")) # creation of Oceania data set
View(w_Oceania)
fivenum(w_Oceania$Quantity,na.rm = TRUE)
mean(w_Oceania$Quantity) # Oceania Quantity column mean
median((w_Oceania$Quantity)) # Oceania Quantity column median
y <- table(w_Oceania$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # Oceania Quantity mode(s)
summary(w_Oceania)


### South America ###
w_S_America <- filter(Wind, Country.or.Area %in% c("Argentina", "Aruba", "Bolivia (plur. State of)", "Bonaire, St Eustatius, Saba", "Brazil",
                                                 "Chile", "Colombia", "Curaçao", "Ecuador", "Falkland Is. (Malvinas)", "Guyana", "Peru",
                                                 "Uruguay",  "Venezuela (Bolivar. Rep.)")) # creation of South America data set
View(w_S_America)
fivenum(w_S_America$Quantity,na.rm = TRUE)
mean(w_S_America$Quantity) # South America Quantity column mean
median((w_S_America$Quantity)) # South America Quantity column median
y <- table(w_S_America$Quantity) # calculating mode
sort(table(y)) # gives summary of how many numbers have 1,2,3,etc. repeats
y
names(y)[which(y==max(y))] # South America Quantity mode(s)
summary(w_S_America)


### EDA Continental Wind Electricity ###
Africa_Wind <- ggplot(w_Africa, aes(Country.or.Area, Quantity, color = Year))
print(Africa_Wind + geom_point(size=3))

Asia_Wind <- ggplot(w_Asia, aes(Country.or.Area, Quantity, color = Year))
print(Asia_Wind + geom_point(size=3))

Caribbean_Wind <- ggplot(w_Caribbean, aes(Country.or.Area, Quantity, color = Year))
print(Caribbean_Wind + geom_point(size=3))

C_America_Wind <- ggplot(w_C_America, aes(Country.or.Area, Quantity, color = Year))
print(C_America_Wind + geom_point(size=3))

Europe_Wind <- ggplot(w_Europe, aes(Country.or.Area, Quantity, color = Year))
print(Europe_Wind + geom_point(size=3))

Middle_E_Wind <- ggplot(w_Middle_E, aes(Country.or.Area, Quantity, color = Year))
print(Middle_E_Wind + geom_point(size=3))

N_America_Wind <- ggplot(w_N_America, aes(Country.or.Area, Quantity, color = Year))
print(N_America_Wind + geom_point(size=3))

Oceania_Wind <- ggplot(w_Oceania, aes(Country.or.Area, Quantity, color = Year))
print(Oceania_Wind + geom_point(size=3))

S_America_Wind <- ggplot(w_S_America, aes(Country.or.Area, Quantity, color = Year))
print(S_America_Wind + geom_point(size=3))

All_Wind <- ggplot(Wind, aes(Country.or.Area, Quantity)) +
            geom_point(data=w_Oceania, color = "gold2", aes(colour = "Oceania")) +
            geom_point(data=w_N_America, color = "green") +
            geom_point(data=w_Middle_E, color = "orange") + 
            geom_point(data=w_Europe, color = "turquoise") +
            geom_point(data=w_Caribbean, color = "royalblue4") +
            geom_point(data=w_C_America, color = "purple") +
            geom_point(data=w_Asia, color = "deeppink1") + 
            geom_point(data=w_Africa, color = "chocolate4") +
            geom_point(data=w_S_America, color = "mediumorchid2") +
            theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5))
print(All_Wind)

All_Wind_yr <- ggplot(Wind, aes(Year, Quantity)) +
  geom_point(data=w_Oceania, color = "gold2", aes(colour = "Oceania")) +
  geom_point(data=w_N_America, color = "green") +
  geom_point(data=w_Middle_E, color = "orange") + 
  geom_point(data=w_Europe, color = "turquoise") +
  geom_point(data=w_Caribbean, color = "blue") +
  geom_point(data=w_C_America, color = "purple") +
  geom_point(data=w_Asia, color = "deeppink1") + 
  geom_point(data=w_Africa, color = "chocolate4") +
  geom_point(data=w_S_America, color = "mediumorchid2")
print(All_Wind_yr)


