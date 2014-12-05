nei <- readRDS("rds/summarySCC_PM25.rds")
names(nei)<- tolower(names(nei))

scc <- readRDS("rds/Source_Classification_Code.rds")
names(scc)<- tolower(names(scc))


#plot1
annual_nei <- aggregate(nei$emissions, by=list(nei$year), sum)
plot(annual_nei, type="l", ylab="Total Emissions", xlab="Year", main="Annual Emissions")
text(annual_nei, labels=annual_nei$x)

#plot2
nei_24510 <- nei[which(nei$fips == "24510"),]
nei2 <- aggregate(nei_24510$emissions, by=list(nei_24510$year), sum)
plot(nei2, type="l", ylab="Total Emissions", xlab="Year", main="Baltimore City, Maryland Annual Emissions")
text(nei2, labels=nei2$x)

#plot3
nei_24510 <- nei[which(nei$fips == "24510"),]
nei_type <- with(nei_24510, aggregate(emissions, by=list(year, type), sum))
names(nei_type) <- c("year", "type", "value")
ggplot(data=nei_type, aes(x=year, y=value, colour=type ))+ geom_line(size = 2) + 
labs(x="Year", y=" The Value of Emissions") + 
labs(title = "Annual Emissions by Type")

#plot4
scc_coal<-scc[grep("Coal", scc$short.name), 1]
nei_scc <- nei[which(nei$scc %in% scc_coal),]
nei_scc_annual <- aggregate(nei_scc$emissions, by=list(nei_scc$year), sum)
plot(nei_scc_annual, type="l", ylab="Total Emissions", xlab="Year", main="Annual Emissions")

#plot5
scc_motor <- scc[grep("Motor", scc$short.name), 1]
nei_24510_scc_motor <- nei[which(nei$fips == "24510" & nei$scc %in% scc_motor),]
nei_scc_annual <- aggregate(nei_24510_scc_motor$emissions, by=list(nei_24510_scc_motor$year), sum)
plot(nei_scc_annual, type="l", ylab="Total Emissions", xlab="Year", main="From Motor Vehicle Annual Emissions")

#plot6
scc_motor <- scc[grep("Motor", scc$short.name), 1]
nei_scc_motor <- nei[which(nei$fips %in% c("24510","06037") & nei$scc %in% scc_motor),]
#nei_scc_motor <- subset(nei, fips %in% c("24510","06037") & scc %in% scc_motor)
nei_scc_annual <- aggregate(nei_scc_motor$emissions, by=list(nei_scc_motor$year, nei_scc_motor$fips), sum)
names(nei_scc_annual) <- c("year", "city", "value")
nei_scc_annual[which(nei_scc_annual$city=="06037"),2] <- "Los Angeles County"
nei_scc_annual[which(nei_scc_annual$city=="24510"),2] <- "Baltimore City"
ggplot(nei_scc_annual, aes(x=year, y=value, colour=city)) + geom_line(size = 2) + 
labs(x="Year", y=" The Value of Emissions") + 
labs(title = "Annual Emissions by Diffenrent City")


