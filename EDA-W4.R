
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", temp)

unzip(temp)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



########################################## 1 ##########################################
      # Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? #

ex1 <- NEI  %>% group_by(year) %>% summarise(sum = sum(Emissions))

par(mar = c(5, 5, 5, 5))
barplot(height = ex1$sum, names.arg = ex1$year, horiz = F, beside = F, col = "steelblue", border = "grey", main = "")
title("Total PM2.5 emissions in the United States from 1999 to 2008", xlab = "Year", ylab = expression(PM[2.5]))

dev.copy(png, file="plot1w4.png", width=480, height=480)
dev.off()

with(ex1, plot(factor(year), factor(sum), col = "magenta", pch = 20, lwd = 2, lty = 1))


########################################## 2 ##########################################
    # Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?

ex2 <- NEI %>% filter(fips == "24510") %>% group_by(year) %>% summarise(sum = sum(Emissions))

barplot(height = ex2$sum, names.arg = ex2$year, horiz = F, beside = F, col = "steelblue", border = "grey", main = "")
title("Total PM2.5 emissions in Baltimore from 1999 to 2008", xlab = "Year", ylab = expression(PM[2.5]))

dev.copy(png, file="plot2w4.png", width=480, height=480)
dev.off()


########################################## 3 ##########################################
    # Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources
    # have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008?

ex3 <- NEI %>% filter(fips == "24510") %>% group_by(year, type) %>% summarise(sum = sum(Emissions)) %>% mutate()

# using base plotting system

with(subset(ex3, type == "NON-ROAD"), {
  plot(year, sum, col = "red", type = "l", lwd = 2)
  points(year, sum, col = "red", pch = 4)}
  )

with(subset(ex3, type == "ON-ROAD"), {
  lines(year, sum, col = "blue", type = "l", lwd = 2)
  points(year, sum, col = "blue", pch = 4)}
)
segments(1999, ex3$sum[1:4], 2002, ex3$sum[5:8], col = factor(ex3$type))
legend("topright", legend = unique(ex3$type), col = unique(factor(ex3$type)), pch  = 20)


# using ggplot2

g <- ggplot(data = ex3, aes(x = factor(year), y = sum)) 
g + geom_line(aes(group = type, color = type), lwd = 1)
  + geom_point(aes(group = type, color = type), lwd = 2) 
  + facet_grid(.~type)
  + labs(title="Total PM2.5 emissions in Baltimore by type of pollutant")
  + labs(x = "Year", y = "PM2.5 (tons)")


########################################## 4 ##########################################

      # Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

coal_sources <- SCC[grepl("coal", SCC$Short.Name, ignore.case = T),][,1]

rows <- NEI$SCC %in% coal_sources

ex4 <- NEI %>% filter(rows) %>% group_by(year) %>% summarise(sum = sum(Emissions)) %>% tbl_df

barplot(height = ex4$sum, names.arg = ex4$year, horiz = F, beside = F, col = "steelblue", border = "grey", main = "")
title("Total PM2.5 emissions in the United States from 1999 to 2008 from coal combustion-related sources")
title(xlab = "Year", ylab = expression(PM[2.5]))

dev.copy(png, file="plot4w4.png", width=480, height=480)
dev.off() 

# ex4a <- SCC %>% filter(grepl("coal", Short.Name, ignore.case = T))




########################################## 5 ##########################################

    # How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

motor_sources <- SCC[grepl("motor", SCC$Short.Name, ignore.case = T),][,1]

rows <- NEI$SCC %in% motor_sources

ex4 <- NEI %>% filter(rows) %>% group_by(year) %>% summarise(sum = sum(Emissions)) %>% tbl_df

barplot(height = ex4$sum, names.arg = ex4$year, horiz = F, beside = F, col = "steelblue", border = "grey", main = "")
title("Total PM2.5 emissions in the United States from 1999 to 2008 from motor vehicle sources")
title(xlab = "Year", ylab = expression(PM[2.5]))


dev.copy(png, file="plot5w4.png", width=480, height=480)
dev.off() 

