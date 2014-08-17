create_data_directory <- function() {
    if (!file.exists("data")) {
        dir.create("data")
    } 
}

download_and_unzip <- function(url, destfile) {
    if (!file.exists(destfile)) {
        download.file(url, destfile, method = "curl")
    }
    unzip(zipfile = destfile, exdir = "data")
}

load_libraries <- function() {
    installed_packages <- installed.packages()[, 1]
    if (!is.element("ggplot2", installed_packages)) {
        cat("Installing ggplot2 package...\n")
        install.packages("ggplot2")
    }
    library(ggplot2)
}

plot3 <- function() {
    create_data_directory()
    download_and_unzip(
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
        destfile = "data/exdata-data-NEI_data.zip")
    
    NEI <- readRDS("data/summarySCC_PM25.rds")
    NEI <- NEI[NEI$fips == "24510", c("Emissions", "year", "type")]
    NEI <- transform(NEI, type = factor(type))
    emissionsSumByYear <- with(NEI, aggregate(x = Emissions, by = list(year, type), sum))
    rm(NEI)
    names(emissionsSumByYear) <- c("Year", "Type", "Emissions")
    
    png("plot3.png")
    
    g <- ggplot(
        data = emissionsSumByYear,
        aes(Year, Emissions))
    p <- g + geom_point() + facet_grid(. ~ Type) + geom_smooth(method = "lm") + ggtitle(label = expression("Baltimore City, Maryland " * PM[2.5] * " Emissions Trend")) + ylab(label = "Total Emissions")
    print(p)
    
    dev.off()
    
    rm(emissionsSumByYear)
}
