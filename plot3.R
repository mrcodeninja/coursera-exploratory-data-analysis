create_data_directory <- function() {
    if (!file.exists("data")) {
        dir.create(path = "data")
    } 
}

download_and_unzip <- function(url, destfile) {
    if (!file.exists(destfile)) {
        download.file(url = url, destfile = destfile, method = "curl")
    }
    unzip(zipfile = destfile, exdir = "data")
}

plot3 <- function() {
    create_data_directory()
    download_and_unzip(
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
        destfile = "data/exdata-data-NEI_data.zip")

    NEI <- readRDS(file = "data/summarySCC_PM25.rds")
    NEI <- subset(
        x = NEI,
        subset = fips == "24510",
        select = c("Emissions", "year", "type"))
    NEI <- transform(NEI, type = factor(type))
    emissionsSumByYear <- with(
        data = NEI,
        aggregate(x = Emissions, by = list(year, type), FUN = sum))
    rm(NEI)
    names(emissionsSumByYear) <- c("Year", "Type", "Emissions")
    
    png(filename = "plot3.png")
    
    require(package = ggplot2)
    g <- ggplot(
        data = emissionsSumByYear,
        aes(Year, Emissions))
    p <- g +
        geom_point() +
        facet_grid(. ~ Type) +
        geom_smooth(method = "lm") +
        ggtitle(
            label = expression(atop(PM[2.5] * " Emissions Trend", atop("Baltimore City, Maryland")))) +
        ylab(label = "Total Emissions")
    print(p)
    
    dev.off()
    
    rm(emissionsSumByYear)
}
