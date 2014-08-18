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

plot2 <- function() {
    create_data_directory()
    download_and_unzip(
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
        destfile = "data/exdata-data-NEI_data.zip")
    
    NEI <- readRDS(file = "data/summarySCC_PM25.rds")
    NEI <- subset(
        x = NEI,
        subset = fips == "24510",
        select = c("Emissions", "year"))
    emissionsSumByYear <- with(
        NEI,
        aggregate(x = Emissions, by = list(year), FUN = sum))
    rm(NEI)
    names(emissionsSumByYear) <- c("Year", "Emissions")
    
    png(filename = "plot2.png")
    
    opt <- options(scipen = 100)
    op <- par(mar = c(5, 7, 4, 2) + 0.1)
    
    with(data = emissionsSumByYear,
         plot(
             x = Year,
             y = Emissions,
             type = "b",
             main = expression(atop(PM[2.5] * " Emissions Trend", atop("Baltimore City, Maryland"))),
             ylab = "",
             las = 1)
    )
    
    title(ylab = "Total Emissions", line = 5.5)
    par(op)
    options(opt)
    
    dev.off()
    
    rm(emissionsSumByYear)
}
