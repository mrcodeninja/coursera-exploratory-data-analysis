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

plot6 <- function() {
    create_data_directory()
    download_and_unzip(
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
        destfile = "data/exdata-data-NEI_data.zip")
    
    NEI <- readRDS(file = "data/summarySCC_PM25.rds")
    NEI <- subset(
        x = NEI,
        subset = fips == "24510" | fips == "06037",
        select = c("fips", "SCC", "Emissions", "year", "type"))
    NEI <- transform(
        NEI,
        fips = factor(fips),
        SCC = factor(SCC),
        type = factor(type))
    SCC <- readRDS(file = "data/Source_Classification_Code.rds")
    motor_vehicles_logical <- grepl(
        pattern = "vehicle",
        x = SCC$EI.Sector,
        ignore.case = TRUE)
    motor_vehicles_scc <- subset(
        x = SCC,
        subset = grepl(pattern = "vehicle", x = EI.Sector, ignore.case = TRUE),
        select = c("SCC", "EI.Sector"))
    motor_vehicles_nei <- subset(
        x = NEI,
        subset = SCC %in% motor_vehicles_scc$SCC,
        select = c("Emissions", "year", "fips"))
    motor_vehicles_nei_aggregated <- with(
        data = motor_vehicles_nei,
        aggregate(x = Emissions, by = list(year, fips), FUN = sum))
    rm(SCC, NEI, motor_vehicles_scc, motor_vehicles_nei)
    
    names(motor_vehicles_nei_aggregated) <- c("Year", "FIPS", "Emissions")
    levels(motor_vehicles_nei_aggregated$FIPS) <- c("Los Angeles County, California", "Baltimore City, Maryland")
    
    png(filename = "plot6.png")

    require(package = ggplot2)
    options(scipen = 10000)
    g <- ggplot(
        data = motor_vehicles_nei_aggregated,
        aes(Year, Emissions))
    p <- g +
        geom_point() +
        facet_grid(. ~ FIPS) +
        geom_smooth(method = "lm") +
        ggtitle(label = expression(atop(PM[2.5] * " Motor Vehicle Emissions Trend", atop("Los Angeles County, California & Baltimore City, Maryland")))) +
        ylab(label = "Total Emissions")
    print(p)
    
    dev.off()
    
    rm(motor_vehicles_nei_aggregated)
}
