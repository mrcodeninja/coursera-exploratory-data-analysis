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

plot4 <- function() {
    create_data_directory()
    download_and_unzip(
        url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
        destfile = "data/exdata-data-NEI_data.zip")
    
    NEI <- readRDS(file = "data/summarySCC_PM25.rds")
    NEI <- subset(
        x = NEI,
        select = c("SCC", "Emissions", "year", "type"))
    NEI <- transform(
        NEI,
        SCC = factor(SCC),
        type = factor(type))
    SCC <- readRDS(file = "data/Source_Classification_Code.rds")
    coal_combustion_related_scc <- subset(
        x = SCC,
        subset = EI.Sector %in% c("Fuel Comb - Comm/Institutional - Coal", "Fuel Comb - Electric Generation - Coal"),
        select = c("SCC", "EI.Sector"))
    coal_combustion_related_nei <- subset(
        x = NEI,
        subset = SCC %in% coal_combustion_related_scc$SCC,
        select = c("Emissions", "year"))
    coal_combustion_related_nei_aggregated <- with(
        data = coal_combustion_related_nei,
        aggregate(x = Emissions, by = list(year), FUN = sum))
    rm(SCC, NEI, coal_combustion_related_scc, coal_combustion_related_nei)

    names(coal_combustion_related_nei_aggregated) <- c("Year", "Emissions")
    
    png(filename = "plot4.png")

    require(package = ggplot2)
    options(scipen = 10000)
    g <- ggplot(
        data = coal_combustion_related_nei_aggregated,
        aes(Year, Emissions))
    p <- g +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(label = expression(atop(PM[2.5] * " Coal-Related Emissions Trend", atop("United States ")))) +
        ylab(label = "Total Emissions")
    print(p)
    
    dev.off()
    
    rm(coal_combustion_related_nei_aggregated)
}
