library(ggplot2)
theme_set(theme_bw())
library("sf")
library(rnaturalearth)

# Energy consumption data
data <- read.csv("data/energy.csv")
data <- data[data$Year == 2021, ]

g <- c(
    "Antarctica",
    "Asia",
    "America",
    "CIS",
    "Europe",
    "Eastern Africa",
    "Western Africa",
    "income",
    "Middle",
    "OECD",
    "USSR"
)
for (c in g){
    data <- data[!grepl(c, data$Entity), ]
}

data <- data[data$Entity != "Africa", ]
data <- data[data$Entity != "Africa (BP)", ]

# TODO: Find a better way to do this
data$Total.Consumption <- data$Other.Consumption +
    data$Biofuels.Consumption
data$Total.Consumption <- data$Total.Consumption + data$Solar.Consumption
data$Total.Consumption <- data$Total.Consumption + data$Wind.Consumption
data$Total.Consumption <- data$Total.Consumption + data$Hydro.Consumption
data$Total.Consumption <- data$Total.Consumption + data$Nuclear.Consumption
data$Total.Consumption <- data$Total.Consumption + data$Gas.Consumption
data$Total.Consumption <- data$Total.Consumption + data$Coal.Consumption
data$Total.Consumption <- data$Total.Consumption + data$Oil.Consumption

data$Biofuels.Share <- (data$Biofuels.Consumption /
    data$Total.Consumption) * 100
data$Solar.Share <- (data$Solar.Consumption /
    data$Total.Consumption) * 100
data$Wind.Share <- (data$Wind.Consumption /
    data$Total.Consumption) * 100
data$Hydro.Share <- (data$Hydro.Consumption /
    data$Total.Consumption) * 100
data$Nuclear.Share <- (data$Nuclear.Consumption /
    data$Total.Consumption) * 100
data$Gas.Share <- (data$Gas.Consumption /
    data$Total.Consumption) * 100
data$Coal.Share <- (data$Coal.Consumption /
    data$Total.Consumption) * 100
data$Oil.Share <- (data$Oil.Consumption /
    data$Total.Consumption) * 100
data$Other.Share <- (data$Other.Consumption /
    data$Total.Consumption) * 100

data$Total.Share <- (data$Total.Consumption /
    data[data$Entity == "World", ]$Total.Consumption) * 100

data <- data[!grepl("World", data$Entity), ]

world <- ne_countries(
    scale = "large",
    type = "sovereignty",
    returnclass = "sf"
)

world$sovereignt[
    world$sovereignt == "United States of America"] <- "United States"

names(world)[names(world) == "sovereignt"] <- "Entity"

world <- subset(
    world,
    select = c(
        Entity,
        geometry,
        pop_est,
        gdp_md
    )
)

world <- merge(world, data, by = "Entity", all = TRUE)
world <- subset(
    world,
    select = -c(
        Code,
        Year
    )
)

g <- ggplot(data = world) +
    geom_sf(aes(fill = Total.Share)) +
    scale_fill_viridis_c(
        option = "F",
        # trans = "sqrt"
        trans = scales::pseudo_log_trans(sigma = 0.5)
    ) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(legend.position = "bottom", legend.key.width = unit(3.5, "cm")) +
    ggtitle("Share of energy consumption (TWh) per country")

plot(g)

ggsave("imgs/energy_consumption_share.png", g)