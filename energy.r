library(ggplot2)
theme_set(theme_bw())
library("sf")
library(rnaturalearth)

# Energy consumption data
data <- read.csv("data/energy.csv")

data <- merge(
    data,
    subset(
        read.csv("data/population.csv"),
        select = c(
            Entity,
            Year,
            Population
        )
    ),
    by = c("Entity", "Year"),
    all = TRUE
    )
data <- data[data$Year == 2021, ]

g <- c(
    "Asia",
    "America",
    "CIS",
    "Europe",
    "Oceania",
    "income",
    "Small",
    "Middle",
    "OECD",
    "USSR",
    "regions",
    "countries"
)
for (c in g)
    data <- data[!grepl(c, data$Entity), ]


data <- data[data$Entity != "Africa", ]
data <- data[data$Entity != "Africa (BP)", ]
data <- data[data$Entity != "Africa (UN)", ]
data <- data[data$Entity != "Eastern Africa", ]
data <- data[data$Entity != "Eastern Africa (BP)", ]
data <- data[data$Entity != "Western Africa", ]
data <- data[data$Entity != "Western Africa (BP)", ]

data$Entity[data$Entity == "Democratic Republic of Congo"
] <- "Democratic Republic of the Congo"

data$Entity[data$Entity == "Sao Tome and Principe"
] <- "São Tomé and Principe"

data$Renewable.Consumption <- data$Other.Consumption +
    data$Biofuels.Consumption +
    data$Solar.Consumption +
    data$Wind.Consumption +
    data$Hydro.Consumption

data$Fossil.Fuel.Consumption <- data$Gas.Consumption +
    data$Coal.Consumption +
    data$Oil.Consumption

data$Total.Consumption <- data$Renewable.Consumption +
    data$Fossil.Fuel.Consumption +
    data$Nuclear.Consumption

# TWh -> KWh (*1000000000)
data$Consumption.per.capita <- (
    data$Total.Consumption / data$Population) * 1000000000

# Source relative to total generation
data$Renewable.Share <- (data$Renewable.Consumption /
    data$Total.Consumption) * 100
data$Fossil.Fuel.Share <- (data$Fossil.Fuel.Consumption /
    data$Total.Consumption) * 100
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
world <- world[world$sovereignt != "Antarctica", ]

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
    geom_sf(
        aes(fill = Renewable.Share)) +
    scale_fill_viridis_c(
        name = "%",
        option = "F",
        # trans = "sqrt"
        # trans = scales::pseudo_log_trans(sigma = 0.1)
    ) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(
        legend.position = "bottom",
        legend.key.width = unit(3.5, "cm")
        ) +
    guides(fill = guide_colorbar(title.position = "top")) +
    ggtitle("Renewable energy consumed relative to total consumption (2021)")

plot(g)

ggsave(
    "imgs/energy_consumption_renewable_share.png",
    g,
    width = 15,
    height = 8,
    dpi = 300
    )