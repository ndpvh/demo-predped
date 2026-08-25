# Define the room, model, and simulation
my_room <- background(
    shape = rectangle(
        center = c(0, 0),
        size = c(10, 6)
    ),
    objects = list(
        rectangle(
            center = c(-2.5, -1.5),
            size = c(3, 0.25)
        ),
        rectangle(
            center = c(2.5, -1.5),
            size = c(3, 0.25)
        ),
        rectangle(
            center = c(2.5, 1.5),
            size = c(3, 0.25)
        ),
        rectangle(
            center = c(-2.5, 1.5),
            size = c(3, 0.25)
        ),
        rectangle(
            center = c(2.5, 0),
            size = c(3, 0.25)
        ),
        rectangle(
            center = c(-2.5, 0),
            size = c(3, 0.25)
        )
    ),
    entrance = c(0, -3)
)

my_model <- predped(
    setting = my_room, 
    archetypes = "BaselineEuropean"
)

trace <- simulate(
    my_model,
    iterations = 100,
    max_agents = 5,
    initial_number_agents = 5
)

# Visualize
plt <- plot(trace)
gifski::save_gif(
    lapply(plt, print),
    file.path("data", "trace.gif"),
    delay = 1/10
)

# Save these data as .txt
data <- time_series(trace)
data <- data[, c("time", "id", "x", "y", "goal_x", "goal_y", "goal_id")]
data$id <- data$id |>
    as.factor() |>
    as.numeric()
data <- data[order(data$time, data$id), ]

write.table(
    data, 
    file.path("data", "data.txt"),
    sep = ","
)

# Also save the trace
saveRDS(
    trace,
    file.path("data", "trace.RDS")
)
