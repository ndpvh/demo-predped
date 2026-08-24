library(predped)

################################################################################
# CREATING AN ENVIRONMENT

# Recreation of our office
office <- background(
    shape = rectangle(
        center = c(0, 0), 
        size = c(4.5, 5)
    ), 
    objects = list(
        # Desks
        rectangle(
            center = c(-0.85, 0), 
            size = c(2.4, 1.6)
        ), 
        # Cabinets
        rectangle(
            center = c(-1, -2.3), 
            size = c(1.2, 0.4)
        ), 
        rectangle(
            center = c(2.05, -1.9), 
            size = c(0.4, 1.2)
        ), 
        # Big bookcase
        rectangle(
            center = c(0.35, 2.275), 
            size = c(3, 0.45)
        ),
        # Plants
        circle(
            center = c(-2, -2.3), 
            radius = 0.15
        )
    ), 
    entrance = c(2.25, 1.3)
)

# Visualization of this office
plot(office)



################################################################################
# FORBIDDEN EDGES

# Create a non-interactable rectangle
my_rectangle <- rectangle(
    center = c(0, 0), 
    size = c(1, 1), 
    interactable = FALSE
)

# Define forbidden edges for a polygon
my_polygon <- polygon(
    points = rbind(
        c(1, 1), 
        c(1, -1), 
        c(-1, -1), 
        c(-1, 1)
    ), 
    forbidden = c(1, 3)
)

# Define the forbidden regions for a circle
my_circle <- circle(
    center = c(0, 0), 
    radius = 1, 
    forbidden = matrix(
        c(0, pi), 
        nrow = 1
    )
)

# Create and plot a version of the office that 
# contains forbidden locations
office <- background(
    shape = rectangle(
        center = c(0, 0), 
        size = c(4.5, 5)
    ), 
    objects = list(
        # Desks
        rectangle(
            center = c(-0.85, 0), 
            size = c(2.4, 1.6),
            forbidden = 1
        ), 
        # Cabinets
        rectangle(
            center = c(-1, -2.3), 
            size = c(1.2, 0.4), 
            forbidden = c(1, 3, 4)
        ), 
        rectangle(
            center = c(2.05, -1.9), 
            size = c(0.4, 1.2), 
            forbidden = 2:4
        ), 
        # Big bookcase
        rectangle(
            center = c(0.35, 2.275), 
            size = c(3, 0.45), 
            forbidden = c(1, 2, 3)
        ),
        # Plants
        circle(
            center = c(-2, -2.3), 
            radius = 0.15,
            forbidden = rbind(
                c(0, pi / 4),
                c(3 * pi / 4, 2 * pi)
            )
        )
    ), 
    entrance = c(2.25, 1.3)
)

plot(
    office, 
    plot_forbidden = TRUE, 
    forbidden.color = "red"
)



################################################################################
# DIRECTIONALITY

# Define a segment and plot its values
my_segment <- segment(
    from = c(0, 0), 
    to = c(1, 1)
)
my_segment



################################################################################
# PARAMETERS

# Load the default parameters of predped
parameters <- load_parameters()
View(parameters$params_archetypes)

# Connect a particular environment with a particular agent set
my_model <- predped(
    setting = supermarket, 
    archetypes = c(
        "BaselineEuropean", 
        "DrunkAussie"
    ),
    weights = c(0.75, 0.25)
)

# Check the slots in the parameter list
typeof(parameters)
names(parameters)

# Inspect the params_sigma slot
typeof(parameters$params_sigma)
names(parameters$params_sigma)
View(parameters$params_sigma$BaselineEuropean)

# Inspect the params_bounds slot
View(parameters$params_bounds)



################################################################################
# USING YOUR OWN PARAMETERS

# Do not run: Specification of predped with file
my_model <- predped(
    setting = supermarket, 
    filename = file.path("path", "to", "file")
)

# Read in default parameters
parameters <- load_parameters()

# Create a new parameter set for a fast pedestrian
new_parameters <- parameters$params_archetypes[1, ]
new_parameters$name <- "FastPedestrian"
new_parameters$preferred_speed <- 2

# Add this new parameter set to the original set
parameters$params_archetypes <- rbind(
    parameters$params_archetypes, 
    new_parameters
)

# Add a covariance for this pedestrian
new_covariance <- parameters$params_sigma$BaselineEuropean
parameters$params_sigma$FastPedestrian <- new_covariance

# Save the complete list in a local file
saveRDS(parameters, file.path("my_parameters.RDS"))

# Use these parameters in your predped specification
my_model <- predped(
    setting = supermarket, 
    archetype = "FastPedestrian",
    filename = file.path("my_parameters.RDS")
)



# Load your parameters
parameters <- readRDS(file.path("my_parameters.RDS"))

# Extract the parameter information for the
# FastPedestrian 
my_archetype <- "FastPedestrian"

means <- subset(
    parameters$params_archetypes,
    name == my_archetype
)
covariances <- parameters$params_sigma[[my_archetype]]
bounds <- parameters$params_bounds

# Use plot_distribution to visualize 
# individual differences
set.seed(1)
plot_distribution(
    1000,
    mean = means,
    Sigma = covariances,
    bounds = bounds
)



################################################################################
# PERFORMING A SIMULATION

# Example of a simulation with my_model
trace <- simulate(
    my_model, 
    max_agents = 3,
    iterations = 100
)

# Create plots of the trace
plt <- plot(trace)

# Save these separate plots into one comprehensive GIF
gifski::save_gif(
    lapply(plt, print),
    file.path("my_simulation.gif"),
    delay = 1/10
)



################################################################################
# INITIAL CONDITIONS

# Create an initial trace
trace <- simulate(
    my_model,
    max_agents = 3,
    iterations = 100
)

# Continue where the previous one left off
continued <- simulate(
    my_model,
    max_agents = 3,
    iterations = 100,
    initial_condition = trace
)

# Show that the end of the first and the beginning
# of the second are the same state
plt_1 <- plot(trace, iteration = 101)[[1]]
plt_2 <- plot(continued, iteration = 1)[[1]]

ggpubr::ggarrange(
    plt_1, 
    plt_2,
    ncol = 1
)



# Start the simulation with 3 agents in the room
trace <- simulate(
    my_model,
    max_agents = 3,
    iterations = 100,
    initial_number_agents = 3
)

# Show that the simulation starts with 3 agents in the
# room
plot(trace, iteration = 1)[[1]]



# Start the simulation with a particular agent in 
# the room
my_agent <- agent(
    id = "my agent",
    radius = 0.25,
    center = c(20, 10),
    orientation = 90,
    current_goal = goal(position = c(22, 22))
)

# Perform the simulation
trace <- simulate(
    my_model,
    max_agents = 3,
    iterations = 100,
    initial_agents = list(my_agent)
)

# Show that the simulation started out with that agent
plot(trace, iteration = 1)[[1]]

# Use a list of agents from this trace to continue from
continued <- simulate(
    my_model,
    max_agents = 3,
    iterations = 100,
    initial_agents = trace@states[[50]]
)

# Show that these are the same
plt_1 <- plot(trace, iteration = 50)[[1]]
plt_2 <- plot(continued, iteration = 1)[[1]]

ggpubr::ggarrange(
    plt_1, 
    plt_2,
    ncol = 1
)



################################################################################
# SITUATIONAL CHANGES

# Define a function that will start an evacuation procedure 
# at iteration 50
start_evacuation <- function(state) {
    # browser()

    # Check whether the current iteration is greater than 
    # 50. If not, then we do not want to change the state.
    if(iteration(state) == 50) {
        # Delete all agents that were waiting in line
        potential_agents(state) <- list()

        # Delete all goals of the agents that are in the 
        # office
        for(i in seq_along(agents(state))) {
            agent_i <- agents(state)[[i]]

            goals(agent_i) <- list()
            current_goal(agent_i)@counter <- 0
            status(agent_i) <- "completing goal"

            agents(state)[[i]] <- agent_i
        }

        # Make sure that agents cannot be added anymore
        # by lowering the maximal number of agents to 0
        iteration_variables(state)$max_agents <- 0
    }

    return(state)
}

# Change the predped-model to have the office instead
# of the supermarket
my_model <- predped(setting = office)

# Create an initial condition where Andrew and I are 
# working in our office
initial_agents <- list(
    agent(
        id = "Andrew", 
        center = c(-1, 1.2),
        radius = 0.25,
        speed = 0.25,
        orientation = 270,
        status = "completing goal",
        current_goal = goal(
            position = c(-1, 0.82),
            counter = 1000
        )
    ),
    agent(
        id = "Niels", 
        center = c(-1, -1.2),
        radius = 0.25,
        speed = 0.25,
        orientation = 90,
        status = "completing goal",
        current_goal = goal(
            position = c(-1, -0.82),
            counter = 1000
        )
    )
)

# Run the simulation itself
trace <- simulate(
    my_model,
    max_agents = 2,
    iterations = 100,
    initial_agents = initial_agents,
    fx = start_evacuation
)

# Check the timelapse of the number of agents 
# in the room
sapply(
    trace@states,
    length
)

# Also plot this scenario with visual indicator
# of when the evacuation started
plt <- plot(trace)
plt[[51]] <- plot(
    trace, 
    iteration = 51,
    shape.fill = "red"
)

gifski::save_gif(
    lapply(plt, print),
    file.path("evacuation.gif"),
    delay = 1/10
)
