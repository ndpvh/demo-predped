devtools::load_all()

################################################################################
# SETUP

# Some setup parameters
N <- 2                  # Number of recoveries to run
time_step <- 0.5        # Time between iterations (inverse sampling rate)
iterations <- 10000     # Number of iterations in the simulations
max_agents <- 8         # Max number of agents in the room
goal_duration <- 5      # Number of iterations each goal takes
add_agent_after <- 10   # Number of iterations between agents coming in and going out
restart <- 3            # Number of times to restart the estimation procedure

# Use Andrew's environment for the recovery study
setting <- background(
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

# Define a function that replaces the participant's current goal with a randomly
# drawn new one. Will allow us to use only a single goal for every participant 
# and change it continuously.
change_goal <- function(state) {
    # Extract the agents from the state
    agents <- state@agents 

    # Loop over all agents
    for(i in seq_along(agents)) {
        # Check whether the agent's current goal is the exit goal.
        exit <- current_goal(agents[[i]])@id == "goal exit"

        # If this is the case, then we change this current goal and replace it 
        # with one randomly taken from the environment instead. Also update 
        # the agent's status and speed 
        if(exit) {
            current_goal(agents[[i]]) <- goal_stack(
                1, 
                state@setting, 
                counter = goal_duration
            )[[1]]
            status(agents[[i]]) <- "plan"
            speed(agents[[i]]) <- 0.25
        }
    }

    # Update the state and return
    state@agents <- agents 
    return(state)
}

# Create agent parameters, also taken from Andrew. Does the following
#   - Define bounds for the parameters
#   - Extract the BaselineEuropean and change their values to the middle of the 
#     interval
#   - Update the means, bounds, and sigma so that there is uniform sampling
limits <- rbind(
    preferred_speed = c(.75, 2),
    randomness = c(.01, .5),
    b_current_direction = c(1, 3),
    blr_current_direction = c(1/5, 5),
    b_goal_direction = c(3, 5),
    b_blocked = c(2, 4),
    b_interpersonal = c(1, 3),
    b_preferred_speed = c(2, 4),
    a_current_direction = c(1, 3),
    a_goal_direction = c(1, 3),
    a_blocked = c(1, 3),
    a_interpersonal = c(1, 3),
    a_preferred_speed = c(1, 3)
)

params <- params_from_csv

be <- params[[1]][params[[1]]$name == "BaselineEuropean",][]
be[row.names(limits)] <- limits[,1] + apply(limits, 1, diff) / 2

params[[1]][params[[1]]$name == "BaselineEuropean", ] <- be
params[[3]][row.names(limits), ] <- limits
diag(params[[2]][["BaselineEuropean"]]) <- 0
diag(params[[2]][["BaselineEuropean"]])[row.names(limits)] <- 1

saveRDS(
    params,
    file = file.path(
        "scripts", 
        "recovery_parameters.Rds"
    )
)

# Create the model to sample from
model <- predped(
    setting = setting,
    filename = file.path("scripts", "recovery_parameters.Rds"),
    archetypes = "BaselineEuropean"
)



################################################################################
# RECOVERY: BASED DIRECTLY ON TRACE

# Loop over the number of recoveries to do
set.seed(56) # White Marble - There's a Light
# results <- parallel::mclapply(
results <- lapply(
    1:N,
    function(i) {
        ###############
        # Simulation

        # Simulate a trace with individual differences on
        trace <- simulate(
            model,
            iterations = iterations,
            max_agents = max_agents,
            add_agent_after = add_agent_after,
            goal_number = 1,
            fx = change_goal,
            time_step = time_step,
            individual_differences = TRUE,
            print_iteration = TRUE,
            sort_goals = FALSE,
            initial_number_agents = max_agents
        )

        # Unpack the information contained in this trace
        data <- unpack_trace(trace)

        # Save the data as well
        saveRDS(
            data, 
            file.path("scripts", "data", paste0("basic_", i, ".Rds"))
        )
        saveRDS(
            trace,
            file.path("scripts", "data", paste0("trace_", i, ".Rds"))
        )



        ###############
        # Visualizing the simulation

        # Transform to a list of plots
        plt <- plot(
            trace,
            dark_mode = TRUE
        )

        # browser()

        # kaka <- trace@states[[4000]]
        # idx <- sapply(kaka, status) == "wait"
        # agents <- kaka[idx]
        # plot(trace@setting) + plot(agents)

        # Save as a gif
        gifski::save_gif(
            lapply(plt, print),
            file.path("scripts", "figures", paste0("basic_", i, ".gif")),
            delay = 1/100
        )



        ###############
        # Saving simulation results

        # Extract agents from the trace. Assumption: All agents
        # are still around in the final state of the trace
        agents <- trace@states[[iterations + 1]]

        # Create a data.frame in which the results will be saved
        results <- matrix(0, nrow = length(agents), ncol = 4 + 2 * nrow(limits)) |>
            as.data.frame() |>
            `colnames<-` (c(
                "iteration",
                "id", 
                "datapoints",
                paste0("true_", rownames(limits)), 
                paste0("est_", rownames(limits)),
                "objective"
            ))
        results$iteration <- i

        # Put agent id and their parameters in the results data.frame
        results$id <- sapply(agents, id)

        params <- sapply(agents, parameters)[rownames(limits), ] |>
            t() |>
            unlist() |>
            matrix(nrow = max_agents) |>
            `colnames<-` (NULL)
        idx <- grepl("true_", colnames(results), fixed = TRUE) |>
            which()
        results[, idx] <- params



        ###############
        # Estimation

        # Free up some memory before estimation
        rm("trace")
        gc()

        # Clean the data to retain only those rows that are of relevance to us.
        data <- data[!is.na(data$ps_speed), ]
        data <- data[data$cell != 0, ]

        # Retrieve the bounds of the parameters
        bounds <- readRDS(file.path("scripts", "recovery_parameters.Rds"))$params_bounds

        # Get the columns in which to put the estimates
        idx <- grepl("est_", colnames(results), fixed = TRUE) |>
            which()

        # Remove participants for whom there isn't enough data
        datapoints <- table(data$id)
        idy <- datapoints <= nrow(bounds) + 1

        results[idy, idx] <- NA
        results$objective[idy] <- NA 
        results$datapoints[idy] <- NA 

        id <- names(datapoints)

        # Loop over the number of participants
        for(j in seq_along(id)) {
            # Select the data to be used
            selection <- data[data$id == results$id[j], ]

            # If there is none left, indicate this in the dataframe and move on
            if(nrow(selection) <= nrow(bounds) + 1) {
                results[j, idx] <- NA 
                results$objective[j] <- NA 
                results$datapoints[j] <- NA

                next
            }

            # Define a variable that keeps track of the results from the 
            # objective function
            obj <- Inf

            # Define a variable that will contain the best parameters for the
            # subject
            params <- list()

            # Loop over the number of different starts that you want to do
            for(k in 1:restart) {
                cat("\rEstimation for participant ", j, ", Restart ", k)

                # Define an initial condition
                x0 <- runif(
                    nrow(bounds),
                    min = bounds[, 1],
                    max = bounds[, 2]
                ) |>
                    matrix(nrow = 1) |>
                    as.data.frame() |>
                    `colnames<-` (rownames(bounds)) |>
                    to_unbounded(bounds) |>
                    unlist() |>
                    `names<-` (NULL)

                # Perform optimization with optim
                optimized <- nloptr::nloptr(
                    x0,
                    function(x) mll(
                        selection, 
                        x,
                        transform = TRUE,
                        bounds = bounds,
                        summed = TRUE,
                        cpp = FALSE
                    ),
                    opts = list(
                        algorithm = "NLOPT_LN_NELDERMEAD",
                        maxeval = 250, 
                        xtol_abs = 1e-2, 
                        xtol_rel = 1e-2,
                        ftol_abs = 1e-5,
                        ftol_rel = 1e-5
                    )
                )

                # Check whether these parameters perform better than the previous
                # ones. If so, then we update them
                if(optimized$objective < obj) {
                    obj <- optimized$objective
                    params[[1]] <- optimized$solution
                }                
            }

            # Once the estimation procedure is over, we take a look at the final
            # solution and add it to the results matrix
            transformed <- params[[1]] |>
                matrix(nrow = 1) |>
                as.data.frame() |>
                `colnames<-` (rownames(bounds)) |>
                to_bounded(bounds) 
            results[j, idx] <- transformed[, rownames(limits)]

            # Also save the objective and move on
            results$objective[j] <- optimized$objective
            results$datapoints[j] <- nrow(selection)
        }

        cat("\n")

        return(results)
    }#,
    #mc.cores = min(c(parallel::detectCores() / 2 - 1, N))
)
results <- do.call("rbind", results)

# Visualize the results and save them
plt <- lapply(
    rownames(limits), 
    function(name) {
        # Define the columns and select the relevant results
        cols <- paste0(c("true_", "est_"), name)
        plot_data <- results[, cols] |>
            `colnames<-` (c("x", "y"))

        # Create the plot itself
        plt <- ggplot2::ggplot(
            data = plot_data, 
            ggplot2::aes(x = x, y = y)
        ) +
            ggplot2::geom_abline(
                intercept = 0, 
                slope = 1, 
                linewidth = 1,
                color = "black"
            ) +
            ggplot2::geom_point(
                color = "cornflowerblue",
                fill = "cornflowerblue",
                shape = 19,
                size = 2, 
                alpha = 0.5
            ) +
            ggplot2::labs(
                x = "True",
                y = "Estimated", 
                title = name
            ) +
            ggplot2::lims(
                x = limits[name, ],
                y = limits[name, ]
            ) +
            ggplot2::theme_minimal()

        return(plt)
    }
)
plt <- ggpubr::ggarrange(
    plotlist = plt, 
    nrow = 1
)

ggplot2::ggsave(
    file.path("scripts", "figures", "recovery_basic.png"),
    plt, 
    width = 750 * nrow(limits),
    height = 825,
    unit = "px"
)

# Save the results
write.table(
    results, 
    file.path("scripts", "results", "recovery_basic.txt")
)



################################################################################
# RECOVERY: RECONSTRUCT BASED ON DATA

# Loop over the number of recoveries to do
set.seed(224) # En In Gent - De Mens
results <- parallel::mclapply(
    1:N,
    function(i) {
        ###############
        # Simulation

        # Simulate a trace with individual differences on
        trace <- simulate(
            model,
            iterations = iterations,
            max_agents = max_agents,
            add_agent_after = add_agent_after,
            goal_number = 1,
            fx = change_goal,
            time_step = time_step,
            individual_differences = TRUE,
            print_iteration = TRUE,
            sort_goals = FALSE,
            initial_number_agents = max_agents
        )

        # Unpack the information contained in this trace
        data <- unpack_trace(trace)

        # Only retain the minimal amount of data and reconstruct the utility 
        # variables. Ideally, this would lead to a faithful reconstruction that 
        # can be used to estimate the parameters
        cat("\rReconstructing utility variables                         ")
        cols <- c("iteration", "time", "id", "x", "y", "goal_id", "goal_x", "goal_y")
        data <- data[, cols]

        data <- compute_utility_variables(
            data, 
            setting,
            time_step = time_step,
            cpp = TRUE
        )



        ###############
        # Visualizing the simulation

        # Transform to a list of plots
        plt <- plot(
            trace,
            dark_mode = TRUE
        )

        # Save as a gif
        gifski::save_gif(
            lapply(plt, print),
            file.path("scripts", "figures", paste0("reconstruct_", i, ".gif")),
            delay = 1/50
        )



        ###############
        # Saving simulation results

        # Extract agents from the trace. Assumption: All agents
        # are still around in the final state of the trace
        agents <- trace@states[[iterations + 1]]

        # Create a data.frame in which the results will be saved
        results <- matrix(0, nrow = length(agents), ncol = 4 + 2 * nrow(limits)) |>
            as.data.frame() |>
            `colnames<-` (c(
                "iteration",
                "id", 
                "datapoints",
                paste0("true_", rownames(limits)), 
                paste0("est_", rownames(limits)),
                "objective"
            ))
        results$iteration <- i

        # Put agent id and their parameters in the results data.frame
        results$id <- sapply(agents, id)

        params <- sapply(agents, parameters)[rownames(limits), ] |>
            t() |>
            unlist() |>
            matrix(nrow = max_agents) |>
            `colnames<-` (NULL)
        idx <- grepl("true_", colnames(results), fixed = TRUE) |>
            which()
        results[, idx] <- params



        ###############
        # Estimation

        # Free up some memory before estimation
        rm("trace")
        gc()

        # Clean the data to retain only those rows that are of relevance to us.
        data <- data[!is.na(data$ps_speed), ]
        data <- data[data$cell != 0, ]

        # Retrieve the bounds of the parameters
        bounds <- readRDS(file.path("scripts", "recovery_parameters.Rds"))$params_bounds

        # Get the columns in which to put the estimates
        idx <- grepl("est_", colnames(results), fixed = TRUE) |>
            which()

        # Remove participants for whom there isn't enough data
        datapoints <- table(data$id)
        idy <- datapoints <= nrow(bounds) + 1

        results[idy, idx] <- NA
        results$objective[idy] <- NA 
        results$datapoints[idy] <- NA 

        id <- names(datapoints)

        # Loop over the number of participants
        for(j in seq_along(id)) {
            # Select the data to be used
            selection <- data[data$id == results$id[j], ]

            # If there is none left, indicate this in the dataframe and move on
            if(nrow(selection) <= nrow(bounds) + 1) {
                results[j, idx] <- NA 
                results$objective[j] <- NA 
                results$datapoints[j] <- NA

                next
            }

            # Define a variable that keeps track of the results from the 
            # objective function
            obj <- Inf

            # Define a variable that will contain the best parameters for the
            # subject
            params <- list()

            # Loop over the number of different starts that you want to do
            for(k in 1:restart) {
                cat("\rEstimation for participant ", j, ", Restart ", k)

                # Define an initial condition
                x0 <- runif(
                    nrow(bounds),
                    min = bounds[, 1],
                    max = bounds[, 2]
                ) |>
                    matrix(nrow = 1) |>
                    as.data.frame() |>
                    `colnames<-` (rownames(bounds)) |>
                    to_unbounded(bounds) |>
                    unlist() |>
                    `names<-` (NULL)

                # Perform optimization with optim
                optimized <- nloptr::nloptr(
                    x0,
                    function(x) mll(
                        selection, 
                        x,
                        transform = TRUE,
                        bounds = bounds,
                        summed = TRUE,
                        cpp = FALSE
                    ),
                    opts = list(
                        algorithm = "NLOPT_LN_NELDERMEAD",
                        maxeval = 250, 
                        xtol_abs = 1e-2, 
                        xtol_rel = 1e-2,
                        ftol_abs = 1e-5,
                        ftol_rel = 1e-5
                    )
                )

                # Check whether these parameters perform better than the previous
                # ones. If so, then we update them
                if(optimized$objective < obj) {
                    obj <- optimized$objective
                    params[[1]] <- optimized$solution
                }                
            }

            # Once the estimation procedure is over, we take a look at the final
            # solution and add it to the results matrix
            transformed <- params[[1]] |>
                matrix(nrow = 1) |>
                as.data.frame() |>
                `colnames<-` (rownames(bounds)) |>
                to_bounded(bounds) 
            results[j, idx] <- transformed[, rownames(limits)]

            # Also save the objective and move on
            results$objective[j] <- optimized$objective
            results$datapoints[j] <- nrow(selection)
        }

        cat("\n")

        return(results)
    },
    mc.cores = min(c(parallel::detectCores() / 2 - 1, N))
)
results <- do.call("rbind", results)

# Visualize the results and save them
plt <- lapply(
    rownames(limits), 
    function(name) {
        # Define the columns and select the relevant results
        cols <- paste0(c("true_", "est_"), name)
        plot_data <- results[, cols] |>
            `colnames<-` (c("x", "y"))

        # Create the plot itself
        plt <- ggplot2::ggplot(
            data = plot_data, 
            ggplot2::aes(x = x, y = y)
        ) +
            ggplot2::geom_abline(
                intercept = 0, 
                slope = 1, 
                linewidth = 1,
                color = "black"
            ) +
            ggplot2::geom_point(
                color = "cornflowerblue",
                fill = "cornflowerblue",
                shape = 19,
                size = 2, 
                alpha = 0.5
            ) +
            ggplot2::labs(
                x = "True",
                y = "Estimated", 
                title = name
            ) +
            ggplot2::lims(
                x = limits[name, ],
                y = limits[name, ]
            ) +
            ggplot2::theme_minimal()

        return(plt)
    }
)
plt <- ggpubr::ggarrange(
    plotlist = plt, 
    nrow = 1
)

ggplot2::ggsave(
    file.path("scripts", "figures", "recovery_reconstruct.png"),
    plt, 
    width = 750 * nrow(limits),
    height = 825,
    unit = "px"
)

# Save the results
write.table(
    results, 
    file.path("scripts", "results", "recovery_reconstruct.txt")
)
