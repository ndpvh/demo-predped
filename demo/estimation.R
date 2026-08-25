library(predped)
library(DEoptim)

################################################################################
# PREPARING DATA

# Read in some prepared data that contains all 
# information necessary to compute the utility
# information. Note that these data are far too
# sparse to lead to good recovery of the M4MA's 
# parameters!
data <- read.table(
    file.path("data", "data.txt"),
    sep = ","
)

# Recreate the room in which these data were 
# gathered
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

# Starting from data, use compute_utility_variables
# to add the necessary information
utility_data <- compute_utility_variables(
    data,
    my_room
)

# Show all columns in this dataset
colnames(utility_data)



# Read in the trace
trace <- readRDS(file.path("data", "trace.RDS"))

# Unpack the information contained in the trace
# and transform to a data.frame
utility_data <- unpack_trace(trace)

# Show all columns in this dataset
colnames(utility_data)



# Check the number of missing values in the 
# utility_data
sum(is.na(utility_data$check))

# Delete these missing values from the data
utility_data <- subset(
    utility_data,
    !is.na(check)
)


################################################################################
# ESTIMATION

# Select the utility_data of only a single person
utility_data_1 <- subset(
    utility_data, 
    id == "pjwmu"
)

# Define the objective function, which is a wrapper
# around the mll() function
objective_function <- function(x) {
    return(
        mll(
            utility_data_1,
            x,
            transform = FALSE,
            summed = TRUE
        )
    )
}

# Defing the bounds of the parameters, here using
# the default bounds of predped
bounds <- load_parameters()$params_bounds

# Estimate the parameters of the M4MA with DEoptim
parameters <- DEoptim(
    objective_function,
    bounds[,1],
    bounds[,2],
    control = DEoptim.control(
        NP = 400,
        itermax = 100,
        CR = 0.6,
        trace = TRUE
    )
)
