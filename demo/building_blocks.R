library(predped)
library(ggplot2)

################################################################################
# USING THE CONSTRUCTOR

# Constructor `circle`, defined by a `center` and a `radius`
my_circle <- circle(
    center = c(0, 0), 
    radius = 1
)

# Constructor `rectangle`, defined by a `center` and a `size`
# containing the width and height respectively
my_rectangle <- rectangle(
    center = c(0, 0),
    size = c(2, 4)
)

# Constructor `polygon`, defined by a collection of `points`
# that make up the polygon (here a triangle)
my_polygon <- polygon(
    points = rbind(
        c(1, 1),
        c(2, 0), 
        c(0, 0)
    )
)



################################################################################
# ACCESSING AND CHANGING ATTRIBUTES

# Print the full contents of the rectangle
my_rectangle

# Retrieve the center of the rectangle first with the @-sign 
# and afterwards with its getter
my_rectangle@center
center(my_rectangle)

# Changing the value of the center of the rectangle with the 
# @-sign and afterwards with its setter
my_rectangle@center <- c(1, 1)
my_rectangle

center(my_rectangle) <- c(1, 1)
my_rectangle



################################################################################
# METHODS

# Compute the area of the rectangle and circle
area(my_rectangle)
area(my_circle)

# Define the coordinate of interest
coordinate <- c(0, 1)

# Loop over the three variables and evaluate whether 
# `coordinate` lies within them
sapply(
    list(my_circle, my_rectangle, my_polygon),
    function(x) in_object(x, coordinate)
)

# Try to compute the area of a `polygon.` Leads to an 
# error
area(my_polygon)



################################################################################
# AGENTS

# Create an object
my_object <- rectangle(
    center = c(0, 0),
    size = c(2, 2)
)

# Create an agent
my_agent <- agent(
    center = c(1, 0),
    radius = 0.25
)

# Check whether they intersect
intersects(my_agent, my_object)

# Create a visualization of this situation, confirming
# that they intersect
ggplot() +
    plot(my_object) + 
    plot(my_agent) +
    theme_minimal() +
    coord_equal()
