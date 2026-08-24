library(predped)

# Create a function that will make a cash register. Used for recreating the 
# supermarket in Delft
cash_register <- function(center, 
                          flipped = FALSE, 
                          orientation = 0) {

    # Create the default cash register. 
    points <- rbind(
        c(0, 0),
        c(0, 1.24),
        c(4, 1.24),
        c(4, 0),
        c(3.01, 0),
        c(3.01, 1.22), 
        c(1.81, 1.22),
        c(1.81, 0)
    )

    # Determine whether the cash register should be flipped. This means that the 
    # space where the cashier sits is put at a different location. The flipping 
    # happens vertically.
    if(flipped) {
        points[,2] <- max(points[,2]) - points[,2]
    }

    # Center the points around (0, 0). Needed for the rotation to take effect.
    points[,1] <- points[,1] - 2
    points[,2] <- points[,2] - 0.62

    # Determine whether the cash register should be rotated to some degree. 
    R <- matrix(
        c(cos(orientation), sin(orientation), -sin(orientation), cos(orientation)),
        nrow = 2,
        ncol = 2
    )

    points <- t(R %*% t(points)) 

    # Move the cash register to the center indicated by the user.
    points[,1] <- points[,1] + center[1]
    points[,2] <- points[,2] + center[2]

    return(points)
}

# An example of a supermarket based on a supermarket in Delft
supermarket <- background(
    shape = polygon(
        points = rbind(
            c(0, 0),
            c(0, 35),
            c(45, 35),
            c(45, 27),
            c(49, 23),
            c(49, 23),
            c(49, 0)
        )
    ),
    # Objects in the environment
    objects = list(
        # Cash registers. Importantly, left some space for potential cashiers 
        # who need to interact with the costumers.
        #
        # To ensure that agents won't step into the way of other agents at these 
        # registers, we create some spacers.
        polygon(
            id = "surface: cash register 1",
            points = cash_register(
                c(43, 0.62), 
                flipped = FALSE,
                orientation = 0
            ),
            forbidden = 1:8
        ),
        rectangle(
            id = "spacer cash registers 1 and 2",
            center = c(43, 2.48),
            size = c(4, 0.04),
            forbidden = 1:4
        ),
        polygon(
            id = "surface: cash register 2",
            points = cash_register(
                c(43, 4.34), 
                flipped = TRUE,
                orientation = 0
            ),
            forbidden = 1:8
        ),

        polygon(
            id = "surface: cash register 3",
            points = cash_register(
                c(43, 5.58), 
                flipped = FALSE,
                orientation = 0
            ),
            forbidden = 1:8
        ),
        rectangle(
            id = "spacer cash registers 3 and 4",
            center = c(43, 7.44),
            size = c(4, 0.04),
            forbidden = 1:4
        ),
        polygon(
            id = "surface: cash register 4",
            points = cash_register(
                c(43, 9.3), 
                flipped = TRUE,
                orientation = 0
            ),
            forbidden = 1:8
        ),

        polygon(
            id = "surface: cash register 5",
            points = cash_register(
                c(43, 10.54), 
                flipped = FALSE,
                orientation = 0
            ),
            forbidden = 1:8
        ),
        rectangle(
            id = "spacer cash registers 5 and 6",
            center = c(43, 12.40),
            size = c(4, 0.04),
            forbidden = 1:4
        ),
        polygon(
            id = "surface: cash register 6",
            points = cash_register(
                c(43, 14.26), 
                flipped = TRUE,
                orientation = 0
            ),
            forbidden = 1:8
        ),
        

        # Entrance. Consists of two one-way streets and a few walls that ensure
        # they have to pass through the cash registers.
        polygon(
            points = rbind(
                c(39, 28),
                c(40, 28),
                c(40, 20),
                c(42, 18),
                c(42, 14.88),
                c(41, 14.88),
                c(41, 18),
                c(39, 20)
            ),
            forbidden = 1:8
        ),
        rectangle(
            center = c(39.5, 33),
            size = c(1, 4),
            forbidden = 1:4
        ),
        rectangle(
            center = c(39.5, 29.5),
            size = c(1, 0.1),
            forbidden = 1:4
        ),


        # Walls and beams. Break the flow of the customer
        circle(
            center = c(22, 21.6),
            radius = 0.4,
            forbidden = matrix(c(0, 2 * pi), nrow = 1)
        ),
        circle(
            center = c(22, 18.8),
            radius = 0.4,
            forbidden = matrix(c(0, 2 * pi), nrow = 1)
        ),
        circle(
            center = c(22, 16.4),
            radius = 0.4,
            forbidden = matrix(c(0, 2 * pi), nrow = 1)
        ),
        circle(
            center = c(22, 13.6),
            radius = 0.4,
            forbidden = matrix(c(0, 2 * pi), nrow = 1)
        ),
        circle(
            center = c(22, 10.8),
            radius = 0.4,
            forbidden = matrix(c(0, 2 * pi), nrow = 1)
        ),


        # Top half: These consist of several irregular forms due to the produce 
        # they sell, such as the bakery, the deli, and vegetables
        rectangle(
            id = "bakery 1",
            center = c(9.3, 34.6),
            size = c(8.6, 0.8),
            forbidden = 1:3
        ),
        polygon(
            id = "bakery 2",
            points = rbind(
                c(4, 33.4),
                c(5, 33.4),
                c(5, 27.4),
                c(1.4, 27.4),
                c(1.4, 28.4),
                c(4, 28.4)
            ),
            forbidden = c(1, 4:6)
        ),
        rectangle(
            id = "bakery 3",
            center = c(0.4, 22),
            size = c(0.8, 10),
            forbidden = c(1:2, 4)
        ),
        rectangle(
            id = "bakery 4",
            center = c(10, 29.6),
            size = c(4.2, 1.6)
        ),

        rectangle(
            id = "meat 1",
            center = c(20.6, 34.6),
            size = c(10, 0.8),
            forbidden = 1:3
        ),
        rectangle(
            id = "meat 2",
            center = c(17.6, 30.4),
            size = c(7.4, 1.8)
        ),
        rectangle(
            id = "meat 3",
            center = c(21, 26.2),
            size = c(6.8, 1.8)
        ),

        polygon(
            id = "deli 1",
            points = rbind(
                c(6, 21.6),
                c(6, 24.8),
                c(6.6, 25.4),
                c(14.8, 25.4),
                c(15.4, 24.8),
                c(15.4, 21.6),
                c(14, 21.6),
                c(14, 24),
                c(7.4, 24),
                c(7.4, 21.6)
            ),
            forbidden = 6:10
        ),
        polygon(
            id = "deli 2",
            points = rbind(
                c(6.6, 19.6),
                c(6.6, 20.4),
                c(10, 20.4),
                c(10, 23.2),
                c(11.6, 23.2),
                c(11.6, 20.4),
                c(15, 20.4),
                c(15, 19.6)
            ),
            forbidden = 1:7
        ),

        rectangle(
            id = "ready to eat",
            center = c(33.2, 34.6),
            size = c(13.6, 0.8),
            forbidden = 1:3
        ),

        rectangle(
            id = "fruit and vegetables 1",
            center = c(31.90, 30.4),
            size = c(7.8, 2.2)
        ),
        rectangle(
            id = "fruit and vegetables 2",
            center = c(32, 26.2),
            size = c(8, 2.2)
        ),
        rectangle(
            id = "fruit and vegetables 3",
            center = c(31.6, 22.6),
            size = c(0.8, 0.8)
        ),
        rectangle(
            id = "fruit and vegetables 4",
            center = c(33.8, 22.6),
            size = c(0.8, 0.8)
        ),
        rectangle(
            id = "fruit and vegetables 5",
            center = c(36, 22.6),
            size = c(0.8, 0.8)
        ),
        rectangle(
            id = "fruit and vegetables 6",
            center = c(38.4, 21),
            size = c(0.8, 2.4),
            forbidden = 2:4
        ),

        rectangle(
            id = "unknown purpose 1",
            center = c(3, 22),
            size = c(1.2, 1.8)
        ),
        rectangle(
            id = "unknown purpose 2",
            center = c(24.8, 30.4),
            size = c(1.8, 2.6)
        ),
        rectangle(
            id = "unknown purpose 3",
            center = c(18.9, 22.6),
            size = c(1.8, 1.2)
        ),
        rectangle(
            id = "unknown purpose 4",
            center = c(25.10, 22.6),
            size = c(1.8, 1.2)
        ),
        rectangle(
            id = "unknown purpose 5",
            center = c(28.7, 22.6),
            size = c(1.8, 1.2)
        ),


        # Lower half: Mostly consists of the typical aisles.
        rectangle(
            id = "dairy",
            center = c(0.4, 7.2),
            size = c(0.8, 14.4),
            forbidden = c(1:2, 4)
        ),

        polygon(
            id = "drinks 1",
            points = rbind(
                c(0.8, 0),
                c(0.8, 0.8),
                c(13.2, 0.8),
                c(13.2, 3.4),
                c(14, 3.4),
                c(14, 0.8),
                c(21.4, 0.8),
                c(21.4, 3.4),
                c(22.2, 3.4),
                c(22.2, 0.8),
                c(23.4, 0.8),
                c(23.4, 0)
            ),
            forbidden = c(1, 11:12)
        ),
        rectangle(
            id = "drinks 2",
            center = c(4.6, 4.3),
            size = c(0.8, 3.4)
        ),
        rectangle(
            id = "drinks 3",
            center = c(7.6, 4.3),
            size = c(0.8, 3.4)
        ),
        rectangle(
            id = "drinks 4",
            center = c(10.6, 4.3),
            size = c(0.8, 3.4)
        ),
        rectangle(
            id = "drinks 5",
            center = c(16.6, 4.3),
            size = c(0.8, 3.4)
        ),
        rectangle(
            id = "drinks 6",
            center = c(19.6, 3.6),
            size = c(0.8, 1.6)
        ),
        rectangle(
            id = "drinks 7",
            center = c(13.6, 5.2),
            size = c(0.8, 1.6)
        ),
        rectangle(
            id = "drinks 8",
            center = c(19.6, 5.9),
            size = c(1, 1)
        ),

        rectangle(
            id = "left aisle 1",
            center = c(12.2, 9.4),
            size = c(16, 0.8)
        ),
        rectangle(
            id = "left aisle 2",
            center = c(12, 12.4),
            size = c(16.4, 0.8)
        ),
        rectangle(
            id = "left aisle 3",
            center = c(12.2, 15.2),
            size = c(16, 0.8)
        ),
        rectangle(
            id = "left aisle 4",
            center = c(8.2, 18),
            size = c(8, 0.8)
        ),
        polygon(
            id = "left aisle 5",
            points = rbind(
                c(15, 19.6),
                c(15.8, 19.6),
                c(15.8, 18.4),
                c(20.2, 18.4),
                c(20.2, 17.6),
                c(15, 17.6)
            )
        ),
        rectangle(
            id = "left aisle 6",
            center = c(18.9, 20),
            size = c(2.6, 0.8)
        ),

        rectangle(
            id = "freezer 1",
            center = c(27.4, 0.4),
            size = c(6, 0.8),
            forbidden = c(1, 3:4)
        ),
        rectangle(
            id = "freezer 2",
            center = c(37.2, 0.4),
            size = c(6, 0.8),
            forbidden = c(1, 3:4)
        ),
        rectangle(
            id = "freezer 3",
            center = c(34.2, 3.6),
            size = c(6.8, 1.6)
        ),
        rectangle(
            id = "freezer 4",
            center = c(31.4, 6.6),
            size = c(13.6, 0.8)
        ),

        rectangle(
            id = "right aisle 1",
            center = c(30.8, 9.4),
            size = c(14.2, 0.8)
        ),
        rectangle(
            id = "right aisle 2",
            center = c(30.2, 12.4),
            size = c(13, 0.8)
        ),
        rectangle(
            id = "right aisle 3",
            center = c(30.2, 15.2),
            size = c(13.6, 0.8)
        ),
        rectangle(
            id = "right aisle 4",
            center = c(29.8, 18),
            size = c(12.2, 0.8)
        ),
        rectangle(
            id = "right aisle 5",
            center = c(30.8, 20),
            size = c(14.4, 0.8)
        )

    ),

    # One-directional flow. Limited to cash registers and entrances in this free
    # flow version
    limited_access = list(
        # Cash registers
        segment(
            from = c(45, 14.88),
            to = c(45, 0)
        ),

        # Entrance
        segment(
            from = c(39, 28),
            to = c(39, 31)
        ),

        # Forbidden regions throughout the store
        segment(
            from = c(5, 34.2),
            to = c(5, 33.4)
        ),
        segment(
            from = c(1.4, 27.4),
            to = c(0.8, 27)
        ),
        segment(
            from = c(6.6, 20.4),
            to = c(6.66, 21.6)
        ),
        segment(
            from = c(15, 21.6),
            to = c(15, 20.4)
        )
    ),

    # Entrances and exits
    entrance = rbind(
        c(41.4, 35),
        c(43.6, 35)
    )
)

plot(supermarket)
