# load library for solving ODE's
# you may need to first run: install.packages(deSolve)
library(deSolve)

# define the ODE that you want to solve
# IMPORTANT: it is OK to add dimensions to V
#            a1 and a2 are constant, b1 and b2 depend on time
exampleODE <- function(t, V, par) {
  tt <- t / par$step + 1
  dV1 <- par$a1 - V[1] + par$b1[tt] * V[2]
  dV2 <- par$a2 - V[2] + par$b2[tt] * V[1]
  return(list(c(dV1, dV2)))
}

# define boundary values for the last time point
boundaryValuesEndOfContract <- c(V1 = 0, V2 = 0)   


# define time points where the function "exampleODE" should be evaluated
# IMPORTANT: the ode solver uses initial boundary conditions, that is why time needs to be reversed
timeStep <- 1/365
timePointsBackward <- seq(from = 10, to = 0, by = -timeStep)
timePointsForward <- seq(from = 0, to = 10, by = timeStep)


# define parameters
par <- list(a1 = -0.2,                        # constant intensity
            b1 = 0.1 * timePointsForward,     # time dependent intensity
            a2 = 0.1,                         # constant intensity
            b2 = 0.1/(1 + timePointsForward), # time dependent intensity
            step = abs(timeStep))             # step size needed for indexation

# solve the ODE
odeSol <- ode(times = timePointsBackward, y = boundaryValuesEndOfContract, func = exampleODE, parms = par)

# have a look at the solutions
plot(timePointsBackward, odeSol[, 2], type = "l", ylim = c(min(odeSol[, 2:3]), max(odeSol[, 2:3])))
lines(timePointsBackward, odeSol[, 3], lty = 2, col = "red")
