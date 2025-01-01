#Name : Tanveer syed
#class:ALY 6050
# Using Linear Programming Models to maximize profits
install.packages("lpSolve")
library(lpSolve)



# Define the objective coefficients
obj <- c(499.99 - 330, 729.99 - 370, 700.99 - 410, 269.99 - 635/5)

# Define the constraint coefficients
const <- matrix(c(330, 370, 410, 127, 5, 8, 25, 25, 1, 1, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, 0, -2, 0), nrow = 5, byrow = TRUE)

# Define the right-hand side of constraints
rhs <- c(170000, 82*30, 0.3, 0, 0)

# Define the direction of constraints (<=)
dir <- c("<=", "<=", ">=", ">=", ">=")

# Solve the linear programming problem
lp_solution <- lp("max", obj, const, dir, rhs)

# Print the optimal solution
print(lp_solution$solution)

# Print the optimal profit
print(lp_solution$objval)

# Generate the sensitivity report
lpSolve::write.lp(lp_solution, "LP_Problem.lp")

#

# Define the variable names
variables <- c("Pressure Washers (X1)", "Go-Karts (X2)", "Generators (X3)", "Water Pumps (X4)")

# Define the final values, reduced costs, and objective coefficients
final_values <- c(-110.0715237, 155.179067, 237.7692613, 118.8846306)
reduced_costs <- c(169.99, 359.99, 290.99, 142.99)
objective_coefficients <- c(110.07, 205.84, 98.20, 131.87)

# Define the constraint names
constraints <- c("Requirement 1 LHS", "Requirement 2 LHS", "Cost/Budget LHS", "Warehouse Space LHS")

# Define the final values and shadow prices for constraints
final_constraint_values <- c(1.629179331, 0, 170000, 12300)
shadow_prices <- c(1.63, -33.68, 0.56, 3.84)

# Define the allowable increases and decreases for constraints
allowable_increase <- c(1e30, 27.91666667, 428.8, 6078.4)
allowable_decrease <- c(1e30, 974.1201949, 56225, 30.9)

# Create a data frame to store the sensitivity report
sensitivity_report <- data.frame(
  "Variable" = variables,
  "Final Value" = final_values,
  "Reduced Cost" = reduced_costs,
  "Objective Coefficient" = objective_coefficients,
  "Constraint" = constraints,
  "Final Constraint Value" = final_constraint_values,
  "Shadow Price" = shadow_prices,
  "Allowable Increase" = allowable_increase,
  "Allowable Decrease" = allowable_decrease
)

# Print the sensitivity report
print(sensitivity_report)

  
 



# Define the final constraint values and convert them to character format with commas and decimals
final_constraint_values <- c(1.629179331, 0, 170000, 12300)
final_constraint_values_formatted <- format(final_constraint_values, nsmall = 2, big.mark = ",")

# Define the shadow prices
shadow_prices <- c(1.63, -33.68, 0.56, 3.84)

# Create a data frame to store the sensitivity report
sensitivity_report <- data.frame(
  "Variable" = variables,
  "Final Value" = final_values,
  "Reduced Cost" = reduced_costs,
  "Objective Coefficient" = objective_coefficients,
  "Constraint" = constraints,
  "Final Constraint Value" = final_constraint_values_formatted, # Use formatted values
  "Shadow Price" = shadow_prices,
  "Allowable Increase" = allowable_increase,
  "Allowable Decrease" = allowable_decrease
)

# Print the sensitivity report
print(sensitivity_report)

  
# Load the lpSolve library
library(lpSolve)

# Define the objective function coefficients
obj <- c(499.99, 729.99, 700.99, 269.99)

# Define the constraint matrix (left-hand side of constraints)
# Budget constraint coefficients
A <- matrix(c(330, 370, 410, 635,
              5, 8, 5, 25,
              -0.3, 0.3, 0, 0,
              0, 0, -2, 1), byrow = TRUE, nrow = 4)

# Define the right-hand side of constraints
rhs <- c(170000, 82, 0, 0)

# Define the direction of constraints (less than or equal to)
const.dir <- c("<=", "<=", ">=", ">=")

# Solve the linear programming problem
lp_solution <- lp(direction = "max",  # Maximize the objective function
                  objective.in = obj,  # Objective function coefficients
                  const.mat = A,  # Constraint matrix
                  const.dir = const.dir,  # Direction of constraints
                  const.rhs = rhs)  # Right-hand side of constraints

# Print the optimal solution
print(lp_solution)

# Extract the optimal values of decision variables
optimal_values <- lp_solution$solution
names(optimal_values) <- c("Pressure Washer", "Go Kart", "Generator", "Water Pump")
print(optimal_values)

# Print the optimal objective value (maximum profit)
optimal_profit <- lp_solution$objval
print(optimal_profit)
22222
# Objective Function Coefficients Sensitivity
obj_sensitivity <- sapply(1:length(obj), function(i) {
  obj_new <- obj
  obj_new[i] <- obj_new[i] + 0.001  # Increase the objective coefficient slightly
  lp_solution_new <- lp(direction = "max", objective.in = obj_new, const.mat = A, const.dir = const.dir, const.rhs = rhs)
  (lp_solution_new$objval - optimal_profit) / 0.001  # Shadow price
})

print("Shadow Prices:")
print(obj_sensitivity)

# Right-hand Side Values of Constraints Sensitivity
rhs_sensitivity <- sapply(1:length(rhs), function(i) {
  rhs_new <- rhs
  rhs_new[i] <- rhs_new[i] + 0.001  # Increase the right-hand side value slightly
  lp_solution_new <- lp(direction = "max", objective.in = obj, const.mat = A, const.dir = const.dir, const.rhs = rhs_new)
  (lp_solution_new$objval - optimal_profit) / 0.001  # Reduced cost
})

print("Reduced Costs:")
print(rhs_sensitivity)


#5. Optimal values of decision variables
optimal_inventory <- c(X = optimal_values["Pressure Washer"],
                       X2 = optimal_values["Go Kart"],
                       X3 = optimal_values["Generator"],
                       X4 = optimal_values["Water Pump"])

# Objective value (total profit)
total_profit <- optimal_profit

# Profits per unit
profits_per_unit <- c(169.99, 359.99, 290.99, 142.99)

# Prices per unit
prices_per_unit <- c(499.99, 729.99, 700.99, 269.99)

# Constraints
constraints <- matrix(c(330, 370, 410, 127, 170000,
                        25, 40, 25, 1.25, 12300,
                        0.7, 0.7, -0.3, -0.3, 1.63), 
                      byrow = TRUE, nrow = 3)
constraint_names <- c("Cost/Budget", "Warehouse Space", "Requirement 1")

# Display results
cat("Decision Variables (Inventory Levels):\n")
print(optimal_inventory)

cat("\nObjective (Total Profit):\n")
print(paste("$", format(round(total_profit, 2), nsmall = 2)))

cat("\nProfits per Unit:\n")
print(profits_per_unit)

cat("\nPrices per Unit:\n")
print(prices_per_unit)

cat("\nConstraints:\n")
print(constraint_names)
print(constraints)



# Define the variable names
variables <- c("Pressure Washers (X1)", "Go-Karts (X2)", "Generators (X3)", "Water Pumps (X4)")

# Define the final values, reduced costs, and objective coefficients
final_values <- c(-110.0715237, 155.179067, 237.7692613, 118.8846306)
reduced_costs <- c(169.99, 359.99, 290.99, 142.99)
objective_coefficients <- c(110.07, 205.84, 98.20, 131.87)

# Define the constraint names
constraints <- c("Requirement 1 LHS", "Requirement 2 LHS", "Cost/Budget LHS", "Warehouse Space LHS")

# Define the final values and shadow prices for constraints
final_constraint_values <- c(1.629179331, 0, 170000, 12300)
shadow_prices <- c(1.63, -33.68, 0.56, 3.84)

# Define the allowable increases and decreases for constraints
allowable_increase <- c(1e30, 27.91666667, 428.8, 6078.4)
allowable_decrease <- c(1e30, 974.1201949, 56225, 30.9)

# Create a data frame to store the sensitivity report
sensitivity_report <- data.frame(
  "Variable" = variables,
  "Final Value" = final_values,
  "Reduced Cost" = reduced_costs,
  "Objective Coefficient" = objective_coefficients,
  "Constraint" = constraints,
  "Final Constraint Value" = final_constraint_values,
  "Shadow Price" = shadow_prices,
  "Allowable Increase" = allowable_increase,
  "Allowable Decrease" = allowable_decrease
)

# Print the sensitivity report
print(sensitivity_report)






# Define the final constraint values and convert them to character format with commas and decimals
final_constraint_values <- c(1.629179331, 0, 170000, 12300)
final_constraint_values_formatted <- format(final_constraint_values, nsmall = 2, big.mark = ",")

# Define the shadow prices
shadow_prices <- c(1.63, -33.68, 0.56, 3.84)

# Create a data frame to store the sensitivity report
sensitivity_report <- data.frame(
  "Variable" = variables,
  "Final Value" = final_values,
  "Reduced Cost" = reduced_costs,
  "Objective Coefficient" = objective_coefficients,
  "Constraint" = constraints,
  "Final Constraint Value" = final_constraint_values_formatted, # Use formatted values
  "Shadow Price" = shadow_prices,
  "Allowable Increase" = allowable_increase,
  "Allowable Decrease" = allowable_decrease
)

#6 Print the sensitivity report
print(sensitivity_report)

