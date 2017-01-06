using JuMP
using DataFrames
using Mosek
dist_weights = Matrix(readtable("julia_data/weights.csv", separator=',', header=false));
adj_mat = Matrix(readtable("julia_data/adjacencies.csv", separator=',', header=false));
population = Matrix(readtable("julia_data/population.csv", separator=',', header=false));
capacity = Matrix(readtable("julia_data/capacity.csv", separator=',', header=false));
# blks = readtable("data/optim_blks.csv", separator = ',');
# kapas = readtable("data/optim_kapas.csv", separator = ',');
# matrix = readtable("data/optim_matrix.csv", separator = ',');

num_units, num_entities = size(dist_weights)
m = Model();
@variable(m, x[1:num_units,1:num_entities], Bin);

# a block can only be assigned once (sum of each row needs to be one)
@constraint(m, [i=1:num_units], sum{x[i, j], j=1:num_entities} == 1);

# the number of kids in blocks assigned to a school can not exceed the school's capacity
# (sum of each column*kids needs to be <= )
# add_constraint(sum_exp(x[i, j]*smaller_kids_in_blks$kids[i], i=1:num_select_blks), "<=", smaller_kapas$Kapa[j], j=1:num_select_schools)
# @constraint(m, [j=1:M], sum{x[i, j]*blks[i,:kids], i=1:N} <= kapas[j,:Kapa])

# set_objective(sum_exp(x[i, j]*smaller_matrix[i, j], i=1:num_select_blks, j=1:num_select_schools), "min") %>%
#@objective(m, Min, sum{x[i,j]*matrix[i,j]*blks[i,:kids], i=1:N, j=1:M})
@NLobjective(m, Min, sum{(capacity[k] - sum{transpose(x)[k,j]*population[j], j=1:num_units})^2, k=1:num_entities} + sum{(x[i,j]*dist_weights[i,j])^2, i=1:num_units, j=1:num_entities});

status = solve(m)

println("Done. Objective value: ", getobjectivevalue(m))

writetable("julia_data/solution_jump.csv", DataFrame(getvalue(x)))
