using JuMP
using DataFrames
using AmplNLWriter
using Cbc
dist_weights = Matrix(readtable("julia_data/weights.csv", separator=',', header=false));
adj_mat = Matrix(readtable("julia_data/adjacencies.csv", separator=',', header=false));
population = Matrix(readtable("julia_data/population.csv", separator=',', header=false));
capacity = Matrix(readtable("julia_data/capacity.csv", separator=',', header=false));

dist_weights = dist_weights[1:100, :];
population = population[1:100, :];

num_units, num_entities = size(dist_weights)
m = Model(solver=AmplNLSolver("/home/noa/programs/scipoptsuite-3.2.1/scip-3.2.1/interfaces/ampl/bin/scipampl", ["/home/noa/idalab/intelligent-zoning-engine/scip_options"]))
@variable(m, x[1:num_units,1:num_entities], Bin);
@constraint(m, [i=1:num_units], sum{x[i, j], j=1:num_entities} == 1);
@NLobjective(m, Min, sum{(capacity[k] - sum{transpose(x)[k,j]*population[j], j=1:num_units})^2, k=1:num_entities} + sum{(x[i,j]*dist_weights[i,j])^2, i=1:num_units, j=1:num_entities});

@objective(m, Min, sum{capacity[k] - sum{transpose(x)[k,j]*population[j], j=1:num_units}, k=1:num_entities} + sum{x[i,j]*dist_weights[i,j], i=1:num_units, j=1:num_entities});

# a block can only be assigned once (sum of each row needs to be one)


# the number of kids in blocks assigned to a school can not exceed the school's capacity
# (sum of each column*kids needs to be <= )
# add_constraint(sum_exp(x[i, j]*smaller_kids_in_blks$kids[i], i=1:num_select_blks), "<=", smaller_kapas$Kapa[j], j=1:num_select_schools)
# @constraint(m, [j=1:M], sum{x[i, j]*blks[i,:kids], i=1:N} <= kapas[j,:Kapa])

# set_objective(sum_exp(x[i, j]*smaller_matrix[i, j], i=1:num_select_blks, j=1:num_select_schools), "min") %>%
#@objective(m, Min, sum{x[i,j]*matrix[i,j]*blks[i,:kids], i=1:N, j=1:M})

status = solve(m)

println("Done. Objective value: ", getobjectivevalue(m))

writetable("julia_data/solution_jump.csv", DataFrame(getvalue(x)))
