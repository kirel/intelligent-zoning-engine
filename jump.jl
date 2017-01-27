using JuMP
using DataFrames
using AmplNLWriter
using Cbc
dist_weights = Matrix(readtable("julia_data/weights.csv", separator=',', header=false));
adj_mat = Matrix(readtable("julia_data/adjacencies.csv", separator=',', header=false));
population = Matrix(readtable("julia_data/population.csv", separator=',', header=false));
capacity = Matrix(readtable("julia_data/capacity.csv", separator=',', header=false));


num_units, num_entities = size(dist_weights)
initial = zeros(num_units, num_entities);

m = Model(solver=AmplNLSolver("/home/noa/programs/ampl/gurobi_ampl", ["timelim=300"]))
@variable(m, x[1:num_units,1:num_entities], Bin);
for i in 1:1012
	setvalue(x[i, 10], 1)
end

@constraint(m, [i=1:num_units], sum{x[i, j], j=1:num_entities} == 1);
@NLobjective(m, Min, 1000000*sum{(capacity[k] - sum{transpose(x)[k,j]*population[j], j=1:num_units})^2, k=1:num_entities} + sum{(x[i,j]*dist_weights[i,j])^2, i=1:num_units, j=1:num_entities});

status = solve(m)

println("Done. Objective value: ", getobjectivevalue(m))

writetable("julia_data/solution_jump.csv", DataFrame(getvalue(x)))
