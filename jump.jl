using JuMP
using DataFrames
using Cbc

blks = readtable("data/optim_blks.csv", separator = ',')
kapas = readtable("data/optim_kapas.csv", separator = ',')
matrix = readtable("data/optim_matrix.csv", separator = ',')

N, M = size(matrix)

m = Model(solver=CbcSolver(logLevel=1, threads=8, seconds=1800))

@variable(m, x[1:N,1:M], Bin)

# a block can only be assigned once (sum of each row needs to be one)
# add_constraint(sum_exp(x[i, j], j=1:num_select_schools), '==', 1, i=1:num_select_blks) %>%
@constraint(m, [i=1:N], sum{x[i, j], j=1:M} == 1)
# the number of kids in blocks assigned to a school can not exceed the school's capacity
# (sum of each column*kids needs to be <= )
# add_constraint(sum_exp(x[i, j]*smaller_kids_in_blks$kids[i], i=1:num_select_blks), "<=", smaller_kapas$Kapa[j], j=1:num_select_schools)
@constraint(m, [j=1:M], sum{x[i, j]*blks[i,:kids], i=1:N} <= kapas[j,:Kapa])

# set_objective(sum_exp(x[i, j]*smaller_matrix[i, j], i=1:num_select_blks, j=1:num_select_schools), "min") %>%
#@objective(m, Min, sum{x[i,j]*matrix[i,j]*blks[i,:kids], i=1:N, j=1:M})
@objective(m, Min, sum{x[i,j]*matrix[i,j], i=1:N, j=1:M})

status = solve(m)

println("Done. Objective value: ", getobjectivevalue(m))

writetable("data/solution_jump.csv", DataFrame(getvalue(x)))
