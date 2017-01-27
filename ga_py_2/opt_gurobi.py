from gurobipy import *
import numpy as np
from read_meta_data_2 import read_data

units, entities, weights, capacity, units_population, adj_mat = read_data('../app/data', 'max')


def opt():
    num_units = len(units)
    num_entities = len(entities)

    m = Model()
    m.setParam('TimeLimit', 60)
    x = m.addVars(num_units, num_entities, vtype=GRB.BINARY)
    c = m.addConstrs((sum(x[i,j] for j in range(num_entities))==1 for i in range(num_units)))
    
    obj = QuadExpr()
    for j in range(num_entities):

        a = 0
        for i in range(num_units):
            x[i,j].start = 0
            x[i,10].start = 1
            #obj += (x[i,j] * weights[i,j]) * (x[i,j] * weights[i,j])
            a += x[i,j] * units_population[i]        
        obj += (capacity[j] - a) * (capacity[j] - a)
    m.setObjective(obj)

    m.optimize()
    res = np.empty((num_units, num_entities))

    for i in range(num_units):
        for j in range(num_entities):
            res[i,j] = x[i,j].getAttr("x")
    import ipdb; ipdb.set_trace()
    return res
