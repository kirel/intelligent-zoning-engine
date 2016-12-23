import sqlite3
import pandas as pd
import numpy as np

sqlite_file = 'zoning_db.sqlite'

solution_table_name = 'solution'
input_table_name = 'input'

instructions_table_name = 'instructions'
instruction_column = 'instruction'


def create_db():
    """This function creates a data base with two tables:
    1) an assignment table. 2) instruction table with instructions for the optimization.

    Returns:

    """
    init_assignment = pd.read_csv('../app-data/assignment.csv')
    init_assignment.set_index('unit_id')

    conn = sqlite3.connect(sqlite_file)

    c = conn.cursor()

    set_assignment(init_assignment)
    set_assignment(init_assignment, input_table_name)
    c.execute("CREATE TABLE {tn} ({nf} {ft} PRIMARY KEY)".
              format(tn=instructions_table_name, nf=instruction_column, ft='TEXT'))

    conn.commit()
    conn.close()

    return


def get_instruction():
    """This function returns the first value in the table and delete it. FIFO.

    Returns: instruction string

    """
    conn = sqlite3.connect(sqlite_file)

    c = conn.cursor()

    read_str = "SELECT * FROM " + instructions_table_name + " ORDER BY ROWID ASC LIMIT 1"
    delete_str = "DELETE FROM " + instructions_table_name +  \
                 " WHERE ROWID IN (Select ROWID from " + instructions_table_name + " ASC LIMIT 1)"
    c.execute(read_str)
    res = c.fetchall()

    if res:
        res = res[0][0]
        c.execute(delete_str)

    conn.commit()
    conn.close()

    return res


def set_assignment(assignment_df, table_name=solution_table_name):
    """This functions writes assignment_df to he data base.
    Should no be used outside this file. Should be used only to create the db.

    Args:
        assignment_df: expecting unit_id as index and one column 'entity_id'

    Returns:

    """

    conn = sqlite3.connect(sqlite_file)
    assignment_df.to_sql(table_name, conn, if_exists='replace')
    conn.commit()
    conn.close()

    return


def get_input_assignment():
    """This function gets the input assignment (units and entities) from the data base.

    Returns: matrix form (zeros and ones) of the current assignment in the data bsae

    """

    conn = sqlite3.connect(sqlite_file)
    query = 'SELECT * FROM {tn}'.format(tn=input_table_name)

    assignment_df = pd.read_sql(query, conn, index_col='index')

    conn.close()

    entities = list(assignment_df['entity_id'].unique())
    units = list(assignment_df['unit_id'])

    assign_mat = np.zeros((len(units), len(entities)))

    for i, entity in enumerate(assignment_df['entity_id']):
        assign_mat[i, entities.index(entity)] = 1

    return assign_mat
