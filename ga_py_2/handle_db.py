import sqlite3
import pandas as pd
import numpy as np

sqlite_file = 'assignments_db.sqlite'
instructions_table_name = 'instructions'
assign_table_name = 'assignments'

instruction_id_column = 'instruction'
instruction_column = 'on_off'


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

    c.execute("CREATE TABLE {tn} ({nf} {ft} PRIMARY KEY)" \
              .format(tn=instructions_table_name, nf=instruction_id_column, ft='TEXT'))
    c.execute("ALTER TABLE {tn} ADD COLUMN '{cn}' {ct}".
              format(tn=instructions_table_name, cn=instruction_column, ct='INTEGER'))
    c.execute("INSERT INTO {tn} ({idf}, {cn}) VALUES ('optimize', 0)".
              format(tn=instructions_table_name, idf=instruction_id_column, cn=instruction_column))
    c.execute("INSERT INTO {tn} ({idf}, {cn}) VALUES ('include_current', 0)".
              format(tn=instructions_table_name, idf=instruction_id_column, cn=instruction_column))

    conn.commit()
    conn.close()

    return


def get_instruction(instruction):
    """This function returns the current value of instruction column in the instructions table.

    Args:
        instruction: either "'optimize'" or "'include_current'"

    Returns: 0 or 1

    """
    conn = sqlite3.connect(sqlite_file)

    c = conn.cursor()

    c.execute("SELECT * FROM {tn} WHERE {cn}={val}".
              format(tn=instructions_table_name, cn=instruction_id_column, val=instruction))
    res = c.fetchall()

    conn.close()

    return res


def set_assignment(assignment_df):
    """This functions writes assignment_df to he data base.

    Args:
        assignment_df: expecting unit_id as index and one column 'entity_id'

    Returns:

    """

    conn = sqlite3.connect(sqlite_file)
    assignment_df.to_sql(assign_table_name, conn, if_exists='replace')
    conn.commit()
    conn.close()

    return


def get_current_assignment():
    """This function gets the current assignment (units and entities) from the data base.

    Returns: matrix form (zeros and ones) of the current assignment in the data bsae

    """

    conn = sqlite3.connect(sqlite_file)
    query = 'SELECT * FROM {tn}'.format(tn=assign_table_name)

    assignment_df = pd.read_sql(query, conn, index_col='index')

    conn.close()

    entities = list(assignment_df['entity_id'].unique())
    units = list(assignment_df['unit_id'])

    assign_mat = np.zeros((len(units), len(entities)))

    for i, entity in enumerate(assignment_df['entity_id']):
        assign_mat[i, entities.index(entity)] = 1

    return assign_mat
