import sqlite3
import pandas as pd

import time

sqlite_file = '../app/data/communication.sqlite'

solution_table_name = 'solution'
input_table_name = 'input'

instructions_table_name = 'instructions'
instruction_column = 'instruction'

time_stamp_table_name = 'solution_meta'
time_column = 'timestamp'
score_column = 'score'

TIMEOUT = 60


def create_db():
    """This function creates a data base with two tables:
    1) an assignment table. 2) instruction table with instructions for the optimization.

    Returns:

    """
    init_assignment = pd.read_csv('../app-data/assignment.csv')
    init_assignment.set_index('unit_id')

    conn = sqlite3.connect(sqlite_file, timeout=TIMEOUT)

    c = conn.cursor()

    c.execute("CREATE TABLE {tn} ({nf} {ft})".
              format(tn=time_stamp_table_name, nf=time_column, ft='INT'))
    c.execute("CREATE TABLE {tn} ({nf} {ft})".
              format(tn=instructions_table_name, nf=instruction_column, ft='TEXT'))
    conn.commit()
    conn.close()

    set_assignment(init_assignment)
    set_assignment(init_assignment, input_table_name)

    return


def get_instruction():
    """This function returns the first value in the table and delete it. FIFO.

    Returns: instruction string

    """
    conn = sqlite3.connect(sqlite_file, timeout=TIMEOUT)

    c = conn.cursor()

    read_str = "SELECT * FROM " + instructions_table_name + " ORDER BY ROWID ASC LIMIT 1"

    delete_str = "DELETE FROM " + instructions_table_name + \
                 " WHERE ROWID IN (SELECT ROWID FROM " + instructions_table_name + " ASC LIMIT 1)"
    c.execute(read_str)
    res = c.fetchall()

    if res:
        res = res[0][0]
        c.execute(delete_str)
    else:
        res = None

    conn.commit()
    conn.close()

    return res


def set_assignment(assignment_df, score, table_name=solution_table_name):
    """This functions writes assignment_df to he data base.
    Should no be used outside this file. Should be used only to create the db.

    Args:
        assignment_df: expecting unit_id as index and one column 'entity_id'

    Returns:

    """

    conn = sqlite3.connect(sqlite_file, timeout=TIMEOUT)
    assignment_df.to_sql(table_name, conn, if_exists='replace', index=False)
    conn.commit()
    conn.close()

    if table_name == solution_table_name:
        conn = sqlite3.connect(sqlite_file, timeout=TIMEOUT)
        c = conn.cursor()

        delete_str = "DELETE FROM " + time_stamp_table_name
        c.execute(delete_str)
        conn.commit()
        c.execute("INSERT INTO {tn} ({cn1}, {cn2}) VALUES ({val1}, {val2})". \
                  format(tn=time_stamp_table_name, cn1=time_column, cn2=score_column,
                         val1=int(time.time()), val2=score))
        conn.commit()
        conn.close()

    return


def get_input_assignment():
    """This function gets the input assignment (units and entities) from the data base.

    Returns: matrix form (zeros and ones) of the current assignment in the data bsae

    """

    conn = sqlite3.connect(sqlite_file, timeout=TIMEOUT)
    query = 'SELECT * FROM {tn}'.format(tn=input_table_name)

    assignment_df = pd.read_sql(query, conn)

    conn.close()

    return assignment_df
