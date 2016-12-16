import sqlite3
import pandas as pd

sqlite_file = 'assignments_db.sqlite'
instructions_table_name = 'instructions'
assign_table_name = 'assignments'

instruction_id_column = 'idx'


def create_db():
    init_assignment = pd.read_csv('../app-data/assignment.csv')
    init_assignment.set_index('unit_id')

    f_type= 'INTEGER'

    conn = sqlite3.connect(sqlite_file)

    c = conn.cursor()

    # units_col = 'units_id'
    # units_field_type = 'INTEGER'
    # entities_col = 'entities_id'
    # entities_field_type = 'TEXT'
    # c.execute('CREATE TABLE {tn} ({nf} {ft} PRIMARY KEY)'.
    #          format(tn=assign_table_name, nf=units_col, ft=units_field_type))
    # c.execute("ALTER TEBLE {tn} ADD COLUMN '{cn}' {ct}".
    #          format(tn=assign_table_name, cn=entities_col, ct=entities_field_type))

    # what is it doing as primary key?

    update_assignment(init_assignment)

    c.execute("CREATE TABLE {tn} ({nf} {ft} PRIMARY KEY)" \
              .format(tn=instructions_table_name, nf=instruction_id_column, ft=f_type))
    c.execute("ALTER TABLE {tn} ADD COLUMN '{cn}' {ct}".
              format(tn=instructions_table_name, cn='optimize', ct='INTEGER'))
    c.execute("INSERT INTO {tn} ({idf}, {cn}) VALUES (0, 0)".
              format(tn=instructions_table_name, idf=instruction_id_column, cn='optimize'))

    conn.commit()
    conn.close()

    return


def read_instruction():
    conn = sqlite3.connect(sqlite_file)

    c = conn.cursor()

    c.execute('SELECT * FROM {tn} WHERE {cn}=0'.
              format(tn=instructions_table_name, cn=instruction_id_column))
    res = c.fetchall()

    conn.close()

    return res


def update_assignment(assignment_df):
    """

    Args:
        assignment_df: expecting unit_id as index and one column 'entity_id'

    Returns:

    """

    conn = sqlite3.connect(sqlite_file)
    assignment_df.to_sql(assign_table_name, conn, if_exists='replace')
    conn.commit()
    conn.close()

    return
