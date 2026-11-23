import sqlite3

# Make a connection
connection = sqlite3.connect('data.db')
cursor = connection.cursor()

try:
    create_table = "CREATE TABLE users (id int, username text, password text)"
    cursor.execute(create_table)
except:
    print("Error creating table. Possibly already exists")

# Insert 1 user
user = ('1', 'bob', 'bob1')
insert_query = "INSERT INTO users VALUES (?,?,?)"
cursor.execute(insert_query, user)

# Insert multiple users
users = [
    ('2', 'rolf', 'rolf1'),
    ('3', 'anne', 'anne1')
]
cursor.executemany(insert_query, users)

# Select Query
select_query = "SELECT * FROM users"
for row in cursor.execute(select_query):
    print(row)

connection.commit()

#  Closes the connection
connection.close()