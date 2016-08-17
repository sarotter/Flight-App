library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), dbname = "Flights.db")

dbSendQuery(conn = db,
            "CREATE TABLE tb_flights
            (id INTEGER PRIMARY KEY AUTOINCREMENT,
            price INTEGER,
            query DATETIME,
            departure DATETIME,
            flight_code VARCHAR(128)
            )")



