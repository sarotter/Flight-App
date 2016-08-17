library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), dbname = "Flights.db")

dbSendQuery(conn = db,
            "CREATE TABLE tb_flights
            (flight_id   INTEGER PRIMARY KEY AUTOINCREMENT,
            flight_code   VARCHAR(128),
            carrier   VARCHAR(128),
            depart   DATETIME,
            query   DATETIME
            )")

dbSendQuery(conn = db,
            "CREATE TABLE tb_flight_prices
            (id INTEGER PRIMARY KEY AUTOINCREMENT,
            flight_ID INTEGER,
            price INTEGER,
            query DATETIME,
            departure DATETIME,
            FOREIGN KEY (flight_id) REFERENCES flights(flight_id)
            )")