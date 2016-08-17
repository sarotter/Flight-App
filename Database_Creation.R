library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), dbname = "Flights.db")

dbSendQuery(conn = db,
            "CREATE TABLE tb_flights
            (flight_id   INTEGER PRIMARY KEY AUTOINCREMENT,
            flight_code   VARCHAR(128),
            carrier   VARCHAR(128)
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

#Inserting into the table flights

query <- sprintf("('%s','%s')", unique(results.db$flights_code_outbound),unique(results.db$flights_carrier_outbound))
query <- paste(query,collapse = ",")
dbSendQuery(db,str_c("INSERT INTO tb_flights(flight_code,carrier) VALUES ",query))
