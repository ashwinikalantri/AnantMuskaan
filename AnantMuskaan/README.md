A database needs to be setup which will store the data locally. The configurations here are for a Postgres database.

The following variables needs to be saved in the Environment:

# Database
- Sys.setenv("DB_HOST" = "xxx.xxx.xxx.xxx") ## Replace with IP Address or domain name of the database server
- Sys.setenv("DB_USER" = "username") ## Replace with database username
- Sys.setenv("DB_PASS" = "password") ## Replace with database password
- Sys.setenv("DB_NAME" = "dbname") ## Replace with database name

# API Key
- Sys.setenv("API_KEY" = "XXXXX") ## Replace with the Redcap API
- Sys.setenv("REDCAP_URL" = "https://nhrp-rdp.icmr.org.in/api/")