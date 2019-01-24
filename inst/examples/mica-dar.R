# Load library
library(micar)

m <- mica.login(username="administrator", password="password", url="https://mica-demo.obiba.org")

# Get data access requests schema form, list and a specific one
mica.dar.form(m)
mica.dars(m)
mica.dar(m, id="390463")

# Close connection
mica.logout(m)