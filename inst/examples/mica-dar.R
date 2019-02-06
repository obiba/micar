# Load library
library(micar)

m <- mica.login(username="administrator", password="password", url="https://mica-demo.obiba.org")

# Get data access requests schema form, list and a specific one
mica.dar.form(m)
mica.dars(m)
mica.dar(m, "595179")
mica.dar.history(m, "595179")
mica.dar.actions(m, "595179")
mica.dar.amendment.form(m)
mica.dar.amendments(m, "595179")
mica.dar.amendments.history(m, "595179")
mica.dar.amendment(m, "595179", "595179-1")
mica.dar.amendment.history(m, "595179", "595179-1")

# Close connection
mica.logout(m)