# Load library
library(micar)

# Get data access requests schema form and list (not open to 'anonymous' user)
mica.dar.form(m)
mica.dars(m)
mica.dar(m, id="390463")

# Close connection
mica.logout(m)