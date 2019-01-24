# Load library
library(micar)

# Open connection as anonymous user
m <- mica.login(url="https://mica-demo.obiba.org")

# Get networks 
mica.networks(m)
mica.networks(m, query="network(in(Mica_network.studyIds,clsa))")
mica.networks(m, query="variable(in(Mlstr_area.Lifestyle_behaviours,Drugs))", locale="en", from=0, limit=10)

# Get studies, populations and DCEs
mica.studies(m)
mica.studies(m, query="study(in(Mica_study.methods-design,cohort_study))")
mica.studies(m, query="variable(in(Mlstr_area.Lifestyle_behaviours,Drugs))", locale="en", from=0, limit=10)

mica.study.populations(m)
mica.study.dces(m)

# Get datasets
mica.datasets(m)
mica.datasets(m, query="dataset(in(Mica_dataset.className,HarmonizationDataset))")
mica.datasets(m, query="variable(in(Mlstr_area.Lifestyle_behaviours,Drugs))")

# Get variables
mica.variables(m)
mica.variables(m, query="variable(in(Mlstr_area.Lifestyle_behaviours,Drugs))")
mica.variables(m, query="dataset(in(Mica_dataset.className,HarmonizationDataset))")

# Get taxonomies, vocabularies, terms
mica.taxonomies(m,target="variable")
mica.taxonomies(m,target="variable", query="sex", locale="en", taxonomies = list("Mlstr_area", "Mlstr_additional"))
mica.taxonomies(m,target="study")

mica.vocabularies(m,target="variable", query="cancer", locale = "en")

# Close connection
mica.logout(m)