# Riser database

Existing configuration and reference tables are described in the
[riser database documentation](../../docs/riser_database.md).

The `structure_properties/` catalogue extends this directory with generic bare
structural section properties. It has an independent schema and citation file;
existing table manifests retain their current scope. The initial data file is
empty and the existing consumers retain their current inputs.

See the [engineering databases plan-lite](../../docs/plans/engineering-databases-plan-lite.md)
for fields, units, permitted sources, validation rules and population batches.
Load the catalogue explicitly with
`digitalmodel.data_systems.engineering_databases.load_structure_properties(directory)`.
