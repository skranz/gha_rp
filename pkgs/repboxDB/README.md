# repboxDB

Utility functions to systematically store repbox data. Key component are table specifications in YAML files (see /inst/repdb).

Currently, table data will just be stored as Rds files called `parcels`. A parcel is an R list that can contain data frames of multiple tables.

In the future also other storage modes, e.g. a SQLite database, should be supported by these utility functions.
