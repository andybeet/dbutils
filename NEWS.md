# dbutils 0.2.2

## Minor update

* `connect_to_database` - new function argument. `ROracle = F`. Allows user to use the driver as specified by the ROracle package instead of using `odbc`


# dbutils 0.1.2

* Updated function documentation (typos)
* Renamed function file name

# dbutils 0.1.1

* Added fixes to pass R-CMD checks
* Added `skipType` argument to `create_species_lookup` to allow user to avoid joining
all 3 species codes SVSPP, NESPP#, ITIS and skip either NESPP3 or SVSPP.
* Included `rlang` as a dependency


# dbutils 0.1.0

* Added a `NEWS.md` file to track changes to the package.

