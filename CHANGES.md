
## v0.5 ( 2023-?? )

* Add subcommand `autofonce promote` to update tests files with current
  results. This command can be applied using standard filters. A `--fake`
  argument can be used to generate files with `.proposed` extension to
  check the results.
* Add selector --failed to run only previously failed tests
* Scripts are run with `AUTOFONCE_SUITE_DIR` and `AUTOFONCE_SUITE_FILE`
   env variables (to locate and identify the `testsuite.at` file)
* Rename `_autotest/` dir to `_autofonce/` dir
* Simplify default `gnucobol.env` to use `atconfig` and `atlocal`: slower
   but probably more portable

## v0.4 ( 2023-02-02 )

* Improve m4 parsing
* display nchecks performed
* allow if/then/else within tests
* min-edition set to 4.10

## v0.3 ( 2023-01-28 )

* add `autofonce new` to create new tests

## v0.2 ( 2023-01-28 )

* Improved documentation

## v0.1 ( 2023-01-28 )

* Initial commit
