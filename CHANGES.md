
## v0.6 ( 2023-02-?? )

* Replace `autofonce.env` by a configuration file:

  * Configuration can be stored in `autofonce.toml` (typically not committed in
    sources) or `.autofonce` (committed in sources)
  * `autofonce` will lookup `autofonce.toml` first, and `.autofonce` if not
    found, and fail if none is found
  * `autofonce init` will create a `autofonce.toml` in the local directory.
  * A `autofonce.toml` file can be renamed to `.autofonce` and committed.
  * The configuration provides:
    * `[testsuites]` section: aliases for testsuites, with the testsuite file,
      the path for included files, and the env name
    * `[envs]` section: possibility to define different envs (included in tests
      scripts) for different testsuites
    * `source_anchors` and `build_anchors` are used to locate the project source
      dir and the project build dir in the upper directories
  * `autofonce` defines several variables at the beginning of test scripts envs:
    * `AUTOFONCE_TESTSUITE`: name of the testsuite
    * `AUTOFONCE_RUN_DIR`: directory where tests are run (containing `_autofonce/)
    * `AUTOFONCE_SOURCE_DIR`: source directory of the project (root/top dir)
    * `AUTOFONCE_BUILD_DIR`: build directory of the project

* The *run-dir* (the directory that will contain the `_autofonce/` subdir) is
  determined by:
  * the directory containing the file `autofonce.toml`, or
  * the build directory as defined by the anchors in the `.autofonce` file


## v0.5 ( 2023-02-04 )

* Add subcommand `autofonce promote` to update tests files with current
  results. This command can be applied using standard filters. By default,
  display a diff of changes to be performed. Use `--apply` to actually
  perform the tests.
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
