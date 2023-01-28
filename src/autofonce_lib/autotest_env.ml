(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

let gnucobol_env =
  {|

TOP_DIR="${AUTOTEST_TESTSUITE}/.."

TEMPLATE="${TOP_DIR}/tests/testsuite.src"

BUILD_DIR="$TOP_DIR/_build"
CONFIG_COB_LIBS="-lcob"
CONFIG_COB_CFLAGS="-pipe -Wno-unused -fsigned-char -Wno-pointer-sign"
CONFIG_COB_OBJECT_EXT="o"
CONFIG_COB_MODULE_EXT="so"
CONFIG_COB_EXE_EXT=""
CONFIG_COB_HAS_64_BIT_POINTER="yes"

COBC="cobc"
COBCRUN="cobcrun"
COBCRUN_DIRECT=""

COB_OBJECT_EXT="$CONFIG_COB_OBJECT_EXT"
COB_MODULE_EXT="$CONFIG_COB_MODULE_EXT"
COB_EXE_EXT="$CONFIG_COB_EXE_EXT"
COB_HAS_64_BIT_POINTER="$CONFIG_COB_HAS_64_BIT_POINTER"

PATH=".:${PATH}"
PATH="$BUILD_DIR/cobc:$BUILD_DIR/bin:${PATH}"
COB_CFLAGS="-I${TOP_DIR} $CONFIG_COB_CFLAGS"
COB_LIBS="-L${BUILD_DIR}/libcob/.libs $CONFIG_COB_LIBS"
COB_CONFIG_DIR="${TOP_DIR}/config"
COB_COPY_DIR="${TOP_DIR}/copy"
LD_LIBRARY_PATH="${BUILD_DIR}/libcob/.libs:$LD_LIBRARY_PATH"
PATH="${BUILD_DIR}/libcob/.libs:$PATH"
DYLD_LIBRARY_PATH="${BUILD_DIR}/libcob/.libs:$DYLD_LIBRARY_PATH"
SHLIB_PATH="${BUILD_DIR}/libcob/.libs:$SHLIB_PATH"
LIBPATH="${BUILD_DIR}/libcob/.libs:$LIBPATH"
COB_LIBRARY_PATH="${BUILD_DIR}/extras"

PATH="${BUILD_DIR}/cobc/.libs:${PATH}"

export COB_CFLAGS COB_LIBS
export COB_CONFIG_DIR COB_COPY_DIR
export LD_LIBRARY_PATH PATH DYLD_LIBRARY_PATH SHLIB_PATH LIBPATH
export COB_LIBRARY_PATH

# NIST tests (tests/cobol85) are executed in a separate perl process with a new environment --> export needed
export COB_HAS_ISAM COB_HAS_XML2 COB_HAS_JSON COB_HAS_CURSES COB_HAS_64_BIT_POINTER
export COBC COBCRUN COBCRUN_DIRECT RUN_PROG_MANUAL
export COB_OBJECT_EXT COB_EXE_EXT COB_MODULE_EXT

COB_STACKTRACE=0
export COB_STACKTRACE

COB_RUNTIME_CONFIG="${TOP_DIR}/config/runtime_empty.cfg"
export COB_RUNTIME_CONFIG

COB_UNIX_LF=1
export COB_UNIX_LF

### COB Independent scripting

# Helper script to unify listings (replace version, date, time)
UNIFY_LISTING="${TOP_DIR}/tests/listings-sed.sh"

# Only works on Linux
PATHSEP=':'
export PATHSEP

# possible path conversion for running the testsuite in an environment
# that doesn't match the one where the tested binaries were built
# Note: not needed for running the testsuite with MSYS as this translates the path
_return_path () {
	echo "$1"
}

AWK=awk
GREP=grep
SED=sed
export AWK GREP SED

# be sure to use the English messages
LC_ALL=C
export LC_ALL
unset LANG

# workaround to adjust the testsuite later:
# FLAGS="-debug -Wall ${COBOL_FLAGS}"
FLAGS="-debug -Wall ${COBOL_FLAGS} -fno-diagnostics-show-option"
COMPILE="${COBC} -x ${FLAGS}"
COMPILE_ONLY="${COBC} -fsyntax-only ${FLAGS} -Wno-unsupported"
COMPILE_MODULE="${COBC} -m ${FLAGS}"

# TODO
# * Unset environment variables used by cobc/cobcrun
# * Add specific flags for performance evaluation

# For the very rare cases where cobc/libcob may need to know if they're running in test mode:
COB_IS_RUNNING_IN_TESTMODE=1 && export COB_IS_RUNNING_IN_TESTMODE


|}
