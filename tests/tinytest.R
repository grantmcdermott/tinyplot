
# Don't run tests on CRAN (because of test snapshot suite size)
# See https://github.com/grantmcdermott/tinyplot/pull/128
NOT_CRAN = toupper(Sys.getenv("NOT_CRAN")) == "TRUE" || isTRUE(Sys.getenv("NOT_CRAN"))

# Only test on Linux machines (for tinysnapshot fidelity)
ON_LINUX = Sys.info()["sysname"] == "Linux"

if ( NOT_CRAN && ON_LINUX && requireNamespace("tinytest", quietly=TRUE) ){
  tinytest::test_package("tinyplot")
}

# Clean up
rm(ON_LINUX, NOT_CRAN)

