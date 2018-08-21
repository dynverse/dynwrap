root <- devtools:::shim_system.file("tests/testthat/", package = "dynwrap")
for (file in list.files(root, full.names = TRUE)) {
  source(file)
}
