# update all packages that start with 'mlr3' :)
old_pkgs = unname(old.packages()[, "Package"])
mlr3_pkgs = old_pkgs[grepl(pattern = "mlr3", old_pkgs)]
update.packages(ask = "no", oldPkgs = mlr3_pkgs)

# old CRAN-only mlr3 packages
update.packages(ask = FALSE, oldPkgs = tools::package_dependencies("mlr3verse")[[1]])

