old_pkgs = unname(old.packages()[,'Package'])
mlr3_pkgs = old_pkgs[grepl(pattern = 'mlr3', old_pkgs)]

# old mlr3 packages
mlr3_pkgs
update.packages(oldPkgs = mlr3_pkgs, ask = 'no')
