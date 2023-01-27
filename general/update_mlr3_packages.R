old_pkgs = unname(old.packages()[,'Package'])
#old_pkgs
mlr3_pkgs = old_pkgs[grepl(pattern = 'mlr3', old_pkgs)]
mlr3_pkgs
update.packages(oldPkgs = mlr3_pkgs, ask = 'no')
