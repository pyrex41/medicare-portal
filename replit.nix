{pkgs}: {
  deps = [
    pkgs.elmPackages.elm
    pkgs.tree
    pkgs.sqlite
    pkgs.litestream
  ];
}
