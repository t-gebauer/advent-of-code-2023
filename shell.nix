{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    (luajit.withPackages (ps: with ps; [
      fennel
      readline
      lpeg
    ]))
  ];
}
