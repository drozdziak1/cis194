let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    ghcid
    hlint
    stack
  ] ++ (with pkgs.haskellPackages; [hindent yesod-bin hdevtools]);
}
