{ stdenv, fetchgit }:
stdenv.mkDerivation {
  name = "kaem";
  src = fetchgit {
    url = "https://git.savannah.nongnu.org/git/mescc-tools.git";
    rev = "be437350ae1589c202e824ecdb97fe9ac6c470a3 ";
    sha256 = "0pv61cd0zw2jb09xa30vfmf22vhh2ndjdpl3g88g27c4j4dn1f19";
  };
  sourceDir = [ "Kaem" ];
  makeFlags = [ "PREFIX=${placeholder "out"}" ];
}
