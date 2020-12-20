{ stdenv, fetchgit, kaem }:

stdenv.mkDerivation rec {
  pname = "mescc-tools";
  version = "1.1.0";

  src = fetchgit {
    url = "https://git.savannah.nongnu.org/git/mescc-tools.git";
    rev = "Release_${version}";
    sha256 = "1asvlkrvf2kk7hn8wp6p7rmsg2p3dijk2l709j4hklyljl1zj0bp";
  };

  nativeBuildInputs = [ kaem ];

  makeFlags = [ "PREFIX=${placeholder "out"}" ];
}
