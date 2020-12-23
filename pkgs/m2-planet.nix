{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "m2-planet";
  version = "unstable-2020-12-21";

  src = fetchFromGitHub {
    owner = "oriansj";
    repo = pname;
    rev = "4de11b1bc634c32c531effb32bc687aa14b602d7";
    sha256 = "02dqbmhddk8zf615a3qsn56fq7xrkpqziw3api7pmdj8ih4k565s";
  };

  makeFlags = [ "PREFIX=${placeholder "out"}" ];
}
