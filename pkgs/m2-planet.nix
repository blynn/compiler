{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "m2-planet";
  version = "unstable-2020-12-15";

  src = fetchFromGitHub {
    owner = "oriansj";
    repo = pname;
    rev = "e5befc4feed411f55303c1fa014226b9d6017e29";
    sha256 = "14v4r4c4nia3zc2r01yzlhvh0wrnh6f61xbxv7rb93fm0kmb9dfb";
  };

  makeFlags = [ "PREFIX=${placeholder "out"}" ];
}
