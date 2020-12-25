{ stdenv, fetchFromGitHub, git }:

stdenv.mkDerivation {
  pname = "mescc-tools-seed";
  version = "unstable-2020-12-20";

  nativeBuildInputs = [ git ];
  src = fetchFromGitHub {
    owner = "oriansj";
    repo = "mescc-tools-seed";
    rev = "b80accf40023bbf5f6f1d08da59256467bc6f1a0";
    sha256 = "04nra450njaq40rs196m11nmmf64sf1zjw5ic059hqz49fvmqxz0";
    fetchSubmodules = true;
  };
  makeFlags = [ "PREFIX=${placeholder "out"}" ];

  dontStrip = true;

  installPhase = ''
    mkdir -p $out/bin
    cp ./bin/* $out/bin
  '';
}
