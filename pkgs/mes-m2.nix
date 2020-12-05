{ stdenv, fetchFromGitHub, mescc-tools }:

stdenv.mkDerivation rec {
  name = "mes-m2";
  src = fetchFromGitHub {
    owner = "oriansj";
    repo = name;
    rev = "6105916f9a58a879fc71f84c3df4fa622696a3ee";
    sha256 = "0h3wivrvwz8czilpii155xxkhm15hfhawmiih248ggkanpjmkcvm";
  };

  nativeBuildInputs = [ mescc-tools ];
  makeFlags = [ "PREFIX=${placeholder "out"}" ];
}
