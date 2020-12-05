{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "m2-planet";

  src = fetchFromGitHub {
    owner = "oriansj";
    repo = name;
    rev = "d34ea502ccc9c2a6afc3d420dd8c5c36d7c8c6d9";
    sha256 = "0mklakrrn7f0n57kfykv46f24d5a0spwx411067rl7qh0hyd404s";
  };

  makeFlags = [ "PREFIX=${placeholder "out"}" ];
}
