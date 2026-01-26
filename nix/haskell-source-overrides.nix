# NOTE: please keep this in sync with cabal.project
{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "240ae7306d8a3ac40aef557ae830b0736e5cafb8";
    sha256 = "sha256-FXx/Cm8/UsXgXSwjFrt6hgpYzjQutOhJvopYFGQLjKs=";
  };

  ieee754 = fetchFromGitHub {
    owner = "fanshi1028";
    repo = "hs-ieee754";
    rev = "01ddbabf11f5fc143503fc134d740668c6a90294";
    sha256 = "sha256-d1zUtplbcsHKiBOt1q4iOtxcjGagJbOAmuNRwzecSrM=";
  };

  hashtables = "1.4.2";
}
