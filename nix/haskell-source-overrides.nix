# NOTE: please keep this in sync with cabal.project
{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "240ae7306d8a3ac40aef557ae830b0736e5cafb8";
    sha256 = "sha256-FXx/Cm8/UsXgXSwjFrt6hgpYzjQutOhJvopYFGQLjKs=";
  };

  hashtables = "1.4.2";
}
