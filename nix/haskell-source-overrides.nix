# NOTE: please keep this in sync with cabal.project
{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "1b3dadd7d24322d2af9be737da674d37d4a8e78a";
    sha256 = "sha256-14CTX3HEYYqoJCShJHNXrPZ3gafGL6jcquPbYXLh21g=";
  };

  ieee754 = fetchFromGitHub {
    owner = "fanshi1028";
    repo = "hs-ieee754";
    rev = "01ddbabf11f5fc143503fc134d740668c6a90294";
    sha256 = "sha256-d1zUtplbcsHKiBOt1q4iOtxcjGagJbOAmuNRwzecSrM=";
  };

  hashtables = "1.4.2";
}
