# NOTE: please keep this in sync with cabal.project
{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "bdae5209435d7df86335683d1804e300f558234c";
    sha256 = "sha256-WipIeh9RXOy0sXrP7AcNHdal85QD2FT1v1gUCWtVIfQ=";
  };

  ieee754 = fetchFromGitHub {
    owner = "fanshi1028";
    repo = "hs-ieee754";
    rev = "01ddbabf11f5fc143503fc134d740668c6a90294";
    sha256 = "sha256-d1zUtplbcsHKiBOt1q4iOtxcjGagJbOAmuNRwzecSrM=";
  };

  miso-aeson = fetchFromGitHub {
    owner = "haskell-miso";
    repo = "miso-aeson";
    rev = "d491d91b3437a43dd4410a716ee9d4531b6cf309";
    sha256 = "sha256-VvCMbZBewybVu1daBGb9PCv5YVCmy0cUIQA2yFW+wEY=";
  };

  hashtables = "1.4.2";
}
