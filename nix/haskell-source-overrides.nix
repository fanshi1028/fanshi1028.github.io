# NOTE: please keep this in sync with cabal.project
{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "2260a698327f0985035bb727acf8116165772a65";
    sha256 = "sha256-zKxTSZd4awg42BbqcR0ILBQSCLwvMXprW5VKfJ3xPfs=";
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
