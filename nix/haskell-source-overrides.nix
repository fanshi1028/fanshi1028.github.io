# NOTE: please keep this in sync with cabal.project
{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "1301c1b2acf1d6ccedff50626bd1b3e249e82fbc";
    sha256 = "sha256-JLzgQ4NYUa2Z2VCUscZeEJk0WiiAmMDRbPA15txY81Y=";
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
