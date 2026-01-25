{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "dee2a5ce312b1547d34d8bcb0c9f2838bf59c122";
    sha256 = "sha256-//ewcDwijf71wyz+Y7yHBqlTPNUSHIY4te68AfIlJW8=";
  };

  hashtables = "1.4.2";
}
