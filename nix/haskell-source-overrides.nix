{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "dee2a5ce312b1547d34d8bcb0c9f2838bf59c122";
    sha256 = "sha256-//ewcDwijf71wyz+Y7yHBqlTPNUSHIY4te68AfIlJW8=";
  };

  cborg = "${
    fetchFromGitHub {
      owner = "well-typed";
      repo = "cborg";
      rev = "36eb23049ba4d0e33a8487420eb3b270899d64a7";
      sha256 = "sha256-RKOmhcxj8HZ9NbvoYjclVKwvQcDxeRkEkvYdVyXJOH0=";
    }
  }/cborg";

  hashtables = "1.4.2";
}
