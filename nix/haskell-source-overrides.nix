{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "af221db695f7df4191f182a9458f708a4e6020ae";
    sha256 = "sha256-JsxFNgYITtPV4fCIsmhjz9aAMp0RP8ECuUdCUn3NkfU=";
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
