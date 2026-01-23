{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "fanshi1028";
    repo = "miso";
    rev = "064336ef01d6d5c82d508b778f98d41cf4f51864";
    sha256 = "sha256-JPJOr8pyf2DVNAbZlOvP7tZqGUsFa3JIoDrR8ZDwQFY=";
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
