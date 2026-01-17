{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "7e026d8e7668867623ff7c546add70a2551bad7a";
    sha256 = "sha256-nLTHRWTFaMw5qD55Kcuw1iWVgG64X2MUAlWk3edj9mI=";
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
