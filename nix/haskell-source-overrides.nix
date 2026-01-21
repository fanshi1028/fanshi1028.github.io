{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "fanshi1028";
    repo = "miso";
    rev = "092b594e299cd1ccf78b53ce7b15bdf5892e2f83";
    sha256 = "sha256-wSPoQ9dzGqakFkm/Tm/7KzZi7CXINLhnST6/SdFnIKg=";
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
