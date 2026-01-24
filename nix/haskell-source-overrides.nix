{ fetchFromGitHub }:
{
  miso = fetchFromGitHub {
    owner = "fanshi1028";
    repo = "miso";
    rev = "de89d5d8dab33db428cbf95a64321e6d9689728a";
    sha256 = "sha256-+3h3rr3SLNdF4bKyARBNq1yno8gJlabzsq4m8w88GvE=";
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
