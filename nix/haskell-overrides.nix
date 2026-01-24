{
  lib,
  haskell,
}:
hself: hsuper: {

  jsaddle = hself.callHackageDirect {
    pkg = "jsaddle";
    ver = "0.9.9.3";
    sha256 = "sha256-/p9p/hBK4TsTR524n1i8tgsJwv7Vw+i288Ccmzb2bfI=";
  } { };

  jsaddle-warp = hself.callHackageDirect {
    pkg = "jsaddle-warp";
    ver = "0.9.9.5";
    sha256 = "sha256-Wv1upgYJP4zHbVtamRLCWNNAcI1NOLQJ4VpR7QDceOY=";
  } { };

  miso = lib.pipe hsuper.miso (with haskell.lib.compose; [ (enableCabalFlag "template-haskell") ]);

  haxl = lib.pipe hsuper.haxl (
    with haskell.lib.compose;
    [
      unmarkBroken
      (overrideCabal (drv: {
        # NOTE: https://github.com/facebook/Haxl/issues/165
        postPatch = ''
          substituteInPlace Setup.hs --replace-fail Setup Main
          sed -i 's/time >= 1.4 \&\& < 1.13/time >= 1.4 \&\& < 1.15/g' haxl.cabal
        '';
      }))
    ]
  );

  cborg = lib.pipe hsuper.cborg [
    (haskell.lib.compose.overrideCabal (drv: {
      patches = [ ];
    }))
  ];

  statistics = hself.callHackageDirect {
    pkg = "statistics";
    ver = "0.16.4.0";
    sha256 = "sha256-BmFcx40Dvazu3fdbZJXLGyB3eNSZ0EZzSkK3cQKdSKo=";
  } { };

  dataframe = hself.callHackageDirect {
    pkg = "dataframe";
    ver = "0.3.3.6";
    sha256 = "sha256-4/O93bE21wTxN/LNWpDmr17o3bo+Xhq1qoB8qG6cq+E=";
  } { };

  granite = hself.callHackageDirect {
    pkg = "granite";
    ver = "0.3.0.5";
    sha256 = "sha256-QLxahMrjQ2tbXeQ0CBn2k5o0tRUgjyFy6EDJFon4T0Y=";
  } { };

  snappy-hs = hself.callHackageDirect {
    pkg = "snappy-hs";
    ver = "0.1.0.4";
    sha256 = "sha256-c9kgHvHYTcMI8Y2jeF2Emazm2lbqDIw+OABjd2VmXJM=";
  } { };

}
