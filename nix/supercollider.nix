{ lib, stdenv, fetchurl, cmake, pkg-config, alsaLib
, libjack2, libsndfile, fftwSinglePrec, curl, gcc, emacs
}:

let optional = lib.optional;
in

stdenv.mkDerivation rec {
  pname = "supercollider";
  version = "3.11.2";

  src = fetchurl {
    url = "https://github.com/supercollider/supercollider/releases/download/Version-${version}/SuperCollider-${version}-Source.tar.bz2";
    sha256 = "wiwyxrxIJnHU+49RZy33Etl6amJ3I1xNojEpEDA6BQY=";
  };

  hardeningDisable = [ "stackprotector" ];

  cmakeFlags = [
    "-DNO_X11=ON"
    "-DSC_ABLETON_LINK=OFF"
    "-DSC_ED=OFF"
    "-DSC_EL=OFF"
    "-DSC_HIDAPI=OFF"
    "-DSC_IDE=OFF"
    "-DSC_QT=OFF"
    "-DSC_VIM=OFF"
    "-DSUPERNOVA=OFF"
  ];

  patches = [ ./fix-scheduler-race.patch ];

  nativeBuildInputs = [ cmake pkg-config ];

  buildInputs = [
    gcc libjack2 libsndfile fftwSinglePrec curl ]
      ++ optional (!stdenv.isDarwin) alsaLib;

  meta = with lib; {
    description = "Programming language for real time audio synthesis";
    homepage = "https://supercollider.github.io";
    maintainers = with maintainers; [ mrmebelman ];
    license = licenses.gpl3;
    platforms = [ "x686-linux" "x86_64-linux" ];
  };
}
