var main = { |dir|
  SynthDef("SinOsc-mono", {
      Out.ar(0, SinOsc.ar(440))
  }).writeDefFile(dir);

  SynthDef("SinOsc-stereo-expanded", {
      Out.ar(0, SinOsc.ar([440, 220]))
  }).writeDefFile(dir);

};

main.value("SYNTHDEFS_DIR".getenv);
0.exit;
