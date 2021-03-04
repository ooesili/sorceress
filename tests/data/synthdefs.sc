var main = { |dir|
  SynthDef("SinOsc-mono", {
    Out.ar(0, SinOsc.ar(440))
  }).writeDefFile(dir);

  SynthDef("SinOsc-stereo-expanded", {
    Out.ar(0, SinOsc.ar([440, 220]))
  }).writeDefFile(dir);

  SynthDef("bubbles", { |out|
    var f, zout;
    f = LFSaw.kr(0.4, 0, 24, LFSaw.kr([8, 7.23], 0, 3, 80)).midicps; // glissando function
    zout = CombN.ar(SinOsc.ar(f, 0, 0.04), 0.2, 0.2, 4); // echoing sine wave
    Out.ar(out, zout)
  }).writeDefFile(dir);

  SynthDef("diskout", { |bufnum|
    DiskOut.ar(bufnum, In.ar(0, 2));
  }).writeDefFile(dir);

  SynthDef("mouse", {
    Out.ar(0, SinOsc.ar(MouseX.kr(1, 5)) * MouseY.kr)
  }).writeDefFile(dir);
};

main.value("SYNTHDEFS_DIR".getenv);
0.exit;
