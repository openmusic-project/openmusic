s.boot



OSCFunc.newMatching({|msg, time, addr, recvPort| \matching.postln}, '/chat', n); // path matching
OSCFunc({|msg, time, addr, recvPort| \oneShot.postln}, '/chat', n).oneShot; // once only
OSCdef(\test, {|msg, time, addr, recvPort| \unmatching.postln}, '/chat', n); // def style

OSCFunc.trace(true);
OSCFunc.trace(false);

'/play.sc_om/reset'.asString.split

(
o.free;
o = OSCFunc.newMatching({
	|msg, time, addr, recvPort|
	// (\treff ++ " " ++ msg).postln;
	msg[1].postln;
	// addr.postln;
	// recvPort.postln;
}, '/play.sc_om', nil); // nil som addresse tar i mot fra alle som sender noe til 57120
)

"en/to/tre".split

s.boot;
s.quit;

OSCFunc.trace(true)
OSCFunc.trace(false)

NetAddr.langPort

(
~deltas=[0];
~freqs=[0];
~durs=[0];
)

s.freeAll
(
var pause;
var resume;
var sc_om_task;

r.free;
r = OSCFunc.newMatching({
	sc_om_task.postln;
	"reset".postln;
	~deltas=[0];
	~freqs=[0];
	~durs=[0];
}, '/play.sc_om/reset', nil);

o.free;
o = OSCFunc.newMatching({
	|msg, time, addr, recvPort|
	[msg,time].postln;
	(\treff ++ " " ++ msg).postln;
	// ~curtime=~curtime+msg[1];
	// ~curtime=msg[1];
	// msg[1].postln;
	~deltas = ~deltas.add(msg[1]).postln;
	~freqs = ~freqs.add(msg[2]);
	~durs = ~durs.add(msg[4]);
	// addr.postln;
	// recvPort.postln;
}, '/play.sc_om/fifos', nil);

p.free;
p = OSCFunc.newMatching({
	|msg, time, addr, recvPort|

	"playing sequence".postln;

	// Pbind(
	// 	\freq, Pseq(~freqs.midicps),
	// 	\delta, Pseq(~deltas/1000),
	// 	\dur, Pseq(~durs/1000)
	// ).play;

	
	sc_om_task = Task({
		var fr, dur, del, n;
		n = ~freqs.size;
		fr = Pseq(~freqs).asStream;
		dur = Pseq(~durs/1000).asStream;
		del = Pseq(~deltas/1000).asStream;
		n.do({
			var vent;
			vent = del.next;
			// vent.postln;
			Synth(\poing, [\key,fr.next, \dur, dur.next, \vel, 40]);
			vent.wait;
		});
	};
	).start;
}, '/play.sc_om/start', nil); // nil som addresse tar i mot fra alle som sender noe til 57120


resume.free;
resume = OSCFunc.newMatching({
	|msg, time, addr, recvPort|
	"resume".postln;
	sc_om_task.resume;
}, '/play.sc_om/continue', nil);

pause.free;
pause = OSCFunc.newMatching({
	|msg, time, addr, recvPort|
	// sc_om_task.postln;
	"pause".postln;
	sc_om_task.pause;
}, '/play.sc_om/pause', nil);

)

[[0,1,2,3],[\a,\b,\c,\d], [10,20,30,40]].flop

(
SynthDef(\poing, {
	arg key=60, dur=1, vel=127;
	var freq = key.midicps, sig, amp;
	sig = SinOsc.ar(freq, 0, EnvGen.kr(Env.perc(0.01, dur), doneAction: 2) * (vel/127.0).pow(2));
	Out.ar(0, [sig,sig]);
}).load(s)
)

Synth(\poing,[\key, 62])

50.midicps;

s.boot

s.quit
0.exit