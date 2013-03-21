s.boot;
MIDIClient.init;

m.noteOn(16,80,100)

(

var sc_om_task;
var deltas, freqs, durs, vels;

deltas=[0];
freqs=[0];
durs=[0];

m.free; m = MIDIOut(0);

OSCdef(\reset).clear;
OSCdef(\reset, {
	"reset".postln;
	sc_om_task.stop;
	m.allNotesOff(16);
	deltas=[0]; freqs=[0]; durs=[0];
}, '/play.sc_om/reset', nil);


OSCdef(\fifos).clear;
OSCdef(\fifos, {
	|msg, time, addr, recvPort|
	deltas = deltas.add(msg[1]);
	freqs = freqs.add(msg[2]);
	durs = durs.add(msg[4]);
}, '/play.sc_om/fifos', nil);

OSCdef(\start).clear;
OSCdef(\start,{

	"playing MIDI sequence".postln;

	sc_om_task = Task({
		var fr, dur, del, n;
		n = freqs.size;
		fr = Pseq(freqs).asStream;
		dur = Pseq(durs/1000).asStream;
 		del = Pseq(deltas/1000).asStream;
		n.do({
			var vent, key, notedur;
			key = fr.next;
			vent = del.next;
			notedur = dur.next;
			m.noteOn(16,key,100);
			{m.noteOff(16,key)}.defer(notedur);
			vent.wait;
		});
	};
	).start;
}, '/play.sc_om/start', nil);


OSCdef(\resumeResp).clear;
OSCdef(\resumeResp, {
		|msg, time, addr, recvPort|
		"resume".postln;
		sc_om_task.resume;
}, '/play.sc_om/continue', nil);

OSCdef(\pauseResp).clear;
OSCdef(\pauseResp, {
	|msg, time, addr, recvPort|
	// sc_om_task.postln;
	"pause".postln;
	sc_om_task.pause;
}, '/play.sc_om/pause', nil);

)
