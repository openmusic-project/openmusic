s = Server.default;

s.options.device_("OpenMusic_SC3");		// give special device-name to identify in JACK

s = Server.default.waitForBoot({

	var sc_om_task;
	var deltas, freqs, durs, vels, chans;

	deltas=[];
	freqs=[];
	durs=[];
	vels=[];
	chans=[];

	MIDIClient.init;					// ALSA portname hardwired to "SuperCollider"
	m.free; m = MIDIOut(0);

	OSCdef(\reset).clear;				
	OSCdef(\reset, {}, '/play.sc_om/reset', nil);
	OSCdef(\reset).add({
		"reset".postln;
		sc_om_task.stop;
		m.allNotesOff(16);
		deltas=[];
		freqs=[];
		durs=[];
		vels=[];
		chans=[];
	});

	OSCdef(\fifos).clear;
	OSCdef(\fifos, {}, '/play.sc_om/fifos', nil);
	OSCdef(\fifos).add({
		|msg, time, addr, recvPort|
		deltas = deltas.add(msg[1]);
		freqs = freqs.add(msg[2]);
		vels = vels.add(msg[3]);
		durs = durs.add(msg[4]);
		chans = chans.add(msg[5]);
	});	

	OSCdef(\start).clear;
	OSCdef(\start, {}, '/play.sc_om/start', nil);
	OSCdef(\start).add({

		"playing MIDI sequence".postln;

		sc_om_task = Task({
			var frstr, durstr, delstr, velstr, n;
			n = freqs.size;
			deltas=deltas.[1..];		// dont want first/0 offset
			frstr = Pseq(freqs).asStream;
			durstr = Pseq(durs/1000).asStream;
			delstr = Pseq(deltas/1000).asStream;
			velstr = Pseq(vels).asStream;
			n.do({
				var delta, key, vel, dur;
				key = frstr.next;
				delta = delstr.next;
				dur = durstr.next;
				vel = velstr.next;
				m.noteOn(16,key,vel);
				{m.noteOff(16,key)}.defer(dur);
				delta.wait;
			});
		};
		).start;
	});

	OSCdef(\resumeResp).clear;
	OSCdef(\resumeResp, {}, '/play.sc_om/continue', nil);
	OSCdef(\resumeResp).add({
		|msg, time, addr, recvPort|
		"resume".postln;
		sc_om_task.resume;
	});

	OSCdef(\pauseResp).clear;
	OSCdef(\pauseResp, {}, '/play.sc_om/pause', nil);
	OSCdef(\pauseResp).add({
		|msg, time, addr, recvPort|
		"pause".postln;
		sc_om_task.pause;
	});

	OSCdef.all.postln;

	"booted".postln;

})

