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

	MIDIClient.init;					// ALSA portname hardwired to "SuperCollider".  Change in sc-code...
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


	// audiofile interface

	OSCdef(\openResp).clear;
	OSCdef(\openResp, {}, '/scfileplayer/open', nil);
	OSCdef(\openResp).add({
		|msg, time, addr, recvPort|
		var name, interval;
		name = msg[1];
		interval = msg[2];
		Post << "open: " << msg;
		b = Buffer.read(s, name, action: {
			|buffer|
			var k = buffer.numChannels;
			// TODO: need to store this node somewhere to address on pause/play
			x = Synth.newPaused("OMPlayBuf"++k, [\bufnum, buffer, interval]);
		});
	});

	OSCdef(\playResp).clear;
	OSCdef(\playResp, {}, '/scfileplayer/play', nil);
	OSCdef(\playResp).add({
		arg msg, time, addr, recvPort;
		var playMsg;
		playMsg = msg[1];
		playMsg.postln;
		Post << "play: " << msg;
		x.run(playMsg);
	});

	OSCdef(\pauseAudioResp).clear;
	OSCdef(\pauseAudioResp, {}, '/scfileplayer/pause', nil);
	OSCdef(\pauseAudioResp).add({
		|msg, time, addr, recvPort|
		var pauseMsg=msg[1];
		Post << "pause: " << msg;
		x.run(pauseMsg.asBoolean.not);
	});
	OSCdef(\resetAudioResp).clear;
	OSCdef(\resetAudioResp, {}, '/scfileplayer/reset', nil);
	OSCdef(\resetAudioResp).add({
		|msg, time, addr, recvPort|
		Post << "reset: " << msg;
		x.free; b.free;
	});

	OSCdef.all.postln;

	// setup synths to handle 1..16 channels, more if necessary...
	// change to use DiskIn here if applies to OM-interface:

	(1..16).do {|n| var synthName;
		SynthDef("OMPlayBuf" ++ n, {
			arg bufnum, rate=1, looping=0, startPos=0, trigger=1;
			var sig;
			sig = PlayBuf.ar(n,bufnum,rate,trigger,startPos,looping, 2);
			Out.ar(0,sig);
		}).add;							// in memory, dont write to synthdesclib
	};

	"\nOM server booted".postln;

})


/*

	// SCRATCHPAD

	m = NetAddr("127.0.0.1", NetAddr.langPort);

	m.sendMsg("/scfileplayer/open", "/home/andersvi/lyd/andersvi/Floratone-1m.wav", 0);
	m.sendMsg("/scfileplayer/play", 1);
	m.sendMsg("/scfileplayer/play", 0);
	m.sendMsg("/scfileplayer/pause", 1);
	m.sendMsg("/scfileplayer/pause", 0);
	m.sendMsg("/scfileplayer/reset");

*/