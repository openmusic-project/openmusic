s = Server.default;
s.options.device_("OpenMusic_SC3");		// give special device-name to identify in JACK

s = Server.default.waitForBoot({

	var sc_om_task;
	var deltas, freqs, durs, vels, chans, srcPort;
	var tmpfil;

	deltas=[];
	freqs=[];
	durs=[];
	vels=[];
	chans=[];

	srcPort=NetAddr.langPort;			// need to grab this one and send back to OM.

	MIDIClient.init;					// ALSA portname hardwired to "SuperCollider".  Change in sc-code...
	m.free; m = MIDIOut(0);

	OSCdef(\reset).clear;				
	OSCdef(\reset, {}, '/play.sc_om/reset', nil);
	OSCdef(\reset).add({
		"reset".postln;
		sc_om_task.stop;
		(1..16).do{|k|m.allNotesOff(k)};
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
			var frstr, durstr, delstr, velstr, chanstr, n;
			deltas=deltas.[1..] ++ 0;		// dont want first/0 offset
			n = deltas.size;
			frstr = Pseq(freqs).asStream;
			durstr = Pseq(durs/1000).asStream;
			delstr = Pseq(deltas/1000).asStream;
			velstr = Pseq(vels).asStream;
			chanstr = Pseq(chans).asStream;
			n.do({
				var delta, key, vel, dur,chan;
				key = frstr.next;
				delta = delstr.next;
				dur = durstr.next;
				vel = velstr.next;
				chan = chanstr.next;
				// ("chan: " ++ chan).postln;
				m.noteOn(chan,key,vel);
				{m.noteOff(chan,key)}.defer(dur);
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
		Buffer.read(s, name, action: {
			|buffer|
			var k = buffer.numChannels;
			// TODO: store nodeID somewhere to address w pause/play
			x = Synth.newPaused("OMPlayBuf"++k, [\bufnum, buffer, \startPos, interval[0], \stopPos, interval[1]]);
		});
	});

	OSCdef(\playResp).clear;
	OSCdef(\playResp, {}, '/scfileplayer/play', nil);
	OSCdef(\playResp).add({
		arg msg, time, addr, recvPort;
		var playMsg;
		playMsg = msg[1];
		// playMsg.postln;
		Post << "play: " << msg;
		x.run(playMsg);
	});

	OSCdef(\pauseAudioResp).clear;
	OSCdef(\pauseAudioResp, {}, '/scfileplayer/pause', nil);
	OSCdef(\pauseAudioResp).add({
		|msg, time, addr, recvPort|
		var pauseMsg=msg[1].asBoolean.not; // pause(0)->run(1)
		Post << "pause: " << msg;
		x.run(pauseMsg);
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
			arg bufnum, rate=1, looping=0, startPos=0, stopPos=bufnum.duration, trigger=1;
			var sig;
			Line.kr(dur: stopPos-startPos, doneAction: 2);
			sig = PlayBuf.ar(n,bufnum,rate,trigger,startPos,looping, doneAction: 2);
			Out.ar(0,sig);
		}).add;
	};
	"\nYo: OM server booted".postln;
	NetAddr.langPort.postcs;

	tmpfil = File("/tmp/OMSC.lang.port.tmp","w");
	tmpfil.write(NetAddr.langPort.asCompileString);
	tmpfil.close;

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