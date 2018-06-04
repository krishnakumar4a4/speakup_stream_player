var BinaryServer = require('binaryjs').BinaryServer;
const Speaker = require('speaker');

console.log('server open on port ' + 9001);

binaryServer = BinaryServer({port: 9001,host: '0.0.0.0'});

 
// Create the Speaker instance
const speaker = new Speaker({
  channels: 1,          // 2 channels
  bitDepth: 16,         // 16-bit samples
  sampleRate: 48000     // 44,100 Hz sample rate
});
 

binaryServer.on('connection', function(client) {
  console.log('new connection');

  client.on('stream', function(stream, meta) {
	// PCM data from stdin gets piped into the speaker
	stream.pipe(speaker);

  stream.on('end', function() {
      console.log('wrote to file ' + outFile);
   });
 });
});
