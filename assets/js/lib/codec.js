/**
 * Encodes PCM float-32 in native endianness into a WAV binary.
 */
export function encodePcmAsWav(buffer, numChannels, samplingRate) {
  // See http://soundfile.sapp.org/doc/WaveFormat

  const HEADER_SIZE = 44;

  const wavBuffer = new ArrayBuffer(HEADER_SIZE + buffer.byteLength);
  const view = new DataView(wavBuffer);

  const numFrames = buffer.byteLength / 4;
  const bytesPerSample = 4;

  const blockAlign = numChannels * bytesPerSample;
  const byteRate = samplingRate * blockAlign;
  const dataSize = numFrames * blockAlign;

  let offset = 0;

  function writeUint32Big(int) {
    view.setUint32(offset, int, false);
    offset += 4;
  }

  function writeUint32(int) {
    view.setUint32(offset, int, true);
    offset += 4;
  }

  function writeUint16(int) {
    view.setUint16(offset, int, true);
    offset += 2;
  }

  function writeFloat32(int) {
    view.setFloat32(offset, int, true);
    offset += 4;
  }

  writeUint32Big(0x52494646);
  writeUint32(36 + dataSize);
  writeUint32Big(0x57415645);

  writeUint32Big(0x666d7420);
  writeUint32(16);
  writeUint16(3); // 3 represents 32-bit float PCM
  writeUint16(numChannels);
  writeUint32(samplingRate);
  writeUint32(byteRate);
  writeUint16(blockAlign);
  writeUint16(bytesPerSample * 8);

  writeUint32Big(0x64617461);
  writeUint32(dataSize);

  const array = new Float32Array(buffer);

  for (let i = 0; i < array.length; i++) {
    writeFloat32(array[i]);
  }

  return wavBuffer;
}

const HEADER_LENGTH = 4;

/**
 * Builds a single buffer with JSON-serialized `meta` and `buffer`.
 */
export function encodeAnnotatedBuffer(meta, buffer) {
  const encoder = new TextEncoder();
  const metaArray = encoder.encode(JSON.stringify(meta));

  const raw = new ArrayBuffer(
    HEADER_LENGTH + metaArray.byteLength + buffer.byteLength,
  );
  const view = new DataView(raw);

  view.setUint32(0, metaArray.byteLength);
  new Uint8Array(raw, HEADER_LENGTH, metaArray.byteLength).set(metaArray);
  new Uint8Array(raw, HEADER_LENGTH + metaArray.byteLength).set(
    new Uint8Array(buffer),
  );

  return raw;
}

/**
 * Decodes binary annotated with JSON-serialized metadata.
 */
export function decodeAnnotatedBuffer(raw) {
  const view = new DataView(raw);
  const metaArrayLength = view.getUint32(0);

  const metaArray = new Uint8Array(raw, HEADER_LENGTH, metaArrayLength);
  const buffer = raw.slice(HEADER_LENGTH + metaArrayLength);

  const decoder = new TextDecoder();
  const meta = JSON.parse(decoder.decode(metaArray));

  return [meta, buffer];
}
