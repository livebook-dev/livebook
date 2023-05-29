import { getAttributeOrThrow, parseInteger } from "../lib/attribute";
import { base64ToBuffer, bufferToBase64 } from "../lib/utils";

const dropClasses = ["bg-yellow-100", "border-yellow-300"];

/**
 * A hook for client-preprocessed audio input.
 *
 * ## Configuration
 *
 *   * `data-id` - a unique id
 *
 *   * `data-phx-target` - the component to send the `"change"` event to
 *
 *   * `data-format` - the desired audio format
 *
 *   * `data-sampling-rate` - the audio sampling rate for
 *
 *   * `data-endianness` - the server endianness, either `"little"` or `"big"`
 *
 */
const AudioInput = {
  mounted() {
    this.props = this.getProps();

    this.inputEl = this.el.querySelector(`[data-input]`);
    this.audioEl = this.el.querySelector(`[data-preview]`);
    this.uploadButton = this.el.querySelector(`[data-btn-upload]`);
    this.recordButton = this.el.querySelector(`[data-btn-record]`);
    this.stopButton = this.el.querySelector(`[data-btn-stop]`);
    this.cancelButton = this.el.querySelector(`[data-btn-cancel]`);

    this.mediaRecorder = null;

    // Render updated value
    this.handleEvent(
      `audio_input_change:${this.props.id}`,
      ({ audio_info: audioInfo }) => {
        if (audioInfo) {
          this.updatePreview({
            data: this.decodeAudio(base64ToBuffer(audioInfo.data)),
            numChannels: audioInfo.num_channels,
            samplingRate: audioInfo.sampling_rate,
          });
        } else {
          this.clearPreview();
        }
      }
    );

    // File selection

    this.uploadButton.addEventListener("click", (event) => {
      this.inputEl.click();
    });

    this.inputEl.addEventListener("change", (event) => {
      const [file] = event.target.files;
      file && this.loadFile(file);
    });

    // Drag and drop

    this.el.addEventListener("dragover", (event) => {
      event.stopPropagation();
      event.preventDefault();
      event.dataTransfer.dropEffect = "copy";
    });

    this.el.addEventListener("drop", (event) => {
      event.stopPropagation();
      event.preventDefault();
      const [file] = event.dataTransfer.files;
      file && !this.isRecording() && this.loadFile(file);
    });

    this.el.addEventListener("dragenter", (event) => {
      this.el.classList.add(...dropClasses);
    });

    this.el.addEventListener("dragleave", (event) => {
      if (!this.el.contains(event.relatedTarget)) {
        this.el.classList.remove(...dropClasses);
      }
    });

    this.el.addEventListener("drop", (event) => {
      this.el.classList.remove(...dropClasses);
    });

    // Microphone capture

    this.recordButton.addEventListener("click", (event) => {
      this.startRecording();
    });

    this.stopButton.addEventListener("click", (event) => {
      this.stopRecording();
    });

    this.cancelButton.addEventListener("click", (event) => {
      this.stopRecording(false);
    });
  },

  updated() {
    this.props = this.getProps();
  },

  getProps() {
    return {
      id: getAttributeOrThrow(this.el, "data-id"),
      phxTarget: getAttributeOrThrow(this.el, "data-phx-target", parseInteger),
      samplingRate: getAttributeOrThrow(
        this.el,
        "data-sampling-rate",
        parseInteger
      ),
      endianness: getAttributeOrThrow(this.el, "data-endianness"),
      format: getAttributeOrThrow(this.el, "data-format"),
    };
  },

  startRecording() {
    this.audioEl.classList.add("hidden");
    this.uploadButton.classList.add("hidden");
    this.recordButton.classList.add("hidden");
    this.stopButton.classList.remove("hidden");
    this.cancelButton.classList.remove("hidden");

    this.audioChunks = [];

    navigator.mediaDevices.getUserMedia({ audio: true }).then((stream) => {
      this.mediaRecorder = new MediaRecorder(stream);

      this.mediaRecorder.addEventListener("dataavailable", (event) => {
        this.audioChunks.push(event.data);
      });

      this.mediaRecorder.start();
    });
  },

  stopRecording(load = true) {
    this.audioEl.classList.remove("hidden");
    this.uploadButton.classList.remove("hidden");
    this.recordButton.classList.remove("hidden");
    this.stopButton.classList.add("hidden");
    this.cancelButton.classList.add("hidden");

    if (load) {
      this.mediaRecorder.addEventListener("stop", (event) => {
        const audioBlob = new Blob(this.audioChunks);

        audioBlob.arrayBuffer().then((buffer) => {
          this.loadEncodedAudio(buffer);
        });
      });
    }

    this.mediaRecorder.stop();
  },

  isRecording() {
    return this.mediaRecorder && this.mediaRecorder.state === "recording";
  },

  loadFile(file) {
    const reader = new FileReader();

    reader.onload = (readerEvent) => {
      this.loadEncodedAudio(readerEvent.target.result);
    };

    reader.readAsArrayBuffer(file);
  },

  loadEncodedAudio(buffer) {
    const context = new AudioContext({ sampleRate: this.props.samplingRate });

    context.decodeAudioData(buffer, (audioBuffer) => {
      const audioInfo = audioBufferToAudioInfo(audioBuffer);
      this.pushAudio(audioInfo);
    });
  },

  updatePreview(audioInfo) {
    const oldUrl = this.audioEl.src;
    const blob = audioInfoToWavBlob(audioInfo);
    this.audioEl.src = URL.createObjectURL(blob);
    oldUrl && URL.revokeObjectURL(oldUrl);
  },

  clearPreview() {
    const oldUrl = this.audioEl.src;
    this.audioEl.src = "";
    oldUrl && URL.revokeObjectURL(oldUrl);
  },

  pushAudio(audioInfo) {
    this.pushEventTo(this.props.phxTarget, "change", {
      data: bufferToBase64(this.encodeAudio(audioInfo)),
      num_channels: audioInfo.numChannels,
      sampling_rate: audioInfo.samplingRate,
    });
  },

  encodeAudio(audioInfo) {
    if (this.props.format === "pcm_f32") {
      return this.fixEndianness32(audioInfo.data);
    } else if (this.props.format === "wav") {
      return encodeWavData(
        audioInfo.data,
        audioInfo.numChannels,
        audioInfo.samplingRate
      );
    }
  },

  decodeAudio(buffer) {
    if (this.props.format === "pcm_f32") {
      return this.fixEndianness32(buffer);
    } else if (this.props.format === "wav") {
      return decodeWavData(buffer);
    }
  },

  fixEndianness32(buffer) {
    if (getEndianness() === this.props.endianness) {
      return buffer;
    }

    // If the server uses different endianness, we swap bytes accordingly
    for (let i = 0; i < buffer.byteLength / 4; i++) {
      const b1 = buffer[i];
      const b2 = buffer[i + 1];
      const b3 = buffer[i + 2];
      const b4 = buffer[i + 3];
      buffer[i] = b4;
      buffer[i + 1] = b3;
      buffer[i + 2] = b2;
      buffer[i + 3] = b1;
    }

    return buffer;
  },
};

function audioBufferToAudioInfo(audioBuffer) {
  const numChannels = audioBuffer.numberOfChannels;
  const samplingRate = audioBuffer.sampleRate;
  const length = audioBuffer.length;

  const size = 4 * numChannels * length;
  const buffer = new ArrayBuffer(size);

  const pcmArray = new Float32Array(buffer);

  for (let channelIdx = 0; channelIdx < numChannels; channelIdx++) {
    const channelArray = audioBuffer.getChannelData(channelIdx);

    for (let i = 0; i < channelArray.length; i++) {
      pcmArray[numChannels * i + channelIdx] = channelArray[i];
    }
  }

  return { data: pcmArray.buffer, numChannels, samplingRate };
}

function audioInfoToWavBlob({ data, numChannels, samplingRate }) {
  const wavBytes = encodeWavData(data, numChannels, samplingRate);
  return new Blob([wavBytes], { type: "audio/wav" });
}

// See http://soundfile.sapp.org/doc/WaveFormat
function encodeWavData(buffer, numChannels, samplingRate) {
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

// We assume the exact same format as above, since we only need to
// decode data we encoded previously
function decodeWavData(buffer) {
  const HEADER_SIZE = 44;

  const pcmBuffer = new ArrayBuffer(buffer.byteLength - HEADER_SIZE);
  const pcmArray = new Float32Array(pcmBuffer);

  const view = new DataView(buffer);

  for (let i = 0; i < pcmArray.length; i++) {
    const offset = HEADER_SIZE + i * 4;
    pcmArray[i] = view.getFloat32(offset, true);
  }

  return pcmBuffer;
}

function getEndianness() {
  const buffer = new ArrayBuffer(2);
  const int16Array = new Uint16Array(buffer);
  const int8Array = new Uint8Array(buffer);

  int16Array[0] = 1;

  if (int8Array[0] === 1) {
    return "little";
  } else {
    return "big";
  }
}

export default AudioInput;
