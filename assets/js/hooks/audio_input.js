import { parseHookProps } from "../lib/attribute";
import { encodeAnnotatedBuffer, encodePcmAsWav } from "../lib/codec";

const dropClasses = ["bg-yellow-100", "border-yellow-300"];

/**
 * A hook for client-preprocessed audio input.
 *
 * ## Props
 *
 *   * `id` - a unique id
 *
 *   * `phx-target` - the component to send the `"change"` event to
 *
 *   * `format` - the desired audio format
 *
 *   * `sampling-rate` - the audio sampling rate for
 *
 *   * `endianness` - the server endianness, either `"little"` or `"big"`
 *
 *   * `audio-url` - the URL to audio file to use for the current preview
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

    // Set the current value URL
    this.audioEl.src = this.props.audioUrl;

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

    this.audioEl.src = this.props.audioUrl;
  },

  getProps() {
    return parseHookProps(this.el, [
      "id",
      "phx-target",
      "sampling-rate",
      "endianness",
      "format",
      "audio-url",
    ]);
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
    this.pushEventTo(this.props.phxTarget, "decoding", {});

    const context = new AudioContext({ sampleRate: this.props.samplingRate });

    context.decodeAudioData(buffer, (audioBuffer) => {
      const audioInfo = audioBufferToAudioInfo(audioBuffer);
      this.pushAudio(audioInfo);
    });
  },

  pushAudio(audioInfo) {
    const meta = {
      num_channels: audioInfo.numChannels,
      sampling_rate: audioInfo.samplingRate,
    };

    const buffer = this.encodeAudio(audioInfo);

    const blob = new Blob([buffer]);
    blob.meta = () => meta;

    this.uploadTo(this.props.phxTarget, "file", [blob]);
  },

  encodeAudio(audioInfo) {
    if (this.props.format === "pcm_f32") {
      return convertEndianness32(audioInfo.data, this.props.endianness);
    } else if (this.props.format === "wav") {
      return encodePcmAsWav(
        audioInfo.data,
        audioInfo.numChannels,
        audioInfo.samplingRate,
      );
    }
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

function convertEndianness32(buffer, targetEndianness) {
  if (getEndianness() === targetEndianness) {
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
