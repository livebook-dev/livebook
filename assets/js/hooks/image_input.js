import {
  getAttributeOrDefault,
  getAttributeOrThrow,
  parseInteger,
} from "../lib/attribute";
import { base64ToBuffer, bufferToBase64 } from "../lib/utils";

const dropClasses = ["bg-yellow-100", "border-yellow-300"];

/**
 * A hook for client-preprocessed image input.
 *
 * ## Configuration
 *
 *   * `data-id` - a unique id
 *
 *   * `data-phx-target` - the component to send the `"change"` event to
 *
 *   * `data-height` - the image bounding height
 *
 *   * `data-width` - the image bounding width
 *
 *   * `data-format` - the desired image format
 *
 *   * `data-fit` - the fit strategy
 *
 * The element should have the following children:
 *
 *   * `[data-input]` - a file input used for file selection
 *
 *   * `[data-preview]` - a container to put image preview in
 */
const ImageInput = {
  mounted() {
    this.props = this.getProps();

    this.inputMode = "file";

    this.inputEl = this.el.querySelector(`[data-input]`);
    this.previewEl = this.el.querySelector(`[data-preview]`);
    this.previewEl.style = "position: relative;";

    this.cameraSelectionEl = this.el.querySelector(`[data-camera-select-menu]`);
    this.cameraListEl = this.el.querySelector(`[data-camera-list]`);
    this.cameraButton = this.el.querySelector(`[camera-button-label]`);
    this.cameraId = "System Default";
    this.cameraList = void 0;
    this.cameraPreview = void 0;
    this.cameraStream = void 0;

    // Render initial value
    this.handleEvent(`image_input_init:${this.props.id}`, (imageInfo) => {
      this.resetToFileMode();
      const canvas = imageInfoToElement(imageInfo);
      this.setPreview(canvas, true);
    });

    // Capture from camera
    this.cameraSelectionEl.addEventListener("click", (event) => {
      const fromCamera = getAttributeOrDefault(event.target, "data-from-camera", "false");
      const cameraId = getAttributeOrDefault(event.target, "data-camera-id", "");

      if (cameraId.length > 0) {
        this.captureFromCamera(cameraId);
      } else {
        if (fromCamera === "false") {
          this.updateCameraList();
          this.cameraSelectionEl.children[0].children[0].click();
        } else {
          if (this.cameraPreview !== void 0) {
            event.stopPropagation();
            const canvas = this.toCanvas(this.cameraPreview);
            this.setPreview(canvas, true);
            this.pushImage(canvas);
            this.resetToFileMode();
          } else {
            event.stopPropagation();
            this.captureFromCamera("System Default");
          }
        }
      }
    });

    // File selection

    this.previewEl.addEventListener("click", (event) => {
      event.stopPropagation();
      const closeButton = getAttributeOrDefault(event.target, "close-button", "false");
      if (closeButton === "true") {
        this.resetToInitilialStatus();
      } else {
        if (this.inputMode === "file") {
          this.inputEl.click();
        }
      }
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
      file && this.resetToFileMode() && this.loadFile(file);
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
  },

  updated() {
    this.props = this.getProps();
  },

  getProps() {
    return {
      id: getAttributeOrThrow(this.el, "data-id"),
      phxTarget: getAttributeOrThrow(this.el, "data-phx-target", parseInteger),
      height: getAttributeOrDefault(this.el, "data-height", null, parseInteger),
      width: getAttributeOrDefault(this.el, "data-width", null, parseInteger),
      format: getAttributeOrThrow(this.el, "data-format"),
      fit: getAttributeOrThrow(this.el, "data-fit"),
    };
  },

  captureFromCamera(targetCameraId) {
    if (this.cameraPreview !== void 0 && this.cameraId === targetCameraId) {
      return;
    }

    let constraints;
    if (targetCameraId === "System Default") {
      constraints = {audio: false, video: true};
    } else {
      constraints = {
        audio: false,
        video: {
          optional: [{
            sourceId: targetCameraId
          }]
        },
        deviceId: {
          exact: targetCameraId
        }
      };
    }
    this.cameraId = targetCameraId;

    navigator.mediaDevices.getUserMedia(constraints)
    .then((stream) => {
      if (this.cameraStream !== void 0) {
        this.stopMediaStream(this.cameraStream);
      }

      if (this.cameraPreview === void 0) {
        this.cameraPreview = document.createElement('video');
        this.cameraPreview.autoplay = true;
      }

      try {
        this.cameraPreview.srcObject = stream;
      } catch (error) {
        this.cameraPreview.src = window.URL.createObjectURL(stream);
      }

      this.inputMode = "camera";
      this.cameraStream = stream;
      this.cameraButton.innerHTML = "Take this photo";
      this.setPreview(this.cameraPreview, true);
    })
    .catch(() => {});
  },

  updateCameraList() {
    if (this.cameraList !== void 0) return;

    navigator.mediaDevices.getUserMedia({audio: false, video: true})
    .then((stream) => {
      this.stopMediaStream(stream);

      navigator.mediaDevices.enumerateDevices()
      .then((devices) => {
        this.cameraListEl.innerHTML = this.generateCameraOption("System Default", "System Default");
        this.cameraList = devices.filter((device) => {
          const isVideoInput = device.kind === 'videoinput';
          if (isVideoInput) {
            this.cameraListEl.innerHTML += this.generateCameraOption(device.deviceId, device.label);
          }
          return isVideoInput;
        });
      }).catch(() => {});
    })
    .catch(() => {});
  },

  generateCameraOption(deviceId, label) {
    return `<button class="menu-item text-gray-500" role="menuitem">
      <span class="font-medium" data-camera-id="${deviceId}">${label}</span>
    </button>`;
  },

  resetToFileMode() {
    if (this.cameraStream !== void 0) {
      this.stopMediaStream(this.cameraStream);
    }
    this.cameraStream = void 0;

    if (this.cameraPreview !== void 0) {
      this.cameraPreview.remove();
      this.cameraPreview = void 0;
    }

    this.cameraButton.innerHTML = "Open camera";
    this.inputMode = "file";
    return true;
  },

  resetToInitilialStatus() {
    this.resetToFileMode();
    const promptEl = document.createElement("div");
    promptEl.classList = ["text-gray-500"];
    promptEl.innerText = "Drag an image file here or click to open file browser";
    this.setPreview(promptEl, false);
  },

  getCloseButton() {
    const closeBtnEl = document.createElement("div");
    closeBtnEl.classList = ["close-thin"];
    closeBtnEl.setAttribute("close-button", "true");
    return closeBtnEl;
  },

  stopMediaStream(mediaStream) {
    mediaStream.getTracks().forEach((track) => {
      track.stop();
    });
  },

  loadFile(file) {
    const reader = new FileReader();

    reader.onload = (readerEvent) => {
      const imgEl = document.createElement("img");

      imgEl.addEventListener("load", (loadEvent) => {
        const canvas = this.toCanvas(imgEl);
        this.setPreview(canvas, true);
        this.pushImage(canvas);
      });

      imgEl.src = readerEvent.target.result;
    };

    reader.readAsDataURL(file);
  },

  pushImage(canvas) {
    this.pushEventTo(this.props.phxTarget, "change", {
      value: {
        data: canvasToBase64(canvas, this.props.format),
        height: canvas.height,
        width: canvas.width,
      },
    });
  },

  toCanvas(imgEl) {
    let width, height;
    if (this.inputMode === "camera") {
      height = imgEl.videoHeight;
      width = imgEl.videoWidth;
    } else {
      let { width: imgWidth, height: imgHeight } = imgEl;
      width = imgWidth;
      height = imgHeight;
    }

    const { width: boundWidth, height: boundHeight } = this.props;

    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d");

    if (
      (boundWidth === null && boundHeight === null) ||
      (boundWidth === width && boundHeight === height)
    ) {
      canvas.width = width;
      canvas.height = height;

      canvas
        .getContext("2d")
        .drawImage(imgEl, 0, 0, width, height, 0, 0, width, height);
    } else if (this.props.fit === "contain") {
      const widthScale = boundWidth / width;
      const heightScale = boundHeight / height;
      const scale = Math.min(widthScale, heightScale);

      const scaledWidth = Math.round(width * scale);
      const scaledHeight = Math.round(height * scale);

      canvas.width = scaledWidth;
      canvas.height = scaledHeight;

      ctx.drawImage(
        imgEl,
        0,
        0,
        width,
        height,
        0,
        0,
        scaledWidth,
        scaledHeight
      );
    } else if (this.props.fit === "crop") {
      const widthScale = boundWidth / width;
      const heightScale = boundHeight / height;
      const scale = Math.max(widthScale, heightScale);

      const scaledWidth = Math.round(width * scale);
      const scaledHeight = Math.round(height * scale);

      canvas.width = boundWidth;
      canvas.height = boundHeight;

      ctx.drawImage(
        imgEl,
        Math.round((scaledWidth - boundWidth) / scale / 2),
        Math.round((scaledHeight - boundHeight) / scale / 2),
        width - Math.round((scaledWidth - boundWidth) / scale),
        height - Math.round((scaledHeight - boundHeight) / scale),
        0,
        0,
        boundWidth,
        boundHeight
      );
    } else if (this.props.fit === "pad") {
      const widthScale = boundWidth / width;
      const heightScale = boundHeight / height;
      const scale = Math.min(widthScale, heightScale);

      const scaledWidth = Math.round(width * scale);
      const scaledHeight = Math.round(height * scale);

      canvas.width = boundWidth;
      canvas.height = boundHeight;

      ctx.fillStyle = "black";
      ctx.fillRect(0, 0, canvas.width, canvas.height);

      ctx.drawImage(
        imgEl,
        0,
        0,
        width,
        height,
        Math.round((boundWidth - scaledWidth) / 2),
        Math.round((boundHeight - scaledHeight) / 2),
        scaledWidth,
        scaledHeight
      );
    } else {
      canvas.width = boundWidth;
      canvas.height = boundHeight;

      ctx.drawImage(imgEl, 0, 0, width, height, 0, 0, boundWidth, boundHeight);
    }

    return canvas;
  },

  setPreview(element, displayCloseButton) {
    element.style.maxHeight = "300px";
    if (displayCloseButton === true) {
      this.previewEl.replaceChildren(this.getCloseButton(), element);
    } else {
      this.previewEl.replaceChildren(element);
    }
  },
};

function canvasToBase64(canvas, format) {
  if (format === "png" || format === "jpeg") {
    const prefix = `data:image/${format};base64,`;
    const dataUrl = canvas.toDataURL(`image/${format}`);
    return dataUrl.slice(prefix.length);
  }

  if (format === "rgb") {
    const imageData = canvas
      .getContext("2d")
      .getImageData(0, 0, canvas.width, canvas.height);

    const buffer = imageDataToRGBBuffer(imageData);
    return bufferToBase64(buffer);
  }

  throw new Error(`Unexpected format: ${format}`);
}

function imageDataToRGBBuffer(imageData) {
  const pixelCount = imageData.width * imageData.height;
  const bytes = new Uint8ClampedArray(pixelCount * 3);

  for (let i = 0; i < pixelCount; i++) {
    bytes[i * 3] = imageData.data[i * 4];
    bytes[i * 3 + 1] = imageData.data[i * 4 + 1];
    bytes[i * 3 + 2] = imageData.data[i * 4 + 2];
  }

  return bytes.buffer;
}

function imageInfoToElement(imageInfo) {
  if (imageInfo.format === "png" || imageInfo.format === "jpeg") {
    const src = `data:image/${imageInfo.format};base64,${imageInfo.data}`;
    const img = document.createElement("img");
    img.src = src;
    return img;
  }

  if (imageInfo.format === "rgb") {
    const canvas = document.createElement("canvas");
    canvas.height = imageInfo.height;
    canvas.width = imageInfo.width;
    const buffer = base64ToBuffer(imageInfo.data);
    const imageData = imageDataFromRGBBuffer(
      buffer,
      imageInfo.width,
      imageInfo.height
    );
    canvas.getContext("2d").putImageData(imageData, 0, 0);
    return canvas;
  }

  throw new Error(`Unexpected format: ${imageInfo.format}`);
}

function imageDataFromRGBBuffer(buffer, width, height) {
  const pixelCount = width * height;
  const bytes = new Uint8Array(buffer);
  const data = new Uint8ClampedArray(pixelCount * 4);

  for (let i = 0; i < pixelCount; i++) {
    data[i * 4] = bytes[i * 3];
    data[i * 4 + 1] = bytes[i * 3 + 1];
    data[i * 4 + 2] = bytes[i * 3 + 2];
    data[i * 4 + 3] = 255;
  }

  return new ImageData(data, width, height);
}

export default ImageInput;
