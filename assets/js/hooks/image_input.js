import { parseHookProps } from "../lib/attribute";

const dropClasses = ["bg-yellow-100", "border-yellow-300"];

/**
 * A hook for client-preprocessed image input.
 *
 * ## Props
 *
 *   * `id` - a unique id
 *
 *   * `phx-target` - the component to send the `"change"` event to
 *
 *   * `height` - the image bounding height
 *
 *   * `width` - the image bounding width
 *
 *   * `format` - the desired image format
 *
 *   * `fit` - the fit strategy
 *
 *   * `image-url` - the URL to the image binary value
 *
 *   * `value-height` - the height of the current image value
 *
 *   * `value-width` - the width fo the current image value
 *
 */
const ImageInput = {
  mounted() {
    this.props = this.getProps();

    this.inputEl = this.el.querySelector(`[data-input]`);
    this.previewEl = this.el.querySelector(`[data-preview]`);

    this.initialPreviewContentEl = this.previewEl.firstElementChild;

    this.cameraPreviewEl = this.el.querySelector(`[data-camera-preview]`);
    this.cameraListEl = this.el.querySelector(`[data-camera-list]`);
    this.cameraItemTemplateEl = this.cameraListEl.firstElementChild;
    this.cameraItemTemplateEl.remove();

    this.uploadButton = this.el.querySelector(`[data-btn-upload]`);
    this.openCameraButton = this.el.querySelector(`[data-btn-open-camera]`);
    this.captureCameraButton = this.el.querySelector(
      `[data-btn-capture-camera]`,
    );
    this.cancelButton = this.el.querySelector(`[data-btn-cancel]`);

    this.cameraListPopulated = false;
    this.cameraVideoEl = null;
    this.cameraStream = null;

    this.updateImagePreview();

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
      file && this.loadFile(file);
      this.closeCameraView();
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

    // Camera capture

    this.openCameraButton.addEventListener("click", (event) => {
      if (!this.cameraListPopulated) {
        this.renderCameraList();
        this.cameraListPopulated = true;
      }
    });

    this.cameraListEl.addEventListener("click", (event) => {
      const button = event.target.closest(`[data-camera-id]`);

      if (button) {
        const cameraId = button.dataset.cameraId;
        this.openCameraView(cameraId);
      }
    });

    this.captureCameraButton.addEventListener("click", (event) => {
      const canvas = this.toCanvas(
        this.cameraVideoEl,
        this.cameraVideoEl.videoWidth,
        this.cameraVideoEl.videoHeight,
      );
      this.pushImage(canvas);
      this.closeCameraView();
    });

    this.cancelButton.addEventListener("click", (event) => {
      this.closeCameraView();
    });
  },

  updated() {
    this.props = this.getProps();

    this.updateImagePreview();
  },

  getProps() {
    return parseHookProps(this.el, [
      "id",
      "phx-target",
      "height",
      "width",
      "format",
      "fit",
      "image-url",
      "value-height",
      "value-width",
    ]);
  },

  updateImagePreview() {
    if (this.props.imageUrl) {
      buildPreviewElement(
        this.props.imageUrl,
        this.props.valueHeight,
        this.props.valueWidth,
        this.props.format,
      ).then((element) => {
        this.setPreview(element);
      });
    } else {
      this.setPreview(this.initialPreviewContentEl);
    }
  },

  loadFile(file) {
    const reader = new FileReader();

    reader.onload = (readerEvent) => {
      const imgEl = document.createElement("img");

      imgEl.addEventListener("load", (loadEvent) => {
        const canvas = this.toCanvas(imgEl, imgEl.width, imgEl.height);
        this.pushImage(canvas);
      });

      imgEl.src = readerEvent.target.result;
    };

    reader.readAsDataURL(file);
  },

  openCameraView(targetCameraId) {
    this.cameraPreviewEl.classList.remove("hidden");
    this.cancelButton.classList.remove("hidden");
    this.captureCameraButton.classList.remove("hidden");
    this.previewEl.classList.add("hidden");
    this.openCameraButton.classList.add("hidden");
    this.uploadButton.classList.add("hidden");

    navigator.mediaDevices
      .getUserMedia(this.cameraConstraints(targetCameraId))
      .then((stream) => {
        this.cameraStream = stream;

        this.cameraVideoEl = document.createElement("video");
        this.cameraVideoEl.autoplay = true;
        this.cameraVideoEl.playsinline = true;
        this.cameraVideoEl.muted = true;
        this.cameraVideoEl.srcObject = stream;
        this.setCameraPreview(this.cameraVideoEl);
      })
      .catch(() => {});
  },

  cameraConstraints(targetCameraId) {
    if (targetCameraId === "system_default") {
      return {
        audio: false,
        video: true,
      };
    } else {
      return {
        audio: false,
        video: { deviceId: targetCameraId },
      };
    }
  },

  renderCameraList() {
    // In Firefox we need to make sure media permissions are granted,
    // then enumerate devices, and only then stop the stream; otherwise
    // device labels are empty
    navigator.mediaDevices
      .getUserMedia({ audio: false, video: true })
      .then((stream) => {
        return navigator.mediaDevices.enumerateDevices().then((devices) => {
          this.stopMediaStream(stream);
          return devices;
        });
      })
      .then((devices) => {
        const deviceOptions = devices
          .filter((device) => device.kind === "videoinput")
          .map((device) => ({
            deviceId: device.deviceId,
            label: device.label,
          }));

        [
          { deviceId: "system_default", label: "System Default" },
          ...deviceOptions,
        ].forEach(({ deviceId, label }) => {
          const item = this.cameraItemTemplateEl.cloneNode(true);
          item
            .querySelector("[data-camera-id]")
            .setAttribute("data-camera-id", deviceId);
          item.querySelector("[data-label]").innerHTML = label;
          this.cameraListEl.appendChild(item);
        });
      })
      .catch((error) => {
        console.error(error);
        this.openCameraButton.disabled = true;
      });
  },

  closeCameraView() {
    if (this.cameraStream !== null) {
      this.stopMediaStream(this.cameraStream);
      this.cameraStream = null;
    }

    if (this.cameraVideoEl !== null) {
      this.cameraVideoEl.remove();
      this.cameraVideoEl = null;
    }

    this.cameraPreviewEl.classList.add("hidden");
    this.cancelButton.classList.add("hidden");
    this.captureCameraButton.classList.add("hidden");
    this.previewEl.classList.remove("hidden");
    this.openCameraButton.classList.remove("hidden");
    this.uploadButton.classList.remove("hidden");

    return true;
  },

  stopMediaStream(mediaStream) {
    mediaStream.getTracks().forEach((track) => {
      track.stop();
    });
  },

  pushImage(canvas) {
    canvasToBuffer(canvas, this.props.format).then((buffer) => {
      const meta = {
        height: canvas.height,
        width: canvas.width,
      };

      const blob = new Blob([buffer]);
      blob.meta = () => meta;

      this.uploadTo(this.props.phxTarget, "file", [blob]);
    });
  },

  toCanvas(imageEl, width, height) {
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
        .drawImage(imageEl, 0, 0, width, height, 0, 0, width, height);
    } else if (this.props.fit === "contain") {
      const widthScale = boundWidth / width;
      const heightScale = boundHeight / height;
      const scale = Math.min(widthScale, heightScale);

      const scaledWidth = Math.round(width * scale);
      const scaledHeight = Math.round(height * scale);

      canvas.width = scaledWidth;
      canvas.height = scaledHeight;

      ctx.drawImage(
        imageEl,
        0,
        0,
        width,
        height,
        0,
        0,
        scaledWidth,
        scaledHeight,
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
        imageEl,
        Math.round((scaledWidth - boundWidth) / scale / 2),
        Math.round((scaledHeight - boundHeight) / scale / 2),
        width - Math.round((scaledWidth - boundWidth) / scale),
        height - Math.round((scaledHeight - boundHeight) / scale),
        0,
        0,
        boundWidth,
        boundHeight,
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
        imageEl,
        0,
        0,
        width,
        height,
        Math.round((boundWidth - scaledWidth) / 2),
        Math.round((boundHeight - scaledHeight) / 2),
        scaledWidth,
        scaledHeight,
      );
    } else {
      canvas.width = boundWidth;
      canvas.height = boundHeight;

      ctx.drawImage(
        imageEl,
        0,
        0,
        width,
        height,
        0,
        0,
        boundWidth,
        boundHeight,
      );
    }

    return canvas;
  },

  setPreview(element) {
    element.style.maxHeight = "300px";
    element.style.maxWidth = "100%";
    this.previewEl.replaceChildren(element);
  },

  setCameraPreview(element) {
    element.style.maxHeight = "300px";
    element.style.maxWidth = "100%";
    this.cameraPreviewEl.replaceChildren(element);
  },
};

function canvasToBuffer(canvas, format) {
  if (format === "png" || format === "jpeg") {
    return new Promise((resolve, reject) => {
      canvas.toBlob((blob) => {
        blob.arrayBuffer().then((buffer) => {
          resolve(buffer);
        });
      }, `image/${format}`);
    });
  }

  if (format === "rgb") {
    const imageData = canvas
      .getContext("2d")
      .getImageData(0, 0, canvas.width, canvas.height);

    const buffer = imageDataToRGBBuffer(imageData);
    return Promise.resolve(buffer);
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

function buildPreviewElement(imageUrl, height, width, format) {
  if (format === "png" || format === "jpeg") {
    const img = document.createElement("img");
    img.src = imageUrl;
    return Promise.resolve(img);
  }

  if (format === "rgb") {
    return fetch(imageUrl)
      .then((response) => response.arrayBuffer())
      .then((buffer) => {
        const canvas = document.createElement("canvas");
        canvas.height = height;
        canvas.width = width;
        const imageData = imageDataFromRGBBuffer(buffer, width, height);
        canvas.getContext("2d").putImageData(imageData, 0, 0);
        return canvas;
      });
  }

  throw new Error(`Unexpected format: ${format}`);
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
