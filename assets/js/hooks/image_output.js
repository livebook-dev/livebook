import { base64ToBuffer } from "../lib/utils";

/**
 * A hook for the image output.
 *
 * Automatically converts image URLs from image/x-pixel to image/png.
 */
const ImageOutput = {
  mounted() {
    this.updateSrc();
  },

  updated() {
    this.updateSrc();
  },

  updateSrc() {
    const dataUrl = this.el.src;
    const prefix = "data:image/x-pixel;base64,";

    const base64Data = dataUrl.slice(prefix.length);

    if (dataUrl.startsWith(prefix)) {
      const buffer = base64ToBuffer(base64Data);

      const view = new DataView(buffer);
      const height = view.getUint32(0, false);
      const width = view.getUint32(4, false);
      const channels = view.getUint8(8);
      const pixelBuffer = buffer.slice(9);

      const imageData = imageDataFromPixelBuffer(
        pixelBuffer,
        width,
        height,
        channels
      );

      const canvas = document.createElement("canvas");
      canvas.height = height;
      canvas.width = width;
      canvas.getContext("2d").putImageData(imageData, 0, 0);
      const pngDataUrl = canvas.toDataURL("image/png");

      this.el.src = pngDataUrl;
    }
  },
};

function imageDataFromPixelBuffer(buffer, width, height, channels) {
  const pixelCount = width * height;
  const bytes = new Uint8Array(buffer);
  const data = new Uint8ClampedArray(pixelCount * 4);

  for (let i = 0; i < pixelCount; i++) {
    if (channels === 1) {
      data[i * 4] = bytes[i];
      data[i * 4 + 1] = bytes[i];
      data[i * 4 + 2] = bytes[i];
      data[i * 4 + 3] = 255;
    } else if (channels === 2) {
      data[i * 4] = bytes[i * 2];
      data[i * 4 + 1] = bytes[i * 2];
      data[i * 4 + 2] = bytes[i * 2];
      data[i * 4 + 3] = bytes[i * 2 + 1];
    } else if (channels === 3) {
      data[i * 4] = bytes[i * 3];
      data[i * 4 + 1] = bytes[i * 3 + 1];
      data[i * 4 + 2] = bytes[i * 3 + 2];
      data[i * 4 + 3] = 255;
    } else if (channels === 4) {
      data[i * 4] = bytes[i * 4];
      data[i * 4 + 1] = bytes[i * 4 + 1];
      data[i * 4 + 2] = bytes[i * 4 + 2];
      data[i * 4 + 3] = bytes[i * 4 + 3];
    }
  }

  return new ImageData(data, width, height);
}

export default ImageOutput;
