/**
 * A hook that disables the auto-zoom behavior when focusing an
 * input on a touch device.
 *
 * It is important that this should not prevent users from manually
 * zooming if they wish. There isn't a portable solution to this
 * problem, so this hook is a no-op if the detected device is not
 * known to behave well.
 *
 * See: https://stackoverflow.com/questions/2989263/disable-auto-zoom-in-input-text-tag-safari-on-iphone
 */
const DisableZoomOnInputFocus = {
  mounted() {
    if (this.shouldActivate()) {
      this.viewportTag = document.querySelector("meta[name='viewport']");
      this.initialContent = this.viewportTag && this.viewportTag.content;

      if (this.viewportTag) {
        this.viewportTag.content = this.initialContent + ', maximum-scale=1.0';
      }
    }
  },

  destroyed() {
    if (this.viewportTag) {
      this.viewportTag.content = this.initialContent;
    }
  },

  shouldActivate() {
    const isWebKit = /AppleWebKit/.test(navigator.userAgent);
    const isTouchScreen = 'ontouchstart' in window || navigator.maxTouchPoints > 0;
    return isWebKit && isTouchScreen;
  }
}

export default DisableZoomOnInputFocus;
