import { load, store } from "../lib/storage";

const OPT_OUT_IDS_KEY = "confirm-opted-out-ids";

const ConfirmModal = {
  mounted() {
    let confirmEvent = null;

    const titleEl = this.el.querySelector("[data-title]");
    const descriptionEl = this.el.querySelector("[data-description]");
    const confirmIconEl = this.el.querySelector("[data-confirm-icon]");
    const confirmTextEl = this.el.querySelector("[data-confirm-text]");
    const optOutEl = this.el.querySelector("[data-opt-out]");
    const optOutElCheckbox = optOutEl.querySelector("input");

    const optedOutIds = load(OPT_OUT_IDS_KEY) || [];

    this.handleConfirmRequest = (event) => {
      confirmEvent = event;

      const { title, description, confirm_text, confirm_icon, opt_out_id } =
        event.detail;

      if (opt_out_id && optedOutIds.includes(opt_out_id)) {
        liveSocket.execJS(event.target, event.detail.on_confirm);
      } else {
        titleEl.textContent = title;
        descriptionEl.textContent = description;
        confirmTextEl.textContent = confirm_text;

        if (confirm_icon) {
          confirmIconEl.className = `align-middle mr-1 ri-${confirm_icon}`;
        } else {
          confirmIconEl.className = "hidden";
        }

        optOutElCheckbox.checked = false;
        optOutEl.classList.toggle("hidden", !opt_out_id);

        liveSocket.execJS(this.el, this.el.getAttribute("data-js-show"));
      }
    };

    window.addEventListener("lb:confirm_request", this.handleConfirmRequest);

    this.el.addEventListener("lb:confirm", (event) => {
      const { opt_out_id } = confirmEvent.detail;

      if (opt_out_id && optOutElCheckbox.checked) {
        optedOutIds.push(opt_out_id);
        store(OPT_OUT_IDS_KEY, optedOutIds);
      }

      liveSocket.execJS(confirmEvent.target, confirmEvent.detail.on_confirm);
    });
  },

  destroyed() {
    window.removeEventListener("lb:confirm_request", this.handleConfirmRequest);
  },
};

export default ConfirmModal;
