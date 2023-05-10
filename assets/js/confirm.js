import { load, store } from "./lib/storage";

const OPT_OUT_IDS_KEY = "confirm-opted-out-ids";

export function loadConfirmOptOutIds() {
  return load(OPT_OUT_IDS_KEY) || [];
}

export function registerGlobalEventHandlersForConfirm() {
  window.addEventListener("phx:add_confirm_opt_out_id", (event) => {
    const optedOutIds = load(OPT_OUT_IDS_KEY) || [];
    const optOutId = event.detail.opt_out_id;
    optedOutIds.push(optOutId);
    store(OPT_OUT_IDS_KEY, optedOutIds);
  });
}
