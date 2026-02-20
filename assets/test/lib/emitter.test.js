import { test, expect, vi } from "vitest";
import Emitter from "../../js/lib/emitter";

test("listener callbacks are called on dispatch", () => {
  const emitter = new Emitter();
  const callback1 = vi.fn();
  const callback2 = vi.fn();

  emitter.addListener(callback1);
  emitter.addListener(callback2);
  emitter.dispatch({ data: 1 });

  expect(callback1).toHaveBeenCalledWith({ data: 1 });
  expect(callback2).toHaveBeenCalledWith({ data: 1 });
});

test("addListener returns a subscription object that can be destroyed", () => {
  const emitter = new Emitter();
  const callback1 = vi.fn();

  const subscription = emitter.addListener(callback1);
  subscription.destroy();
  emitter.dispatch({});

  expect(callback1).not.toHaveBeenCalled();
});
