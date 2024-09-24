import Emitter from "../../js/lib/emitter";

test("listener callbacks are called on dispatch", () => {
  const emitter = new Emitter();
  const callback1 = jest.fn();
  const callback2 = jest.fn();

  emitter.addListener(callback1);
  emitter.addListener(callback2);
  emitter.dispatch({ data: 1 });

  expect(callback1).toHaveBeenCalledWith({ data: 1 });
  expect(callback2).toHaveBeenCalledWith({ data: 1 });
});

test("addListener returns a subscription object that can be destroyed", () => {
  const emitter = new Emitter();
  const callback1 = jest.fn();

  const subscription = emitter.addListener(callback1);
  subscription.destroy();
  emitter.dispatch({});

  expect(callback1).not.toHaveBeenCalled();
});
