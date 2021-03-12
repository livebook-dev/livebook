import PubSub from "../../js/lib/pub_sub";

describe("PubSub", () => {
  test("subscribed callback is called on the specified topic", () => {
    const pubsub = new PubSub();
    const callback1 = jest.fn();
    const callback2 = jest.fn();

    pubsub.subscribe("topic1", callback1);
    pubsub.subscribe("topic2", callback2);
    pubsub.broadcast("topic1", { data: 1 });

    expect(callback1).toHaveBeenCalledWith({ data: 1 });
    expect(callback2).not.toHaveBeenCalled();
  });

  test("unsubscribed callback is not called on the specified topic", () => {
    const pubsub = new PubSub();
    const callback1 = jest.fn();

    pubsub.subscribe("topic1", callback1);
    pubsub.unsubscribe("topic1", callback1);
    pubsub.broadcast("topic1", {});

    expect(callback1).not.toHaveBeenCalled();
  });
});
