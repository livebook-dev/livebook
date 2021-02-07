defprotocol LiveBook.Runtime do
  def get_node(runtime)

  # or detach or ?
  def disconnect(runtime)
end
