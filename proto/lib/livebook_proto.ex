defmodule LivebookProto do
  @moduledoc false

  alias LivebookProto.{Request, Response}

  @request_mapping (for {_id, field_prop} <- Request.__message_props__().field_props,
                        into: %{} do
                      {field_prop.type, field_prop.name_atom}
                    end)

  @response_mapping (for {_id, field_prop} <- Response.__message_props__().field_props,
                         into: %{} do
                       {field_prop.type, field_prop.name_atom}
                     end)

  def build_request_frame(%struct{} = data, id \\ -1) do
    type = request_type(struct)
    message = Request.new!(id: id, type: {type, data})

    {:binary, Request.encode(message)}
  end

  def build_response(%struct{} = data, id \\ -1) do
    type = response_type(struct)
    Response.new!(id: id, type: {type, data})
  end

  defp request_type(module), do: Map.fetch!(@request_mapping, module)
  defp response_type(module), do: Map.fetch!(@response_mapping, module)
end
