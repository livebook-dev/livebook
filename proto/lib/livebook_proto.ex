defmodule LivebookProto do
  @moduledoc false

  alias LivebookProto.{
    CreateSecretRequest,
    CreateSecretResponse,
    HandshakeRequest,
    HandshakeResponse,
    Request,
    Response
  }

  @request_mapping (for {_id, field_prop} <- Request.__message_props__().field_props,
                        into: %{} do
                      {field_prop.type, field_prop.name_atom}
                    end)

  @response_mapping (for {_id, field_prop} <- Response.__message_props__().field_props,
                         into: %{} do
                       {field_prop.type, field_prop.name_atom}
                     end)

  @type request_proto :: HandshakeRequest.t() | CreateSecretRequest.t()
  @type response_proto :: HandshakeResponse.t() | CreateSecretResponse.t()

  @doc """
  Builds a request frame with given data and id.
  """
  @spec build_request_frame(request_proto(), integer()) :: {:binary, iodata()}
  def build_request_frame(%struct{} = data, id \\ -1) do
    type = request_type(struct)
    message = Request.new!(id: id, type: {type, data})

    {:binary, Request.encode(message)}
  end

  @doc """
  Builds a create secret request struct.
  """
  @spec build_create_secret_request(keyword()) :: CreateSecretRequest.t()
  defdelegate build_create_secret_request(fields), to: CreateSecretRequest, as: :new!

  @doc """
  Builds a handshake request struct.
  """
  @spec build_handshake_request(keyword()) :: HandshakeRequest.t()
  defdelegate build_handshake_request(fields), to: HandshakeRequest, as: :new!

  @doc """
  Builds a response with given data and id.
  """
  @spec build_response(response_proto(), integer()) :: Response.t()
  def build_response(%struct{} = data, id \\ -1) do
    type = response_type(struct)
    Response.new!(id: id, type: {type, data})
  end

  defp request_type(module), do: Map.fetch!(@request_mapping, module)
  defp response_type(module), do: Map.fetch!(@response_mapping, module)
end
