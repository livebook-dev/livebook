defmodule LivebookProto do
  @event_mapping (for {_id, field_prop} <- LivebookProto.Event.__message_props__().field_props,
                      into: %{} do
                    {field_prop.type, field_prop.name_atom}
                  end)

  @type event_proto ::
          LivebookProto.AgentConnected.t()
          | LivebookProto.AgentJoined.t()
          | LivebookProto.AgentLeft.t()
          | LivebookProto.AppDeploymentStarted.t()
          | LivebookProto.AppDeploymentStopped.t()
          | LivebookProto.FileSystemCreated.t()
          | LivebookProto.FileSystemDeleted.t()
          | LivebookProto.FileSystemUpdated.t()
          | LivebookProto.SecretCreated.t()
          | LivebookProto.SecretDeleted.t()
          | LivebookProto.SecretUpdated.t()
          | LivebookProto.DeploymentGroupCreated.t()
          | LivebookProto.DeploymentGroupDeleted.t()
          | LivebookProto.DeploymentGroupUpdated.t()
          | LivebookProto.UserConnected.t()
          | LivebookProto.UserDeleted.t()

  @doc """
  Builds an event with given data.
  """
  @spec build_event(event_proto()) :: LivebookProto.Event.t()
  def build_event(%struct{} = data) do
    %LivebookProto.Event{type: {event_type(struct), data}}
  end

  defp event_type(module), do: Map.fetch!(@event_mapping, module)
end
