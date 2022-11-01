defmodule LivebookProto do
  @moduledoc false

  @mapping (for {_id, field_prop} <- LivebookProto.Request.__message_props__().field_props,
                into: %{} do
              {field_prop.type, field_prop.name_atom}
            end)

  def request_type(module), do: Map.fetch!(@mapping, module)
end
