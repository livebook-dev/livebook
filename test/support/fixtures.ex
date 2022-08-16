defmodule Livebook.Fixtures do
  @moduledoc false

  def create_fly(id, attrs \\ %{}) do
    id_attrs = %{id: id}
    attrs = Map.merge(attrs, id_attrs)
    {org_attrs, attrs} = Map.pop(attrs, :organization)
    org_attrs = if org_attrs, do: Map.merge(org_attrs, id_attrs), else: id_attrs

    attrs
    |> Map.merge(%{organization: org_attrs})
    |> fly_fixture()
    |> Livebook.Hubs.save_fly()
  end

  def fly_fixture(attrs \\ %{}) do
    default_fields = %{
      id: Livebook.Utils.random_id(),
      name: "My Personal Hub",
      color: "#FF00FF",
      token: Livebook.Utils.random_cookie()
    }

    {org_attrs, attrs} = Map.pop(attrs, :organization)

    fields = Map.merge(default_fields, attrs)

    organization = fly_organization_fixture(org_attrs)
    fields = Map.put(fields, :organization, organization)

    struct!(Livebook.Hubs.Fly, fields)
  end

  def fly_organization_fixture(attrs \\ %{}) do
    default_fields = %{
      id: Livebook.Utils.random_id(),
      name: "Foo Bar Baz",
      slug: "personal",
      type: "PERSONAL"
    }

    attrs = if attrs, do: attrs, else: %{}
    fields = Map.merge(default_fields, attrs)

    struct!(Livebook.Hubs.Fly.Organization, fields)
  end
end
