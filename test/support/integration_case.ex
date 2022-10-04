defmodule Livebook.IntegrationCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use LivebookWeb.ConnCase

      @moduletag :integration
    end
  end

  setup_all do
    Code.require_file("../setup_enterprise.exs", __DIR__)
    :ok
  end
end
