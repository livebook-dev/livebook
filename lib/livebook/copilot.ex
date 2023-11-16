defmodule Livebook.Copilot do
  # TODO I noticed this isn't used elsewhere in livebook - should I avoid it?
  require Logger

  # Arguments are passed through straight from the phoneix JS client event
  # for now for rapid prototyping.
  def handle_request(
        %{
          "type" => "completion",
          "context_before_cursor" => pre,
          "context_after_cursor" => suf
        } = request
      ) do
    result_ref = make_ref()
    pid = self()

    {pre, backend, backend_config} = get_copilot_config(pre)
    # TODO this ended up a bit too nested ^^ needs a tidy up. Any tips welcome
    # TODO should this be start_link instead?
    # TODO the model_loaded? and load_model! stuff has race conditions for Nx.Serving creation and ETS table creation / access. Need to have a single process init

    Task.start(fn ->
      try do
        if !apply(backend, :model_loaded?, [backend_config]) do
          send(
            pid,
            {:copilot_response, result_ref, request,
             {:loading_model,
              "Loading copilot model #{backend} with #{inspect(backend_config)} - this may take a while."}}
          )

          {time, _} =
            :timer.tc(fn ->
              backend
              |> apply(:load_model!, [backend_config])
            end)

          send(
            pid,
            {:copilot_model_loaded,
             "Loaded {backend} #{inspect(backend_config)} in #{trunc(time / 1_000)}ms"}
          )
        end

        {time, completion} =
          :timer.tc(fn ->
            backend
            |> apply(:completion, [backend_config, pre, suf])
          end)

        response =
          {:ok, completion,
           %{
             time: trunc(time / 1_000),
             backend: backend
           }}

        send(pid, {:copilot_response, result_ref, request, response})
      rescue
        e ->
          send(
            pid,
            {:copilot_response, result_ref, request,
             {:error, "Copilot error: #{Exception.format_banner(:error, e)}"}}
          )
      end
    end)

    result_ref
  end

  # TODO remove this hack once the feature is shipped
  # During development it's useful to set the copilot config on a per-cell basis
  # To do so, format the first line like this:
  # # copilot_config = %{backend: Livebook.Copilot.OpenaiBackend, backend_config: %{}}
  # If no such string is present (or it can't be parsed) the application config is used
  defp get_copilot_config("# copilot_config =" <> _ = pre) do
    [config_line | rest] = String.split(pre, "\n")
    pre = Enum.join(rest, "\n")

    try do
      parsed_config =
        config_line |> String.split(" = ") |> List.last() |> Code.eval_string() |> elem(0)

      {pre, parsed_config[:backend], parsed_config[:backend_config]}
    rescue
      e ->
        Logger.warning(
          "Can't parse magic config comment - falling back to application config: #{Exception.format_banner(:error, e)}"
        )

        get_copilot_config(pre)
    end
  end

  defp get_copilot_config(pre) do
    # fall back to defaults
    try do
      {pre, Application.get_env(:livebook, Livebook.Copilot)[:backend],
       Application.get_env(:livebook, Livebook.Copilot)[:backend_config] || %{}}
    rescue
      e ->
        Logger.warning(
          "Invalid copilot config - using defaults: #{Exception.format_banner(:error, e)}"
        )

        {pre, Livebook.Copilot.OpenaiBackend, %{}}
    end
  end
end
