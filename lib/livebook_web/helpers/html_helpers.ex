defmodule LivebookWeb.HTMLHelpers do
  use Phoenix.Component

  use LivebookWeb, :verified_routes

  @doc """
  Returns path to specific process dialog within LiveDashboard.
  """
  @spec live_dashboard_process_path(pid) :: String.t()
  def live_dashboard_process_path(pid) do
    pid_str = Phoenix.LiveDashboard.PageBuilder.encode_pid(pid)
    ~p"/dashboard/#{node()}/processes?info=#{pid_str}"
  end

  @doc """
  Returns path to specific node within LiveDashboard.
  """
  @spec live_dashboard_node_path(String.t()) :: String.t()
  def live_dashboard_node_path(node) do
    ~p"/dashboard/#{node}/home"
  end

  @doc """
  Converts human-readable strings to strings which can be used
  as HTML element IDs (compatible with HTML5)

  At the same time duplicate IDs are enumerated to avoid duplicates
  """
  @spec names_to_html_ids(list(String.t())) :: list(String.t())
  def names_to_html_ids(names) do
    names
    |> Enum.map(&name_to_html_id/1)
    |> Enum.map_reduce(%{}, fn html_id, counts ->
      counts = Map.update(counts, html_id, 1, &(&1 + 1))

      case counts[html_id] do
        1 -> {html_id, counts}
        count -> {"#{html_id}-#{count}", counts}
      end
    end)
    |> elem(0)
  end

  defp name_to_html_id(name) do
    name
    |> String.downcase()
    |> String.replace(~r/[^\s\w]/u, "")
    |> String.trim()
    |> String.replace(~r/\s+/u, "-")
    |> case do
      "" -> Base.url_encode64(name, padding: false)
      id -> id
    end
  end

  @doc """
  Returns the text in singular or plural depending on the quantity

  ## Examples

      iex> LivebookWeb.HTMLHelpers.pluralize(1, "notebook is not persisted", "notebooks are not persisted")
      "1 notebook is not persisted"

      iex> LivebookWeb.HTMLHelpers.pluralize(3, "notebook is not persisted", "notebooks are not persisted")
      "3 notebooks are not persisted"

  """
  @spec pluralize(non_neg_integer(), String.t(), String.t()) :: String.t()
  def pluralize(1, singular, _plural), do: "1 #{singular}"
  def pluralize(count, _singular, plural), do: "#{count} #{plural}"

  @doc """
  Formats the given UTC datetime relatively to present.
  """
  @spec format_datetime_relatively(DateTime.t() | NaiveDateTime.t()) :: String.t()
  def format_datetime_relatively(%DateTime{time_zone: "Etc/UTC"} = date) do
    date |> DateTime.to_naive() |> Livebook.Utils.Time.time_ago_in_words()
  end

  def format_datetime_relatively(%NaiveDateTime{} = date) do
    Livebook.Utils.Time.time_ago_in_words(date)
  end

  @doc """
  Returns a list of human readable messages for all upload and upload
  entry errors.
  """
  @spec upload_error_messages(Phoenix.LiveView.UploadConfig.t()) :: list(String.t())
  def upload_error_messages(upload) do
    errors = upload_errors(upload) ++ Enum.flat_map(upload.entries, &upload_errors(upload, &1))
    Enum.map(errors, &upload_error_to_string/1)
  end

  @doc """
  Converts an upload or entry error to string.
  """
  @spec upload_error_to_string(term()) :: String.t()
  def upload_error_to_string(:too_large), do: "Too large"
  def upload_error_to_string(:too_many_files), do: "You have selected too many files"
  def upload_error_to_string(:not_accepted), do: "You have selected an unacceptable file type"

  def upload_error_to_string({:writer_failure, message}) when is_binary(message) do
    Livebook.Utils.upcase_first(message)
  end
end
