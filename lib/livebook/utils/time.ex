defmodule Livebook.Utils.Time do
  @moduledoc false

  # A simplified version of https://gist.github.com/tlemens/88e9b08f62150ba6082f478a4a03ac52

  @doc """
  Formats the given point in time relatively to present.
  """
  @spec time_ago_in_words(NaiveDateTime.t()) :: String.t()
  def time_ago_in_words(naive_date_time) when is_struct(naive_date_time, NaiveDateTime) do
    now = NaiveDateTime.utc_now()

    if NaiveDateTime.compare(naive_date_time, now) == :gt do
      raise ArgumentError, "expected a datetime in the past, got: #{inspect(naive_date_time)}"
    end

    distance_of_time_in_words(naive_date_time, now)
  end

  @doc """
  Formats time distance between `from_ndt` and `to_ndt`
  as a human-readable string.

  ## Examples

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:15:04])
      "less than 5 seconds"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:15:09])
      "less than 10 seconds"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:15:19])
      "less than 20 seconds"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:15:20])
      "half a minute"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:15:39])
      "half a minute"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:15:40])
      "less than a minute"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:15:59])
      "less than a minute"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:16:00])
      "1 minute"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:16:29])
      "1 minute"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:16:30])
      "2 minutes"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:58:30])
      "44 minutes"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 18:59:30])
      "about 1 hour"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-20 19:59:30])
      "about 2 hours"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-21 18:14:00])
      "about 24 hours"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-21 18:15:00])
      "1 day"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-06-22 18:15:00])
      "2 days"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2020-07-22 18:15:00])
      "about 1 month"

      iex> Livebook.Utils.Time.distance_of_time_in_words(~N[2020-06-20 18:15:00], ~N[2021-08-22 18:15:00])
      "about 14 months"
  """
  @spec distance_of_time_in_words(NaiveDateTime.t(), NaiveDateTime.t()) :: String.t()
  def distance_of_time_in_words(from_ndt, to_ndt)
      when is_struct(from_ndt, NaiveDateTime) and is_struct(to_ndt, NaiveDateTime) do
    duration_seconds = NaiveDateTime.diff(to_ndt, from_ndt)

    {:seconds, duration_seconds}
    |> maybe_convert_to_minutes()
    |> duration_in_words()
  end

  defp maybe_convert_to_minutes({:seconds, seconds}) when seconds > 59 do
    {:minutes, round(seconds / 60)}
  end

  defp maybe_convert_to_minutes(duration), do: duration

  defp duration_in_words({:seconds, seconds}) when seconds in 0..4 do
    "less than 5 seconds"
  end

  defp duration_in_words({:seconds, seconds}) when seconds in 5..9 do
    "less than 10 seconds"
  end

  defp duration_in_words({:seconds, seconds}) when seconds in 10..19 do
    "less than 20 seconds"
  end

  defp duration_in_words({:seconds, seconds}) when seconds in 20..39 do
    "half a minute"
  end

  defp duration_in_words({:seconds, seconds}) when seconds in 40..59 do
    "less than a minute"
  end

  defp duration_in_words({:minutes, minutes}) when minutes == 1 do
    "1 minute"
  end

  defp duration_in_words({:minutes, minutes}) when minutes in 2..44 do
    "#{minutes} minutes"
  end

  defp duration_in_words({:minutes, minutes}) when minutes in 45..89 do
    "about 1 hour"
  end

  # 90 mins up to 24 hours
  defp duration_in_words({:minutes, minutes}) when minutes in 90..1439 do
    "about #{round(minutes / 60)} hours"
  end

  # 24 hours up to 42 hours
  defp duration_in_words({:minutes, minutes}) when minutes in 1440..2519 do
    "1 day"
  end

  # 42 hours up to 30 days
  defp duration_in_words({:minutes, minutes}) when minutes in 2520..43_199 do
    "#{round(minutes / 1440)} days"
  end

  # 30 days up to 45 days
  defp duration_in_words({:minutes, minutes}) when minutes in 43_200..64_799 do
    "about 1 month"
  end

  # 45 days up to 60 days
  defp duration_in_words({:minutes, minutes}) when minutes in 64_800..86_399 do
    "about 2 months"
  end

  defp duration_in_words({:minutes, minutes}) do
    "about #{round(minutes / 43_200)} months"
  end
end
