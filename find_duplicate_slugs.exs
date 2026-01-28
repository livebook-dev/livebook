#!/usr/bin/env elixir

# Script to find Livebook notebooks with duplicate slugs
#
# Usage:
#   elixir find_duplicate_slugs.exs <directory>
#
# Example:
#   elixir find_duplicate_slugs.exs ./lib/livebook/notebook/learn

defmodule DuplicateSlugFinder do
  @doc """
  Extracts the slug from a file path.
  The slug is the filename without extension, with underscores replaced by dashes.
  """
  def extract_slug(path) do
    path
    |> Path.basename()
    |> Path.rootname()
    |> String.replace("_", "-")
  end

  @doc """
  Extracts the notebook title from the file content.
  The title is the first level-1 heading (# Title).
  """
  def extract_title(content) do
    case Regex.run(~r/^#\s+(.+)$/m, content) do
      [_, title] -> String.trim(title)
      nil -> "(no title)"
    end
  end

  @doc """
  Finds all .livemd files in the given directory recursively.
  """
  def find_notebooks(directory) do
    Path.join(directory, "**/*.livemd")
    |> Path.wildcard()
  end

  @doc """
  Processes a notebook file and returns its metadata.
  """
  def process_notebook(path) do
    content = File.read!(path)

    %{
      path: path,
      slug: extract_slug(path),
      title: extract_title(content)
    }
  end

  @doc """
  Groups notebooks by slug and filters to only those with duplicates.
  """
  def find_duplicates(notebooks) do
    notebooks
    |> Enum.group_by(& &1.slug)
    |> Enum.filter(fn {_slug, entries} -> length(entries) > 1 end)
    |> Enum.sort_by(fn {slug, _} -> slug end)
  end

  @doc """
  Formats the output for display.
  """
  def format_output(duplicates) do
    if Enum.empty?(duplicates) do
      IO.puts("No duplicate slugs found.")
    else
      IO.puts("Found #{length(duplicates)} slug(s) with duplicates:\n")

      for {slug, notebooks} <- duplicates do
        IO.puts("Slug: #{slug}")
        IO.puts(String.duplicate("-", 40))

        for notebook <- Enum.sort_by(notebooks, & &1.path) do
          IO.puts("  File:  #{notebook.path}")
          IO.puts("  Title: #{notebook.title}")
          IO.puts("")
        end
      end
    end
  end

  def run(directory) do
    unless File.dir?(directory) do
      IO.puts(:stderr, "Error: '#{directory}' is not a valid directory")
      System.halt(1)
    end

    notebooks = find_notebooks(directory)

    if Enum.empty?(notebooks) do
      IO.puts("No .livemd files found in '#{directory}'")
      System.halt(0)
    end

    IO.puts("Scanning #{length(notebooks)} notebook(s) in '#{directory}'...\n")

    notebooks
    |> Enum.map(&process_notebook/1)
    |> find_duplicates()
    |> format_output()
  end
end

# Main entry point
case System.argv() do
  [directory] ->
    DuplicateSlugFinder.run(directory)

  [] ->
    IO.puts(:stderr, "Usage: elixir find_duplicate_slugs.exs <directory>")
    IO.puts(:stderr, "")
    IO.puts(:stderr, "Example:")
    IO.puts(:stderr, "  elixir find_duplicate_slugs.exs ./lib/livebook/notebook/learn")
    System.halt(1)

  _ ->
    IO.puts(:stderr, "Error: Too many arguments")
    IO.puts(:stderr, "Usage: elixir find_duplicate_slugs.exs <directory>")
    System.halt(1)
end
